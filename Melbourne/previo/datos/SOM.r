require(tidyverse)
require(magrittr)

library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

data = diamonds 


map_dimension = 20
n_iterations = 1000
recalculate_map = T
recalculate_no_clusters = T
numerics = summarise_all( data, is.numeric ) %>%
  as.logical()
factors = names(data)%>%
  .[!numerics]

numerics = names(data)%>%
  .[numerics]


data_list = list()
distances = vector()

for (fac in factors){
  data_list[[fac]] = kohonen::classvec2classmat( data[[fac]] )
  distances = c(distances, 'tanimoto')
}

data_list[['numerics']] = scale(data[,numerics])
distances = c( distances, 'euclidean')

str(data_list)


names(data_list)
distances

som_grid = kohonen::somgrid(xdim = map_dimension
                            , ydim=map_dimension
                            , topo="hexagonal")



if(recalculate_map == F & file.exists('som.Rdata') == T){
  
  load('som.Rdata')
  
} else{
  
  m = kohonen::supersom( data_list
                         , grid=som_grid
                         , rlen= n_iterations
                         , alpha = 0.05
                         , whatmap = c(factors, 'numerics')
                         , dist.fcts = distances
                         #, user.weights = weight_layers
                         #, maxNA.fraction = .5
  )
  
  save(m, file = 'som.Rdata')
  
}

plot(m, type="changes")
plot(m, type="dist.neighbours")


codes = tibble( layers = names(m$codes)
                ,codes = m$codes ) %>%
  mutate( codes = purrr::map(codes, as_tibble) ) %>%
  spread( key = layers, value = codes) %>%
  apply(1, bind_cols) %>%
  .[[1]] %>%
  as_tibble()

# generate distance matrix for codes
dist_m = dist(codes) %>%
  as.matrix()

# generate seperate distance matrix for map location

dist_on_map = kohonen::unit.distances(som_grid)


#exponentiate euclidean distance by distance on map
dist_adj = dist_m ^ dist_on_map


indexes = c( "kl", "ch")#, "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex",  "sdbw")

if(recalculate_no_clusters == F & file.exists('nbclust.Rdata') == T ){
  
  load('nbclust.Rdata')
  
}else{
  
  results_nb = list()
  
  safe_nb = purrr::safely(NbClust::NbClust)
  
  # we will time the execution time of each step
  times = list()
  
  for(i in 1:length(indexes) ){
    
    t = lubridate::now()
    
    nb = safe_nb(as.dist(dist_adj)
                 , distance = "euclidean"
                 , min.nc = 2              
                 , max.nc = 15             
                 , method = "ward.D2"
                 , index = indexes[i]
    )
    
    results_nb[[i]] = nb
    
    times[[i]]      = lubridate::now()-t
    
    #print(indexes[i]) 
    #print(nb$result$Best.nc)
    
  }
  
  df_clust = tibble( indexes = indexes
                     , times = times
                     , nb = results_nb) %>%
    mutate( results = purrr::map(nb,'result') 
            , error = purrr::map(nb, 'error')
            , is_ok = purrr::map_lgl(error, is_null))
  
  df_clust_success = df_clust %>%
    filter( is_ok ) %>%
    mutate( names      = purrr::map(results, names)
            ,all_index = purrr::map(results, 'All.index')
            ,best_nc   = purrr::map(results, 'Best.nc')
            ,best_nc   = purrr::map(best_nc, function(x) x[[1]])
            ,is_ok     = !purrr::map_lgl(best_nc, is_null)
    ) %>%
    filter(is_ok) %>%
    mutate( best_nc    = purrr::flatten_dbl(best_nc))
  
  save(df_clust_success, file = 'nbclust.Rdata')
}

df_clust_success %>%
  filter(!is_null(best_nc) )%>%
  ggplot( aes(x = as.factor(best_nc))) +
  geom_bar()+
  labs(title = 'Votes on optimal number of clusters\nVotes from Gap and Silhouete\nnot considered'
       ,x     = 'Best No of Clusters')

dist_adj =  dist_m ^ dist_on_map

clust_adj = hclust(as.dist(dist_adj), 'ward.D2')


som_cluster_adj = cutree(clust_adj, 11)
plot(m, type="codes", main = "Clusters", bgcol = col_vector[som_cluster_adj], pchs = NA)
