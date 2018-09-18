###

files= dir("datos/")
forder = as.numeric(sub(".*\\D(\\d+).*", "\\1", paste(" ", files)))
files2 = files[order(forder)]

df = read.table(paste0("datos/",files2[1]), sep=",", head=T)
m = matrix(NA, ncol = 1200, nrow= length(files2))

for(i in 1:length(files2)){
  df = read.table(paste0("datos/",files2[i]), sep=",", head=T)
  m[i,] = df$ocupacion[1:1200]
}

write.table(m, "parking.csv", sep=";", col.names=F, row.names=F)





lists = strsplit(files, "-")
myfun = function(x){
  as.numeric(sub(".*\\D(\\d+).*", "\\1", paste(" ", x)))
}
 
lists = lapply(X = lists, FUN = myfun)      


exp = read.table("expendedores.csv", sep=";", head=T)
exp = exp[,c(1,3,4)]

exp$Latitud = as.numeric(gsub(",", ".", exp$Latitud))
exp$Longitud = as.numeric(gsub(",", ".", exp$Longitud))


myfun2= function(x){
  exp[exp$Id %in% x,]  
}

all = lapply(X = lists, FUN = myfun2)

myfun3= function(x){
  if(nrow(x)>1){
    colMeans(x)
  }else{
    x
  }  
}

all2= lapply(all, myfun3)

final = do.call("rbind", all2)

df = read.table(paste0("datos/",files[1]), sep=",", head=T)
m = matrix(NA, ncol = 1200, nrow= length(files))

for(i in 1:length(files)){
  df = read.table(paste0("datos/",files[i]), sep=",", head=T)
  m[i,] = df$ocupacion[1:1200]
}



library(dtwclust)



hc<- tsclust( m, type = "h", k = 2,seed = 899,
             distance = "sbd", centroid = shape_extraction)
hc

m2 = data.frame(m)
m2$cl = hc@cluster

library("TSclust")
