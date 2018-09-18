locations = read.table("locations.txt", sep=" ")
locations = locations[-c(5,15,18),]

k = 1
dir.create(paste0(getwd(),"/kmeansResR/k",k), showWarnings = FALSE)
df2 = data.frame(sensor = locations$V1, cl=1)
write.table(df2, paste0(getwd(),"/kmeansResR/k",k,"/groupsk",k,".txt"), row.names=F, sep=" ", col.names=F)

df = locations[,c(2:3)]

for(k in 2:20){
  dir.create(paste0(getwd(),"/kmeansResR/k",k), showWarnings = FALSE)
  cl = kmeans(df, k)
  df2 = data.frame(sensor = locations$V1, cl = cl$cluster)
  write.table(df2, paste0(getwd(),"/kmeansResR/k",k,"/groupsk",k,".txt"), row.names=F, sep=" ", col.names=F)
  
}






locations = read.table("../../datos/expendedores2.csv", sep=";", head=F)

k = 1
dir.create(paste0(getwd(),"/kmeansParkResR/k",k), showWarnings = FALSE)
df2 = data.frame(sensor = locations$V1, cl=rep(1, nrow(locations)))
write.table(df2, paste0(getwd(),"/kmeansParkResR/k",k,"/groupsk",k,".txt"), row.names=F, sep=" ", col.names=F)

df = locations[,c(2:3)]

for(k in 2:20){
  dir.create(paste0(getwd(),"/kmeansParkResR/k",k), showWarnings = FALSE)
  cl = kmeans(df, k)
  df2 = data.frame(sensor = locations$V1, cl = cl$cluster)
  write.table(df2, paste0(getwd(),"/kmeansParkResR/k",k,"/groupsk",k,".txt"), row.names=F, sep=" ", col.names=F)
}