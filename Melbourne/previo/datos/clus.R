df = read.table("parking2.csv", sep=";")
matplot(t(df), type="l")



library(psych)
data(bfi)
x <- bfi 
plot(hclust(as.dist(1-abs(cor(na.omit(x))))))


hc = hclust(as.dist(1-abs(cor(na.omit(t(df))))))
cl= cutree(hc,10)

matplot(t(df[which(cl==4),]), type="l")



hc = hclust(as.dist(1-(cor(na.omit(t(df))))))
cl2= cutree(hc,10)

matplot(t(df[which(cl2==4),]), type="l")

write.table(cl, "corClus.txt", sep=";", row.names=F, col.names=F)


library("kml")
cldSDQ <- clusterLongData(as.matrix(df))
kml(cldSDQ, nbClusters=5:10)
try(choice(cldSDQ))
CLU = getClusters(cldSDQ,5)
table(CLU)

as.numeric(CLU)