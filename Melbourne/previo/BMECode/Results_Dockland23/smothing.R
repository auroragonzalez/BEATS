df = read.table("nodes.csv", sep=",", head=F)

dat = df$V1
index = which(dat %in% boxplot(dat)$out)

dat = df$V2
index = c(index,which(dat %in% boxplot(dat)$out))
dat = df$V3
index = c(index,which(dat %in% boxplot(dat)$out))
dat = df$V4
index = c(index,which(dat %in% boxplot(dat)$out))

todelete = unique(index)

df2 = df[-todelete,]

write.table(df2, "nodesSoft.csv", row.names = F)

library("data.table")
df3 = t(data.table(df2))
write.table(df3, "nodesSoft2.csv", row.names = F, sep=";", col.names = F)
