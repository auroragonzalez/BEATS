library("data.table")
df = fread("beach-water-quality-automated-sensors-1.csv", sep=",")

names(df) = paste0("V",1:ncol(df))


library(lubridate)
mdy_hms("01/22/2013 11:00:00 PM")
df$datetime = mdy_hms(df$V2)


df = df[,c(1,3,11)]
df$V3 = as.numeric(sub(",", ".", df$V3, fixed = TRUE))

df$V1 = as.factor(df$V1)

uno = df[df$V1==levels(df$V1)[1],]
names(uno)[c(1,2)] = c(paste0("beach",1), paste0("value",1))
dos = df[df$V1==levels(df$V1)[2],]
names(dos)[c(1,2)] = c(paste0("beach",2), paste0("value",2))
mg = merge(uno, dos, by=c("datetime"))

for(i in 3:length(levels(df$V1))){
  tres= df[df$V1==levels(df$V1)[i],]
  names(tres)[c(1,2)] = c(paste0("beach",i), paste0("value",i))
  mg = merge(mg, tres, by=c("datetime"))
}

df2  = mg[-1,c(3,5,7,9,11,13)]

write.table(t(df2), "beach.csv", sep=";", row.names = F, col.names=F)









