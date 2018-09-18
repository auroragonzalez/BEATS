source("outlierDetec.R")

df = read.table("02.aggConsPerHour.csv", head=T, sep=";")
df$energy[which(DoubleMADsFromMedian(df$energy) > 10)] = NA

write.table(df, "03.aggConsPerHourOutliersNA.csv", sep=";", row.names=F)
