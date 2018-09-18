df = read.table("All_Result_PMF_True_Acc.csv", sep=",", head=T)



df = data.frame(t(df))
df = df[-1,]


mean = aggregate(df$X3, by = list(df$X1), FUN= mean)
min = aggregate(df$X3, by = list(df$X1), FUN= mean)
max = aggregate(df$X3, by = list(df$X1), FUN= max)
png("./fig/meanExact_Acc")
plot(mean, main="meanExact_Acc", type="l")
points(mean)
dev.off()

png("./fig/minTrueAccX3")
plot(min, main="min TrueAcc X3", type="l")
points(min)
dev.off()

png("./fig/maxTrueAccX3")
plot(max, main="max TrueAcc X3", type="l")
points(max)
dev.off()


mean = aggregate(df$X4, by = list(df$X1), FUN= mean)
min = aggregate(df$X4, by = list(df$X1), FUN= mean)
max = aggregate(df$X4, by = list(df$X1), FUN= max)
png("./fig/meanTrueAccX4")
plot(mean, main="mean TrueAcc X4", type="l")
points(mean)
dev.off()

png("./fig/minTrueAccX4")
plot(min, main="min TrueAcc X4", type="l")
points(min)
dev.off()

png("./fig/maxTrueAccX4")
plot(max, main="max TrueAcc X4", type="l")
points(max)
dev.off()


mean = aggregate(df$X5, by = list(df$X1), FUN= mean)
min = aggregate(df$X5, by = list(df$X1), FUN= mean)
max = aggregate(df$X5, by = list(df$X1), FUN= max)
png("./fig/meanTrueAccX5")
plot(mean, main="mean TrueAcc X5", type="l")
points(mean)
dev.off()

png("./fig/minTrueAccX5")
plot(min, main="min TrueAcc X5", type="l")
points(min)
dev.off()

png("./fig/maxTrueAccX5")
plot(max, main="max TrueAcc X5", type="l")
points(max)
dev.off()



df = read.table("All_Result_PMF_True_RMSE.csv", sep=",", head=T)

df = data.frame(t(df))
df = df[-1,]
names(df)[3:5] = c("val_error", "rmse_data", "max_error")

mean = aggregate(df$val_error, by = list(df$X1), FUN= mean)
min = aggregate(df$val_error, by = list(df$X1), FUN= mean)
max = aggregate(df$val_error, by = list(df$X1), FUN= max)
png("./fig/meanValError")
plot(mean, main="meanValError", type="l")
points(mean)
dev.off()

png("./fig/minValError")
plot(min, main="minValError", type="l")
points(min)
dev.off()

png("./fig/maxValError")
plot(max, main="maxValError", type="l")
points(max)
dev.off()



mean = aggregate(df$rmse_data, by = list(df$X1), FUN= mean)
min = aggregate(df$rmse_data, by = list(df$X1), FUN= mean)
max = aggregate(df$rmse_data, by = list(df$X1), FUN= max)
png("./fig/meanRmse_data")
plot(mean, main="meanRmse_data", type="l")
points(mean)
dev.off()

png("./fig/minRmse_data")
plot(min, main="minRmse_data", type="l")
points(min)
dev.off()

png("./fig/maxRmse_data")
plot(max, main="maxRmse_data", type="l")
points(max)
dev.off()



mean = aggregate(df$max_error, by = list(df$X1), FUN= mean)
min = aggregate(df$max_error, by = list(df$X1), FUN= mean)
max = aggregate(df$max_error, by = list(df$X1), FUN= max)
png("./fig/meanMax_error")
plot(mean, main="meanMax_error", type="l")
points(mean)
dev.off()

png("./fig/minMax_error")
plot(min, main="minMax_error", type="l")
points(min)
dev.off()

png("./fig/maxMax_error")
plot(max, main="maxMax_error", type="l")
points(max)
dev.off()

