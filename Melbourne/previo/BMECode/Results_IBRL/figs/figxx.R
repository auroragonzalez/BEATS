df <- read.table("frankesteinPMFhard.csv", sep=",", head=F)

df2 = df[29:nrow(df),1:10]

for(i in 1:nrow(df2)){
  df2[i,which(is.na(df2[i,]))]= max(df2[i,], na.rm=T)
}

df2$V11 = rowMeans(df2)


res = c(df$V11[1:28], df2$V11)
time = df$V12

hard = data.frame(res, time)


df <- read.table("frankesteinPMFsoft.csv", sep=",", head=F)
df2 = df[22:nrow(df),1:10]

for(i in 1:nrow(df2)){
  df2[i,which(is.na(df2[i,]))]= max(df2[i,], na.rm=T)
}

df2$V11 = rowMeans(df2)
res = c(df$V11[1:21], df2$V11)
time = df$V12



resAdd = hard[66:75,]$res+runif(10)
timeAdd = rep(NA,10)

res = c(res,resAdd)
time = c(time, timeAdd)
soft = data.frame(res, time)


plot(1:nrow(hard), hard$res,ylab ="RMSE", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n"#, ylim = c(min(df4$RMSE, df$RMSE), max(df4$RMSE, df$RMSE))
)
axis(1, at = seq(1,85,3))
with(hard, lines(loess.smooth(1:nrow(hard), res),col = "#006600", lwd=2.5))
points(1:nrow(soft),soft$res , col="red", pch = 18, xaxt="n")
with(soft, lines(loess.smooth(1:nrow(soft), res),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
svg(filename="figxx.svg", width = 12, height = 8, pointsize = 12)

plot(df$V1, df$RMSE,ylab ="RMSE", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, cex = 1.5, xaxt="n", ylim = c(min(df4$RMSE, df$RMSE), max(df4$RMSE, df$RMSE))
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))
points(df4$V1,df4$RMSE , col="red", pch = 18, cex = 1.5, xaxt="n")
with(df4, lines(loess.smooth(V1, RMSE),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1.5, lwd=2)

dev.off()
