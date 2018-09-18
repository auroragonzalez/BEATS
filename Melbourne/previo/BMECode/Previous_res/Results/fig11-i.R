df = read.table("fig11NA10pNUMCLchanges.csv", sep=",")
df[df == "NaN"] = NA

df = data.frame(t(df))
names(df) = c("cl", "time", "rmse")
rmsePMF = tapply(df$rmse, df$cl, mean, na.rm=T)
timePMF = tapply(df$time, df$cl, sum, na.rm=T)


#df2 <- read.table("fig2.csv", sep=",", head=F)
#names(df2)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")


plot(1:20, rmsePMF,ylab ="RMSE", xlab="# clusters"
     , main="PMF per cluster"# vs BME temp IBRL data"#, ylim = c(3,4.5)
     , col="#00994C", pch = 19, xaxt="n")
axis(1, at = 1:20)
with(df, lines(loess.smooth(1:20, rmsePMF),col = "#006600", lwd=2.5))

#points(df2$RMSE[1:20], pch = 20,lwd=1, col="red")
#with(df2, lines(loess.smooth(1:20, df2$RMSE[1:20]),col = "red", lwd=2.5))



dev.off()
svg(filename="fig11.svg", width = 12, height = 8, pointsize = 12)
plot(1:20, rmsePMF,ylab ="RMSE", xlab="# clusters"
     , main="PMF per cluster"# vs BME temp IBRL data"#, ylim = c(3,4.5)
     , col="#00994C", pch = 19, xaxt="n")
axis(1, at = 1:20)
with(df, lines(loess.smooth(1:20, rmsePMF),col = "#006600", lwd=2.5))
dev.off()
