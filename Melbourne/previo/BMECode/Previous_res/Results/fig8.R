df <- read.table("fig8.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")

df = df[2:30,]

## op2

plot(df$V1, df$RMSE,ylab ="RMSE", xlab="Percentage of missing values (fixed nh = 3)"
, main="BME with raw temp IBRL data", col="#00994C", pch = 19)
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))


dev.off()
svg(filename="fig8-opt2.svg", width = 12, height = 8, pointsize = 12)
plot(df$V1, df$MAPE,ylab ="MAPE (%)", xlab="Percentage of missing values (fixed nh = 3)"
     , main="BME with raw temp IBRL data", ylim = c(3,4.5)
     , col="#00994C", pch = 19, xaxt="n")
axis(1, at = seq(120,1200,120))
with(df, lines(loess.smooth(V1, MAPE),col = "#006600", lwd=2.5))

dev.off()
