df <- read.table("fig5.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")

## op1 

library("ggplot2")
library("lattice")
library("reshape")
dat.m <- melt(df, measure.vars = c("MAPE","RMSE","CVRMSE", "time"))
levels(dat.m$variable) = c("MAPE (%)", "RMSE", "CVRMSE (%)", "time (s)")
xyplot(value~V1|variable, data = dat.m, layout = c(1,(ncol(df))-1), lwd=2, pch=19, type="b",scales = list(y = list(relation = "free", rot=0), x = list(at= seq(120,1200,120))),
xlab ="# Observations per sensor (fixed nh = 3)", ylab="", main="BME with normalised (per sensor) temp IBRL data")

dev.off()
svg(filename="fig5-opt1.svg", width = 12, height = 8, pointsize = 12)
xyplot(value~V1|variable, data = dat.m, layout = c(1,(ncol(df))-1), lwd=2, pch=19, type="b",scales = list(y = list(relation = "free", rot=0), x = list(at= seq(120,1200,120))),
       xlab ="# Observations per sensor (fixed nh = 3)", ylab="", main="BME with normalised (per sensor) temp IBRL data")
dev.off()

## op2

plot(df$V1, df$MAPE,ylab ="MAPE (%)", xlab="# Observations per sensor (fixed nh = 3)"
, main="BME with normalised (per sensor) temp IBRL data"#, ylim = c(3,4.5)
, col="#00994C", pch = 19, xaxt="n")
axis(1, at = seq(120,1200,120))
with(df, lines(loess.smooth(V1, MAPE),col = "#006600", lwd=2.5))


dev.off()
svg(filename="fig5-opt2.svg", width = 12, height = 8, pointsize = 12)
plot(df$V1, df$MAPE,ylab ="MAPE (%)", xlab="# Observations per sensor (fixed nh = 3)"
     , main="BME with normalised (per sensor) temp IBRL data"#, ylim = c(3,4.5)
     , col="#00994C", pch = 19, xaxt="n")
axis(1, at = seq(120,1200,120))
with(df, lines(loess.smooth(V1, MAPE),col = "#006600", lwd=2.5))

dev.off()
