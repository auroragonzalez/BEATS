df <- read.table("fig10NAchangesNH6.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")
df = df[1:84,]
## op1 

library("ggplot2")
library("lattice")
library("reshape")
dat.m <- melt(df, measure.vars = c("RMSE","time"))
dat.m <- dat.m[,-c(2,3)]
levels(dat.m$variable) = c("RMSE", "time (s)")
xyplot(value~V1|variable, data = dat.m, 
       layout = c(1,(ncol(dat.m))-1), 
       lwd=2, pch=1, type="b",
       scales = list(y = list(relation = "free", rot=0),
                     x = list(at= seq(1,85,3))), xlab =" Percentage of missing values (nh = 6)", ylab="", main="BME with raw temp IBRL data")

dev.off()
svg(filename="fig10-opt1.svg", width = 12, height = 8, pointsize = 12)
xyplot(value~V1|variable, data = dat.m, 
       layout = c(1,(ncol(dat.m))-1), 
       lwd=2, pch=1, type="b",
       scales = list(y = list(relation = "free", rot=0),
                     x = list(at= seq(1,85,3))), xlab ="Percentage of missing values (nh = 6)", ylab="", main="BME with raw temp IBRL data")

dev.off()


## op2

plot(df$V1, df$RMSE,ylab ="RMSE", xlab ="Percentage of missing values (nh = 6)"
, main="BME with raw temp IBRL data"#, ylim = c(4.003,4.027)
, col="#00994C", pch = 19, xaxt="n"
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))


dev.off()
svg(filename="fig10-opt2.svg", width = 12, height = 8, pointsize = 12)
plot(df$V1, df$RMSE,ylab ="RMSE", xlab ="Percentage of missing values (nh = 6)"
     , main="BME with raw temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n"
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))
dev.off()
