df <- read.table("fig9NA10pNHchanges.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")

## op1 

library("ggplot2")
library("lattice")
library("reshape")
dat.m <- melt(df, measure.vars = c("RMSE","time"))
dat.m <- dat.m[,-c(2,3)]
levels(dat.m$variable) = c("RMSE", "time (s)")
xyplot(value~V1|variable, data = dat.m, layout = c(1,(ncol(dat.m))-1), lwd=2, pch=1, type="b",scales = list(y = list(relation = "free", rot=0), x = list(at= seq(3,53,4))), xlab =" Maximum # hard sensor as neighbors (10 % missing values)", ylab="", main="BME with raw temp IBRL data")

dev.off()
svg(filename="fig9-opt1.svg", width = 12, height = 8, pointsize = 12)
xyplot(value~V1|variable, data = dat.m, layout = c(1,(ncol(dat.m))-1), lwd=2, pch=1, type="b",scales = list(y = list(relation = "free", rot=0), x = list(at= seq(3,53,4))), xlab =" Maximum # hard sensor as neighbors (10 % missing values)", ylab="", main="BME with raw temp IBRL data")
dev.off()




## op3

df = df[-1,]

plot(df$V1, df$RMSE,ylab ="RMSE", xlab =" Maximum # hard sensor as neighbors (10 % missing values)"
, main="BME with raw temp IBRL data"#, ylim = c(4.003,4.027)
, col="#00994C", pch = 19, xaxt="n"
)
axis(1, at = df$V1)
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))


dev.off()
svg(filename="fig9-opt2.svg", width = 12, height = 8, pointsize = 12)
plot(df$V1, df$RMSE,ylab ="RMSE", xlab =" Maximum # hard sensor as neighbors (10 % missing values)"
     , main="BME with raw temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n"
)
axis(1, at = df$V1)
dev.off()



plot(df$V1, df$RMSE,ylab ="RMSE", xlab =" Maximum # hard sensor as neighbors (10 % missing values)"
     , main="BME with raw temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n"
)
axis(1, at = df$V1)
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))
