pmf = read.table("PMFsoftDeltas.csv",sep=",")
bme = read.table("BMEsoftDeltas.csv", sep=",")

pmf = pmf[2:16,]
bme = bme[2:16,]

plot(1:nrow(bme), bme$V1, ylab ="RMSE", xlab ="delta"
     , main="Hard & soft temp IBRL data, 10% missing"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V1, pmf$V1), max(bme$V1, pmf$V1))
)
axis(1, at = seq(1,20,1), labels = seq(0.1,2,0.1))
with(bme, lines(loess.smooth(1:nrow(bme), V1),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V1 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V1),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 3, #ss = 2)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
svg(filename="figxx2.svg", width = 12, height = 8, pointsize = 12)
plot(1:nrow(bme), bme$V1, ylab ="RMSE", xlab ="delta"
     , main="Hard & soft temp IBRL data, 10% missing"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V1, pmf$V1), max(bme$V1, pmf$V1))
)
axis(1, at = seq(1,20,1), labels = seq(0.1,2,0.1))
with(bme, lines(loess.smooth(1:nrow(bme), V1),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V1 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V1),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 3, #ss = 2)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
