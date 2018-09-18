pmf = read.table("hardPMF2dock.csv",sep=",")
bme = read.table("hardBME2dock.csv", sep=",")

pmf = pmf[1:89,]
bme = bme[1:89,]

plot(1:nrow(bme), bme$V2, ylab ="RMSE", xlab ="percentage of missing data"
     , main="Hard Hum Doklands data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V2, pmf$V1), max(bme$V2, pmf$V1))
)
axis(1, at = seq(1,89,3), labels = seq(1,89,3))
with(bme, lines(loess.smooth(1:nrow(bme), V2),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V1 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V1),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF", "BME (#hs=3)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
svg(filename="fig4dock.svg", width = 12, height = 8, pointsize = 12)
plot(1:nrow(bme), bme$V2, ylab ="RMSE", xlab ="percentage of missing data"
     , main="Hard Hum Doklands data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V2, pmf$V1), max(bme$V2, pmf$V1))
)
axis(1, at = seq(1,89,3), labels = seq(1,89,3))
with(bme, lines(loess.smooth(1:nrow(bme), V2),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V1 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V1),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF", "BME (#hs=3)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)
dev.off()








bme$V4[24] = bme$V4[23]
plot(1:nrow(bme), bme$V4, ylab ="seconds", xlab ="percentage of missing data"
     , main="Hard Hum Doklands data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V4, pmf$V2), max(bme$V4, pmf$V2))
)
axis(1, at = seq(1,89,3), labels = seq(1,89,3))
with(bme, lines(loess.smooth(1:nrow(bme), V4),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V2 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V2),col = "red", lwd=2.5))

legend("topright", legend=c("PMF", "BME (#hs=3)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
svg(filename="fig4dockTIME.svg", width = 12, height = 8, pointsize = 12)
plot(1:nrow(bme), bme$V4, ylab ="seconds", xlab ="percentage of missing data"
     , main="Hard Hum Doklands data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(bme$V4, pmf$V2), max(bme$V4, pmf$V2))
)
axis(1, at = seq(1,89,3), labels = seq(1,89,3))
with(bme, lines(loess.smooth(1:nrow(bme), V4),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V2 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V2),col = "red", lwd=2.5))

legend("topright", legend=c("PMF", "BME (#hs=3)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)
dev.off()
