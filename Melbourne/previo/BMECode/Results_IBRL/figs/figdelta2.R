pmf = read.table("softPMF2nowFinalDeltas.csv",sep=",")
bme = read.table("softBME2nowFinalDeltas.csv", sep=",")

#pmf = pmf[2:16,]
#bme = bme[2:16,]


dev.off()
svg(filename="figdelta2.svg", width = 12, height = 8, pointsize = 12)
plot(1:nrow(bme), bme$V1, ylab ="RMSE", xlab ="delta"
     , main="Soft humidity Docklands data, 10% missing"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, cex=1.5, xaxt="n", ylim = c(min(bme$V1, pmf$V1), max(bme$V1, pmf$V1))
)


axis(1, at = seq(1,15,1), labels = seq(3,10,0.5))
with(bme, lines(loess.smooth(1:nrow(bme), V1),col = "#006600", lwd=2.5))
points(1:nrow(pmf),pmf$V1, cex=1.5 , col="red", pch = 18, xaxt="n")
with(pmf, lines(loess.smooth(1:nrow(pmf), V1),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF", "BME (#ss = 3)"),
       col=c("red", "#00994C"), lty = 1, cex=1.5, lwd=2)

dev.off()
