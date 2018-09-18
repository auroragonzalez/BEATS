## HARD##

df <- read.table("../hardBME2.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")
df = df[1:84,]


df2 = read.table("../hardPMF22res.csv", sep=",", head=F)
df2[df2 == "NaN"] = NA
df2 = df2[1:85,]

df2 = data.frame(t(df2))
names(df2)[1] = c("cl")

df3 = aggregate(df2[, -c(1)], list(df2$cl), mean, na.rm=T)

df3$X5[18] = NA
df3$X19[19] = NA
df3$X21[18] = NA
df4 = df3[,-c(1)]
matplot(t(df4[c(1,2,3),]), type = "l", col="red", lwd=2, ylim = c(min(df4, na.rm=T),3.5))
lines(as.numeric(df4[c(4),]), lwd=1.5)
lines(as.numeric(df4[c(5),]), lty = 2, lwd=1.5)
lines(as.numeric(df4[c(6),]), lty = 3, lwd=1.5)

legend("topleft", legend=c("cl1 h", "cl2 h", "cl3 h","cl4 h", "cl5 h", "cl6 h"),
       col=c("red","red","red","black","black","black"), lty = 1:3, cex=1.5, lwd=2)


####soft

df5 = read.table("../softPMF2res.csv", sep=",", head=F)
df5[df5 == "NaN"] = NA
df5 = df5[1:85,]

df5 = data.frame(t(df5))
names(df5)[1] = c("cl")

df6 = aggregate(df5[, -c(1)], list(df5$cl), mean, na.rm=T)
df6$X8[19] = NA
df6$X81[10] = NA
df6$X45[18] = NA
df6$X33[15] = NA

df7 = df6[,-c(1)]


lines(as.numeric(df7[c(1),]), lwd=1.5, col="green")
lines(as.numeric(df7[c(2),]), lty = 2, lwd=1.5, col="green")
lines(as.numeric(df7[c(3),]), lty = 3, lwd=1.5, col="green")

lines(as.numeric(df7[c(4),]), lwd=1.5, col="blue")
lines(as.numeric(df7[c(5),]), lty = 2, lwd=1.5, col="blue")
lines(as.numeric(df7[c(6),]), lty = 3, lwd=1.5, col="blue")




dev.off()
svg(filename="fig3.svg", width = 12, height = 8, pointsize = 12)

matplot(t(df4[c(1,2,3),]), type = "l", col="red", lwd=2, ylim = c(min(df4, na.rm=T),3.5))
lines(as.numeric(df4[c(4),]), lwd=1.5)
lines(as.numeric(df4[c(5),]), lty = 2, lwd=1.5)
lines(as.numeric(df4[c(6),]), lty = 3, lwd=1.5)


lines(as.numeric(df7[c(1),]), lwd=1.5, col="green")
lines(as.numeric(df7[c(2),]), lty = 2, lwd=1.5, col="green")
lines(as.numeric(df7[c(3),]), lty = 3, lwd=1.5, col="green")

lines(as.numeric(df7[c(4),]), lwd=1.5, col="blue")
lines(as.numeric(df7[c(5),]), lty = 2, lwd=1.5, col="blue")
lines(as.numeric(df7[c(6),]), lty = 3, lwd=1.5, col="blue")

legend("topleft", legend=c("cl1 h", "cl2 h", "cl3 h","cl4 h", "cl5 h", "cl6 h"),
       col=c("red","red","red","black","black","black"), lty = 1:3, cex=1.5, lwd=2)


dev.off()