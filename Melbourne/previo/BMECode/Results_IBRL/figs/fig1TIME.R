df <- read.table("hardUseCase1.csv", sep=",", head=F)
df = df[1:73,]

df$V2[which.max(df$V2)] = df$V2[which.max(df$V2)-1]

plot(1:nrow(df), df$V1,ylab ="seconds", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data", cex=1.5#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(df), max(df))
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(1:nrow(df), V1),col = "#006600", lwd=2.5))
points(1:nrow(df),df$V2 , col="red", pch = 18, xaxt="n", cex=1.5)
with(df, lines(loess.smooth(1:nrow(df), V2),col = "red", lwd=2.5))

legend("topright", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1.5, lwd=2)

dev.off()
svg(filename="figUsecase1.svg", width = 12, height = 8, pointsize = 12)
plot(1:nrow(df), df$V1,ylab ="seconds", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data", cex=1.5#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(df), max(df))
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(1:nrow(df), V1),col = "#006600", lwd=2.5))
points(1:nrow(df),df$V2 , col="red", pch = 18, xaxt="n", cex=1.5)
with(df, lines(loess.smooth(1:nrow(df), V2),col = "red", lwd=2.5))

legend("topright", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1.5, lwd=2)
dev.off()
