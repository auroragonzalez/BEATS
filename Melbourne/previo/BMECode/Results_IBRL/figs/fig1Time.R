df <- read.table("../hardBME2.csv", sep=",", head=F)
names(df)[2:5] = c("MAPE", "RMSE", "CVRMSE", "time")
df = df[1:84,]



#number of clusters = 10

df2 = read.table("../hardPMF22res.csv", sep=",", head=F)
df2[df2 == "NaN"] = NA
df2 = df2[1:85,]

df2 = data.frame(t(df2))
names(df2)[1] = c("cl")

df3 = aggregate(df2[, -c(1)], list(df2$cl), mean, na.rm=T)

df4 = data.frame(V1 =df$V1, RMSE = as.numeric(df3[10,-1]))  

plot(df$V1, df$RMSE,ylab ="RMSE", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, xaxt="n", ylim = c(min(df4$RMSE, df$RMSE), max(df4$RMSE, df$RMSE))
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))
points(df4$V1,df4$RMSE , col="red", pch = 18, xaxt="n")
with(df4, lines(loess.smooth(V1, RMSE),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1, lwd=2)

dev.off()
svg(filename="fig1.svg", width = 12, height = 8, pointsize = 12)

plot(df$V1, df$RMSE,ylab ="RMSE", xlab ="Percentage of missing values"
     , main="Hard sensors temp IBRL data"#, ylim = c(4.003,4.027)
     , col="#00994C", pch = 19, cex = 1.5, xaxt="n", ylim = c(min(df4$RMSE, df$RMSE), max(df4$RMSE, df$RMSE))
)
axis(1, at = seq(1,85,3))
with(df, lines(loess.smooth(V1, RMSE),col = "#006600", lwd=2.5))
points(df4$V1,df4$RMSE , col="red", pch = 18, cex = 1.5, xaxt="n")
with(df4, lines(loess.smooth(V1, RMSE),col = "red", lwd=2.5))

legend("topleft", legend=c("PMF (#cl = 10)", "BME (#hs= 6)"),
       col=c("red", "#00994C"), lty = 1, cex=1.5, lwd=2)

dev.off()
