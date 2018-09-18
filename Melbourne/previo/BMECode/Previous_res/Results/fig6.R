rmse <- read.table("../metrics/RMSE.csv", sep=",", head=F)
rmse = rmse[-1,]
rmse[rmse==-999] = NA

rmse = as.data.frame(t(rmse))
names(rmse) = c("cl", "gr", "rmse")
rmsePY = rmse[complete.cases(rmse), ]
rmsepC = tapply(rmsePY$rmse,rmsePY$cl, mean, na.omit=T)

matlabRMSE <- read.table("fig5.csv", sep=",")




dev.off()
svg(filename="fig6.svg", width = 12, height = 8, pointsize = 12)

plot(3:20, rmsepC,ylab ="mean within cluster RMSE", xlab="# Number of clusters"
     , main="PMF with normalised (per sensor) temp IBRL data"#, ylim = c(3,4.5)
     , col="#00994C", pch = 19)
#axis(1, at = seq(120,1200,120))
with(df, lines(loess.smooth(3:20, rmsepC),col = "#006600", lwd=2.5))

dev.off()



rpBME = read.table("../../realAndPredMatlab.csv", sep=",")
names(rpBME) = c("real", "pred")


rpPMF = read.table("../metrics/predandRealPMF.csv", sep=",", head=T)
names(rpPMF) = c("cluster", "mote", "real", "pred")

head(rpPMF)


rpPMFcl3 = rpPMF[rpPMF$cluster==5,]
rpPMFcl3order = actual[order(actual$mote),]

index = 1:500
plot(index,rpPMFcl3order$real[index], type="l")
lines(index, rpPMFcl3order$pred[index], col="red")

plot(index, rpBME$real[index],type="l")
lines(index, rpBME$pred[index], col="red")

Variety = as.factor(c("a","b","a","b","a","b","a","b","a","b"))
Var1 = runif(10)
Var2 = runif(10)
mydata = as.data.frame(cbind(Variety,Var1,Var2))
library("ggplot2")
ggplot(mydata, aes(Var1, Var2)) + geom_point() + facet_grid(~ Variety)



for (var in unique(mydata$Variety)) {
  dev.new()
  print( ggplot(mydata[mydata$Variety==var,], aes(Var1, Var2)) + geom_point() )
}





