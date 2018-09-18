library("lubridate") # week
f.runningMean = function(X,d){
  Y = c()
  L =  length(X);
  Y=X;
  
  for (i in ((1+floor(d/2)): (L-floor(d/2) )) ){
    Y[i] = mean(X[(1+i-floor(d/2) ): (i+floor(d/2))], na.rm = T)
  }
  
  Y[ 1: floor(d/2) ] = Y[ 1+floor(d/2)]*matrix(1, length(Y[1: floor(d/2)]), length(Y[1: floor(d/2)])) 
  M = matrix(NA, length(Y[(length(Y)- floor(d/2)+1):(length(Y))]), 
             length(Y[(length(Y)- floor(d/2)+1):(length(Y))])) 
    Y[(length(Y)- floor(d/2)+1):(length(Y))] = M
return (Y)  
}

f.weeklyProfile = function(min, maxx, weekDayStart){
  YY = c()
  L = length(maxx);
  Y =  min*rep(1, length(maxx))
  delta = (8-weekDayStart)*24;  # number of hours
  
  for (i in 1:floor(L/24/7) ){
    Y[(i-1)*24*7+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];  # cogemos el lunes de 8 am a 7 pm
    Y[(i-1)*24*7+24+(8:19)+delta] = maxx[(i-1)*24*7+24+(8:19)+delta];
    Y[(i-1)*24*7+2*24+(8:19)+delta] = maxx[(i-1)*24*7+2*24+(8:19)+delta];
    Y[(i-1)*24*7+3*24+(8:19)+delta] = maxx[(i-1)*24*7+3*24+(8:19)+delta];
    Y[(i-1)*24*7+4*24+(8:19)+delta] = maxx[(i-1)*24*7+4*24+(8:19)+delta];    
  }
  
  
  if (L%%(i*24*7)>0){
    print(i)
    Y[(i-1)*24*7+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];
    Y[(i-1)*24*7+24+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];
    Y[(i-1)*24*7+2*24+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];
    Y[(i-1)*24*7+3*24+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];
    Y[(i-1)*24*7+4*24+(8:19)+delta] = maxx[(i-1)*24*7+(8:19)+delta];
    print(i)
  }
  
  # #Remove redundant data
  # if (length(Y)>L){
  #   Y[24*days+1:length(Y)) = NA  
  # }
  YY[1] = Y[1]
  YY[length(Y)] = Y[length(Y)];
  #%Smooth down
  for (i in 2:(length(Y)-1)){
    YY[i] = (Y[i-1]+Y[i+1])/2;
  }
  
  
  return (YY)
  
}


f.rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
f.mae <- function(error)
{
  mean(abs(error))
}

f.mape <- function(actual, predicted){
  mean(abs((actual-predicted)/actual) * 100, na.rm = T)  
}



df = read.table("Elec.csv", sep=",", head=F)

df = df[order(df$V1),]

threshold = 12

f.dateMtoR = function(val){
  as.POSIXct((val - 719529)*86400, origin = "1970-01-01", tz = "UTC")  
}

inic = 736435
fin = 736665

time = seq(f.dateMtoR(inic), f.dateMtoR(fin), by = "5 min")

df$date = f.dateMtoR(df$V1)


elec = approx(x = df$date, 
       y = df$V2, 
       xout = time
      , method = "linear")
 elec = elec$y
 
#library("RcppRoll")
#xx = roll_mean(elec, n=threshold)
#elec = c(rep(xx[2],6),xx, rep(NA,5))

elec = f.runningMean(elec,threshold)



#consW = tapply(df$V1, strftime(df$date,format="%W"), sum)
#initCons = consW[c(15:26,35:47)]




Tout = read.table("Tout.csv",sep=",")
Tout$date = f.dateMtoR(Tout$V1)
tout = approx(x = Tout$date, 
             y = Tout$V2, 
             xout = time
             , method = "linear")


time2 = seq(inic, fin, by = 1/(24*12))
start = c(736427, 736577)
stop = c(736532, 736687)


p1 = which(time2>736427 & time2 <736532)
p2 = which(time2>736577 & time2 <736687)

elec2 = elec[c(p1,p2)]

threshold = 240
#tout = roll_mean(tout$y, n=threshold)
tout = f.runningMean(tout$y, threshold)
threshold = 12;
tops = tout




thc=20.5;
thh=15;

for(i in 1:length(tops)){
  if(is.na(tops[i])){
    tops[i] = 0

  }else{
    if(tops[i]> thh & tops[i]< thc){
      tops[i] = 0
    }else{
      if(tops[i]< thh){
        tops[i] = thh- tops[i]
      }else{
        if(tops[i]> thc){
          tops[i]=tops[i]-thc
        }
        
      }
      
    }
  }
}

tops = 0.8*tops;

tops = tops+5.5;


tops2 = tops;

inc = 12*1;

f.downsampling = function(X,l){
  k=0
  Y=c()
  for(i in 1:length(X)){
    if(i%%l ==0){
      k=k+1;
      Y[k] = X[i];
    }
  }
  return(Y)
}

newdf = data.frame(time2, elec, time)
consWnew = tapply(newdf$elec, strftime(newdf$time,format="%W"), sum)



consWnew = tapply(newdf$elec, strftime(newdf$time,format="%W") , sum)
elecCons = consWnew




timeF = f.downsampling(time2, 12)
toutF = f.downsampling(tout,12)
elecF = f.downsampling(elec,12) #no useo elec2 osea holidays... mal
topsF = f.downsampling(tops,12)

YY= f.weeklyProfile(min = 1.55, maxx = topsF, weekDayStart = 5)


plot(timeF, toutF, ylim = c(0,40))
lines(timeF, 0.0001*elecF, col="blue")
lines(timeF, YY, col="red")




consW = tapply(elecF, strftime(f.dateMtoR(timeF),format="%W"), sum)
constantsReal = rep(consW, table(strftime(f.dateMtoR(timeF),format="%W")))

consWP = tapply(YY, strftime(f.dateMtoR(timeF), format="%W"), sum)
constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF),format="%W")))

plot(timeF, constantsReal, col="black", lwd=4, type="l"
     , ylim = c(min(constantsReal, constantsPred, na.rm=T), max(constantsReal, constantsPred, na.rm=T)))
lines(timeF, constantsPred, col="red", lwd=4)



f.subdatesSetLinesPlot = function(date1, date2){
  lim = c(which(timeF < date1), which(timeF >date2 ))
  
  
  
  consW = tapply(elecF[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
  constantsReal = rep(consW, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))
  
  consWP = tapply(YY[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
  constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))
  
  plot(timeF[lim], constantsReal, col="black", lwd=4, type="l"
       , ylim = c(min(constantsReal, constantsPred, na.rm = T), max(constantsReal, constantsPred, na.rm = T)))
  lines(timeF[lim], constantsPred, col="red", lwd=4)


}

f.subdatesSetPointsPlot = function(date1, date2){
  lim = c(which(timeF < date1), which(timeF >date2 ))
  consW = tapply(elecF[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
  constantsReal = rep(consW, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))
  
  consWP = tapply(YY[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
  constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))
  
  plot(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consW, xlab="", col=1,
       ylim = c(min(constantsReal, constantsPred, na.rm=T), max(constantsReal, constantsPred, na.rm=T)))
  #points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWx, xlab="", col=3)
  points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWP*10000, xlab="", col=2)
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (consW- consWP*10000)[!is.na(consW- consWP*10000)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(consW, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(consW, consWP*10000)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  

  #  xs = as.numeric(names(table(week(f.dateMtoR(timeF[lim])))))[4]
#  ys = mean(consW, na.rm = T)
 xs = (par("usr")[1]+par("usr")[2] ) /2
#   ys = consW[order(-consW)][3]
ys = par("usr")[4]-sep
   text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  
   ys = par("usr")[4]-2*sep
  
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))

  lim2 = c(which(dfx$V1 < date1), which(dfx$V1 >date2 ))
  consWx = tapply(dfx$V2[lim2]*1/6, strftime(dfx$date[lim2],format="%W"), sum)
  constantsRealx = rep(consWx, table(strftime(dfx$date[lim2],format="%W")))
  points(as.numeric(names(table(names(consWx)))),consWx, xlab="", col=3)
  
  
  }

f.subdatesSetPointsPlotREAL = function(date1, date2){
  lim = c(which(timeF < date1), which(timeF >date2 ))
  lim2 = c(which(dfx$V1 < date1), which(dfx$V1 >date2 ))
  consWx = tapply(dfx$V2[lim2]*1/6, strftime(dfx$date[lim2],format="%W"), sum)
  constantsRealx = rep(consWx, table(strftime(dfx$date[lim2],format="%W")))
  
  consWP = tapply(YY[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
  constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))
  
  plot(as.numeric(names(table(names(consWx)))),consWx, xlab="week", col=1,
       ylim = c(min(constantsReal, constantsPred, na.rm=T), max(constantsReal, constantsPred, na.rm=T)))
  #points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWx, xlab="", col=3)
  points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWP*10000, xlab="", col=2)
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (consWx- consWP*10000)[!is.na(consWx- consWP*10000)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(consW, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(consWP*10000, consWx)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  
  
  #  xs = as.numeric(names(table(week(f.dateMtoR(timeF[lim])))))[4]
  #  ys = mean(consW, na.rm = T)
  xs = (par("usr")[1]+par("usr")[2] ) /2
  #   ys = consW[order(-consW)][3]
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  
  ys = par("usr")[4]-2*sep
  
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  

  
  
}

date1 =736519
date2 = 736575


#df1 = dfx[which(week(dfx$date[lim2]) == 17),]


f.dateMtoR(date1)
f.dateMtoR(date2)


dev.off()
f.subdatesSetPointsPlot(date1,date2)
dfx = df[which(df$V1 > inic & df$V1 < fin),]
f.subdatesSetPointsPlotREAL(date1,date2)




date1 =736519
date2 = 736585
f.dateMtoR(date1)
f.dateMtoR(date2)

f.subdatesSetPointsPlot(date1,date2)


dev.off()
png("myplotreal")
f.subdatesSetPointsPlotREAL(date1,date2)
dev.off()




#### SECOND PHASE



lim = c(which(timeF < date1), which(timeF >date2 ))
lim2 = c(which(dfx$V1 < date1), which(dfx$V1 >date2 ))


consREAL = tapply(dfx$V2[lim2]*1/6, as.Date(strftime(dfx$date[lim2],format="%d/%m/%Y"),format="%d/%m/%Y"), sum)


dd = data.frame(toutF, timeF, date = f.dateMtoR(timeF))

dd2= dd[dd$timeF<(date1+1),]
dd3= dd[dd$timeF>date2,]
dd = rbind(dd2,dd3)

dd= dd[-c((nrow(dd)-23):nrow(df)),]

'%!in%' <- function(x,y)!('%in%'(x,y))

missing = which(as.Date(names(consREAL))%!in%as.Date(f.dateMtoR(dd$timeF)))
consREAL = consREAL[-c(missing)]

dd4 = dd[-which(as.POSIXlt(dd$date)$wday %in% c(6,0)),]
weekends = which(as.Date(names(consREAL))%!in%as.Date(f.dateMtoR(dd4$timeF)))
consREAL = consREAL[-c(weekends)]


# Tienes que quitar también las vacaciones del TOUT leñee


m = matrix(dd4$toutF, ncol=24, byrow = T)
m = m[-missing,]
m = cbind(m, consREAL)
m2 = na.omit(as.data.frame(m))


library("caret")
dfxx = m2


outl = which(m2$consREAL %in% boxplot(m2$consREAL)$out)

dfxx = dfxx[-outl,]

set.seed(23)
nTraining <- as.integer(nrow(dfxx) * 0.75)
indices <- sample(1:nrow(dfxx), nTraining)


training <- dfxx[indices,]
test <- dfxx[-indices,]



trainingData <- training
testData <- test




set.seed(23)

paramGrid <- expand.grid(.C = c(1:15,18,20,22))


# Use t-times k-fold validation, with t = 5 and k = 10 (by default)
ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")

# Find the optimum combination of parameters
svm.final <- caret::train(
  consREAL~., # We want to predict Temp_Comfort3B according to the predictors
  data = trainingData, # Inputs + Outputs
  method = "svmRadialCost",
  metric = "RMSE", # Metric to evaluate
  tuneGrid = paramGrid, # Parameters for tunning
  trControl = ctrl, # Parameters of control    
  preProc = c("center", "scale")  
)



svm.final.prediction <- predict(svm.final, testData[,-25])
real = testData[,25]
dif = (real-svm.final.prediction)

f.rmse(dif)
cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
cvrmse 
f.mae(dif)
mape = f.mape(svm.final.prediction,real)
mape





plot(1:length(real), real)
lines(1:length(svm.final.prediction), svm.final.prediction, col="red")






svm.final.prediction <- predict(svm.final, dfxx[-25,])
real = dfxx[,25]
dif = (real-svm.final.prediction)

f.rmse(dif)
cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
cvrmse 
f.mae(dif)
mape = f.mape(svm.final.prediction,real)
mape


plot(1:length(real), real)
lines(1:length(svm.final.prediction), svm.final.prediction, col="red")









consW = tapply(elecF[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
#constantsReal = rep(consW, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))




datesInclW = levels(factor(as.Date(strftime(dfx$date[lim2],format="%d/%m/%Y"),format="%d/%m/%Y")))

probs <- predict(svm.final, m2[,-25])
ind <- weekends
val <- c( probs, rep(1.5,length(ind)) )
id  <- c( seq_along(probs), ind+0.5 )
prediction = val[order(id)]

dates = datesInclW[-c(length(datesInclW), length(datesInclW)-1)]

consWP = tapply(prediction, strftime(dates,format="%W"), sum)

consW = consW[-c(14:22,)]

consWP= consWP[1:22]
consW= consW[1:22]

#constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))

plot(as.numeric(names(consW)),consW, xlab="", col=1,
     ylim = c(min(constantsReal, constantsPred, na.rm=T), max(constantsReal, constantsPred, na.rm=T)))
#points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWx, xlab="", col=3)
points(as.numeric(names(consWP)),consWP, xlab="", col=2)
legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)

dif = (consW- consWP)[!is.na(consW- consWP)]
f.rmse(dif)
cvrmse = f.rmse(dif) / mean(consW, na.rm = T)*100
cvrmse 
f.mae(dif)
mape = f.mape(consW, consWP)
mape

sep = (par("usr")[3] + par("usr")[4])/30


#  xs = as.numeric(names(table(week(f.dateMtoR(timeF[lim])))))[4]
#  ys = mean(consW, na.rm = T)
xs = (par("usr")[1]+par("usr")[2] ) /2
#   ys = consW[order(-consW)][3]
ys = par("usr")[4]-sep
text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))

ys = par("usr")[4]-2*sep

text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))

lim2 = c(which(dfx$V1 < date1), which(dfx$V1 >date2 ))
consWx = tapply(dfx$V2[lim2]*1/6, strftime(dfx$date[lim2],format="%W"), sum)
constantsRealx = rep(consWx, table(strftime(dfx$date[lim2],format="%W")))
points(as.numeric(names(table(names(consWx)))),consWx, xlab="", col=3)







min = 1.55
weekDayStart=5
maxx = predict(svm.final, m2[,-25])
YY = c()
L = length(maxx);
Y =  min*rep(1, length(maxx))
delta = (8-weekDayStart)*24;  # number of hours





consWP = tapply(YY[lim], strftime(f.dateMtoR(timeF[lim]),format="%W"), sum)
constantsPred = rep(consWP*10000, table(strftime(f.dateMtoR(timeF[lim]),format="%W")))

plot(as.numeric(names(table(names(consWx)))),consWx, xlab="week", col=1,
     ylim = c(min(constantsReal, constantsPred, na.rm=T), max(constantsReal, constantsPred, na.rm=T)))
#points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWx, xlab="", col=3)
points(as.numeric(names(table(strftime(f.dateMtoR(timeF[lim]),format="%W")))),consWP*10000, xlab="", col=2)
legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)

dif = (real-svm.final.prediction)

f.rmse(dif)
cvrmse = f.rmse(dif) / mean(consW, na.rm = T)*100
cvrmse 
f.mae(dif)
mape = f.mape(svm.final.prediction,real)
mape

sep = (par("usr")[3] + par("usr")[4])/30


#  xs = as.numeric(names(table(week(f.dateMtoR(timeF[lim])))))[4]
#  ys = mean(consW, na.rm = T)
xs = (par("usr")[1]+par("usr")[2] ) /2
#   ys = consW[order(-consW)][3]
ys = par("usr")[4]-sep
text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))

ys = par("usr")[4]-2*sep

text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))







time = seq(f.dateMtoR(inic), f.dateMtoR(fin), by = "1 h")

Tout = read.table("Tout.csv",sep=",")
Tout$date = f.dateMtoR(Tout$V1)
tout = approx(x = Tout$date, 
              y = Tout$V2, 
              xout = time
              , method = "linear")


elec2 = elec[c(p1,p2)]

threshold = 240
#tout = roll_mean(tout$y, n=threshold)
tout = f.runningMean(tout$y, threshold)
