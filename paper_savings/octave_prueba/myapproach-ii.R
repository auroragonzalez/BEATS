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







#####
# THIRD PHASE
#####

df = read.table("13.reduced.csv", sep=";", head=T)
df$date2 = as.Date(df$date, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")

lim = c(which(df$date2 < date1), which(df$date2 >date2 ))

dfx = df[lim,]
HOLIDAYS = seq(as.Date(date1), as.Date(date2), "day")
dfxx = dfx[dfx$holiday==0,]

HOLIDAYS= c(HOLIDAYS,unique(dfx[dfx$holiday==1,]$date2))

#consREAL = tapply(dfx$energy[dfx$holiday==0], dfx$date2[dfx$holiday==0], sum)
#consREAL = tapply(dfx$energy[dfx$holiday==1], dfx$date2[dfx$holiday==1], sum)



dd = data.frame(energy = dfxx$energy, toutF = dfxx$stMU62_IMI_tmed, date = dfxx$date, dow = dfxx$dow, date2 = dfxx$date2)


dd4 = dd[-which(as.POSIXlt(dd$date)$wday %in% c(6,0)),]


weekends = which(as.POSIXlt(dd$date)$wday %in% c(6,0))

incomp=c()
a = tapply(dd4$energy, dd4$date2,list)
for(i in 1:length(a)){
  if(length(a[[i]])!=24){
    incomp = c(i, incomp)
  }
}

datesToDelete = names(a[incomp])
dd4 = dd4[-which(dd4$date2 %in% as.Date(datesToDelete)),]

DISCARDED = as.Date(datesToDelete)

m = matrix(dd4$toutF, ncol=24, byrow = T)

consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, energy = consREAL)

inic = 736435
fin = 736665

index = rownames(m) > f.dateMtoR(inic) & rownames(m) < f.dateMtoR(fin)
DISCARDED = c(DISCARDED, names(consREAL[!index]))

m = m[index,]

library("caret")

f.allprocess = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}

svm.total.predictionOUT =f.allprocess(m)


# Lo comentamos, este es el modelo de Alfonso, ambos errores +1
# dd1 = data.frame(dates=f.dateMtoR(timeF), temp = toutF)
# dd1$dates2 = as.Date(format(dd1$dates, "%Y-%m-%d"))
# dd2 = dd1[which(dd1$dates2 %in% as.Date(names(consREAL))),]
# which(table(dd2$dates2)!=24) #ninguno, por lo tanto son 24 h para cada día
# m = matrix(dd2$temp, ncol=24, byrow = T)
# m = cbind(m, energy = consREAL[which(as.Date(names(consREAL)) %in% dd2$dates2)])
# datesIN = names(consREAL[which(as.Date(names(consREAL)) %in% dd1$dates2)])
# 
# svm.total.predictionOUT = f.allprocess(na.omit(m))
# 

dates = as.Date(svm.total.predictionOUT$date)
which(dates %in% as.Date(HOLIDAYS))

minx = 1056

consREAL = tapply(dd$energy, as.character(dd$date2), sum)
time2 = seq(dates[1], dates[length(dates)], by = "day")

aux1 = data.frame(time2)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2")



prediction = data.frame(time2 = time2,pred = rep(minx, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val
prediction[which(prediction$time2 %in% as.Date(HOLIDAYS)),]$pred = NA
prediction[which(prediction$time2 %in% as.Date(DISCARDED)),]$pred = NA

auxF = merge(aux3,prediction, by = "time2", all = T)



f.pinta= function(time , pred,real){
  plot(time, real, col="black", ylim = c(min(real, pred, na.rm = T), max(real,pred, na.rm = T)))
  points(time, pred, col="red")
  
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)
data.frame(unique(week), predW, realW)

## weekly


realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")


f.pinta(time= unique(week), pred= predW, real = realW)
table(strftime(auxF$time2,format="%W"))


# hasta aquí lo habíamos hecho SIN holidays
# which(table(strftime(auxF$time2,format="%W"))!=7)
# auxF$wday = strftime(auxF$time2,format="%W")
# #¿hay menos de 7 (o justo 6) porque uno de esos dias fue holiday? Siii...
# 
# table(dfx[dfx$holiday==1,]$date2)
# which(dfx[dfx$holiday==1,]$date2 =="2016-03-30")
# aux5 = dfx[dfx$holiday==1,][which(dfx$date2 !="2016-03-30"),]
# consHoliday = tapply(aux5$energy, aux5$date2, sum)
# meanHoliday = mean(consHoliday)
# 
# auxF$pred[which( auxF$time2%in% unique(dfx[dfx$holiday==1,]$date2))] = meanHoliday
# 
# consREALwithHold = tapply(dfx$energy, as.character(dfx$date2), sum)
# which(as.Date(names(consREALwithHold)) %in% auxF$date2)
# 
# table(na.omit(auxF)$wday)
# 







consREAL = tapply(dfx$energy, as.character(dfx$date2), sum)
time2 = seq(dates[1], dates[length(dates)], by = "day")

aux1 = data.frame(time2)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2")




prediction = data.frame(time2 = time2,pred = rep(minx, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val
#deleteI = time[c(which(time %in% as.Date(HOLIDAYS)),which(time %in% as.Date(DISCARDED)))]

prediction[which(prediction$time2 %in% as.Date(HOLIDAYS)),]$pred = NA
prediction[which(prediction$time2 %in% as.Date(DISCARDED)),]$pred = NA

auxF = merge(aux3,prediction, by = "time2", all = T)
auxF$wday = strftime(auxF$time2,format="%W")
auxF$pred[which( auxF$time2%in% unique(dfx[dfx$holiday==1,]$date2))] = meanHoliday
auxF = na.omit(auxF)


f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")


f.pinta(time= unique(week), pred= predW, real = realW)

auxF2 = auxF[which(!auxF$wday%in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)






















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




f.dateMtoR = function(val){
  as.POSIXct((val - 719529)*86400, origin = "1970-01-01", tz = "UTC")  
}

#####
# FOURTH PHASE (working, already to write something)
#####

df = read.table("13.reduced.csv", sep=";", head=T)
#df$date2 = as.Date(df$date, format="%Y-%m-%d")
df$date2 = as.Date(df$time, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
HOLIDAYS= which(df$holiday==1)
WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed, date = df$date, dow = df$dow, date2 = df$date2)

deleteIndex = c(SUMMER, HOLIDAYS, WEEKENDS, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, energy = consREAL)

library("caret")

f.allprocess = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}


svm.total.predictionOUT =f.allprocess(m)


# Lo comentamos, este es el modelo de Alfonso, ambos errores +1
# dd1 = data.frame(dates=f.dateMtoR(timeF), temp = toutF)
# dd1$dates2 = as.Date(format(dd1$dates, "%Y-%m-%d"))
# dd2 = dd1[which(dd1$dates2 %in% as.Date(names(consREAL))),]
# which(table(dd2$dates2)!=24) #ninguno, por lo tanto son 24 h para cada día
# m = matrix(dd2$temp, ncol=24, byrow = T)
# m = cbind(m, energy = consREAL[which(as.Date(names(consREAL)) %in% dd2$dates2)])
# datesIN = names(consREAL[which(as.Date(names(consREAL)) %in% dd1$dates2)])
# 
# svm.total.predictionOUT = f.allprocess(na.omit(m))
# 

dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL = tapply(dd$energy, as.character(dd$date2), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 1056
holx = 1300# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF = merge(aux3,prediction, by = "time2")
auxF = auxF[-which(auxF$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime


f.pinta= function(time , pred,real){
  plot(time, real, col="black")
  points(time, pred, col="red")
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)


dev.off()
png("./pics_res/daily_24h", width=800, height = 600)
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)
dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

dev.off()
png("./pics_res/weekFULL_24h", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW)
dev.off()

# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

dev.off()
png("./pics_res/week_24h", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW)
dev.off()


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


f.dateMtoR = function(val){
  as.POSIXct((val - 719529)*86400, origin = "1970-01-01", tz = "UTC")  
}

#####
# FIFTH PHASE (representación, NO HA IDO MEJOR)
#####

df = read.table("13.reduced.csv", sep=";", head=T)
#df$date2 = as.Date(df$date, format="%Y-%m-%d")
df$date2 = as.Date(df$time, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
HOLIDAYS= which(df$holiday==1)
WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed, date = df$date, dow = df$dow, date2 = df$date2)

deleteIndex = c(SUMMER, HOLIDAYS, WEEKENDS, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)


# fft: complex numbers

# dtw: does not improve
# library("wavelets")
# sc = m
# wtData <- NULL
# for (i in 1:nrow(sc)) {
#   a <- (sc[i,])
#   wt <- dwt(a, filter="haar", boundary="periodic")
#   wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
#   }
# wtData <- as.data.frame(wtData)
# m = wtData


# SAX: many of them have same variance, difficult to say... not applicable
# library("TSclust")  # PAA in f.toSAX
# f.toSAX <- function(sc, w, alpha){
#   saxData <- NULL
#   for (i in 1:nrow(sc)) {
#     a <- t(sc[i,-1])
#     x <- (a - mean(a)) /sd(a)
#     paax <- PAA(x, w) #generate PAA reductions
#     SAXx <- convert.to.SAX.symbol( paax, alpha)
#     saxData = rbind(saxData, SAXx)
#   }
#   rownames(saxData) = 1:nrow(sc)
#   return(as.data.frame(saxData))
# }
# alpha = 6
# m = f.toSAX(m,w= 23,alpha)

consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, energy = consREAL)

library("caret")

f.allprocess = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  
  inputs_train <- subset(trainNormalized,select = -c(energy))
  inputs_test <- subset(testNormalized,select = -c(energy))
  
  
  output_train <- trainingData$EaparcialSUM
  output_test <- testData$EaparcialSUM
  
  # Applying PCA with the required number of components to get the 98% of confidence
  
  set.seed(123)
  transformador.pca <- preProcess(
    inputs_train,
    method = c("pca"),
    thres = 0.98
  )
  
  if(PCA){
    train.pca <- predict(transformador.pca, inputs_train)
    test.pca <- predict(transformador.pca, inputs_test)
    
  }
  
  
  
  
  
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}

m = as.matrix(m)
svm.total.predictionOUT =f.allprocess(m)


# Lo comentamos, este es el modelo de Alfonso, ambos errores +1
# dd1 = data.frame(dates=f.dateMtoR(timeF), temp = toutF)
# dd1$dates2 = as.Date(format(dd1$dates, "%Y-%m-%d"))
# dd2 = dd1[which(dd1$dates2 %in% as.Date(names(consREAL))),]
# which(table(dd2$dates2)!=24) #ninguno, por lo tanto son 24 h para cada día
# m = matrix(dd2$temp, ncol=24, byrow = T)
# m = cbind(m, energy = consREAL[which(as.Date(names(consREAL)) %in% dd2$dates2)])
# datesIN = names(consREAL[which(as.Date(names(consREAL)) %in% dd1$dates2)])
# 
# svm.total.predictionOUT = f.allprocess(na.omit(m))
# 

dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL = tapply(dd$energy, as.character(dd$date2), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 1056
holx = 1300# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF = merge(aux3,prediction, by = "time2")
auxF = auxF[-which(auxF$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime


f.pinta= function(time , pred,real){
  plot(time, real, col="black")
  points(time, pred, col="red")
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)



#####
# SIXTH PHASE: every 8 hours. WORKS (see graphs not very much improvement, it is relative)
#####

df = read.table("13.reduced.csv", sep=";", head=T)
df$hour = as.POSIXlt(df$date)$hour

df$moment = NA
df$moment[df$hour %in% 7:14] = 1
df$moment[df$hour %in% 15:21] = 2
df$moment[df$hour %in% c(22,23,0,1:6)] = 3

df$date2 = as.Date(df$time, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
HOLIDAYS= which(df$holiday==1)
WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed,
                date = df$date, dow = df$dow, date2 = df$date2
                , moment = df$moment)

deleteIndex = c(SUMMER, HOLIDAYS, WEEKENDS, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)

#morning

consREAL1 = tapply(dd4$energy[dd4$moment==1], as.character(dd4$date2[dd4$moment==1]), sum)
m = cbind(m, energy = consREAL1)

library("caret")

f.allprocess = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}

m = as.matrix(m)
svm.total.predictionOUT =f.allprocess(m)


dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL1 = tapply(dd$energy[dd$moment==1], as.character(dd$date2[dd$moment==1]), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL1)), consREAL1)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 320
holx = 600# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF1 = merge(aux3,prediction, by = "time2")
auxF1 = auxF1[-which(auxF1$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime


f.pinta= function(time , pred,real){
  plot(time, real, col="black")
  points(time, pred, col="red")
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}
f.pinta(time= auxF1$time2, pred= auxF1$pred, real = auxF1$consREAL1)


#afternoon

consREAL2 = tapply(dd4$energy[dd4$moment==2], as.character(dd4$date2[dd4$moment==2]), sum)
m = matrix(dd4$toutF, ncol=24, byrow = T)
m = cbind(m, energy = consREAL2)

m = as.matrix(m)
svm.total.predictionOUT =f.allprocess(m)


dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL2 = tapply(dd$energy[dd$moment==2], as.character(dd$date2[dd$moment==2]), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL2)), consREAL2)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 300
holx = 600# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF2 = merge(aux3,prediction, by = "time2")
auxF2 = auxF2[-which(auxF2$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime

f.pinta(time= auxF2$time2, pred= auxF2$pred, real = auxF2$consREAL2)



#night

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
#HOLIDAYS= which(df$holiday==1)
#WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed,
                date = df$date, dow = df$dow, date2 = df$date2
                , moment = df$moment)

deleteIndex = c(SUMMER, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)


consREAL3 = tapply(dd4$energy[dd4$moment==3], as.character(dd4$date2[dd4$moment==3]), sum)
m = matrix(dd4$toutF, ncol=24, byrow = T)
m = cbind(m, energy = consREAL3)

m = as.matrix(m)
svm.total.predictionOUT =f.allprocess(m)


dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL3 = tapply(dd$energy[dd$moment==3], as.character(dd$date2[dd$moment==3]), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL3)), consREAL3)
aux3 = merge(aux1,aux2, by="time2", all=T)


#minx = 300
#holx = 600# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

#prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
#prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF3 = merge(aux3,prediction, by = "time2")
auxF3 = auxF3[-which(auxF3$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime

f.pinta(time= auxF3$time2, pred= auxF3$pred, real = auxF3$consREAL3)


## daily
names(auxF1)= c("time2", "consREAL", "pred")
names(auxF2) = names(auxF1)
names(auxF3)= names(auxF1)

auxF1$cte = 1
auxF2$cte = 2
auxF3$cte = 3

auxFtot = rbind(auxF1, auxF2, auxF3)
auxFtot$time2 = as.Date(auxFtot$time2)
auxFtot = auxFtot[order(auxFtot$time2),]

f.pinta(time = as.Date(names(pred)), 
        pred = tapply(auxFtot$pred, auxFtot$time2, sum)
        , real=tapply(auxFtot$consREAL, auxFtot$time2, sum))


dev.off()
png("./pics_res/daily_3moments", width=800, height = 600)
f.pinta(time = as.Date(names(pred)), 
        pred = tapply(auxFtot$pred, auxFtot$time2, sum)
        , real=tapply(auxFtot$consREAL, auxFtot$time2, sum))

dev.off()


auxF = data.frame(time2 = as.Date(names(pred)), consREAL =  tapply(auxFtot$consREAL, auxFtot$time2, sum),
                  pred = tapply(auxFtot$pred, auxFtot$time2, sum))
## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

dev.off()
png("./pics_res/weekFULL_3moments", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW)
dev.off()


# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
dev.off()
png("./pics_res/week_3moments", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW)
dev.off()







#####
# SEVENTH PHASE: add dow FUNCIONA!
#####

df = read.table("13.reduced.csv", sep=";", head=T)
#df$date2 = as.Date(df$date, format="%Y-%m-%d")
df$date2 = as.Date(df$time, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
HOLIDAYS= which(df$holiday==1)
WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed, date = df$date, dow = df$dow, date2 = df$date2)

deleteIndex = c(SUMMER, HOLIDAYS, WEEKENDS, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
dowe = matrix(dd4$dow, ncol=24, byrow=T)[,1]
m = cbind(m, dow = dowe, energy = consREAL)

dowe2 = dowe
dowe2[dowe2 !=6] = 1
dowe2[dowe2 ==6] = 2

m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, dow = dowe2, energy = consREAL)

library("caret")

f.allprocess = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}


svm.total.predictionOUT =f.allprocess(m)


# Lo comentamos, este es el modelo de Alfonso, ambos errores +1
# dd1 = data.frame(dates=f.dateMtoR(timeF), temp = toutF)
# dd1$dates2 = as.Date(format(dd1$dates, "%Y-%m-%d"))
# dd2 = dd1[which(dd1$dates2 %in% as.Date(names(consREAL))),]
# which(table(dd2$dates2)!=24) #ninguno, por lo tanto son 24 h para cada día
# m = matrix(dd2$temp, ncol=24, byrow = T)
# m = cbind(m, energy = consREAL[which(as.Date(names(consREAL)) %in% dd2$dates2)])
# datesIN = names(consREAL[which(as.Date(names(consREAL)) %in% dd1$dates2)])
# 
# svm.total.predictionOUT = f.allprocess(na.omit(m))
# 

dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL = tapply(dd$energy, as.character(dd$date2), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 1056
holx = 1300# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF = merge(aux3,prediction, by = "time2")
auxF = auxF[-which(auxF$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime


f.pinta= function(time , pred,real){
  plot(time, real, col="black")
  points(time, pred, col="red")
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)


# dev.off()
# png("./pics_res/daily+123456dow_24h", width=800, height = 600)
# f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)
# dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

#  dev.off()
#  png("./pics_res/daily+123456dow_week", width=800, height = 600)
# f.pinta(time= unique(week), pred= predW, real = realW)
#  dev.off()


# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

#  dev.off()
#  png("./pics_res/daily+123456dow_weekFULL", width=800, height = 600)
# f.pinta(time= unique(week), pred= predW, real = realW)
#  dev.off()



#####
# EIGTH PHASE: make it he whole year: NICE
#####

df = read.table("13.reduced.csv", sep=";", head=T)
#df$date2 = as.Date(df$date, format="%Y-%m-%d")
df$date2 = as.Date(df$time, format="%Y-%m-%d")

date1 = as.Date("2016-07-08", format="%Y-%m-%d")
date2 = as.Date("2016-09-02", format="%Y-%m-%d")


NOSUMMER = c(which(df$date2 < date1), which(df$date2 > date2 ))
SUMMER = which(df$date2 >= date1 & df$date2 <=date2 )
HOLIDAYS= which(df$holiday==1)
WEEKENDS = which(as.POSIXlt(df$date)$wday %in% c(6,0))
inic = 736435
fin = 736665
#DISCARD= c(which(df$date2 < as.Date(f.dateMtoR(inic))), which(df$date2 > as.Date(f.dateMtoR(fin))))

dd = data.frame(energy = df$energy, toutF = df$stMU62_IMI_tmed, date = df$date, dow = df$dow, date2 = df$date2)

deleteIndex = c(SUMMER, HOLIDAYS, WEEKENDS)#, DISCARD)
dd4 = dd[-deleteIndex,]


m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
dowe = matrix(dd4$dow, ncol=24, byrow=T)[,1]
m = cbind(m, dow = dowe, energy = consREAL)

mx = as.data.frame(m[,c("dow", "energy")])
#friedman.test(y = mx$energy, groups =  mx$dow , blocks = mx$energy )
pwtt = pairwise.t.test(mx$energy, mx$dow )
pwtt # JUSTIFICATION OF SEPARATING FRIDAYS # checked shapiro and bartlett

dowe2 = dowe
dowe2[dowe2 !=6] = 1
dowe2[dowe2 ==6] = 2

m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, dow = dowe2, energy = consREAL)

library("caret")

f.allprocessSVM = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "svmRadialCost",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(svm.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(svm.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}

f.allprocessBRNN = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  
  paramGrid <- expand.grid(.neurons = c(1,2,3,4,5,10,20))
  
  
  paramGrid <- expand.grid(.C = c(1:15,18,20,22))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  brnn.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "brnn",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")    )
  
  
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(brnn.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(brnn.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}

f.allprocessRF = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  paramGrid <- expand.grid(.mtry = c(1:10))
  set.seed(23)

  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  rf.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "rf",
    metric = "RMSE", # Metric to evaluate
    tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")    )
  
  
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(rf.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print(mape)
  
  plot(1:length(svm.final.prediction),svm.final.prediction)
  points(1:length(svm.final.prediction), testData$energy, col="red")
  
  
  svm.total.prediction <- predict(rf.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}







svm.total.predictionOUT =f.allprocessSVM(m)
svm.total.predictionOUT =f.allprocessBRNN(m)
svm.total.predictionOUT =f.allprocessRF(m)
# Lo comentamos, este es el modelo de Alfonso, ambos errores +1
# dd1 = data.frame(dates=f.dateMtoR(timeF), temp = toutF)
# dd1$dates2 = as.Date(format(dd1$dates, "%Y-%m-%d"))
# dd2 = dd1[which(dd1$dates2 %in% as.Date(names(consREAL))),]
# which(table(dd2$dates2)!=24) #ninguno, por lo tanto son 24 h para cada día
# m = matrix(dd2$temp, ncol=24, byrow = T)
# m = cbind(m, energy = consREAL[which(as.Date(names(consREAL)) %in% dd2$dates2)])
# datesIN = names(consREAL[which(as.Date(names(consREAL)) %in% dd1$dates2)])
# 
# svm.total.predictionOUT = f.allprocess(na.omit(m))
# 

dates = as.Date(svm.total.predictionOUT$date) # prediction dates
time2 = seq(dates[1], dates[length(dates)], by = "day")
aux1 = data.frame(time2)
consREAL = tapply(dd$energy, as.character(dd$date2), sum)
aux2 = data.frame(time2 = as.Date(names(consREAL)), consREAL)
aux3 = merge(aux1,aux2, by="time2", all=T)


minx = 1056
holx = 1500# NA
prediction = data.frame(time2 = time2,pred = rep(NA, length(time2)))
prediction[which(prediction$time2 %in% as.Date(dates)),]$pred = svm.total.predictionOUT$val

prediction[which(prediction$time2 %in% as.Date(df$date2[HOLIDAYS])),]$pred = holx
prediction[which(prediction$time2 %in% as.Date(df$date2[WEEKENDS])),]$pred = minx

auxF = merge(aux3,prediction, by = "time2")
auxF = auxF[-which(auxF$time2 %in% as.Date(df$date2[SUMMER])),] # delete summertime


f.pinta= function(time , pred,real){
  plot(time, real, col="black")
  points(time, pred, col="red")
  legend("topright", legend = c("real", "predicted"), col=c(1,2), pch=1)
  
  dif = (real- pred)[!is.na(real- pred)]
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  cvrmse 
  f.mae(dif)
  mape = f.mape(pred, real)
  mape
  
  sep = (par("usr")[3] + par("usr")[4])/30
  xs = (par("usr")[1]+par("usr")[2] ) /2
  ys = par("usr")[4]-sep
  text(x =xs, y = ys, labels =  paste0("CVRMSE= ", round(cvrmse,3)  ))
  ys = par("usr")[4]-2*sep
  text(x =xs, y = ys, labels =  paste0("MAPE= ", round(mape,3)  ))
  
}

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)


# dev.off()
# png("./pics_res/daily+123456dow_24h", width=800, height = 600)
# f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL)
# dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

#  dev.off()
#  png("./pics_res/daily+123456dow_week", width=800, height = 600)
# f.pinta(time= unique(week), pred= predW, real = realW)
#  dev.off()


# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW)

#  dev.off()
#  png("./pics_res/daily+123456dow_weekFULL", width=800, height = 600)
# f.pinta(time= unique(week), pred= predW, real = realW)
#  dev.off()

