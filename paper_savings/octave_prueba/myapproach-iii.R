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


#####
# EIGTH PHASE: make it he whole year: NICE
#####

df = read.table("13.reduced.csv", sep=";", head=T)
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
anova = aov(mx$energy~ mx$dow)
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



f.allprocessXGB = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  paramGrid <- expand.grid(
    nrounds = 1000,
    eta = c(0.01, 0.001, 0.0001),
    max_depth = c(2, 4, 6, 8, 10),
    gamma = 1
  )
  
  set.seed(23)
  
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  XGB.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "xgbTree",
    metric = "RMSE", # Metric to evaluate
    #  tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")    )
  
  
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(XGB.final, testData[-index])
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
  
  
  svm.total.prediction <- predict(XGB.final, dfxx[-index])
  real = dfxx[,index]
  
  plot(1:length(svm.total.prediction),svm.total.prediction)
  points(1:length(svm.total.prediction), real, col="red")
  
  return(data.frame(date = rownames(dfxx[-index]), val = svm.total.prediction))
}



svm.total.predictionOUT =f.allprocessSVM(m)
svm.total.predictionOUT =f.allprocessBRNN(m)
svm.total.predictionOUT =f.allprocessRF(m)
svm.total.predictionOUT =f.allprocessXGB(m)

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


f.pinta= function(time , pred,real, type){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n")
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
  
  }  else{
    plot(time, real, col="black")
  }
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

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")


 dev.off()
 png("./pics_res/daily_phase8SVM", width=800, height = 600)
 f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")
 dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW, type="week")

  dev.off()
  png("./pics_res/phase8d_weekSVM", width=800, height = 600)
 f.pinta(time= unique(week), pred= predW, real = realW, type = "week")
  dev.off()


# delete those weeks that don't have 7 days

auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW, type="week")

dev.off()
png("./pics_res/phase8d_weekFULLSVM", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW, type="week")
dev.off()



#####
# time series pic
#####
  
df = read.table("13.reduced.csv", sep=";", head=T)
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

#http://dr-k-lo.blogspot.com.es/2013/07/a-color-blind-friendly-palette-for-r.html

plot(dd4$energy, type="l", col="blue")
lines(dd4$toutF*30, col="red")
dd4[73:240,]
dev.off()
svg("uno.svg")
plot(dd4$toutF[73:(73+23)],type="l", xlab="", ylab="",bty="n",xaxt="n",yaxt="n", lwd=3, col="#009E73")

points(dd4$toutF[73:(73+23)], col=1, pch=20)
dev.off()
sum(dd4$energy[73:(73+23)])
svg("dos.svg")
plot(dd4$toutF[97:(97+23)],type="l", xlab="", ylab="",bty="n",xaxt="n",yaxt="n", lwd=3, col="#009E73")
points(dd4$toutF[97:(97+23)], col=1, pch=20)
dev.off()
sum(dd4$energy[97:(97+23)])
svg("tres.svg")
plot(dd4$toutF[121:(121+23)],type="l", xlab="", ylab="",bty="n",xaxt="n",yaxt="n", lwd=3, col="#009E73")
points(dd4$toutF[121:(121+23)], col=1, pch=20)
dev.off()
sum(dd4$energy[121:(121+23)])
svg("cuatro.svg")
plot(dd4$toutF[169:(169+23)],type="l", xlab="", ylab="",bty="n",xaxt="n",yaxt="n", lwd=3, col="#009E73")
points(dd4$toutF[169:(169+23)], col=1, pch=20)
dev.off()
sum(dd4$energy[169:(169+23)])

