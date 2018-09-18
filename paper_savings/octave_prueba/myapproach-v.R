

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


#####
# EIGHT PHASE: make it he whole year: NICE
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
pwtt = pairwise.t.test(mx$energy, mx$dow)
#(m http://www.bbc.co.uk/programmes/b006qshdx$energy, mx$dow )
pwtt # JUSTIFICATION OF SEPARATING FRIDAYS # checked shapiro and bartlett

dowe2 = dowe
dowe2[dowe2 !=6] = 1
dowe2[dowe2 ==6] = -1

m = matrix(dd4$toutF, ncol=24, byrow = T)
consREAL = tapply(dd4$energy, as.character(dd4$date2), sum)
m = cbind(m, dow = dowe2, energy = consREAL)

library("caret")


f.allprocessGAUSS = function(m){
  dfxx = as.data.frame(m)
  set.seed(23)
  nTraining <- as.integer(nrow(dfxx) * 0.75)
  indices <- sample(1:nrow(dfxx), nTraining)
  trainingData <- dfxx[indices,]
  testData <- dfxx[-indices,]
  
  set.seed(23)
  paramGrid <- expand.grid(.sigma = c(0.5,1,2))
  # Use t-times k-fold validation, with t = 5 and k = 10 (by default)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all")
  
  # Find the optimum combination of parameters
  svm.final <- caret::train(
    energy~., # We want to predict Temp_Comfort3B according to the predictors
    data = trainingData, # Inputs + Outputs
    method = "gaussprRadial",
    metric = "RMSE", # Metric to evaluate
 #   tuneGrid = paramGrid, # Parameters for tunning
    trControl = ctrl, # Parameters of control    
    preProc = c("center", "scale")  
  )
  print(svm.final$bestTune)
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
  print(svm.final$bestTune)
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
  
  
  #paramGrid <- expand.grid(.C = c(1:15,18,20,22))
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
  print(brnn.final$bestTune)
  
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
  
  paramGrid <- expand.grid(.mtry = c(5:15))
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
  print(rf.final$bestTune)
  
  index = which(colnames(testData) == "energy")
  svm.final.prediction <- predict(rf.final, testData[-index])
  real = testData$energy
  dif = (real-svm.final.prediction)
  
  f.rmse(dif)
  cvrmse = f.rmse(dif) / mean(real, na.rm = T)*100
  print("cvrmse")
  print(cvrmse) 
  f.mae(dif)
  mape = f.mape(svm.final.prediction,real)
  print("mape")
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
  print(XGB.final$bestTune)
  
  
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

mdf = as.data.frame(m)
mdf$dow = factor(mdf$dow)

svm.total.predictionOUT =f.allprocessSVM(m)
svm.total.predictionOUT =f.allprocessSVM(mdf)

svm.total.predictionOUT =f.allprocessBRNN(m)
svm.total.predictionOUT =f.allprocessBRNN(mdf)

svm.total.predictionOUT =f.allprocessRF(m)
svm.total.predictionOUT =f.allprocessRF(mdf)

svm.total.predictionOUT =f.allprocessXGB(m)
svm.total.predictionOUT =f.allprocessXGB(mdf)

svm.total.predictionOUT =f.allprocessGAUSS(m)
svm.total.predictionOUT =f.allprocessGAUSS(mdf)

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


original  = which(prediction$time2 %in% as.Date(dates))
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
indices=splitAt(original, (which(diff(original)!=1))+1)


f.pinta= function(time , pred,real, type, typeline ="p"){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n", type = typeline)
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
  
  }  else{
    plot(time, real, col="black")
  }
  points(time, pred, col="red", type = typeline)
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


f.pinta2= function(time , pred,real, type, typeline ="p"){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n", type = "l", lwd=2, ylab = "Wh", xlab = "datestamp")
    points(time, real, col="black",  type = "p", pch=16)
#     for(i in 1:length(indices)){
#       lines(time[indices[[i]]], real[indices[[i]]]) 
#     }
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
    
  }  else{
    plot(time, real, col="black", type = "l")
  }
  points(time, pred, col="red", type = "p", pch=4)
  points(time, pred, col="red", type = "l", lwd=2, lty=2)
  legend("topleft", legend = c("real", "predicted"), col=c(1,2), lty=c(1,2)
         , lwd = c(2,3.4), pch=c(16, 4))
  
}

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")

f.pinta2(time= auxF$time2[1:164], pred= auxF$pred[1:164], real = auxF$consREAL[1:164], type="month")
f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")

f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")

f.pinta2(time= c(auxF$time2[1:164], NA,auxF$time2[165:322])
         ,pred= c(auxF$pred[1:164], NA,auxF$pred[165:322])
         , real = c(auxF$consREAL[1:164], NA,auxF$consREAL[165:322]), type="month")

dev.off()
pdf("./pics2_res/daily_RF1.pdf", width=10, height = 7)
f.pinta2(time= auxF$time2[1:164], pred= auxF$pred[1:164], real = auxF$consREAL[1:164], type="month")
dev.off()

dev.off()
pdf("./pics2_res/dailydaily_RF2.pdf", width=10, height = 7)
f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")
dev.off()

dev.off()
pdf("./pics2_res/dailydaily_RF3.pdf", width=12, height = 6)
f.pinta2(time= c(auxF$time2[1:164], NA,auxF$time2[165:322])
         ,pred= c(auxF$pred[1:164], NA,auxF$pred[165:322])
         , real = c(auxF$consREAL[1:164], NA,auxF$consREAL[165:322]), type="month")
dev.off()


 dev.off()
 png("./pics2_res/daily_phase8SVM", width=800, height = 600)
 f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")
 dev.off()


## weekly

 f.pinta3= function(time , pred,real, type, typeline ="p", labs = NA, printRes = F){
   if(type=="month"){
     plot(time, real, col="black", xaxt="n", type = "l", lwd=2, ylab = "Wh", xlab = "datestamp")
     points(time, real, col="black",  type = "p", pch=16)
     #     for(i in 1:length(indices)){
     #       lines(time[indices[[i]]], real[indices[[i]]]) 
     #     }
     lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
     axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
     
   }  else{
     plot(time, real, col="black", xaxt="n", type = "p", lwd=2, ylab = "Wh", xlab = "week number")
     lab = time
     axis(1,at=lab,labels=labs)#;axis(2);box()
   }
   
   points(time, pred, col="red", type = "p", pch=4, lwd=3)
   #points(time, pred, col="red", type = "l", lwd=2, lty=2)
   
   legend("bottomright", legend = c("real", "predicted"), col=c(1,2)#, lty=c(1,2)
         # , lwd = c(2.5,2.5)
          , pch=c(1, 4)
         , lwd = c(2.5,2.5))
   
   if(printRes){
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
   
 }
 
 
 

 week = strftime(auxF$time2,format="%W-%Y")
 week[which(week=="00-2017")] = "52-2016"  # The days in the year before the first week are in week 0. But I dont want that
 
 realW = tapply(auxF$consREAL, week, sum)
 predW = tapply(auxF$pred, week, sum)
 
 week = strftime(auxF$time2,format="%W")
 week[which(week=="00")] = "52"
 x <- scan(what = character(), text = week)
 xindex= as.numeric(rle(x)$values)

 f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
          , printRes = T)
 
 
 
 
 # delete those weeks that don't have 7 days
week = strftime(auxF$time2,format="%W")
week[which(week=="00")] = "52"
auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
x <- scan(what = character(), text = week)
xindex= as.numeric(rle(x)$values)
f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
         , printRes = F)


f.rmse <- function(error)
{
  sqrt(mean(error^2, na.rm=T))
}

dif = realW-predW
f.rmse(dif)
cvrmse = f.rmse(dif) / mean(realW, na.rm = T)*100
print(cvrmse) 
f.mae(dif)
mape = f.mape(predW,realW)
print(mape)


dev.off()
pdf("./pics2_res/weekly_RF1-i.pdf", width=12, height = 6)
f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
         , printRes = F)
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




#####
# NINTH PHASE: adding setting point
#####


settingP = read.table("chem_sup.csv", sep=";", head=T)
meanSP = aggregate(settingP$tsetMean, list(settingP$date), mean)

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
m = data.frame(m)
m$index = rownames(m)
names(meanSP)[1]="index"
m2 = merge(m, meanSP, by = "index", all.x =T)
m3 = m2[c(2:26,28,27)]
names(m3)[26] = "settingPoint" # it does nt work
library("DMwR")
m3= knnImputation(m3,k=3)
m = as.matrix(m3)
# common = which(as.character(meanSP$Group.1) %in% rownames(m))
# m2 = data.frame(m)
rownames(m) = m2$index
  
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
  
  
  #paramGrid <- expand.grid(.C = c(1:15,18,20,22))
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
png("./pics2_res/daily_phase9RF", width=800, height = 600)
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")
dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW, type="week")

dev.off()
png("./pics2_res/phase9_weekRF.png", width=800, height = 600)
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
png("./pics2_res/phase9_weekRF2.png", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW, type="week")
dev.off()






#####
# TENTH PHASE: adding hours of devices working (not very good acutally)


settingP = read.table("chem_sup.csv", sep=";", head=T)
meanSP = aggregate(settingP$tsetMean, list(settingP$date), mean)
meanTimeOpe = aggregate(settingP$timesOn, list(settingP$date), sum)

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
m = data.frame(m)
m$index = rownames(m)
names(meanSP)[1]="index"
m2 = merge(m, meanSP, by = "index", all.x =T)
names(meanTimeOpe)[1]="index"
m2 = merge(m2, meanTimeOpe, by = "index", all.x =T)

m3 = m2[c(2:26,28,29,27)]
names(m3)[26] = "settingPoint"
names(m3)[27] = "operatingTime"
library("DMwR")
m3= knnImputation(m3,k=3)
m = as.matrix(m3)
# common = which(as.character(meanSP$Group.1) %in% rownames(m))
# m2 = data.frame(m)
rownames(m) = m2$index

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
  
  
  #paramGrid <- expand.grid(.C = c(1:15,18,20,22))
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
png("./pics2_res/daily_phase8SVM", width=800, height = 600)
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")
dev.off()


## weekly

realW = tapply(auxF$consREAL, strftime(auxF$time2,format="%W"), sum)
predW = tapply(auxF$pred, strftime(auxF$time2,format="%W"), sum)
week = strftime(auxF$time2,format="%W")
f.pinta(time= unique(week), pred= predW, real = realW, type="week")

dev.off()
png("./pics2_res/phase8d_weekSVM", width=800, height = 600)
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
png("./pics2_res/phase8d_weekFULLSVM", width=800, height = 600)
f.pinta(time= unique(week), pred= predW, real = realW, type="week")
dev.off()






















#####
# Nth PHASE: make it he whole year: NICE
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
#dowe = matrix(dd4$dow, ncol=24, byrow=T)[,1]
#m = cbind(m, dow = dowe, energy = consREAL)
m = cbind(m, energy = consREAL)

mx = as.data.frame(m)
#friedman.test(y = mx$energy, groups =  mx$dow , blocks = mx$energy )


summary(dd4$toutF)

dif = (max(dd4$toutF) - min(dd4$toutF))/6
B = c()
aux = min(dd4$toutF) + dif
B = aux
for(i in 2:6){
  aux = B[i-1]+dif
  B = c(B,aux)
  }

B1 = B[1]

temp = dd4$toutF[1:20]

T1 = matrix(NA, nrow=length(temp), ncol=6)
f.betas = function(temp,T){
  dif = (max(temp) - min(temp))/6
  B = c()
  aux = min(temp) + dif
  B = aux
  for(i in 2:6){
    aux = B[i-1]+dif
    B = c(B,aux)
  }
  
  B1 = B[1]
  
  
  for(i in 1:length(temp)){
    x = temp[i]
    if(x >= B[1]){
      T[i,1] = B[1]  
    }
    if(x < B1){
      T[i,1] = x
      T[i,2] = 0
      T[i,3] = 0
      T[i,4] = 0
      T[i,5] = 0
      T[i,6] = 0
      # Si es menor o igual se pone en el primero y el resto cero
    }else{
    
    for(n in 2:5){
      if(x >= B[n]){
        T[i,n] = B[n]- B[n-1]
      }
      if(x<B[n]){
        T[i,n] = x- B[n-1]
        for(m in (n+1):6){
          T[i,m] = 0        
        }
        break
      }
    }
    
    
    if(x > B[5]){
      T[i,5] = B[5]- B[4]
      T[i,6] = x- B[5]
    }
    }  
  }
return(T)
  }

T2 = f.betas(temp,T1)

dates = as.numeric(format(as.POSIXlt(dd4$date), format="%H"))
dates[dates==0 & !is.na(dates)] = 24
dd4$dates = dates

M = matrix(NA, nrow= nrow(dd4), ncol=24)
for(i in 1:nrow(M)){
  M[i,dd4$dates[i]] = 1
}

T1 = matrix(NA, nrow=length(dd4$toutF), ncol=6)
T3 = f.betas(dd4$toutF,T1)

M2 = as.data.frame(cbind(M, T3, dd4$energy))
M2[is.na(M2)] = 0

lm(V31 ~ ., data = M2)


library("caret")

set.seed(1234)
trainIndex <- createDataPartition(M2$V31, p = .75, 
                                  list = FALSE, 
                                  times = 1)


M2Train <- M2[ trainIndex,]
M2Test  <- M2[-trainIndex,]

fit = lm(V31 ~ ., data = M2Train)

real = M2Test$V31
M2Test = M2Test[1:30]


x= as.numeric(fit$coefficients[-1])
pred = fit$coefficients[1] + rowSums(t(t(M2Test)*x))

f.rmse <- function(error)
{
  sqrt(mean(error^2))
}

f.rmse(pred-real)

cvrmse = f.rmse(pred-real) / mean(real, na.rm = T)*100
print(cvrmse) 


# Vale, estas predicciones son para cada hora. Ahora nos interesa hacerlas diarias y 
# para ser capaces de agregar por días necesitamos que en el conjunto test haya días completos

M2 = as.data.frame(cbind(M, T3, dd4$energy, dd4$date2))
M2[is.na(M2)] = 0


library("caret")

set.seed(1234)
daystrainIndex <- createDataPartition(M2$V32, p = .989, 
                                      list = FALSE, 
                                      times = 1)


daysTest = M2[-daystrainIndex,]$V32

M2Test = M2[which(M2$V32 %in% daysTest),][1:31]
M2Train  <- M2[-which(M2$V32 %in% daysTest),][1:31]


fit = lm(V31 ~., data = M2Train)


real = M2Test$V31
M2Test = M2Test[1:30]


x= as.numeric(fit$coefficients[-1])
pred = fit$coefficients[1] + rowSums(t(t(M2Test)*x))

f.rmse <- function(error)
{
  sqrt(mean(error^2))
}

f.rmse(pred-real)

cvrmse = f.rmse(pred-real) / mean(real, na.rm = T)*100
print(cvrmse) 

v = c(1,2,3,4,3,2,1,2,3,4,5,6,7,8,3,4,6)
n=2
unname(tapply(v, (seq_along(v)-1) %/% n, sum))


n=24
v = real
realDay = unname(tapply(v, (seq_along(v)-1) %/% n, sum))

n=24
v = pred
predDay = unname(tapply(v, (seq_along(v)-1) %/% n, sum))


f.rmse(predDay-realDay)
cvrmse = f.rmse(predDay-realDay) / mean(realDay, na.rm = T)*100
print(cvrmse) 


## Hay muy pocos días... es porque has hecho los cálculos de la selección del conjutno de test mal

M2 = as.data.frame(cbind(M, T3, dd4$energy, dd4$date2))
M2[is.na(M2)] = 0


library("caret")

set.seed(1234)
daystrainIndex <- createDataPartition(unique(M2$V32), p = .75, 
                                      list = FALSE, 
                                      times = 1)


daysTest = unique(M2$V32)[daystrainIndex]

M2Test = M2[which(M2$V32 %in% daysTest),][1:31]
M2Train  <- M2[-which(M2$V32 %in% daysTest),][1:31]


fit = lm(V31 ~., data = M2Train)


real = M2Test$V31
M2Test = M2Test[1:30]


x= as.numeric(fit$coefficients[-1])
pred = fit$coefficients[1] + rowSums(t(t(M2Test)*x))


f.rmse(pred-real)

cvrmse = f.rmse(pred-real) / mean(real, na.rm = T)*100
print(cvrmse) 

v = c(1,2,3,4,3,2,1,2,3,4,5,6,7,8,3,4,6)
n=2
unname(tapply(v, (seq_along(v)-1) %/% n, sum))


n=24
v = real
realDay = unname(tapply(v, (seq_along(v)-1) %/% n, sum))

n=24
v = pred
predDay = unname(tapply(v, (seq_along(v)-1) %/% n, sum))


f.rmse(predDay-realDay)
cvrmse = f.rmse(predDay-realDay) / mean(realDay, na.rm = T)*100
print(cvrmse) 


f.mape <- function(actual, predicted){
  mean(abs((actual-predicted)/actual) * 100, na.rm = T)  
}

f.mape(realDay, predDay)

# No acabado. Falta completar con los días festivos. 










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


original  = which(prediction$time2 %in% as.Date(dates))
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
indices=splitAt(original, (which(diff(original)!=1))+1)


f.pinta= function(time , pred,real, type, typeline ="p"){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n", type = typeline)
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
    
  }  else{
    plot(time, real, col="black")
  }
  points(time, pred, col="red", type = typeline)
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


f.pinta2= function(time , pred,real, type, typeline ="p"){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n", type = "l", lwd=2, ylab = "Wh", xlab = "datestamp")
    points(time, real, col="black",  type = "p", pch=16)
    #     for(i in 1:length(indices)){
    #       lines(time[indices[[i]]], real[indices[[i]]]) 
    #     }
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
    
  }  else{
    plot(time, real, col="black", type = "l")
  }
  points(time, pred, col="red", type = "p", pch=4)
  points(time, pred, col="red", type = "l", lwd=2, lty=2)
  legend("topleft", legend = c("real", "predicted"), col=c(1,2), lty=c(1,2)
         , lwd = c(2,3.4), pch=c(16, 4))
  
}

f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")

f.pinta2(time= auxF$time2[1:164], pred= auxF$pred[1:164], real = auxF$consREAL[1:164], type="month")
f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")

f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")

f.pinta2(time= c(auxF$time2[1:164], NA,auxF$time2[165:322])
         ,pred= c(auxF$pred[1:164], NA,auxF$pred[165:322])
         , real = c(auxF$consREAL[1:164], NA,auxF$consREAL[165:322]), type="month")

dev.off()
pdf("./pics2_res2/daily_RF1.pdf", width=10, height = 7)
f.pinta2(time= auxF$time2[1:164], pred= auxF$pred[1:164], real = auxF$consREAL[1:164], type="month")
dev.off()

dev.off()
pdf("./pics2_res2/dailydaily_RF2.pdf", width=10, height = 7)
f.pinta2(time= auxF$time2[165:322], pred= auxF$pred[165:322], real = auxF$consREAL[165:322], type="month")
dev.off()

dev.off()
pdf("./pics2_res/dailydaily_RF3.pdf", width=12, height = 6)
f.pinta2(time= c(auxF$time2[1:164], NA,auxF$time2[165:322])
         ,pred= c(auxF$pred[1:164], NA,auxF$pred[165:322])
         , real = c(auxF$consREAL[1:164], NA,auxF$consREAL[165:322]), type="month")
dev.off()


dev.off()
png("./pics2_res/daily_phase8SVM", width=800, height = 600)
f.pinta(time= auxF$time2, pred= auxF$pred, real = auxF$consREAL, type="month")
dev.off()


## weekly

f.pinta3= function(time , pred,real, type, typeline ="p", labs = NA, printRes = F){
  if(type=="month"){
    plot(time, real, col="black", xaxt="n", type = "l", lwd=2, ylab = "Wh", xlab = "datestamp")
    points(time, real, col="black",  type = "p", pch=16)
    #     for(i in 1:length(indices)){
    #       lines(time[indices[[i]]], real[indices[[i]]]) 
    #     }
    lab = seq(as.Date(time[1]), as.Date(time[length(time)]), "months")
    axis(1,at=lab,labels=format(lab, format="%Y-%m"))#;axis(2);box()
    
  }  else{
    plot(time, real, col="black", xaxt="n", type = "p", lwd=2, ylab = "Wh", xlab = "week number")
    lab = time
    axis(1,at=lab,labels=labs)#;axis(2);box()
  }
  
  points(time, pred, col="red", type = "p", pch=4, lwd=3)
  #points(time, pred, col="red", type = "l", lwd=2, lty=2)
  legend("bottomright", legend = c("real", "predicted"), col=c(1,2)#, lty=c(1,2)
         #  , lwd = c(2,3.4)
         , pch=c(1, 4))
  
  if(printRes){
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
  
}




week = strftime(auxF$time2,format="%W-%Y")
week[which(week=="00-2017")] = "52-2016"  # The days in the year before the first week are in week 0. But I dont want that

realW = tapply(auxF$consREAL, week, sum)
predW = tapply(auxF$pred, week, sum)

week = strftime(auxF$time2,format="%W")
week[which(week=="00")] = "52"
x <- scan(what = character(), text = week)
xindex= as.numeric(rle(x)$values)

f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
         , printRes = T)




# delete those weeks that don't have 7 days
week = strftime(auxF$time2,format="%W")
week[which(week=="00")] = "52"
auxF2 = auxF[which(!week %in%names(which(table(strftime(auxF$time2,format="%W"))!=7))),]
table(strftime(auxF2$time2,format="%W"))

realW = tapply(auxF2$consREAL, strftime(auxF2$time2,format="%W"), sum)
predW = tapply(auxF2$pred, strftime(auxF2$time2,format="%W"), sum)
week = strftime(auxF2$time2,format="%W")
x <- scan(what = character(), text = week)
xindex= as.numeric(rle(x)$values)
f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
         , printRes = F)

dev.off()
pdf("./pics2_res/weekly_RF1.pdf", width=12, height = 6)
f.pinta3(time= 1:length(xindex), pred= predW, real = realW, type="week", labs =xindex
         , printRes = F)
dev.off()


