source("functions-ii.R")

#########
## RANDOMLY GENERATED LHS

# Preparing the data:

df <- rbind(read.table("../data/ArrowHead/ArrowHead_TEST", sep=","),read.table("../data/ArrowHead/ArrowHead_TRAIN", sep=","))
names(df)[1] = "classId"
classId <- as.factor(df$classId)
dfSC2 = df[1:193]
dfSC = dfSC2[,-1]
set.seed(seed)
arrh = dfSC2

##
# BEATS:
##

## 1. Data segmentation

arrhBEATS = f.BEATS(sc = arrh)  # segment the data using BEATS
arrhBEATS2 = data.frame(cbind(classId, arrhBEATS))   # add the class to the segmentated data
head(arrhBEATS2)

## 2. Classification

### 2.1 THE CODE: 

arrhBEATSrf = f.MachineLearning(data = arrhBEATS2, modelo = "rf")
#save(arrhBEATSrf, file = "resutls_models/arrhBEATSrf.Rdata")
arrhBEATSsvm = f.MachineLearning(data = arrhBEATS2, modelo = "svm")
#save(arrhBEATSsvm, file = "resutls_models/arrhBEATSsvm.Rdata")


## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

arrhBEATShclust = f.myHclust(data=arrhBEATS[iS,], saxM =NA , 
                         type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.4557 

arrhBEATSkmeans = f.myKmeans(data=arrhBEATS[iS,], saxM =NA, 
                         type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] #  0.5393 

#save(arrhBEATShclust, file = "resutls_models/arrhBEATShclust.Rdata")
#save(arrhBEATSkmeans, file = "resutls_models/arrhBEATSkmeans.Rdata")

## 4. Hurst coef

arrhBEATSHurst= f.HurstVector(df = arrhBEATS )
head(arrhBEATSHurst)

##
# SAX:
##

# 1. Data segmentation and Classification

### 1.1 THE CODE: 

# We are selecting the alphabet size according to the best results in accuracy.
# So as to achieve it, we apply RF and SVM to each of the 7 segmentations produced
# by alfa = 3,4,...,10 and select the one that returns better accuracy.

arrhSAXrf_all = NULL
arrhSAXsvm_all=NULL
w= ncol(arrhBEATS)  # as many windows as variables BEATS returns
for(alpha in 3:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxData2 =  data.frame(cbind(classId, saxData))
  arrhSAXrf_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "rf")) # best result: alfa = 6 --> 0.44
  arrhSAXsvm_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "svm")) # best result: alfa = 6 --> 0.43
}
#save(arrhSAXrf_all, file = "resutls_models/arrhSAXrf_all.Rdata")
#save(arrhSAXsvm_all, file = "resutls_models/arrhSAXsvm_all.Rdata")


## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

arrhSAXhclust_all = NULL
arrhSAXkmeans_all=NULL
w= ncol(arrhBEATS)  # as many windows as variables BEATS returns
for(alpha in 3:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  arrhSAXhclust_all[alpha] = f.myHclust(data=saxData[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]    
}

for(alpha in 5:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  arrhSAXkmeans_all[alpha] = f.myKmeans(data=saxData2[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]
}

#save(arrhSAXhclust_all, file = "resutls_models/arrhSAXhclust_all.Rdata")
#save(arrhSAXkmeans_all, file = "resutls_models/arrhSAXkmeans_all.Rdata")




## 3. Hurst coef

### choosing the same alfa that was the best for the case of the previous SVM in SAX

load("resutls_models/arrhSAXsvm_all.Rdata")
accuraciesSAXsvm = unlist(arrhSAXsvm_all)
arrhAlphasvm = which.max(accuraciesSAXsvm) + 2 # Alpha for best accuracy of SVM with SAX: 5
arrhSAXalphasvm = f.toSAX(dfSC2, w, arrhAlphasvm)

### Computing HURST coefficient

arrhSAXHurst= f.HurstVector(df = arrhSAXalphasvm )
head(arrhSAXHurst)


##
# Eigen:
##

## 1. Data segmentation

arrhEigen = f.Eigen(sc = arrh)  # segment the data using BEATS
arrhEigen2 = data.frame(cbind(classId, arrhEigen))   # add the class to the segmentated data
head(arrhEigen2)

## 2. Classification

### 2.1 THE CODE: 

arrhEigenrf = f.MachineLearning(data = arrhEigen2, modelo = "rf")
#save(arrhEigenrf, file = "resutls_models/arrhEigenrf.Rdata")

arrhEigensvm = f.MachineLearning(data = arrhEigen2, modelo = "svm")
#save(arrhEigensvm, file = "resutls_models/arrhEigensvm.Rdata")



## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

arrhEigenhclust = f.myHclust(data=arrhEigen[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

arrhEigenkmeans = f.myKmeans(data=arrhEigen[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608


#save(arrhEigenhclust, file = "resutls_models/arrhEigenhclust.Rdata")
#save(arrhEigenkmeans, file = "resutls_models/arrhEigenkmeans.Rdata")

## 4. Hurst coef

arrhEigenHurst= f.HurstVector(df = arrhEigen )
head(arrhEigenHurst)

##
# Raw: Using the whole raw datset
##

## 1. Classification

### 1.1 THE CODE: 
arrhRawrf = f.MachineLearning(data = dfSC2, modelo = "rf")
#save(arrhRawrf, file = "resutls_models/arrhRawrf.Rdata")

arrhRawsvm = f.MachineLearning(data = dfSC2, modelo = "svm")
#save(arrhRawsvm, file = "resutls_models/arrhRawsvm.Rdata")

## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

arrhRawhclust = f.myHclust(data=dfSC2[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

arrhRawkmeans = f.myKmeans(data=dfSC2[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608

#save(arrhRawhclust, file = "resutls_models/arrhRawhclust.Rdata")
#save(arrhRawkmeans, file = "resutls_models/arrhRawkmeans.Rdata")


## 3. Hurst coef

arrhRawHurst= f.HurstVector(df =dfSC )
head(arrhRawHurst)






# ALL RESULTS LOADING AND CREATING DATA FRAME

file_names=as.list(dir("resutls_models"))
lapply(paste0("resutls_models/",file_names),load,.GlobalEnv)

accuraciesSAXrf = as.numeric(as.character(lapply(arrhSAXrf_all, "[[",1)))
arrhAlpharf = which.max(accuraciesSAXrf)
arrhSAX1rf = accuraciesSAXrf[which.max(accuraciesSAXrf)] 

accuraciesSAXsvm = as.numeric(as.character(lapply(arrhSAXsvm_all, "[[",1)))
arrhAlphasvm = which.max(accuraciesSAXsvm)
arrhSAX1svm = accuraciesSAXsvm[which.max(accuraciesSAXsvm)] 

arrhSAX2rf = accuraciesSAXrf[arrhAlphasvm] 
arrhSAX2svm = accuraciesSAXsvm[arrhAlpharf] 


arrhAlphahclust = which.max(arrhSAXhclust_all) # Alpha for best silhouette of hclust with SAX: 6
arrhSAX1hclust = arrhSAXhclust_all[arrhAlphahclust] # Best silhouette of hclust with SAX inputs: 0.51

arrhAlphakmeans = which.max(arrhSAXkmeans_all) # Alpha for best silhouette of kmeans with SAX: 8
arrhSAX1kmeans = arrhSAXkmeans_all[arrhAlphakmeans] # Best silhouette of hclust with SAX inputs: 0.42

arrhSAX2hclust = arrhSAXhclust_all[arrhAlphakmeans] # One accuracy of hclust with SAX (best alfa for kmeans) inputs: 0.426
arrhSAX2kmeans = arrhSAXkmeans_all[arrhAlphahclust] # One accuracy of kmeans with SAX (best alfa for hclust) inputs: 0.44


segmentationCLA = c("BEATS", "BEATS", 
                    paste0("SAX (a=",arrhAlpharf, ")"),paste0("SAX (a=",arrhAlpharf, ")"), 
                    paste0("SAX (a=",arrhAlphasvm, ")"),paste0("SAX (a=",arrhAlphasvm, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
model = c("RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM")
accuracy = unlist(c(arrhBEATSrf[[1]],arrhBEATSsvm[[1]], arrhSAX1rf,arrhSAX2svm, arrhSAX2rf,arrhSAX1svm,
                    arrhEigenrf[[1]],arrhEigensvm[[1]], arrhRawrf[[1]], arrhRawsvm[[1]]))
segmentationCLU = c("BEATS", "BEATS", 
                    paste0("SAX (a=",arrhAlphahclust, ")"), paste0("SAX (a=",arrhAlphahclust, ")"),
                    paste0("SAX (a=",arrhAlphakmeans, ")"), paste0("SAX (a=",arrhAlphakmeans, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
method= c("Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans")
silhouette = c(arrhBEATShclust, arrhBEATSkmeans, arrhSAX1hclust, arrhSAX2kmeans,arrhSAX2hclust,
               arrhSAX1kmeans, arrhEigenhclust, arrhEigenkmeans, arrhRawhclust, arrhRawkmeans)

df = data.frame(segmentationCLA, model, accuracy,segmentationCLU, method, silhouette)



### arrh HURST  all
hurst.df <- data.frame(arrhRawHurst, arrhBEATSHurst, arrhEigenHurst,arrhSAXHurst)
hurst.df2 = abs(hurst.df- hurst.df[,1])[-1]
colMeans(hurst.df2, na.rm=T) 
#arrhBEATSHurst arrhEigenHurst   arrhSAXHurst 
#0.0434318    0.0507070    0.1272414 
hurst.df3 = data.frame(timeS = 1:nrow(hurst.df2), hurst.df2)
hurst.df4 <- melt(hurst.df3 ,  id.vars = 'timeS')
names(hurst.df4) = c("timeSeries", "substracted", "difference")
ggplot(hurst.df4, aes(timeSeries,difference)) + geom_line(size = 0.75,aes(colour = substracted, linetype = substracted))



