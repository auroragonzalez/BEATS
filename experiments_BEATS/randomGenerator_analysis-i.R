source("functions-ii.R")

#########
## RANDOMLY GENERATED LHS

# Preparing the data:

df1 = read.table("../data/random_LHS_generator_drift/100_gauss", sep=",", head=F)
df2 = read.table("../data/random_LHS_generator_drift/100_expo", sep=",", head=F)
df3 = read.table("../data/random_LHS_generator_drift/100_triang", sep=",", head=F)

classId = c(rep(1,100), rep(2, 100), rep(3, 100))
dfx = rbind(t(df1[-c(ncol(df1))]), t(df2[-c(ncol(df2))]), t(df3[-c(ncol(df3))]))
rownames(dfx) = 1:nrow(dfx)
df = data.frame(classId, dfx)
df$classId = as.factor(df$classId)

dfSC2 = df
dfSC = dfx
rg = dfSC2

##
# BEATS:
##

## 1. Data segmentation

rgBEATS = f.BEATS(sc = rg)  # segment the data using BEATS
rgBEATS2 = data.frame(cbind(classId, rgBEATS))   # add the class to the segmentated data
head(rgBEATS2)

## 2. Classification

### 2.1 THE CODE: 

rgBEATSrf = f.MachineLearning(data = rgBEATS2, modelo = "rf")
#save(rgBEATSrf, file = "resutls_models/rgBEATSrf.Rdata")
rgBEATSsvm = f.MachineLearning(data = rgBEATS2, modelo = "svm")
#save(rgBEATSsvm, file = "resutls_models/rgBEATSsvm.Rdata")


## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

rgBEATShclust = f.myHclust(data=rgBEATS[iS,], saxM =NA , 
                         type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.4557 

rgBEATSkmeans = f.myKmeans(data=rgBEATS[iS,], saxM =NA, 
                         type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] #  0.5393 

#save(rgBEATShclust, file = "resutls_models/rgBEATShclust.Rdata")
#save(rgBEATSkmeans, file = "resutls_models/rgBEATSkmeans.Rdata")

## 4. Hurst coef

rgBEATSHurst= f.HurstVector(df = rgBEATS )
head(rgBEATSHurst)

##
# SAX:
##

# 1. Data segmentation and Classification

### 1.1 THE CODE: 

# We are selecting the alphabet size according to the best results in accuracy.
# So as to achieve it, we apply RF and SVM to each of the 7 segmentations produced
# by alfa = 3,4,...,10 and select the one that returns better accuracy.

rgSAXrf_all = NULL
rgSAXsvm_all=NULL
w= ncol(rgBEATS)  # as many windows as variables BEATS returns
for(alpha in 3:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxData2 =  data.frame(cbind(classId, saxData))
  rgSAXrf_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "rf")) # best result: alfa = 6 --> 0.44
  rgSAXsvm_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "svm")) # best result: alfa = 6 --> 0.43
}
#save(rgSAXrf_all, file = "resutls_models/rgSAXrf_all.Rdata")
#save(rgSAXsvm_all, file = "resutls_models/rgSAXsvm_all.Rdata")


## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

rgSAXhclust_all = NULL
rgSAXkmeans_all=NULL
w= ncol(rgBEATS)  # as many windows as variables BEATS returns
for(alpha in 3:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  rgSAXhclust_all[alpha] = f.myHclust(data=saxData[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]    
}

for(alpha in 5:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  rgSAXkmeans_all[alpha] = f.myKmeans(data=saxData[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]
}

#save(rgSAXhclust_all, file = "resutls_models/rgSAXhclust_all.Rdata")
#save(rgSAXkmeans_all, file = "resutls_models/rgSAXkmeans_all.Rdata")




## 3. Hurst coef

### choosing the same alfa that was the best for the case of the previous SVM in SAX

load("resutls_models/rgSAXsvm_all.Rdata")
accuraciesSAXsvm = as.numeric(as.character(lapply(rgSAXsvm_all, "[[",1)))
rgAlphasvm = which.max(accuraciesSAXsvm)
rgSAXalphasvm = f.toSAX(dfSC2, w, rgAlphasvm)

### Computing HURST coefficient

rgSAXHurst= f.HurstVector(df = rgSAXalphasvm )
head(rgSAXHurst)


##
# Eigen:
##

## 1. Data segmentation

rgEigen = f.Eigen(sc = rg)  # segment the data using BEATS
rgEigen2 = data.frame(cbind(classId, rgEigen))   # add the class to the segmentated data
head(rgEigen2)

## 2. Classification

### 2.1 THE CODE: 

rgEigenrf = f.MachineLearning(data = rgEigen2, modelo = "rf")
#save(rgEigenrf, file = "resutls_models/rgEigenrf.Rdata")

rgEigensvm = f.MachineLearning(data = rgEigen2, modelo = "svm")
#save(rgEigensvm, file = "resutls_models/rgEigensvm.Rdata")



## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

rgEigenhclust = f.myHclust(data=rgEigen[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

rgEigenkmeans = f.myKmeans(data=rgEigen[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608


#save(rgEigenhclust, file = "resutls_models/rgEigenhclust.Rdata")
#save(rgEigenkmeans, file = "resutls_models/rgEigenkmeans.Rdata")

## 4. Hurst coef

rgEigenHurst= f.HurstVector(df = rgEigen )
head(rgEigenHurst)

##
# Raw: Using the whole raw datset
##

## 1. Classification

### 1.1 THE CODE: 
rgRawrf = f.MachineLearning(data = dfSC2, modelo = "rf")
#save(rgRawrf, file = "resutls_models/rgRawrf.Rdata")

rgRawsvm = f.MachineLearning(data = dfSC2, modelo = "svm")
#save(rgRawsvm, file = "resutls_models/rgRawsvm.Rdata")

## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

rgRawhclust = f.myHclust(data=dfSC2[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

rgRawkmeans = f.myKmeans(data=dfSC2[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608

#save(rgRawhclust, file = "resutls_models/rgRawhclust.Rdata")
#save(rgRawkmeans, file = "resutls_models/rgRawkmeans.Rdata")


## 3. Hurst coef

rgRawHurst= f.HurstVector(df =dfSC )
head(rgRawHurst)






# ALL RESULTS LOADING AND CREATING DATA FRAME

file_names=as.list(dir("resutls_models"))
lapply(paste0("resutls_models/",file_names),load,.GlobalEnv)



accuraciesSAXrf = as.numeric(as.character(lapply(rgSAXrf_all, "[[",1)))
rgAlpharf = which.max(accuraciesSAXrf)
rgSAX1rf = accuraciesSAXrf[which.max(accuraciesSAXrf)] 

accuraciesSAXsvm = as.numeric(as.character(lapply(rgSAXsvm_all, "[[",1)))
rgAlphasvm = which.max(accuraciesSAXsvm)
rgSAX1svm = accuraciesSAXsvm[which.max(accuraciesSAXsvm)] 

rgSAX2rf = accuraciesSAXrf[rgAlphasvm] 
rgSAX2svm = accuraciesSAXsvm[rgAlpharf] 


rgAlphahclust = which.max(rgSAXhclust_all) # Alpha for best silhouette of hclust with SAX: 6
rgSAX1hclust = rgSAXhclust_all[rgAlphahclust] # Best silhouette of hclust with SAX inputs: 0.51

rgAlphakmeans = which.max(rgSAXkmeans_all) # Alpha for best silhouette of kmeans with SAX: 8
rgSAX1kmeans = rgSAXkmeans_all[rgAlphakmeans] # Best silhouette of hclust with SAX inputs: 0.42

rgSAX2hclust = rgSAXhclust_all[rgAlphakmeans] # One accuracy of hclust with SAX (best alfa for kmeans) inputs: 0.426
rgSAX2kmeans = rgSAXkmeans_all[rgAlphahclust] # One accuracy of kmeans with SAX (best alfa for hclust) inputs: 0.44


segmentationCLA = c("BEATS", "BEATS", 
                    paste0("SAX (a=",rgAlpharf, ")"),paste0("SAX (a=",rgAlpharf, ")"), 
                    paste0("SAX (a=",rgAlphasvm, ")"),paste0("SAX (a=",rgAlphasvm, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
model = c("RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM")
accuracy = unlist(c(rgBEATSrf[[1]],rgBEATSsvm[[1]], rgSAX1rf,rgSAX2svm, rgSAX2rf,rgSAX1svm,
                    rgEigenrf[[1]],rgEigensvm[[1]], rgRawrf[[1]], rgRawsvm[[1]]))
segmentationCLU = c("BEATS", "BEATS", 
                    paste0("SAX (a=",rgAlphahclust, ")"), paste0("SAX (a=",rgAlphahclust, ")"),
                    paste0("SAX (a=",rgAlphakmeans, ")"), paste0("SAX (a=",rgAlphakmeans, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
method= c("Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans")
silhouette = c(rgBEATShclust, rgBEATSkmeans, rgSAX1hclust, rgSAX2kmeans,rgSAX2hclust,
               rgSAX1kmeans, rgEigenhclust, rgEigenkmeans, rgRawhclust, rgRawkmeans)

df = data.frame(segmentationCLA, model, accuracy,segmentationCLU, method, silhouette)



### RG HURST  all
hurst.df <- data.frame(rgRawHurst, rgBEATSHurst, rgEigenHurst,rgSAXHurst)
hurst.df2 = abs(hurst.df- hurst.df[,1])[-1]
colMeans(hurst.df2, na.rm=T) 
#rgBEATSHurst rgEigenHurst   rgSAXHurst 
#0.0434318    0.0507070    0.1272414 
hurst.df3 = data.frame(timeS = 1:nrow(hurst.df2), hurst.df2)
hurst.df4 <- melt(hurst.df3 ,  id.vars = 'timeS')
names(hurst.df4) = c("timeSeries", "substracted", "difference")
ggplot(hurst.df4, aes(timeSeries,difference)) + geom_line(size = 0.75,aes(colour = substracted, linetype = substracted))
