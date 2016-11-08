source("functions-ii.R")

#########
## RANDOMLY GENERATED LHS

# Preparing the data:

df1 = read.table("../data/driftdataset/batch1-i.dat", sep=";", head=F, stringsAsFactors = F)
classId = c(rep(1,29), rep(2,27))

df = df1

names(df)[1] = "classId"
names(df)[2] = "conc"
df$classId <- as.factor(df$classId)
df = df[-2]
dfSC2 = df
dfSC = dfSC2[-1]
classId = df$classId

gs = dfSC2

##
# BEATS:
##

## 1. Data segmentation

gsBEATS = f.BEATS(sc = gs)  # segment the data using BEATS
gsBEATS2 = data.frame(cbind(classId, gsBEATS))   # add the class to the segmentated data
head(gsBEATS2)

## 2. Classification

### 2.1 THE CODE: 

gsBEATSrf = f.MachineLearning(data = gsBEATS2, modelo = "rf")
#save(gsBEATSrf, file = "resutls_models/gsBEATSrf.Rdata")
gsBEATSsvm = f.MachineLearning(data = gsBEATS2, modelo = "svm")
#save(gsBEATSsvm, file = "resutls_models/gsBEATSsvm.Rdata")


## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

gsBEATShclust = f.myHclust(data=gsBEATS[iS,], saxM =NA , 
                         type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.4557 

gsBEATSkmeans = f.myKmeans(data=gsBEATS[iS,], saxM =NA, 
                         type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] #  0.5393 

#save(gsBEATShclust, file = "resutls_models/gsBEATShclust.Rdata")
#save(gsBEATSkmeans, file = "resutls_models/gsBEATSkmeans.Rdata")

## 4. Hurst coef

gsBEATSHurst= f.HurstVector(df = gsBEATS )
head(gsBEATSHurst)

##
# SAX:
##

# 1. Data segmentation and Classification

### 1.1 THE CODE: 

# We are selecting the alphabet size according to the best results in accuracy.
# So as to achieve it, we apply RF and SVM to each of the 7 segmentations produced
# by alfa = 3,4,...,10 and select the one that returns better accuracy.

gsSAXrf_all = NULL
gsSAXsvm_all=NULL
w= ncol(gsBEATS)  # as many windows as variables BEATS returns
for(alpha in 4:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxData2 =  data.frame(cbind(classId, saxData))
  gsSAXrf_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "rf")) # best result: alfa = 6 --> 0.44
  gsSAXsvm_all[alpha] = list(f.MachineLearning(data = saxData2, modelo = "svm")) # best result: alfa = 6 --> 0.43
}
#save(gsSAXrf_all, file = "resutls_models/gsSAXrf_all.Rdata")
#save(gsSAXsvm_all, file = "resutls_models/gsSAXsvm_all.Rdata")


## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

gsSAXhclust_all = NULL
gsSAXkmeans_all=NULL
w= ncol(gsBEATS)  # as many windows as variables BEATS returns

for(alpha in 3:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  saxData2 =  data.frame(cbind(classId, saxData))
  gsSAXhclust_all[alpha] = f.myHclust(data=saxData2[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]    
}

for(alpha in 6:10){
  saxData = f.toSAX(dfSC2, w, alpha)
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  gsSAXkmeans_all[alpha] = f.myKmeans(data=saxData2[iS,], saxM, type="sax", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]]
}

#save(gsSAXhclust_all, file = "resutls_models/gsSAXhclust_all.Rdata")
#save(gsSAXkmeans_all, file = "resutls_models/gsSAXkmeans_all.Rdata")




## 3. Hurst coef

### choosing the same alfa that was the best for the case of the previous SVM in SAX

load("resutls_models/gsSAXsvm_all.Rdata")
accuraciesSAXsvm = unlist(gsSAXsvm_all)
gsAlphasvm = which.max(accuraciesSAXsvm) + 2 # Alpha for best accuracy of SVM with SAX: 5
gsSAXalphasvm = f.toSAX(dfSC2, w, gsAlphasvm)

### Computing HURST coefficient

gsSAXHurst= f.HurstVector(df = gsSAXalphasvm )
head(gsSAXHurst)


##
# Eigen:
##

## 1. Data segmentation

gsEigen = f.Eigen(sc = gs)  # segment the data using BEATS
gsEigen2 = data.frame(cbind(classId, gsEigen))   # add the class to the segmentated data
head(gsEigen2)

## 2. Classification

### 2.1 THE CODE: 

gsEigenrf = f.MachineLearning(data = gsEigen2, modelo = "rf")
#save(gsEigenrf, file = "resutls_models/gsEigenrf.Rdata")

gsEigensvm = f.MachineLearning(data = gsEigen2, modelo = "svm")
#save(gsEigensvm, file = "resutls_models/gsEigensvm.Rdata")



## 3. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

gsEigenhclust = f.myHclust(data=gsEigen[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

gsEigenkmeans = f.myKmeans(data=gsEigen[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608


#save(gsEigenhclust, file = "resutls_models/gsEigenhclust.Rdata")
#save(gsEigenkmeans, file = "resutls_models/gsEigenkmeans.Rdata")

## 4. Hurst coef

gsEigenHurst= f.HurstVector(df = gsEigen )
head(gsEigenHurst)

##
# Raw: Using the whole raw datset
##

## 1. Classification

### 1.1 THE CODE: 
gsRawrf = f.MachineLearning(data = dfSC2, modelo = "rf")
#save(gsRawrf, file = "resutls_models/gsRawrf.Rdata")

gsRawsvm = f.MachineLearning(data = dfSC2, modelo = "svm")
#save(gsRawsvm, file = "resutls_models/gsRawsvm.Rdata")

## 2. Clustering

partition = 0.1   #selecting a 10 % of the series
set.seed(1234)
iS = createDataPartition(classId, p=partition)$Resample1

gsRawhclust = f.myHclust(data=dfSC2[iS,], saxM =NA , 
                           type="cos", n = NA, kk=length(levels(factor(classId))))[[3]]  #   0.5608 

gsRawkmeans = f.myKmeans(data=dfSC2[iS,], saxM =NA, 
                           type="cos", n = ncol(dfSC), kk=length(levels(factor(classId))))[[3]] # 0.5608

#save(gsRawhclust, file = "resutls_models/gsRawhclust.Rdata")
#save(gsRawkmeans, file = "resutls_models/gsRawkmeans.Rdata")


## 3. Hurst coef

gsRawHurst= f.HurstVector(df =dfSC )
head(gsRawHurst)






# ALL RESULTS LOADING AND CREATING DATA FRAME

file_names=as.list(dir("resutls_models"))
lapply(paste0("resutls_models/",file_names),load,.GlobalEnv)



accuraciesSAXrf = as.numeric(as.character(lapply(gsSAXrf_all, "[[",1)))
gsAlpharf = which.max(accuraciesSAXrf) 
gsSAX1rf = accuraciesSAXrf[which.max(accuraciesSAXrf)] 

accuraciesSAXsvm = as.numeric(as.character(lapply(gsSAXsvm_all, "[[",1)))
gsAlphasvm = which.max(accuraciesSAXsvm) 
gsSAX1svm = accuraciesSAXsvm[which.max(accuraciesSAXsvm)] 

gsSAX2rf = accuraciesSAXrf[gsAlphasvm] 
gsSAX2svm = accuraciesSAXsvm[gsAlpharf] 


gsAlphahclust = which.max(gsSAXhclust_all) # Alpha for best silhouette of hclust with SAX: 6
gsSAX1hclust = gsSAXhclust_all[gsAlphahclust] # Best silhouette of hclust with SAX inputs: 0.51

gsAlphaskmeans = which( gsSAXkmeans_all == max(gsSAXkmeans_all, na.rm=T) ) # In case that is the same alfa than previoulsy
ifelse(gsAlphahclust %in% gsAlphaskmeans, gsAlphakmeans <- gsAlphahclust, gsAlphakmeans <- gsAlphaskmeans[1])  # Alpha for best silhouette of kmeans with SAX: 8

gsSAX1kmeans = gsSAXkmeans_all[gsAlphakmeans] # Best silhouette of hclust with SAX inputs: 0.42

gsSAX2hclust = gsSAXhclust_all[gsAlphakmeans] # One accuracy of hclust with SAX (best alfa for kmeans) inputs: 0.426
gsSAX2kmeans = gsSAXkmeans_all[gsAlphahclust] # One accuracy of kmeans with SAX (best alfa for hclust) inputs: 0.44


segmentationCLA = c("BEATS", "BEATS", 
                    paste0("SAX (a=",gsAlpharf, ")"),paste0("SAX (a=",gsAlpharf, ")"), 
                    paste0("SAX (a=",gsAlphasvm, ")"),paste0("SAX (a=",gsAlphasvm, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
model = c("RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM", "RF", "SVM")
accuracy = unlist(c(gsBEATSrf[[1]],gsBEATSsvm[[1]], gsSAX1rf,gsSAX2svm, gsSAX2rf,gsSAX1svm,
                    gsEigenrf[[1]],gsEigensvm[[1]], gsRawrf[[1]], gsRawsvm[[1]]))
segmentationCLU = c("BEATS", "BEATS", 
                    paste0("SAX (a=",gsAlphahclust, ")"), paste0("SAX (a=",gsAlphahclust, ")"),
                    paste0("SAX (a=",gsAlphakmeans, ")"), paste0("SAX (a=",gsAlphakmeans, ")"),
                    "Eigen", "Eigen", "Raw", "Raw")
method= c("Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans", "Hclust", "Kmeans")
silhouette = c(gsBEATShclust, gsBEATSkmeans, gsSAX1hclust, gsSAX2kmeans,gsSAX2hclust,
               gsSAX1kmeans, gsEigenhclust, gsEigenkmeans, gsRawhclust, gsRawkmeans)

df = data.frame(segmentationCLA, model, accuracy,segmentationCLU, method, silhouette)



### gs HURST  all
hurst.df <- data.frame(gsRawHurst, gsBEATSHurst, gsEigenHurst,gsSAXHurst)
hurst.df2 = abs(hurst.df- hurst.df[,1])[-1]
colMeans(hurst.df2, na.rm=T) 
#gsBEATSHurst gsEigenHurst   gsSAXHurst 
#0.0434318    0.0507070    0.1272414 
hurst.df3 = data.frame(timeS = 1:nrow(hurst.df2), hurst.df2)
hurst.df4 <- melt(hurst.df3 ,  id.vars = 'timeS')
names(hurst.df4) = c("timeSeries", "substracted", "difference")
ggplot(hurst.df4, aes(timeSeries,difference)) + geom_line(size = 0.75,aes(colour = substracted, linetype = substracted))
