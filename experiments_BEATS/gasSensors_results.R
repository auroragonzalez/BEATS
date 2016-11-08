
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

