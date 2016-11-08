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

dfRG = data.frame(segmentationCLA, model, accuracy,segmentationCLU, method, silhouette)
