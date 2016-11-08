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

dfAH = data.frame(segmentationCLA, model, accuracy,segmentationCLU, method, silhouette)

