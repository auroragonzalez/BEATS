library("lsa")  # library to compute cosinus distance
library("party") # library for ctree
library("wavelets")
library("TSclust")
library("doParallel")
library("caret")
library("randomForest")
library("brnn")
library("TSMining")  # for the clustering
library("cluster")
library("e1071")
library("rpart")
library("fractaldim")
library("clValid") # dunn index for clustering
library("dtt")

seed = 1234
options(myseed = seed)

# a is a matrix
f.fastDCT = function(a){
  siz=dim(a)
  ndim=2
  ww = list()
  ind = list()
  for(i in 1:ndim){
    n=siz[i]
    ww[[i]] = 2*(exp((-complex(real=0, imaginary = 1)*pi/(2*n))*(0:7)))/ (sqrt(2*n))
    ww[[i]][1] = ww[[i]][1]/sqrt(2)
    ind[[i]] = matrix(c(seq(1,n,2),rev(seq(2,n,2))),8,8)  + matrix(seq(0,n*(prod(siz)/n-1), n),8,8,T)
  }
  xx = matrix(a[ind[[1]]],8,8)
  xx= apply(xx,2,fft)  # we compute fft column-wise
  xx = t(Re(xx*matrix(ww[[1]],8,8,F)))
  
  xx2 = t(matrix(xx[t(ind[[2]])],8,8))
  xx2= apply(xx2,2,fft)  # we compute fft column-wise
  xx2 = t(Re(xx2*matrix(ww[[2]],8,8,F)))
  return(xx2)
}
  
#We create the U matrix for DCTs, based on this information: [http://www.whydomath.org/node/wavlets/dct.html](http://www.whydomath.org/node/wavlets/dct.html)

# TO DO: SEE IF IT MAKE SENSE TO DO IT WITH MATRICES OF LESS THAN 8 X 8
f.U <- function(n){
  if(n==7){
    l0 = rep(sqrt(2)/2,7)
    for(i in 1:6){
      assign(paste0("l",i), cos((i*pi + (0:6)*i*2*pi)/16))   
    }
    
    U = 1/2*matrix(c(l0,l1,l2,l3,l4,l5,l6),7,byrow=T)
  }
  if(n==8){
    l0 = rep(sqrt(2)/2,8)
    for(i in 1:7){
      assign(paste0("l",i), cos((i*pi + (0:7)*i*2*pi)/16))   
    }
    
    U = 1/2*matrix(c(l0,l1,l2,l3,l4,l5,l6,l7),8,byrow=T)
  }
  
  if (n == 4){
    
  }
  
  return(U)
}



#f. eigenValuesDCT computes the DCT of your matrix and return its eigenvalues. Inputs: your matrix and the U matrix for the transformation

f.eigenValues <- function(A){
  v = c(eigen(A)$values)
  return (v)  
}

f.eigenValuesDCT <- function(A,U){
  B = U%*%A%*%t(U)
  v = f.eigenValues(B)
  return (v)  
}


f.DCT <- function(A,U){
  B = U%*%A%*%t(U)
  return (B)  
}


# Sliding windows: We create a function that returns a list of indices of the data that we are going to consider for each matrix that we are going to compute the eigenvalues from (depending on the window size and the number of data we slide for each group)

f.sliwi <- function(slide=8, window=64,size_data = n){
  iM <- list()
  nM <- as.integer((size_data-window)/slide + 1) # number of matrices
  for(i in 0:(nM-1)){
    index = (1+i*slide):(window+i*slide)
    iM[[i+1]] <- index
  }
  return(iM)  
}



# Cosine simmilarity between vectors: Once we have a list of vectors of eigenvalues (each one coming from each matrix), we compute the cosine distance between the pairs and save it into a matrix of SIMILARITIES (then we will do 1-similarities to get the dissimilarity)


f.cosDistMat <- function(vectors_list = v, name="v", numberM = nM){
  DIST=matrix(data=NA, nrow=numberM, ncol=numberM)
  for(j in 1:numberM){
    for(i in 1:numberM){
      DIST[i,j] = cosine(vectors_list[[i]], vectors_list[[j]])
    }
  }
  
  dist <- data.frame(DIST)
  rownames(dist) <- paste0(name,1:numberM)
  colnames(dist) <- paste0(name,1:(numberM))
  return(dist)
}



# f.cosDistMat2 <- function(vectors_list1 = v,vectors_list2 = w, name1="v", name2="w" ){
#   DIST=matrix(data=NA, nrow=length(vectors_list1), ncol=length(vectors_list2))
#   for(j in 1:length(vectors_list2)){
#     for(i in 1:length(vectors_list1)){
#       DIST[i,j] = cosine(vectors_list1[[i]], vectors_list2[[j]])
#     }
#   }
#   
#   dist <- data.frame(DIST)
#   rownames(dist) <- paste0(name1,1:length(vectors_list1))
#   colnames(dist) <- paste0(name2,1:length(vectors_list2))
#   return(dist)
# }


f.toSAX <- function(sc, w, alpha){
  saxData <- NULL
  for (i in 1:nrow(sc)) {
    a <- t(sc[i,-1])
    x <- (a - mean(a)) /sd(a)
    paax <- PAA(x, w) #generate PAA reductions
    SAXx <- convert.to.SAX.symbol( paax, alpha)
    saxData = rbind(saxData, SAXx)
  }
  rownames(saxData) = 1:nrow(sc)
  return(as.data.frame(saxData))
}


f.toDWT <- function(sc){
  wtData <- NULL
  for (i in 1:nrow(sc)) {
    a <- t(sc[i,-1])
    wt <- dwt(a, filter="haar", boundary="periodic")
    wtData <- rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
  }
  return(as.data.frame(wtData))
}





f.tomySAX <- function(sc, sl = 64, wd= 64){
  mysaxData = NULL
    indices = f.sliwi(slide = sl, window = wd, size_data = (ncol(sc)-1))
    le = 8
  
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- f.DCT(matrix(a[indices[[k]]],le),f.U(le))
      v = c(v, v1)
    }
    mysaxData <- rbind(mysaxData, v)
  }
  rownames(mysaxData) = 1:nrow(sc)
  return(as.data.frame(mysaxData))
}




f.tomySAX2 <- function(sc, sl = 64, wd= 64){
  mysaxData = NULL
  indices = f.sliwi(slide = sl, window = wd, size_data = ncol(sc))
  le = 8
  
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- eigen(f.DCT(matrix(a[indices[[k]]],le),f.U(le))[1:2,1:2])$values
      v = c(v, v1)
    }
    mysaxData <- rbind(mysaxData, v)
  }
  rownames(mysaxData) = 1:nrow(sc)
  return(as.data.frame(mysaxData))
}

q = c(16,12,14,14,18,24,49,72,11,12,13,17,22,35,64,92,10,14,16,22,37,55,78,95,16,19,24,29,56,64,
  87,98,24,26,40,51,68,81,103,112,40,58,57,87,109,104,121,100,51,60,69,80,103,113,120,103,61,55,
  56,62,77,92,101,99)
Z = matrix(q, 8, 8)

# module of the eigenvalues of the quantized matrix

f.tomySAX3 <- function(sc, sl = 64, wd= 64){
  mysaxData = NULL
  indices = f.sliwi(slide = sl, window = wd, size_data = (ncol(sc)))
  le = 8
  
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- f.DCT(matrix(a[indices[[k]]],le),f.U(le))
      v = c(v, eigen(round(v1/Z, 2)[1:4, 1:4])$values)
    }
    mysaxData <- rbind(mysaxData, v)
  }
  rownames(mysaxData) = 1:nrow(sc)
  return(as.data.frame(mysaxData))
}


f.toSVD <- function(sc){
  svdData = NULL
  if(sqrt(ncol(sc)) < 8){
    le = as.integer(sqrt(ncol(sc)))
    indices = f.sliwi(slide = le*le, window = le*le, size_data = ncol(sc))
    le = 7
    
  } else{
    indices = f.sliwi(slide = 64, window = 64, size_data = ncol(sc))
    le = 8
  }
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- svd(matrix(a[indices[[k]]],le))$d 
      v = c(v, v1)
    }
    svdData <- rbind(svdData, v)
  }
  rownames(svdData) = 1:nrow(sc)
  return(as.data.frame(svdData))
}

f.DCT <- function(A,U){
  B = U%*%A%*%t(U)
  return (B)  
}

f.toSVDfromDCT <- function(sc){
  mysaxData = NULL
  if(sqrt(ncol(sc)) < 8){
    le = as.integer(sqrt(ncol(sc)))
    indices = f.sliwi(slide = le*le, window = le*le, size_data = ncol(sc))
    le = 7
    
  } else{
    indices = f.sliwi(slide = 64, window = 64, size_data = ncol(sc))
    le = 8
  }
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- svd(f.DCT(matrix(a[indices[[k]]],le),f.U(le)))$d 
      v = c(v, v1)
    }
    mysaxData <- rbind(mysaxData, v)
  }
  rownames(mysaxData) = 1:nrow(sc)
  return(as.data.frame(mysaxData))
}




f.toSVD2 <- function(sc, wd, sl){
  svdData = NULL
  if(sqrt(ncol(sc)) < 8){
    le = as.integer(sqrt(ncol(sc)))
    indices = f.sliwi(slide = sl, window = wd, size_data = ncol(sc))
    le = 7
    
  } else{
    indices = f.sliwi(slide = sl, window = wd, size_data = ncol(sc))
    le = 8
  }
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      v1 <- svd(matrix(a[indices[[k]]],le))$d 
      v = c(v, v1)
    }
    svdData <- rbind(svdData, v)
  }
  rownames(svdData) = 1:nrow(sc)
  return(as.data.frame(svdData))
}




f.realpart <- function(M){
  newM = matrix( NA, nrow = nrow(M), ncol=ncol(M))
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      newM[i,j] = Re(M[i,j])
    }
  }
  return(as.data.frame(newM))
}


# returns the module of complex numbers in a data frame or matrix



f.mod <- function(M){
  newM = matrix( NA, nrow = nrow(M), ncol=ncol(M))
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      newM[i,j] = Mod(M[i,j])
    }
  }
  return(as.data.frame(newM))
}

f.mod_plus_Arg <- function(M){
  newM = matrix( NA, nrow = nrow(M), ncol=ncol(M))
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      newM[i,j] = Mod(M[i,j])+ Arg(M[i,j])
    }
  }
  return(as.data.frame(newM))
}


f.Cantor_pairing <- function(df){
  df2 = matrix(NA, nrow=nrow(df), ncol = ncol(df))
  for(i in 1:ncol(df)){
    Re = Re(df[,i])
    Im = Im(df[,i])
    cantor = 1/2*(Re + Im)*(Re + Im + 1) + Im 
    df2[,i] = cantor
  }
  return(as.data.frame(df2))
}


f.Cantor_pairingRounded <- function(df){
  df2 = matrix(NA, nrow=nrow(df), ncol = ncol(df))
  df = round(df*1000)
  for(i in 1:ncol(df)){
    Re = Re(df[,i])
    Im = Im(df[,i])
    cantor = 1/2*(Re + Im)*(Re + Im + 1) + Im 
    df2[,i] = cantor
  }
  return(as.data.frame(df2))
}

library(rPython)
python.load("hilbert.py")

f.HilbertRounded <- function(df){
  df2 = matrix(NA, nrow=nrow(df), ncol = ncol(df))
  df = round(df*10)
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){
      Im = Im(df[i,j])
      hilbert = python.call( "Hilbert_to_int", c(Re,Im))
      df2[i,j] = hilbert
    }
  }
  return(as.data.frame(df2))
}


#install.packages("devtools", repos = "http://cran.us.r-project.org")
#install_github("sigopt/SigOptR")

library(devtools)
library(SigOptR)
f.GAopNOPARAL <- function(X, y, model="rf", j=20){
  dev_token = "NKBSDZRNCQBDIKDGLJHXFTSXYRTEWQOJOERNTZOCNWRQHWBA"
  usr_toke ="TSRCWGUBWSFZJGFGFJVHCKTOYBNZHYRXGJLSBALCIVUVJXFE"
  Sys.setenv(SIGOPT_API_TOKEN=dev_token)
  if(model=="rf"){
    set.seed(seed)
    experiment <- create_experiment(list(
      name="Random Forest (R)",
      parameters=list(
        list(name="mtry", type="int", bounds=list(min=1, max=ncol(X)-1)),
        list(name="ntree", type="int", bounds=list(min=1, max=100)),
        list(name="sampsize", type="double", bounds=list(min=0.25, max=1)),
        list(name="nodesize", type="int", bounds=list(min=1, max=10))
      )
    ))
    print(paste("Created experiment: https://sigopt.com/experiment", experiment$id, sep="/"))
    evaluate_model <- function(assignments, X, y) {
      # evaluate cross folds for accuracy
      num_folds = 5
      cv_accuracies = c()
      for (k in 1:num_folds){
        set.seed(seed)
        cv_split <- sample(2, nrow(X), replace=TRUE, prob=c(1-1.0/num_folds, 1.0/num_folds))
        X_train <- X[cv_split==1, ]
        y_train <- y[cv_split==1]
        X_valid <- X[cv_split==2, ]
        y_valid <- y[cv_split==2]
        set.seed(seed)
        rf_fit <- randomForest(y=factor(y_train),
                               x=X_train,
                               ntree=assignments$ntree,
                               mtry=assignments$mtry,
                               sampsize=assignments$sampsize*nrow(X_train),
                               nodesize=assignments$nodesize,
                               proximity=TRUE)
        set.seed(seed)
        prediction <- predict(rf_fit, X_valid)
        correct_predictions <- prediction == y_valid
        accuracy <- sum(correct_predictions)/nrow(X_valid)
        cv_accuracies <- c(accuracy, cv_accuracies)
      }
      return(c(mean(cv_accuracies), sd(cv_accuracies)))
    }
    for (i in 1:j) {
      # Receive a Suggestion from SigOpt
      set.seed(seed)
      suggestion <- create_suggestion(experiment$id)
      
      # Evaluate the model locally
      set.seed(seed)
      res <- evaluate_model(suggestion$assignments, X, y)
      
      # Report an Observation (with standard deviation) back to SigOpt
      set.seed(seed)
      create_observation(experiment$id, list(suggestion=suggestion$id,
                                             value=res[1],
                                             value_stddev=res[2]))
    }
    
    # Re-fetch the experiment to get the best observed value and assignments
    set.seed(seed)
    experiment <- fetch_experiment(experiment$id)
    set.seed(seed)
    best_assignments <- experiment$progress$best_observation$assignments
  }
  

  if(model=="brnn"){
    experiment <- create_experiment(list(
      name="Bayesian Regularized Neural Network(R)",
      parameters=list(
        list(name="neurons", type="int", bounds=list(min=1, max=50)),
        list(name="epochs", type="int", bounds=list(min=500, max=1500)),
        list(name="mu", type="double", bounds=list(min=0.001, max=0.01))
      )
    ))
    print(paste("Created experiment: https://sigopt.com/experiment", experiment$id, sep="/"))
    evaluate_model <- function(assignments, X, y) {
      # evaluate cross folds for accuracy
      num_folds = 5
      cv_accuracies = c()
      for (k in 1:num_folds){
        set.seed(seed)
        cv_split <- sample(2, nrow(X), replace=TRUE, prob=c(1-1.0/num_folds, 1.0/num_folds))
        X_train <- X[cv_split==1, ]
        y_train <- y[cv_split==1]
        X_valid <- X[cv_split==2, ]
        y_valid <- y[cv_split==2]
        
        brnn_fit <- brnn(y=as.numeric(y_train),
                               x=as.matrix(X_train),
                               neurons=assignments$neurons,
                               epochs=assignments$epochs,
                               mu=assignments$mu)
        prediction <- predict(brnn_fit , X_valid)
        correct_predictions <- prediction == y_valid
        accuracy <- sum(correct_predictions)/nrow(X_valid)
        cv_accuracies <- c(accuracy, cv_accuracies)
      }
      return(c(mean(cv_accuracies), sd(cv_accuracies)))
    }
    for (i in 1:j) {
      # Receive a Suggestion from SigOpt
      suggestion <- create_suggestion(experiment$id)
      
      # Evaluate the model locally
      res <- evaluate_model(suggestion$assignments, X, y)
      
      # Report an Observation (with standard deviation) back to SigOpt
      create_observation(experiment$id, list(suggestion=suggestion$id,
                                             value=res[1],
                                             value_stddev=res[2]))
    }
    
    # Re-fetch the experiment to get the best observed value and assignments
    experiment <- fetch_experiment(experiment$id)
    best_assignments <- experiment$progress$best_observation$assignments
  }
  
  
  if(model=="svm"){
    set.seed(seed)
    experiment <- create_experiment(list(
      name="SVM (R)",
      parameters=list(
        list(name="degree", type="int", bounds=list(min=1, max=7)),
        list(name="cost", type="double", bounds=list(min=0.1, max=10))
      )
    ))
    print(paste("Created experiment SVM: https://sigopt.com/experiment", experiment$id, sep="/"))
    evaluate_model <- function(assignments, X, y) {
      # evaluate cross folds for accuracy
      num_folds = 5
      cv_accuracies = c()
      for (k in 1:num_folds){
        set.seed(seed)
        cv_split <- sample(2, nrow(X), replace=TRUE, prob=c(1-1.0/num_folds, 1.0/num_folds))
        X_train <- X[cv_split==1, ]
        y_train <- y[cv_split==1]
        X_valid <- X[cv_split==2, ]
        y_valid <- y[cv_split==2]
        
        svm_fit <- svm(factor(y_train),
                         x=X_train,
                        kernel ="radial",
                         degree=assignments$degree,
                         cost=assignments$cost)
        
        
        prediction <- predict(svm_fit , X_valid)
        correct_predictions <- prediction == y_valid
        accuracy <- sum(correct_predictions)/nrow(X_valid)
        cv_accuracies <- c(accuracy, cv_accuracies)
      }
      return(c(mean(cv_accuracies), sd(cv_accuracies)))
    }
    for (i in 1:j) {
      # Receive a Suggestion from SigOpt
      suggestion <- create_suggestion(experiment$id)
      
      # Evaluate the model locally
      res <- evaluate_model(suggestion$assignments, X, y)
      
      # Report an Observation (with standard deviation) back to SigOpt
      create_observation(experiment$id, list(suggestion=suggestion$id,
                                             value=res[1],
                                             value_stddev=res[2]))
    }
    
    # Re-fetch the experiment to get the best observed value and assignments
    experiment <- fetch_experiment(experiment$id)
    best_assignments <- experiment$progress$best_observation$assignments
  }
  return(best_assignments)
  
}



f.MachineLearning <- function(data, modelo="rf", iteraciones=20){
  set.seed(seed)
  trainIndex <- createDataPartition(data$classId, p = .75,
                                    list = FALSE,
                                    times = 1)
  trainset <- data[ trainIndex, -1]
  testset  <- data[-trainIndex, -1]
  set.seed(seed)
  preproc = preProcess(trainset, method=c("center", "scale"))
  set.seed(seed)
  trainTransformed <- predict(preproc, trainset)
  set.seed(seed)
  testTransformed <- predict(preproc, testset)
  #trainF =cbind(classId=  data[ trainIndex, 1], trainTransformed)
  best_assignments <- f.GAopNOPARAL(X = trainTransformed, y = data[ trainIndex, 1], model = modelo, j= iteraciones)
  
  if(modelo=="rf"){
    set.seed(seed)
    mod.final <- randomForest(x=trainTransformed,
                       y=factor(data[ trainIndex, 1]),
                       ntree=best_assignments$ntree,
                       mtry=best_assignments$mtry,
                       sampsize=best_assignments$sampsize*nrow(trainTransformed),
                       nodesize=best_assignments$nodesize,
                       proximity=TRUE)
  }
  if(modelo=="brnn"){
    set.seed(seed)
    mod.final <- brnn(x=trainTransformed,
                              y=data[ trainIndex, 1],
                      neurons=best_assignments$neurons,
                      epochs=best_assignments$epochs,
                      mu=best_assignments$mu)
  }
  
  
  
  if(modelo=="svm"){
    set.seed(seed)
    mod.final <- svm(x=trainTransformed,
                     y=factor(data[ trainIndex, 1]),
                     kernel = "radial",
                     degree=best_assignments$degree,
                     cost=best_assignments$cost)
  }
  
  set.seed(seed)
  predictions = predict(mod.final, newdata = testTransformed)
  table(data[-trainIndex, 1], predictions)
  finalAcc = (sum(data[-trainIndex, 1]==predictions)) / nrow(testset)
  return(list(finalAcc, 
              best_assignments, 
              c(sum(data[-trainIndex, 1]==predictions), nrow(testset)))
         )
  
}


f.MachineLearning2 <- function(data, modelo="rf", iteraciones=20){
  set.seed(seed)
  trainIndex <- createDataPartition(data$classId, p = .75,
                                    list = FALSE,
                                    times = 1)
  trainset <- data[ trainIndex, -1]
  testset  <- data[-trainIndex, -1]
  preproc = preProcess(trainset, method=c("center", "scale"))
  trainTransformed <- predict(preproc, trainset)
  testTransformed <- predict(preproc, testset)
  #trainF =cbind(classId=  data[ trainIndex, 1], trainTransformed)
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, returnResamp = "all", allowParallel = T)
  
 # best_assignments <- f.GAopNOPARAL(X = trainTransformed, y = data[ trainIndex, 1], model = modelo, j= iteraciones)
  
  if(modelo=="rf"){
    paramGrid <- expand.grid(.mtry = c(1:10))
    set.seed(seed)
    
    # Find the optimum combination of parameters
    
    mod.final <- caret::train(
      x=trainTransformed,
      y=data[ trainIndex, 1],
      method = "rf",
      metric = "Accuracy", # Metric to evaluate
      tuneGrid = paramGrid, # Parameters for tunning
      trControl = ctrl, # Parameters of control   
      preProc = c("center", "scale") 
    )
    
  
  }
  if(modelo=="brnn"){
    paramGrid <- expand.grid(.neurons = c(2,4,5,10,20))
    set.seed(seed)
    mod.final <- caret::train(
      x=trainTransformed,
      y=data[ trainIndex, 1],
      method = "brnn",
      metric = "Accuracy", # Metric to evaluate
      tuneGrid = paramGrid, # Parameters for tunning
      trControl = ctrl, # Parameters of control   
      preProc = c("center", "scale") 
    )
  }
  
  
  
  if(modelo=="svm"){
    paramGrid <- expand.grid(.C = c(4:15,18,20,22))
    set.seed(seed)
    mod.final <- caret::train(
      x=trainTransformed,
      y=data[ trainIndex, 1],
      method = "svmRadialCost",
      metric = "Accuracy", # Metric to evaluate
      tuneGrid = paramGrid, # Parameters for tunning
      trControl = ctrl, # Parameters of control   
      preProc = c("center", "scale") 
    )
  }
  
  
  predictions = predict(mod.final, newdata = testTransformed)
  table(data[-trainIndex, 1], predictions)
  finalAcc = (sum(data[-trainIndex, 1]==predictions)) / nrow(testset)
  return(list(finalAcc, best_assignments))
  
}

f.myCOSdistance = function(data){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      matrixPrepared[j,i] = cosine(as.numeric(data[j,]), as.numeric(data[i,]))
    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(1-matrixPrepared2)
  return(dist2)
  
}

norm_vec <- function(x) sqrt(sum(x^2))
fromComplexto2d =function(vector){
  return(as.vector(t(data.frame(Re(as.complex(vector)), Im(as.complex(vector))))))  
}

f.myCOSdistanceForComplex = function(data){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      cos = NULL
      for(k in 2: ncol(data)){
        x1 = data[i,k]
        x2 = data[j,k]
        cos = c(cos, cosine(c(Re(x1), Im(x1)), c(Re(x2), Im(x2))))
        }
      cosSum = sqrt(sum(cos^2)/ncol(data))
      
      matrixPrepared[j,i] = cosSum
    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(1-matrixPrepared2)
  return(dist2)
  
}



f.myCOSdistanceForComplex3 = function(data){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      matrixPrepared[j,i] = cosine(fromComplexto2d(data[j,]), fromComplexto2d(data[i,]))

    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(1-matrixPrepared2)
  return(dist2)
  
}

f.myEUCdistanceForComplex2 = function(data){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      matrixPrepared[j,i] = as.numeric(dist(rbind(fromComplexto2d(data[j,]), fromComplexto2d(data[i,]))))
    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(matrixPrepared2)
  return(dist2)
  
}

f.myANGdistanceForComplex = function(data){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      cos = NULL
      for(k in 2: ncol(data)){
        x1 = data[i,k]
        x2 = data[j,k]
        cos = c(cos, cosine(c(Re(x1), Im(x1)), c(Re(x2), Im(x2))))
      }
      cosSum = sqrt(sum(cos^2)/ncol(data))
      
      matrixPrepared[j,i] = acos(cosSum)/pi
    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(1-matrixPrepared2)
  return(dist2)
  
}

f.SAXdistance = function(data, saxM, n=60){
  
  matrixPrepared = matrix(NA, nrow = nrow(data), ncol = nrow(data))
  for(i in 1:(nrow(data)-1)){
    for(j in (1+i):nrow(data)){
      matrixPrepared[j,i] = Func.dist(as.character(data[j,]), as.character(data[i,]), mat = saxM, n)
    }
  }
  
  matrixPrepared2 = matrixPrepared
  dist2 = as.dist(matrixPrepared2)
  return(dist2)
  
}
f.myHclust = function (data, type, saxM, n, kk, pl = F){
  if(type=="sax"){
    dist1 = f.SAXdistance(data, saxM,n)
  }
  if(type == "cos"){
    dist1 = f.myCOSdistanceForComplex3(data)
  }
  if(type == "euc"){
    dist1 = f.myEUCdistanceForComplex2(data)
  }
  df.hclust<- hclust(dist1, method="complete")
  if(pl){
    plot(df.hclust,hang=-1)
  }
  df.hclustk <- cutree(df.hclust,k=kk)
  
  sil <- silhouette(df.hclustk, dist1)
  sil.summ <- summary(sil)
  dunn_index = dunn(distance = dist1, clusters = df.hclustk)
  return(list(df.hclustk,table(df.hclustk), sil.summ$si.summary[4], dunn_index, sil[,3]))
  
}
f.myKmeans = function (data, type, saxM, n, kk){
  if(type=="sax"){
    dist1 = f.SAXdistance(data, saxM,n)
  }
  if(type=="cos"){
    dist1 = f.myCOSdistanceForComplex3(data)
  }
  if(type == "euc"){
    dist1 = f.myEUCdistanceForComplex2(data)
  }
  k= kmeans(dist1, centers=kk)
  sil <- silhouette(k$cl, dist1)
  sil.summ <- summary(sil)
  dunn_index = dunn(distance = dist1, clusters = k$cl)
  return(list(k$cl,table(k$cl), sil.summ$si.summary[4], dunn_index, sil[,3]))
  
}



f.myClustering = function(clases, dataSets2, tipo, partition = 0.1, alpha = 5){
  set.seed(seed)
  iS = createDataPartition(clases, p=partition)$Resample1
  saxM = Func.matrix(alpha)
  colnames(saxM) = 1:alpha
  rownames(saxM) = 1:alpha
  clHResults = NULL
  for(i in 1:length(dataSets2)){
      cluster = f.myHclust(data= dataSets2[[i]][iS,], saxM, type=tipo[i], n = ncol(dataSets2[[1]]), kk=length(levels(factor(clases))))
      clHResults[[i]] = cluster
  }
  clKResults = NULL
  for(i in 1:length(dataSets2)){
      cluster = f.myKmeans(data= dataSets2[[i]][iS,], saxM, type=tipo[i], n = ncol(dataSets2[[1]]), kk=length(levels(factor(clases))))
      clKResults[[i]] = cluster
      }
  return(list(clHResults, clKResults))
}





f.Hurst <- function(rf){
  fd2 <- fd.estimate(rf,methods = list("hallwood"),
                     window.size = 5000, step.size = 5000, plot.loglog = F, nlags = 10)
  H = 2 - mean(fd2$fd)
  return(H)
}

f.HurstVector <- function(df, nseries=100){
  hurstV=NULL
  for(j in 1:nseries){
    hurstV = c(hurstV, f.Hurst(as.numeric(df[j,])))
  }
  return(hurstV)
}



require(ggplot2)
require(reshape)

f.hurstDiff = function(dataSets2, nseries = nrow(dfSC)){
  hurst = list()
  for(i in 1:length(dataSets2)){
    h = NULL
    for(j in 1:nseries){
      h = c(h, f.Hurst(as.numeric(dataSets2[[i]][j,])))
    }
    hurst[[i]] = h
  }
  
  hurst.df <- data.frame(sapply(hurst,c))
  names(hurst.df)= c("all", "sax", "svd", "svdDCT", "fetaMod")
  hurst.df2 = abs(hurst.df- hurst.df[,1])[-1]
  hurst.df3 = data.frame(timeS = 1:nrow(hurst.df2), hurst.df2)
  hurst.df4 <- melt(hurst.df3 ,  id.vars = 'timeS')
  names(hurst.df4) = c("timeSeries", "substracted", "difference")
  return (list(hurst.df2,  
               ggplot(hurst.df4, aes(timeSeries,difference)) + geom_line(size = 0.75,aes(colour = substracted, linetype = substracted))
               ))
}





f.centroid = function(clusterdf){
  return(colMeans(clusterdf))
}


getDist = function(x,y, dist = "Eucl"){
  d=0
  if(dist == "Eucl"){
    d = dist(rbind(x,y))
  }
  return(as.numeric(d))
}


library("plyr")
my_kmeans = function(df, centers, cutoff, seed= 1234){
  set.seed(seed)
  initial = df[sample(nrow(df), centers), ]
  clusters = list()
  for(i in 1:nrow(initial)){
    clusters[[i]] = initial[i,]
  }
  clusters
  
  
  lists <- vector("list", length(clusters))
  clInic = clusters[[i]][-1,]
  
  for(i in 1:length(lists)){
    lists[[i]] = clInic
  }
  
  while (T) {
    for(i in 1:length(lists)){
      lists[[i]] = clInic
    }
    for(m in 1:nrow(df)){
      smallest_distance = getDist(df[m,],f.centroid(clusters[[1]]))
      index = 1
      for (i in 1:(length(clusters)-1)){
        distance = getDist(df[m,], f.centroid(clusters[[i+1]]))
        if (distance < smallest_distance){
          smallest_distance = distance
          index = i+1
        }
      }
      lists[[index]] = rbind(lists[[index]], df[m,])
    }
    
    
    biggest_shift = 0.0
    for (l in 1:length(clusters)){
      shift = getDist(f.centroid(clusters[[l]]), f.centroid(lists[[l]]))
      clusters[[l]] = lists[[l]]
      biggest_shift = max(biggest_shift, shift)
    }
    
    if(biggest_shift < cutoff){
      break
    }
  }
  
  nrow(clusters[[1]]) + nrow(clusters[[2]]) + nrow(clusters[[3]])
  
  as.numeric(rownames(clusters[[1]]))
  as.numeric(rownames(clusters[[2]]))
  as.numeric(rownames(clusters[[3]]))
  
  
  for(i in 1:length(clusters)){
    clusters[[i]] = data.frame(clusters[[i]], class = rep(i,nrow(clusters[[i]])), id = as.numeric(rownames(clusters[[i]])))
  }
  df <- ldply(clusters, data.frame)
  df = df[order(df$id),]
  table(df$class)
  return(df$class)
  
}





f.agregations = function(dfSC, w, alpha, classId,wd = 64 , sl=64){
  dfSC2 <- data.frame(cbind(classId, dfSC))
  saxData = f.toSAX(dfSC2, w, alpha)  
  mysaxData = f.mod(f.tomySAX3(sc = dfSC2,wd = wd , sl=sl))
 # svdData = f.toSVD(sc=dfSC2)
#  svdDctData = f.toSVDfromDCT(sc=dfSC2)
#  mysaxDataMod = f.mod(mysaxData)
  
  
  saxData2 =  data.frame(cbind(classId, saxData))
  mysaxData2 = data.frame(cbind(classId, mysaxData))
 # svdData2 = data.frame(cbind(classId, svdData))
#  svdDctData2 = data.frame(cbind(classId, svdDctData))
#  mysaxDataMod2 = data.frame(cbind(classId, mysaxDataMod))
  
  dataSetsId = list(dfSC2, saxData2, mysaxData2)
  dataSets = list(dfSC, saxData, mysaxData)
  return(list(dataSets, dataSetsId))
  
}



f.myRF = function(dataSets){
  rfResults = list()
  for(i in 1:length(dataSets)){
    a = Sys.time()
    saxAcc = f.MachineLearning(data = dataSets[[i]], modelo = "rf")
    timeD = Sys.time()-a
    saxAcc[[3]] = timeD  
    rfResults[[i]] = saxAcc
  }
  return(rfResults)
}

f.mySVM = function(dataSets){
  svmResults = list()
  for(i in 1:length(dataSets)){
    a = Sys.time()
    saxAcc = f.MachineLearning(data = dataSets[[i]], modelo = "svm")
    timeD = Sys.time()-a
    saxAcc[[3]] = timeD  
    svmResults[[i]] = saxAcc
  }
  return(svmResults)
}


f.roundingfactor = function(value){
  rf = 2
  x = abs(floor(log10(value))-4)
  if(x > 2){
    rf = x
  }
  return(rf)
}

myImagePlot <- function(x, ...){
  min <- min(x)
  max <- max(x)
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
  
  # Red and green range from 0 to 1 while Blue ranges from 1 to 0
  ColorRamp <- rgb( seq(1,0,length=256),  # Red
                    seq(0,1,length=256)  # Green
                    ,seq(0,1,length=1)  # Blue
  )
  
  ColorRamp  <- colorRampPalette(c("green", "yellow", "red"))(n = 299)
  ColorLevels <- seq(min, max, length=length(ColorRamp))
  # Reverse Y axis
  reverse <- nrow(x) : 1
  yLabels <- yLabels[reverse]
  x <- x[reverse,]
  
  # Data Map
  par(mar = c(3,5,2.5,2))
  image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
        ylab="", axes=FALSE, zlim=c(min,max))
  if( !is.null(title) ){
    title(main=title)
  }
  axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
  axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
       cex.axis=0.7)
  
  # Color Scale
  par(mar = c(3,2.5,2.5,2))
  image(1, ColorLevels,
        matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
        col=ColorRamp,
        xlab="",ylab="",
        xaxt="n")
  
  layout(1)
}




# sc: a data frame where each row is a variable to convert (or a time series)
f.BEATS = function(sc,sl = 64,wd = 64, size =(ncol(sc)-1)){
  beatsData = NULL
  indices = f.sliwi(slide = sl, window = wd, size_data = size)
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      matriz = matrix(a[indices[[k]]],sqrt(wd))
      v1 <- f.DCT(matriz,f.U(sqrt(wd)))
      x = Mod(eigen(round(v1/Z, f.roundingfactor(max(v1)))[1:4, 1:4])$values)
      sentence = !duplicated(x)
      if(sum(sentence)>= 3){
        x = x[sentence][1:3]
      }
      if(sum(sentence)<3){
        sentence[which(!sentence)[1]] = T
        x = x[sentence]
      }
      v = c(v, x)
    }
    beatsData <- rbind(beatsData, v)
  }
  rownames(beatsData) = 1:nrow(sc)
  return(beatsData)
}





f.Eigen = function(sc,sl = 64,wd = 64, size =(ncol(sc)-1)){
  eigenData = NULL
  indices = f.sliwi(slide = sl, window = wd, size_data = size)
  for(i in 1:nrow(sc)){
    v = NULL
    a <- t(sc[i,-1])
    for(k in 1:length(indices)){
      matriz = matrix(a[indices[[k]]],sqrt(wd))
      v1 <- f.DCT(matriz,f.U(sqrt(wd)))
      x = Mod(eigen(v1)$values)
      sentence = !duplicated(x)
      if(sum(sentence)>= 3){
        x = x[sentence][1:3]
      }
      if(sum(sentence)<3){
        sentence[which(!sentence)[1]] = T
        x = x[sentence]
      }
      v = c(v, x)
    }
    eigenData <- rbind(eigenData, v)
  }
  rownames(eigenData) = 1:nrow(sc)
  return(eigenData)
}



visualize = function (object, data = NULL, stand = TRUE, geom = c("point", 
                                                                  "text"), repel = FALSE, show.clust.cent = TRUE, frame = TRUE, 
                      frame.type = "convex", frame.level = 0.95, frame.alpha = 0.2, 
                      pointsize = 2, labelsize = 4, title = "Cluster plot", jitter = list(what = "label", 
                                                                                          width = NULL, height = NULL), outlier.color = "black", 
                      outlier.shape = 19) 
{
  if (inherits(object, c("partition", "hkmeans", "eclust"))) 
    data <- object$data
  else if ((inherits(object, "kmeans") & !inherits(object, 
                                                   "eclust")) | inherits(object, "dbscan")) {
    if (is.null(data)) 
      stop("data is required for plotting kmeans/dbscan clusters")
  }
  else if (inherits(object, "Mclust")) {
    object$cluster <- object$classification
    data <- object$data
  }
  else if (inherits(object, "HCPC")) {
    object$cluster <- res.hcpc$call$X$clust
    data <- res.hcpc <- object
    stand <- FALSE
  }
  else if (inherits(object, "hcut")) {
    if (inherits(object$data, "dist")) {
      if (is.null(data)) 
        stop("The option 'data' is required for an object of class hcut.")
    }
    else data <- object$data
  }
  else if (!is.null(object$data) & !is.null(object$cluster)) {
    data <- object$data
    cluster <- object$cluster
  }
  else stop("Can't handle an object of class ", class(object))
  if (stand) 
    data <- scale(data)
  cluster <- as.factor(object$cluster)
  pca_performed <- FALSE
  if (inherits(data, c("matrix", "data.frame"))) {
    if (ncol(data) > 2) {
      pca <- stats::prcomp(data, scale = FALSE, center = FALSE)
      ind <- facto_summarize(pca, element = "ind", result = "coord", 
                             axes = 1:2)
      eig <- get_eigenvalue(pca)[, 2]
      xlab = paste0("Dim", 1, " (", round(eig[1], 1), "%)")
      ylab = paste0("Dim", 2, " (", round(eig[2], 1), "%)")
    }
    else if (ncol(data) == 2) {
      ind <- as.data.frame(data)
      ind <- cbind.data.frame(name = rownames(ind), ind)
      xlab <- colnames(data)[1]
      ylab <- colnames(data)[2]
    }
    else {
      stop("The dimension of the data < 2! No plot.")
    }
    colnames(ind)[2:3] <- c("x", "y")
    label_coord <- ind
  }
  else if (inherits(data, "HCPC")) {
    ind <- res.hcpc$call$X[, c("Dim.1", "Dim.2", "clust")]
    ind <- cbind.data.frame(name = rownames(ind), ind)
    colnames(ind)[2:3] <- c("x", "y")
    label_coord <- ind
    eig <- get_eigenvalue(res.hcpc$call$t$res)[, 2]
    xlab = paste0("Dim", 1, " (", round(eig[1], 1), "%)")
    ylab = paste0("Dim", 2, " (", round(eig[2], 1), "%)")
  }
  else stop("A data of class ", class(data), " is not supported.")
  label = FALSE
  if ("text" %in% geom) 
    label = TRUE
  if (!("point" %in% geom)) 
    pointsize = 0
  if (is.null(jitter$what)) 
    jitter$what <- "label"
#  if (jitter$what %in% c("both", "b")) 
#    label_coord <- ind <- .jitter(ind, jitter)
#  else if (jitter$what %in% c("point", "p")) 
#    ind <- .jitter(ind, jitter)
#  else if (jitter$what %in% c("label", "l")) 
#    label_coord <- .jitter(label_coord, jitter)
  plot.data <- cbind.data.frame(ind, cluster = cluster)
  label_coord <- cbind.data.frame(label_coord, cluster = cluster)
  is_outliers = FALSE
  if (inherits(object, "dbscan")) {
    outliers <- which(cluster == 0)
    if (length(outliers) > 0) {
      is_outliers = TRUE
      outliers_data <- plot.data[outliers, , drop = FALSE]
      outliers_labs <- label_coord[outliers, , drop = FALSE]
      ind <- ind[-outliers, , drop = FALSE]
      cluster <- cluster[-outliers]
      plot.data <- plot.data[-outliers, , drop = FALSE]
      label_coord <- label_coord[-outliers, , drop = FALSE]
    }
  }
  ngroups <- length(levels(plot.data$cluster))
  p <- ggplot()
  if ("point" %in% geom) {
    if (ngroups <= 6) {
      p <- p + geom_point(data = plot.data, aes_string("x", 
                                                       "y", color = "cluster", shape = "cluster"), size = pointsize)
    }
    else p <- p + geom_point(data = plot.data, aes_string("x", 
                                                          "y", color = "cluster", shape = "cluster"), size = pointsize) + 
        ggplot2::scale_shape_manual(values = 1:ngroups, labels = levels(plot.data$cluster))
  }
  if ("text" %in% geom) {
    if (repel) 
      p <- p + ggrepel::geom_text_repel(data = label_coord, 
                                        aes_string("x", "y", label = "name", color = "cluster"), 
                                        size = labelsize)
    else p <- p + geom_text(data = label_coord, aes_string("x", 
                                                           "y", label = "name", color = "cluster"), size = labelsize, 
                            vjust = -0.7)
  }
  clustcent <- stats::aggregate(ind[, 2:3], by = list(cluster = cluster), 
                                mean)
  colnames(clustcent) <- c("cluster", "x", "y")
  if (show.clust.cent) {
    if ("point" %in% geom) 
      p <- p + geom_point(data = clustcent, aes_string("x", 
                                                       "y", color = "cluster", shape = "cluster"), size = pointsize * 
                            2)
    if ("text" %in% geom) {
      if (repel) 
        p <- p + ggrepel::geom_text_repel(data = clustcent, 
                                          aes_string("x", "y", color = "cluster"), label = clustcent$cluster, 
                                          size = labelsize * 1.2)
      else p <- p + geom_text(data = clustcent, aes_string("x", 
                                                           "y", color = "cluster"), label = clustcent$cluster, 
                              size = labelsize * 1.2, vjust = -1)
    }
  }
  if (frame) {
    if (frame.type == "convex") {
      frame.data <- .cluster_chull(ind[, c("x", "y")], 
                                   cluster)
      mapping = aes_string(x = "x", y = "y", colour = "cluster", 
                           fill = "cluster")
      p <- p + ggplot2::geom_polygon(data = frame.data, 
                                     mapping = mapping, alpha = frame.alpha)
    }
    else if (frame.type %in% c("t", "norm", "euclid")) {
      mapping = aes_string(x = "x", y = "y", colour = "cluster", 
                           fill = "cluster")
      p <- p + ggplot2::stat_ellipse(mapping = mapping, 
                                     data = plot.data, level = frame.level, type = frame.type, 
                                     geom = "polygon", alpha = frame.alpha)
    }
  }
  if (is_outliers) 
    p <- p + add_outliers(p, outliers_data, outliers_labs, outlier.color, 
                       outlier.shape, pointsize, labelsize, geom, repel = repel)
  title2 <- title
  p <- p + labs(title = title2, x = xlab, y = ylab)
  p
}