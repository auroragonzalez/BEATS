library("markovchain")
library("data.table")
df <- fread("TsetCleanV7.csv", sep=";")

fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}

mEt <- array(NA, c(10, 10, ncol(df)))  
mUt <- array(NA, c(10, 10, ncol(df)))  
mLt  <- array(NA, c(10, 10, ncol(df)))
'%!in%' <- function(x,y)!('%in%'(x,y))
coln=as.character(c(21:30))

f.fillMatrixAurora = function(matrix,coln, with=0){
  colnmatrix=colnames(matrix)
  matrix=matrix[match(coln,colnmatrix),match(coln,colnmatrix)]
  colnames(matrix)=coln
  rownames(matrix) = coln
  matrix[is.na(matrix)] <- with
  return(matrix)
}

for(i in 1:ncol(df)){
  print(i)
  x = as.numeric(unlist(df[,..i]))
  if(sum(!is.na(x))!=0){
    y = fillNAgaps(x, firstBack=T)
    weatherFittedMLE <- markovchainFit(data = y, method = "mle", name = "weatherFittedMLE", confidencelevel = 0.95)
    mE = weatherFittedMLE$estimate
    mE = mE@transitionMatrix
    mU = weatherFittedMLE$upperEndpointMatrix
    mL = weatherFittedMLE$lowerEndpointMatrix
    if(ncol(mE)!=length(coln)){
      print("Filling")
      mE = f.fillMatrixAurora(mE, coln)
      mU = f.fillMatrixAurora(mU, coln, with = -999)
      mL = f.fillMatrixAurora(mL, coln, with = -999)
    }
    
    mEt[,,i] = mE
    mUt[,,i] = mU
    mLt[,,i] = mL
  }else{
    print("TOOODO NA") # se queda con NA
  }
  

}



library("R.matlab")
writeMat("mEt.mat",mEt= mEt)
writeMat("mUt.mat",mUt= mUt)
writeMat("mLt.mat",mLt= mLt)




x= mEt[1,,1]
x2 = x
library("fitdistrplus")

plotdist(x, histo = TRUE, demp = TRUE)
fw <- fitdist(x2[-c(7:10)], "weibull")
fg <- fitdist(x2, "gamma")
fln <- fitdist(x2, "lnorm")
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)

summary(fw)
summary(fg)
summary(fln)
