library("markovchain")

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



