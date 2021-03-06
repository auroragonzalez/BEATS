x <- c(1,2,1,1,3,4,4,1,2,4,1,4,3,4,4,4,3,1,3,2,3,3,3,4,2,2,3)
xChar<-as.character(x)
library(markovchain)
mcX<-markovchainFit(xChar)$estimate
mcX@transitionMatrix


# #ID = rep(1, length(x))
# ID = 1:length(x)
# Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = length(x)))
# Rating=x
# 
# data3 = data.frame(ID, Date, Rating)
# 
# snapshots <- 0
# interval  <- 0
# startDate <- 0
# endDate   <- 0
# library("RTransProb")
# data3$Rating = factor(data3$Rating)
# Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)


mat <- array(dim=c(500, 10, 10))  # i changed your four examples to 5, to be clear

library("data.table")
library("markovchain")
df = fread("TsetCleanV7.csv", sep=";", head=F)
df2 = df

df2[df2 == 61] <- NA
df2[df2 == -35] <- NA

zeros =c()
cambio = c()
cambio2 = c()

mat <- array(dim=c(10, 500, 10))  # i changed your four examples to 5, to be clear

aux=1
for(i in 1:ncol(df2)){
  x= as.matrix(df2)[,i]
  x[is.na(x)]=0
  if(sum(x)==0){  # Nunca encendieron el aire acondicionado
    zeros = c(i,zeros)
  }
  if(length(which(x==30))>0){  # Quito los 30 pues son muy pocos los que lo tienen
    x[x==30] = 29
    cambio = c(i,cambio)
  }
  if(length(which(x==20))>0){  # Quito los 30 pues son muy pocos los que lo tienen
    x[x==20] = 21
    cambio2 = c(i,cambio2)
  }
  if(sum(x)!=0){ # Si lo han encendido calcula su transition probability matrix
    xChar<-as.character(x)
    mcX<-markovchainFit(xChar)$estimate
    tpm = mcX@transitionMatrix
    if(ncol(tpm)<10){
      
      missing = c(0,21:29)[which(!c(0,21:29) %in% as.numeric(colnames(tpm)))]
      tpm2 = data.frame(tpm)
      for(mm in 1:length(missing)){
        tpm2 = cbind(tpm2,0)
        tpm2 = rbind(tpm2,0)
      }
      
      tpm3 = tpm2[,order(as.numeric(c(colnames(tpm), missing)))]
      names(tpm3) = as.character(c(0,21:29))
      tpm4 = tpm3[order(as.numeric(c(rownames(tpm), missing))),]
      rownames(tpm4) = as.character(c(0,21:29))
      tpm = as.matrix(tpm4)
      }
      
    for(s in 1:10){
      print(s)
      print(aux)
      mat[,aux, s] = tpm[s,]
    }
    aux = aux+1
    
  }
}


mlist = list()
for(k in 1:10){
  final = mat[,,k]
  final = final[complete.cases(final), ]
  final = data.frame(final)
#  clus=  kmeans(final,2)
#  final$cl = clus$cluster 
 mlist[[k]] = final
}

mlist[[1]]$cl
mlist[[2]]$cl


require("pdc")

num.ts <- 20 # number of time series
num.dim <- 12 # number of dimensions
len.ts <- 600*10 # number of time series

# generate Gaussian white noise
data <- array(dim = c(len.ts, num.ts, num.dim),data = rnorm(num.ts*num.dim*len.ts))

# obtain clustering with embedding dimension of 5
pdc <- pdclust(X = data, m=5,t=1)

# plot hierarchical clustering
plot(pdc)


pdc <- pdclust(X = mat[,1:3,], m=5,t=1)

# plot hierarchical clustering
plot(pdc)

plot(cutree(pdc, k = 5))



A = mat[,1,] - mat[,2,]
norm(A, type = "F")

A = mat[,1,] - mat[,3,]
norm(A, type = "F")

A = mat[,1,] - mat[,4,]
norm(A, type = "F")

A = mat[,1,] - mat[,5,]



test <- matrix(ncol=5,nrow=5)
test[lower.tri(test)] <- c(.76,2.97,4.88,3.86,.8,4.17,1.96,.21,1.51,.51)
diag(test) <- 0


temp_dist = matrix(NA,460,460)
for (i in 1:460)
{
  for (j in 1:460)
  {
    temp_dist[i,j]<-norm((mat[,i,] - mat[,j,]), type = "F")
  }
}

test <- as.dist(temp_dist, diag = TRUE)
hclust(test)
plot(hclust(test))

x = cutree(hclust(test),20)
grupo1 = which(x!=20)
grupo2= which(x==20)




df2 = data.frame(df2)

df2 = df2[,-zeros]

sample = df2[,grupo1]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)
data3 = data2
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA
data3$Rating[data3$Rating==20] = 21
data3$Rating[data3$Rating==30] = 29

#names= c("OFF", "21-23", "24-25", "26-27","28-30")


data3 = data3[complete.cases(data3), ]
data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)


snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
library("RTransProb")
Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

stateNames = levels(data3$Rating)
stateNames[1] ="OFF"
row.names(ra) <- stateNames
colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("g1.pdf", height = 6, width = 8)
#par(mar=c(0,0,0,0)+0.1)
library("diagram")
plotmat(ra,arr.type="triangle",#,pos = c(1,1,1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.05, 
        box.type = "circle", 
        box.prop = 0.4,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.015,
        self.shiftx = .085,
        main = "Markov Chain Transition Matrix")
dev.off()







sample = df2[,grupo2]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)
data3 = data2
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA
data3$Rating[data3$Rating==20] = 21
data3$Rating[data3$Rating==30] = 29

#names= c("OFF", "21-23", "24-25", "26-27","28-30")


data3 = data3[complete.cases(data3), ]
data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)


snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
library("RTransProb")
Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

stateNames = levels(data3$Rating)
stateNames[1] ="OFF"
row.names(ra) <- stateNames
colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("g2.pdf", height = 6, width = 8)
#par(mar=c(0,0,0,0)+0.1) 
library("diagram")
plotmat(ra,arr.type="triangle",#,pos = c(1,1,1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.05, 
        box.type = "circle", 
        box.prop = 0.4,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.015,
        self.shiftx = .085,
        main = "Markov Chain Transition Matrix")
dev.off()



#### despues de times.R debes tener un objeto data y un objeto cl

subset = which(cl==1)
df2 = as.data.frame(df2)
sample = df2[,subset]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)
data3 = data2
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA
data3$Rating[data3$Rating==20] = 21
data3$Rating[data3$Rating==28] = 27
data3$Rating[data3$Rating==29] = 27
data3$Rating[data3$Rating==30] = 27

#names= c("OFF", "21-23", "24-25", "26-27","28-30")


data3 = data3[complete.cases(data3), ]
data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)


snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
library("RTransProb")
Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

ra[ra < 0.05] <- 0

stateNames = levels(data3$Rating)
stateNames[1] ="OFF"
stateNames[2] = "<=21"
stateNames[length(stateNames)] = ">=27"
row.names(ra) <- stateNames
colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("g3.pdf", height = 6, width = 8)
#par(mar=c(0,0,0,0)+0.1) 
library("diagram")
plotmat(ra,arr.type="triangle",#,pos = c(1,1,1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.05, 
        box.type = "circle", 
        box.prop = 0.4,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.015,
        self.shiftx = .085,
        main = "Markov Chain Transition Matrix")
dev.off()


subset = which(cl==2)
df2 = as.data.frame(df2)
sample = df2[,subset]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)
data3 = data2
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA
data3$Rating[data3$Rating==20] = 21
data3$Rating[data3$Rating==28] = 27
data3$Rating[data3$Rating==29] = 27
data3$Rating[data3$Rating==30] = 27

#names= c("OFF", "21-23", "24-25", "26-27","28-30")


data3 = data3[complete.cases(data3), ]
data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)


snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
library("RTransProb")
Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

ra[ra < 0.05] <- 0

stateNames = levels(data3$Rating)
stateNames[1] ="OFF"
stateNames[2] = "<=21"
stateNames[length(stateNames)] = ">=27"
row.names(ra) <- stateNames
colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("g4.pdf", height = 6, width = 8)
#par(mar=c(0,0,0,0)+0.1) 
library("diagram")
plotmat(ra,arr.type="triangle",#,pos = c(1,1,1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.05, 
        box.type = "circle", 
        box.prop = 0.4,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.015,
        self.shiftx = .085,
        main = "Markov Chain Transition Matrix")
dev.off()

