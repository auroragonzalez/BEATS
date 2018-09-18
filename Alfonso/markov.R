# https://rpubs.com/JanpuHou/326048
library("markovchain")
data(rain)
mysequence<-rain$rain
createSequenceMatrix(mysequence)
head(rain)


myFit<-markovchainFit(data=mysequence,confidencelevel = .9,method = "mle")
myFit


alofiMc<-myFit$estimate
alofiMc

a11=alofiMc[1,1]
a12=alofiMc[1,2]
a13=alofiMc[1,3]
a21=alofiMc[2,1]
a22=alofiMc[2,2]
a23=alofiMc[2,3]
a31=alofiMc[3,1]
a32=alofiMc[3,2]
a33=alofiMc[3,3]

## Hard code the transition matrix
stateNames <- c("No Rain","Light Rain","Heavy Rain")
ra <- matrix(c(a11,a12,a13,a21,a22,a23,a31,a32,a33),nrow=3, byrow=TRUE)
#ra <- matrix(c(0.660,0.230,0.110,0.463,0.306,0.231,0.198,0.312,0.490),nrow=3, byrow=TRUE)

dtmcA <- new("markovchain",transitionMatrix=ra, states=c("No Rain","Light Rain","Heavy Rain"), name="MarkovChain A") 

dtmcA
plot(dtmcA)


row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)
library("diagram")
plotmat(ra,pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")


x1 <- matrix(c(1,0,0),nrow=1, byrow=TRUE)
x1 %*% ra

ra2 <- ra ^ 2
ra3 <- ra ^ 3
ra4 <- ra ^ 4
ra5 <- ra ^ 5
ra6 <- ra ^ 6
ra7 <- ra ^ 7
cat("Day 1 Forecast")
round(x1%*%ra,3)
cat("Day 2 Forecast")
round(x1%*%ra2,3)
cat("Day 3 Forecast")
round(x1%*%ra3,3)




#https://analyticsrusers.blog/2017/08/15/use-r-to-easily-estimate-migration-matrices-with-rtransprob-part-1/



library("RTransProb")
head(data)

snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
Example1  <- TransitionProb(data,startDate,endDate,'cohort', snapshots, interval)

ra = Example1$transMat/100

dtmcA <- new("markovchain",transitionMatrix=ra
             , states=c("A","B","C","D","E", "F", "G"), name="MarkovChain A") 

plot(dtmcA)


stateNames = c("A","B","C","D","E", "F", "G")
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)
library("diagram")
plotmat(ra,#pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")







############

#library("R.matlab")
#df = readMat("TsetCleanV7.mat")
#write.table(df,"TsetCleanV7.csv", sep=";", row.names=F, col.names=F)
library("data.table")
library("markovchain")
library("RTransProb")
df = fread("TsetCleanV7.csv", sep=";", head=F)
df2 = df
#df2 = df$TsetClean

sample = df2[,1:10]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)

snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0
Example1  <- TransitionProb(data2,startDate,endDate,'cohort', snapshots, interval)


ra = Example1$transMat/100

dtmcA <- new("markovchain",transitionMatrix=ra
             , states=levels(data2$Rating), name="MarkovChain A") 

plot(dtmcA)

stateNames = levels(data2$Rating)
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)
library("diagram")
plotmat(ra,arr.type="triangle",#pos = c(1,2),
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")












data3 = data2
data3$Rating[data3$Rating==22] = 21
data3$Rating[data3$Rating==23] = 21
data3$Rating[data3$Rating==24] = 25
data3$Rating[data3$Rating==26] = 27
data3$Rating[data3$Rating==28] = 29
#data3$Rating[data3$Rating==29] = 27

data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)

Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

dtmcA <- new("markovchain",transitionMatrix=ra
             , states=levels(data2$Rating), name="MarkovChain A") 

plot(dtmcA)

stateNames = levels(data3$Rating)
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

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
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")





sample = df2[,1:100]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)

data3 = data2
data3$Rating[data3$Rating==22] = 21
data3$Rating[data3$Rating==23] = 21
data3$Rating[data3$Rating==24] = 25
data3$Rating[data3$Rating==26] = 27
data3$Rating[data3$Rating==28] = 29
data3$Rating[data3$Rating==30] = 29
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA

data3 = data3[complete.cases(data3), ]

data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)

Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

dtmcA <- new("markovchain",transitionMatrix=ra
             , states=levels(data3$Rating), name="MarkovChain A") 

plot(dtmcA)

stateNames = levels(data3$Rating)
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

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
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")




stateNames = levels(data3$Rating)
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

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
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")











sample = df2

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)

data3 = data2
data3$Rating[data3$Rating==20] = 21
data3$Rating[data3$Rating==22] = 23
data3$Rating[data3$Rating==24] = 25
data3$Rating[data3$Rating==26] = 27
data3$Rating[data3$Rating==28] = 29
data3$Rating[data3$Rating==30] = 29
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA

data3 = data3[complete.cases(data3), ]

data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)

Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

#dtmcA <- new("markovchain",transitionMatrix=ra
             , states=levels(data3$Rating), name="MarkovChain A") 

#plot(dtmcA)

stateNames = levels(data3$Rating)
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("mcplot1.pdf")
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
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")
dev.off()



