# https://rpubs.com/JanpuHou/326048
#https://analyticsrusers.blog/2017/08/15/use-r-to-easily-estimate-migration-matrices-with-rtransprob-part-1/
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

sample = df2[,1:100]

ID = rep(1:ncol(sample),each=nrow(sample))
Date = rep(seq(as.Date("2000/1/1"), by = "day", length.out = nrow(sample)), ncol(sample))
Rating = as.vector(as.matrix(sample))
data2 = data.frame(ID,Date,Rating)
data2$Rating[is.na(data2$Rating)]=0
data2$Rating = as.factor(data2$Rating)
data2$Num_Ratings = as.numeric(data2$Rating)


data3$Rating[data3$Rating==22] = 21
data3$Rating[data3$Rating==23] = 21
data3$Rating[data3$Rating==24] = 25
data3$Rating[data3$Rating==26] = 27
data3$Rating[data3$Rating==28] = 29
data3$Rating[data3$Rating==30] = 29
data3$Rating[data3$Rating==61] = NA
data3$Rating[data3$Rating==-35] = NA

names= c("OFF", "21-23", "24-25", "26-27","28-30")


data3 = data3[complete.cases(data3), ]
data3$Rating = factor(data3$Rating)
data3$Num_Ratings = as.numeric(data3$Rating)


snapshots <- 0
interval  <- 0
startDate <- 0
endDate   <- 0

Example1  <- TransitionProb(data3,startDate,endDate,'cohort', snapshots, interval)
ra = Example1$transMat/100

#stateNames = levels(data3$Rating)
stateNames = names
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("100mc.pdf", height = 6, width = 8)
#par(mar=c(0,0,0,0)+0.1)
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

names= c("OFF", "20-21", "22-23", "24-25","26-27", "28-30")
stateNames = names
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,3)

dev.off()
pdf("Allmc.pdf")
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



