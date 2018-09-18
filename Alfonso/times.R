library("data.table")
library("rlist")
df = fread("TsetCleanV7.csv", sep=";")

time = read.table("timeSorted-i.csv")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")
time2 = as.POSIXlt(paste(time$V1, time$V2),
                   format = "%d-%b-%Y %H:%M:%S")


time3 = cbind(df, time2)
df2 = cbind(df, time2)


f.aggregateVble = function(variable, date, IDdevice, func){
  date = as.Date(date, format="%Y-%m-%d")
  aggregate(variable, list(date, IDdevice), func, na.rm=T)
}



#####
# mean setting point
#####

aggregate(df2$V1, list(as.Date(df2$time2)), mean, na.rm=T)



msp = list()
msp[[1]] = aggregate(df2[[i]], list(as.Date(df2$time2)), mean, na.rm=T)
names(msp[[1]]) = c("Date", paste0("V",i))

for(i in 2:(ncol(df2)-1)){
  msp[[i]] = aggregate(df2[[i]], list(as.Date(df2$time2)), mean, na.rm=T)
  names(msp[[i]]) = c("Date", paste0("V",i))
  msp[[i]] = msp[[i]][,-1]
}


msp2 = list.cbind(msp)
names(msp2)[2:ncol(msp2)] = paste0("V", 1:(ncol(msp2)-1))


####
# tiempo de funcionamiento (veces ON)
####
library(dplyr)

to = list()
to[[1]] = aggregate(df2$V1[!is.na(df2$V1)], list(as.Date(df2$time2)[!is.na(df2$V1)]), length)
names(to[[1]])[1] = "timestamp"
ts <- msp2$Date
df <- data.frame(timestamp=ts)
to[[1]] <- full_join(df,to[[1]])

for(i in 2:(ncol(df2)-1)){
  if(length(df2[[i]][!is.na(df2[[i]])]) != 0){
    to[[i]] = aggregate(df2[[i]][!is.na(df2[[i]])], list(as.Date(df2$time2)[!is.na(df2[[i]])]), length)
    names(to[[i]])[1] = "timestamp"
    to[[i]] <- full_join(df,to[[i]])
    to[[i]] = to[[i]][,-1]
    
  }else{
    to[[i]]=rep(NA,length(ts))
  }
}

to2 = list.cbind(to)
names(to2)[2:ncol(to2)] = paste0("V", 1:(ncol(to2)-1))


####
# cambios en el termostato
####

myfun = function(x){
  sum(diff(x)!=0)
}



tc = list()
tc[[1]] = aggregate(df2$V1[!is.na(df2$V1)], list(as.Date(df2$time2)[!is.na(df2$V1)]), myfun)
names(tc[[1]])[1] = "timestamp"
ts <- msp2$Date
dfts <- data.frame(timestamp=ts)
tc[[1]] <- full_join(dfts,tc[[1]])

for(i in 2:(ncol(df2)-1)){
  if(length(df2[[i]][!is.na(df2[[i]])]) != 0){
    tc[[i]] = aggregate(df2[[i]][!is.na(df2[[i]])], list(as.Date(df2$time2)[!is.na(df2[[i]])]), myfun)
    names(tc[[i]])[1] = "timestamp"
    tc[[i]] <- full_join(df,tc[[i]])
    tc[[i]] = tc[[i]][,-1]
    
  }else{
    tc[[i]]=rep(NA,length(ts))
  }
}

tc2 = list.cbind(tc)
names(tc2)[2:ncol(tc2)] = paste0("V", 1:(ncol(tc2)-1))



write.table(msp2, "meanSettingPoint.csv", sep=";", row.names=F, col.names=F)
write.table(to2, "timesOn.csv", sep=";", row.names=F, col.names=F)
write.table(tc2, "settingPointInteraction.csv", sep=";", row.names=F, col.names=F)

require("pdc")

num.ts <- nrow(msp2) # number of time series
num.dim <- 3 # number of dimensions
len.ts <- ncol(msp2)-1 # number of time series

data <- array(dim = c(len.ts, num.ts, num.dim),data = -9999)

data[,,1] = t(msp2[,-1])
data[,,2] = t(to2[,-1])
data[,,3] = t(tc2[,-1])


data[is.na(data)]=0

# obtain clustering with embedding dimension of 5
pdc <- pdclust(X = data, m=5,t=1)

# plot hierarchical clustering
plot(pdc)
cl = cutree(pdc,2)


dfclu = data.frame(id= 1:num.ts, cl2=cutree(pdc,2), cl3=cutree(pdc,3)
                   , cl4 = cutree(pdc,4), cl5=cutree(pdc,5), cl6=cutree(pdc,6)
                   , cl7=cutree(pdc,7))

write.table(dfclu, "clusterIndex.csv", sep=";", col.names=F, row.names=F)

table(data[,which(cl==1),1])
table(data[,which(cl==2),1])


table(data[,which(cl==1),2])
table(data[,which(cl==2),2])


table(data[,which(cl==1),3])
table(data[,which(cl==2),3])



