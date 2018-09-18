#https://cran.r-project.org/web/packages/gstat/vignettes/st.pdf
library(spacetime)
#rm(list = ls())
data(air)
ls()


if (!exists("rural")){
  rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))  
}

rr = rural[,"2005::2010"]
unsel = which(apply(as(rr, "xts"), 2, function(x) all(is.na(x))))
r5to10 = rr[-unsel,]
summary(r5to10)

rn = row.names(r5to10@sp)[4:7]
rn


par(mfrow=c(2,2))
# select 4, 5, 6, 7
for(i in rn)
 acf(na.omit(r5to10[i,]), main = i)
par(mfrow=c(1,1))
acf(na.omit(as(r5to10[rn,], "xts")))



#2.2 Spatial correlation, variograms
# pooled variogram


rs = sample(dim(r5to10)[2], 100)
lst = lapply(rs, function(i) { x = r5to10[,i]; x$ti = i; rownames(x@coords) = NULL; x} )
pts = do.call(rbind, lst)

library(gstat)
v = variogram(PM10~ti, pts[!is.na(pts$PM10),], dX=0)
vmod = fit.variogram(v, vgm(100, "Exp", 200))
plot(v, vmod)
vmod
dim(r5to10)

vv = variogram(PM10~1, r5to10, width=20, cutoff = 200, tlags=0:5)

metricVgm <- vgmST("metric",
                   joint=vgm(50,"Exp",100,0),
                   stAni=50)

metricVgm <- fit.StVariogram(vv, metricVgm)




# https://es.mathworks.com/matlabcentral/fileexchange/20355-experimental-semi-variogram