DoubleMAD <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x         <- x[!is.na(x)]
  m         <- median(x)
  abs.dev   <- abs(x - m)
  left.mad  <- median(abs.dev[x<=m])
  right.mad <- median(abs.dev[x>=m])
  if (left.mad == 0 || right.mad == 0){
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c(  "na", "warn and na")){
      if (left.mad  == 0) left.mad  <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}

DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  two.sided.mad <- DoubleMAD(x, zero.mad.action)
  m <- median(x, na.rm=TRUE)
  x.mad <- rep(two.sided.mad[1], length(x))
  x.mad[x > m] <- two.sided.mad[2]
  mad.distance <- abs(x - m) / x.mad
  mad.distance[x==m] <- 0
  return(mad.distance)
}


x <- c(1, 4, 4, 4, 5, 5, 5, 5, 7, 7, 8, 10, 16, 30)
x[DoubleMADsFromMedian(x) > 3]

# http://web.ipac.caltech.edu/staff/fmasci/home/astro_refs/BetterThanMAD.pdf
#  Detecting outliers: Do not use standard deviation around the mean, use absolutedeviation around the median
#


#require(zoo)

# Example
#TS <- x
#rollapply(x[1:10], width = 5, by = 2, FUN = DoubleMADsFromMedian, align = "left")

#DoubleMADsFromMedian(x[1:5])
#DoubleMADsFromMedian(x[3:8])
#DoubleMADsFromMedian(x[5:10])

## Each row will correspond to the previous 30 observations of one day
#roll = rollapply(x, width = 30, by = 1, FUN = DoubleMADsFromMedian, align = "left")
#which(roll > 40)

#df$energy[DoubleMADsFromMedian(df$energy) > 10]