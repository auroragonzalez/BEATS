df <- read.table("fig4MAPE.csv", sep=",", head=F)

## op1 
tol14rainbow=c("#882E72", "#B178A6", "#1965B0", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#DC050C")

matplot(t(df[c(1,49)]), type="l", lwd = 3, ylab = "MAPE (%)"
        , xlab =" Maximum # hard sensor as neighbors", main="BME with raw temp IBRL data",col = tol14rainbow, lty=1)
legend(x=45, y = 3.75,  legend = seq(120,1200,120), lty=c(1,1), lwd=c(2,2), col = tol14rainbow)
text("# obs", x=45, y = 3.75)

dev.off()
svg(filename="fig4MAPE.svg", width = 12, height = 8, pointsize = 12)
matplot(t(df), type="l", lwd = 3, ylab = "MAPE (%)"
        , xlab =" Maximum # hard sensor as neighbors", main="BME with raw temp IBRL data",col = tol14rainbow, lty=1)
legend(x=45, y = 3.75,  legend = seq(120,1200,120), lty=c(1,1), lwd=c(2,2), col = tol14rainbow)

dev.off()

## op2