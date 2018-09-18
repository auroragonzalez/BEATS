data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
names(my_data)= c("output", "feature 1", "feature 2", "feature 3", "feature 4", "feature 5")
res <- cor(my_data)
library("corrplot")
dev.off()
svg(filename="cor.svg", 
    width=5, 
    height=4)
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)
dev.off()

dev.off()
setEPS()
postscript("cor.eps")
corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)
dev.off()