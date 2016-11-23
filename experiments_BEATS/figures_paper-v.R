library(ggplot2)
library(cowplot)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

source("arrowHeads_results.R")
source("gasSensors_results.R")
source("randomGenerator_results.R")

dfAH= dfAH[dfAH$segmentationCLA!="Raw",]
dfGS= dfGS[dfGS$segmentationCLA!="Raw",]
dfRG= dfRG[dfRG$segmentationCLA!="Raw",]

# ONE

dfClas = dfAH[1:3]
names(dfClas) = c("Input", "Model", "Accuracy")

one = ggplot(dfClas, aes(x=Input, y=Accuracy)) + 
  geom_line(aes(colour=Model, group=Model, linetype=Model),size=1) + 
  geom_point(aes(colour=Model, shape = Model),               
             size=3) + ggtitle("Arrow Heads dataset")  +
  scale_linetype_manual("Model",values=c("RF"=1,"SVM"=2)) +
  guides(fill = guide_legend(keywidth = 3, keyheight = 3),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))+
  theme(legend.background = element_rect(fill="gray92", size=0.5, linetype=1, colour="black"))+
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen", "SAX (a=10)", "SAX (a=6)"),
                     labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=10)")), expression(paste("SAX (",alpha,"=6)"))))
                                                                                                                



dfClas = dfRG[1:3]
names(dfClas) = c("Input", "Model", "Accuracy")

two = ggplot(dfClas, aes(x=Input, y=Accuracy)) + 
  geom_line(aes(colour=Model, group=Model, linetype=Model),size=1) + 
  geom_point(aes(colour=Model, shape = Model),               
             size=3) + ggtitle("Random LHS Generator dataset")  +
  scale_linetype_manual("Model",values=c("RF"=1,"SVM"=2)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen", "SAX (a=4)", "SAX (a=5)"),
                   labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=4)")), expression(paste("SAX (",alpha,"=5)"))))                                              


dfClas = dfGS[1:3]
names(dfClas) = c("Input", "Model", "Accuracy")
three = ggplot(dfClas, aes(x=Input, y=Accuracy)) + 
  geom_line(aes(colour=Model, group=Model, linetype=Model),size=1) + 
  geom_point(aes(colour=Model, shape = Model),               
             size=3) + ggtitle("Gas Sensors dataset")  +
  scale_linetype_manual("Model",values=c("RF"=1,"SVM"=2)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))  +
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen", "SAX (a=7)", "SAX (a=8)"),
                   labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=7)")), expression(paste("SAX (",alpha,"=8)"))))  

pdf(file = "figures/classification8F.pdf", width = 15.3, height = 3.7)
grid_arrange_shared_legend(one, three, two, nrow = 1, ncol = 3)
dev.off()




# TWO

dfClus = dfAH[4:6]
names(dfClus) = c("Input", "Method", "Silhouette")
one = ggplot(dfClus, aes(x=Input, y=Silhouette)) + 
  geom_line(aes(colour=Method, group=Method, linetype=Method),size=1) + 
  geom_point(aes(colour=Method, shape = Method),               
             size=3) + ggtitle("Arrow Heads dataset")  +
  # scale_linetype_manual("Method",values=c("hclust"=1,"kmeans"=2)) +
  guides(fill = guide_legend(keywidth = 3, keyheight = 3),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))+
  theme(legend.background = element_rect(fill="gray92", size=0.5, linetype=1, colour="black"))+
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen", "SAX (a=5)", "SAX (a=6)"),
                   labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=5)")), expression(paste("SAX (",alpha,"=6)"))))  



dfClus = dfRG[4:6]
names(dfClus) = c("Input", "Method", "Silhouette")

two = ggplot(dfClus, aes(x=Input, y=Silhouette)) + 
  geom_line(aes(colour=Method, group=Method, linetype=Method),size=1) + 
  geom_point(aes(colour=Method, shape = Method),               
             size=3) + ggtitle("LHS generator dataset")  +
  #  scale_linetype_manual("Method",values=c("rf"=1,"svm"=2)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen", "SAX (a=3)"),
                   labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=3)"))))                                              


dfClus = dfGS[4:6]
names(dfClus) = c("Input", "Method", "Silhouette")


three = ggplot(dfClus, aes(x=Input, y=Silhouette)) + 
  geom_line(aes(colour=Method, group=Method, linetype=Method),size=1) + 
  geom_point(aes(colour=Method, shape = Method),               
             size=3) + ggtitle("Gas Sensors dataset")  +
  #  scale_linetype_manual("Method",values=c("rf"=1,"svm"=2)) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))+
  scale_x_discrete(breaks=c("BEATS", "Raw", "Eigen","SAX (a=8)"),
                   labels=c("BEATS", "Raw", "Eigen", expression(paste("SAX (",alpha,"=8)"))))                                              


pdf(file = "clustering9F.pdf", width = 15.3, height = 3.7)
grid_arrange_shared_legend(one, three, two, nrow = 1, ncol = 3)
dev.off()

cairo_ps("clustering.eps")
grid_arrange_shared_legend(one, three, two, nrow = 1, ncol = 3)
dev.off()


ggsave("clustering.eps", width = 20, height = 20, units = "cm")
grid_arrange_shared_legend(one, three, two, nrow = 1, ncol = 3)
dev.off()




# Arrow Head n = 52 (test set)
## ramdon forest
acc = c(39,41)
per = c(52,52)
prop.test(acc, per)
###  X-squared = 0.05, df = 1, p-value = 0.816 > 0.05 NO DIFFERENCES

## svm
acc = c(40,44)
per = c(52,52)
prop.test(acc, per)
###  X-squared = 0.55, df = 1, p-value = 0.4554 > 0.05 NO DIFFERENCES


# Gas sensors n = 110 (est set)
## ramdon forest
acc = c(104,84)
per = c(110,108)
prop.test(acc, per)
###  X-squared = 11.536, df = 1, p-value = 0.0006826 < 0.05 DIFFERENCIES
acc = c(95,84)
per = c(110,108)
prop.test(acc, per)
###  X-squared = 2.1815, df = 1, p-value = 0.1397 > 0.05 NO DIFFERENCIES

# Random generator n = 75 (est set)
## ramdon forest
acc = c(50,36)
per = c(75,75)
prop.test(acc, per)
###  X-squared = 4.6057, df = 1, p-value = 0.03187 < 0.05 DIFFERENCES
acc = c(55,36)
per = c(75,75)
prop.test(acc, per)
###  X-squared = 3.2272, df = 1, p-value = 0.002624 < 0.05 DIFFERENCIES
