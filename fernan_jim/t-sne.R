## calling the installed package
train<- read.table("Git_repos/postgrado/doctorado/ENTROPY3/13.reduced.csv", sep=";", head=T) ## Choose the train.csv file downloaded from the link above  
train = train[1:3000,]
train$energy<- cut(train$energy, breaks = 4)
library(plyr)
train$energy = mapvalues(train$energy, from = levels(factor(train$energy)), to = 1:length(levels(factor(train$energy))))

library(Rtsne)
## Curating the database for analysis with both t-SNE and PCA
Labels<-train$energy
train$label<-as.factor(train$energy)
## for plotting
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)

## Executing the algorithm on curated data
tsne <- Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
#exeTimeTsne<- system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$label, col=colors[train$label])



colors = rainbow(length(unique(train$dow)))
names(colors) = unique(train$dow)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$dow, col=colors[train$dow])

