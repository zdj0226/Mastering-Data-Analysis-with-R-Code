d<-dist(mtcars)
h<-hclust(d)
h
plot(h)
rect.hclust(h,k=3,border = "red")
(cn<-cutree(h,k=3))
table(cn)
round(aggregate(mtcars,FUN=mean,by=list(cn)),1)
round(aggregate(mtcars,FUN=sd,by=list(cn)),1)
round(sapply(mtcars,sd),1)
round(apply(aggregate(mtcars,FUN=mean,by=list(cn)),2,sd),1)
library(NbClust)
NbClust(mtcars,method='complete',index='dindex')
NbClust(mtcars,method='complete',index='hartigan')$Best.nc
NbClust(mtcars,method='complete',index='kl')$Best.nc
NbClust(iris[,-5],method='complete',index='all')$Best.nc[1,]
(k<-kmeans(mtcars,3))
all(cn==k$cluster)
cbind(cn,k$cluster)
library(cluster)
clusplot(mtcars,k$cluster,color = TRUE,shade = TRUE,labels = 2)
factors<-c('cyl','vs',"am",'carb','gear')
mtcars[,factors]<-lapply(mtcars[,factors],factor)
library(poLCA)
P<-poLCA(cbind(cyl,vs,am,carb,gear)~1,data=mtcars,graphs=TRUE,nclass=3)
P$probs
P$P
rm(mtcars)
mtcars$gear<-factor(mtcars$gear)
library(MASS)
d<-lda(gear~.,data = mtcars,cv=TRUE)
(tab<-table(mtcars$gear,d$class))
tab/row$Sums(tab)