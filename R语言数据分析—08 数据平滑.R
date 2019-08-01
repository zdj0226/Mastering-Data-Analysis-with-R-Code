##完全随机缺失，每个样本值都可能缺失
##随机缺失，缺失模式至少可确认
##完全非随机缺失，因某原因缺失，缺失值被认为是无应答。
##缺失值处理：去掉或估计值替代
library(hflights)
table(complete.cases(hflights))
prop.table(table(complete.cases(hflights)))*100
sort(sapply(hflights,function(x) sum(is.na(x))))
mean(cor(apply(hflights,2,function(x) as.numeric(is.na(x)))),na.rm=TRUE)
##去掉缺失值，na.omit或na.exclude函数,差别不大
na.omit(c(1:5,NA))
na.exclude(c(1:5,NA))
##差别在于返回的NA,前者出现在残差和预测值，后者在向量中不考虑
x<-rnorm(10);y<-rnorm(10)
x[1]<-NA;y[2]<-NA
exclude<-lm(y~x,na.action = "na.exclude")
omit<-lm(y~x,na.action = "na.omit")
residuals(exclude)
residuals(omit)
m<-matrix(1:9,3)
m[which(m%%4==0,arr.ind = TRUE)]<-NA
m
##na.action的属性显示行号
na.omit(m)
##na.rm=TRUE性能好点，但后期需多次指定，所以一开始就去掉缺失值更好
mean(hflights$ActualElapsedTime,na.rm = TRUE)
mean(na.omit(hflights$ActualElapsedTime))
##填补缺失值方法：1、已知值 2、平均值 3、建模填补
ActualElapsedTime<-hflights$ActualElapsedTime
mean(ActualElapsedTime,na.rm=TRUE)
ActualElapsedTime[which(is.na(ActualElapsedTime))]<-
         mean(ActualElapsedTime,na.rm = TRUE)
mean(ActualElapsedTime)
##平均值填充法简化代码
library(Hmisc)
mean(impute(hflights$ActualElapsedTime,mean))
##平均值填补法的问题是数据的方差变小
sd(hflights$ActualElapsedTime,na.rm = TRUE)
sd(ActualElapsedTime)
##建模填补缺失值
summary(iris)
library(missForest)
set.seed(81)
miris<-prodNA(iris,noNA=0.2)
summary(miris)
iiris<-missForest(miris,xtrue = iris,verbose = TRUE)
str(iiris)
##拟合随机森林模型填补缺失值
miris<-miris[,1:4]
iris_mean<-impute(miris,fun=mean)
iris_forest<-missForest(miris)
diag(cor(iris[,-5],iris_mean))
diag(cor(iris[,-5],iris_forest$ximp))
##异常值
detach('package:missForest')
detach('package:randomForest')
##发现异常值
library(outliers)
outlier(hflights$DepDelay)
summary(hflights$DepDelay)
##气泡图展示数据
library(lattice)
bwplot(hflights$DepDelay)
##数据集中与上四分位数距离大于1.5IQR的值的数量
IQR(hflights$DepDelay,na.rm = TRUE)
##异常值检验(样本符合正态分布)
set.seed(83)
dixon.test(c(runif(10),4))
##异常值检验(样本分布不要求)
summary(lm(Sepal.Length~Petal.Length,data=miris))
lm(Sepal.Length~Petal.Length,data=iris)$coefficients
library(MASS)
summary(rlm(Sepal.Length~Petal.Length,data=miris))
##把Sepal.Length~Petal.Length之间的公式关系赋值到f
f<-formula(Sepal.Length~Petal.Length)
##比较原数据的线性回归、含缺失数据的线性回归和模糊回归的差别
cbind(orig=lm(f,data=iris)$coefficients,lm=lm(f,data=miris)$coefficients,
rlm=rlm(f,data=miris)$coefficients)
miris$Sepal.Length[1]<-14
cbind(orig=lm(f,data=iris)$coefficients,lm=lm(f,data=miris)$coefficients,
      rlm=rlm(f,data=miris)$coefficients)