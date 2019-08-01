##正态性检验
library(hflights)
##提取数据，以下两种方法等效
JFK<-hflights[which(hflights$Dest=='JFK'),c('TaxiIn','TaxiOut')]
JFK<-subset(hflights,Dest=='JFK',select=c(TaxiIn,TaxiOut)) 
##生成同一行的两个图
par(mfrow=c(1,2))
##展示经验变量分位数和正态分布的差别，并增加趋势线
qqnorm(JFK$TaxiIn,ylab = 'TaxiIn')
qqline(JFK$TaxiIn)
qqnorm(JFK$TaxiOut,ylab = 'TaxiOut')
qqline(JFK$TaxiOut)
##shapiro-Wilk正态检验,结果非正态
shapiro.test(JFK$TaxiIn)
##MVN包检验多元变量正态性
library(MVN)
JFK<-na.omit(JFK)
mvn(JFK)
##变量之间的依赖关系
##sapply函数判断hflights数据集中哪些是数值型，以TRUE或FALSE输出
##which函数把TRUE部分的列进行集合
hflights_numeric<-hflights[,which(sapply(hflights,is.numeric))]
##两两比较相关关系
cor(hflights_numeric,use='pairwise.complete.obs')
##由于缺失值多，结果基本是NA
str(cor(hflights_numeric,use='pairwise.complete.obs'))
##去除年份和缺失值
hflights_numeric<-hflights[,which(sapply(hflights,function(x) 
  is.numeric(x)&& var(x,na.rm=TRUE) !=0))]
##查看结果
table(is.na(cor(hflights_numeric,use='pairwise.complete.obs')))
##数据中还有缺失值，可视化两两比较相关结果
library(ellipse)
par(mfrow=1)
plotcorr(cor(hflights_numeric,use="pairwise.complete.obs"))
plotcorr(cor(data.frame(1:10, 1:10+runif(10), 1:10+runif(10)*5,runif(10),
          10:1,check.names = FALSE)))
cor(hflights$FlightNum,hflights$Month)
##KMO检验
library(psych)
KMO(cor(hflights_numeric,use = "pairwise.complete.obs"))
cor(hflights_numeric[,c('Cancelled','AirTime')])
cancelled<-which(hflights_numeric$Cancelled==1)
table(hflights_numeric$AirTime[cancelled],exclude = NULL)
table(hflights_numeric$Cancelled)
hflights_numeric<-subset(hflights_numeric,select = -cancelled)
which(is.na(cor(hflights_numeric,use="pairwise.complete.obs")),
      arr.ind = TRUE)
hflights_numeric<-subset(hflights_numeric,select=-Diverted)
KMO(cor(hflights_numeric[,-c(14)],use = "pairwise.complete.obs"))
KMO(mtcars)
##Bartlett检验
cortest.bartlett(cor(mtcars))
##主成分分析
##prcomp函数实现Q-PCA算法，princomp函数实现R-PCA算法
prcomp(mtcars,scale=TRUE)
##第一、二成分的标准差大于1，包含大部分原始变量数据
##第一、二成分合计能解释84%的变量
summary(prcomp(mtcars,scale=TRUE))
sum(prcomp(scale(mtcars))$sdev^2)
prcomp(scale(mtcars))$sdev^2
(6.61+2.65)/11
library(psych)
##碎石图显示，特征值等于1处水平线高亮显示分界点，称为Kaiser准则（2）
VSS.scree(cor(mtcars))
##Elbow规则，拐弯处变得平缓的点，就是分界点（3）
scree(cor(mtcars))
##parallel分析建议主成分为2个变量
fa.parallel(mtcars)
##两个主成分描述原样本
pc<-prcomp(mtcars,scale =TRUE )
head(pc$x[,1:2])
##以上值是原始数据集与变量系数相乘得到，称为主成分载荷或成分矩阵
##过程是一个标准线性变换，可通过以下流程得到
head(scale(mtcars)%*%pc$rotation[,1:2])
##变量经过标准化，均值为0，标准差如上
summary(pc$x[,1:2])
apply(pc$x[,1:2],2,sd)
pc$sdev[1:2]
##
round(cor(pc$x))
pc$rotation[,1:2]
biplot(pc,cex=c(0.8,1.2))
abline(h=0,v=0,lty='dashed')
cor(mtcars,pc$x[,1:2])
##旋转方法
varimax(pc$rotation[,1:2])
pcv<-varimax(pc$rotation[,1:2])$loadings
plot(scale(mtcars)%*%pcv,type='n',xlab='Transmission',ylab = 'Power')
text(scale(mtcars)%*%pcv,labels=rownames(mtcars))
library(GPArotation)
promax(pc$rotation[,1:2])
cor(promax(pc$rotation[,1:2])$loadings)
##因子分析
m<-subset(mtcars,select = c(mpg,cyl,hp,carb))
(f<-fa(m))
fa.diagram(f)
cor(f$scores,mtcars$disp)
##多维尺度分析
##21个欧洲城市之间相互距离，显示前5个
as.matrix(eurodist)[1:5,1:5]
##多维尺度分析，形成两个主成分
(mds<-cmdscale(eurodist))
##二维图显示
plot(mds)
#用mtcars数据集演示
mds<-cmdscale(dist(mtcars))
plot(mds,type='n')
text(mds[,1],mds[,2],rownames(mds))
