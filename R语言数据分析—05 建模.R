library(gamlss.data)
data(usair)
model.0<-lm(y~x3,data = usair)
summary(model.0)
plot(y~x3,data=usair,cex.lab=1.5)
abline(model.0,col="red",lwd=2.5)
legend('bottomright',legend = 'y~x3',lty = 1,col = 'red',
       lwd = 2.5,title = 'Regression line')
usair$prediction<-predict(model.0)
usair$residual<-resid(model.0)
plot(y~x3,data=usair,cex.lab=1.5)
abline(model.0,col="red",lwd=2.5)
segments(usair$x3,usair$y,usair$x3,usair$prediction,col = 'blue',lty = 2)
legend('bottomright',legend = c('y~x3','residuals'),
       lty = c(1,2),col = c('red','blue'),lwd = 2.5,
       title = 'Regression line')
##update函数，等效于 model.1<-lm(y~x3+x2,data=usair)
model.1<-update(model.0,.~.+x2)
summary(model.1)
##假设x2=150，x3=400，因子转换数值，求值
as.numeric(predict(model.1,data.frame(x2=150,x3=400)))
library(scatterplot3d)
plot3d<-scatterplot3d(usair$x3,usair$x2,usair$y,pch=19,
                      type='h',highlight.3d = TRUE,main = '3-D Scatterplot')
plot3d$plane3d(model.1,lty='solid',col='red')
##排列一行两图
par(mfrow=c(1,2))
plot(y~x3,data=usair,main = '2-D projection for x3')
abline(model.1,col="red",lwd=2.5)
plot(y~x2,data=usair,main = '2-D projection for x2')
abline(lm(y~x2+x3,data = usair),col="red",lwd=2.5)
library(Hmisc)
library(ggplot2)
library(gridExtra)
set.seed(7)
x<-sort(rnorm(1000,10,100))[26:975]
y<-x*500+rnorm(950,5000,20000)
##cut2函数，在x的数值范围内，按照数字个数，平均切割区间
df<-data.frame(x=x,y=y,cuts=factor(cut2(x,g=5)),resid=resid(lm(y~x)))
scatterP1<-ggplot(df,aes(x=x,y=y))+geom_point(aes(colour=cuts,fill=cuts),
         shape=1,show_guide=FALSE)+geom_smooth(method=lm,level=0.99)
plot_left<-ggplot(df,aes(x=y,fill=cuts))+geom_density(alpha=.5)+
          coord_flip()+scale_y_reverse()
plot_right<-ggplot(data=df,aes(x=,resid,fill=cuts))+geom_density(alpha=.5)+
  coord_flip()
grid.arrange(plot_left,scatterP1,plot_right,ncol=3,nrow=1,widths=c(1,3,1))
library(gvlma)
##gvlma函数验证模型假定是否满足
gvlma(model.1)
model.2<-update(model.1,data=usair[-31,])
gvlma(model.2)
model.0<-update(model.0,data=usair[-31,])
summary(model.0)[c('r.squared','adj.r.squared')]
summary(model.2)[c('r.squared','adj.r.squared')]
##AIC准则以信息论为基础，对模型参数个数引入惩罚机制，
##解决复杂模型看似拟合效果更优问题，AIC值更小者更优。
summary(model.3<-update(model.2,.~.-x2+x1))$coefficients
summary(model.4<-update(model.2,.~.-x3+x1))$coefficients
AIC(model.3,model.4)
##引入离散型变量x5（年度降水天数，分低、中、高，分界点是30和45）
plot(y~x5,data=usair,cex.lab=1.5)
abline(lm(y~x5,data=usair),col="red",lwd=2.5,lty=1)
abline(lm(y~x5,data=usair[usair$x5<=45,]),col="red",lwd=2.5,lty=3)
abline(lm(y~x5,data=usair[usair$x5>=30,]),col="red",lwd=2.5,lty=2)
abline(v=c(30,45),col="blue",lwd=2.5)
legend('topleft',lty = c(1,3,2,1),lwd =rep(2.5,4),legend = c('y~x5',
        'y~x5|x5<=45','y~x5|x5>=30','Critical zone'),
       col=c('red','red','red','blue'))
##用rpart函数，回归树递归调用，依据降雨天数选择最佳划分
library(partykit)
library(rpart)
plot(as.party(rpart(y~x5,data=usair)))
usair$x5_3<-cut2(usair$x5,c(30,45))
plot(y~as.numeric(x5_3),data = usair,cex.lab=1.5,
     xlab='Categorized annual rainfall(x5)',xaxt='n')
axis(1,at=1:3,labels = levels(usair$x5_3))
lines(tapply(usair$y,usair$x5_3,mean),col='red',lwd=2.5,lty=1)
legend('topright',legend = 'Linear prediction',col = 'red')
##引入分界点为30,45的X5_3,使用广义线性模型GLM拟合（经典线性回归不支持）
##summary(model.5<-lm(y~x2+x3+x5_3,data=usair[-31,]))
summary(glmmodel.1<-glm(y~x2+x3+x5_3,data=usair[-31,]))