##预测模型常用工作流程
##1、主要预测变量和所有混杂因子引入模型
##2、后退法逐步去掉作用不大的变量去掉混杂因子
##3、确定直接使用连续型变量还是先进行离散化
##4、检验模型，提高拟合，又避免过度拟合，使用AIC准则来平衡考虑
library(catdata)
data(deathpenalty)
##expand.dft函数对数据进行转换
library(vcdExtra)
deathpenalty.expand<-expand.dft(deathpenalty)
binom.model.0<-glm(DeathPenalty~DefendantRace,data=deathpenalty.expand,family = binomial)
summary(binom.model.0)
##计算OR值
exp(cbind(OR=coef(binom.model.0),confint(binom.model.0)))
binom.model.1<-update(binom.model.0,.~.+VictimRace)
summary(binom.model.1)
exp(cbind(OR=coef(binom.model.1),confint(binom.model.1)))
##输出四格表概率
prop.table(table(factor(deathpenalty.expand$VictimRace,
          labels = c('VictimRace=0','VictimRace=1')),
          factor(deathpenalty.expand$DefendantRace,
          labels = c("DefendantRace=0","DefendantRace=1"))),1)
library(lmtest)
lrtest(binom.model.1)
library(BaylorEdPsych)
PseudoR2(binom.model.1)
lrtest(binom.model.0,binom.model.1)