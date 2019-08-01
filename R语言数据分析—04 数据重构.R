# 数据重构

library(data.table)
library(dplyr)

#1.矩阵转置
(m<-matrix(1:9,3))
t(m)
# 适用于data.frame对象
head(iris)
t(head(iris))

# 2.基于字符串匹配实现数据筛选
library(dplyr)
library(hflights)
str(select(hflights,ends_with("delay")))
# ignore.case 是否区分大小写
str(select(hflights,contains('T',ignore.case=FALSE)))

# 正则表达式
# match
# 长度为5或者6的列名
str(select(hflights,matches("^[[:alpha:]]{5,6}$")))   #[]{n}

# 符号- 筛选所有不符合表达式条件的列名
# 列名定义时最常用的字符个数
table(nchar(names(hflights)))
names(hflights)
colnames(hflights)
# 去掉列名长度为7或8的列
names(select(hflights,-matches("^[[:alpha:]]{7,8}$")))

# 3.数据重排序
str(arrange(hflights,ActualElapsedTime))
# 管道命令操作符
hflights %>% arrange(ActualElapsedTime) %>% str

# dplyr
hflights %>% 
  arrange(ActualElapsedTime) %>% 
  select(ActualElapsedTime,Dest) %>% 
  subset(Dest != 'Aus') %>% 
  head %>% 
  str
# data.table
str(head(data.table(hflights,key = 'ActualElapsedTime')[Dest != 'AUS',c('ActualElapsedTime','Dest'),with = FALSE]))

str(head(na.omit(
  data.table(hflights,key = 'ActualElapsedTime'))[Dest != 'AUS',.(ActualElapsedTime,Dest)]))

# na.omit在哪里调用

# 速度比较
system.time(hflights_dt$DistanceKMs <- hflights_dt$Distance / 0.62137)
system.time(hflights_dt[,DistanceKMs := Distance / 0.62137])

# dplyr和data.table
# 内存使用分析
# 内存位置//指针的值
library(pryr)
hflights_dt <- data.table(hflights)
address(hflights_dt)
# 查看传统的赋值操作符是否会改变存放对象的地址
hflights_dt$DistanceKMs <- hflights_dt$Distance / 0.62137
address(hflights_dt)

# 查看data.table包的:=的使用方法
hflights_dt <- data.table(hflights)
address(hflights_dt)
hflights_dt[,DistanceKMs := Distance / 0.62137]
address(hflights_dt)

# within
system.time(within(hflights_dt,DistanceKMs <- Distance / 0.62137))

# 同时创建多个变量
hflights_dt[,c('DistanceKMs','DistanceFeets'):= .(Distance / 0.62137,Distance * 5280)]

carriers <- unique(hflights_dt$UniqueCarrier)
carriers
hflights_dt[,paste('carrier',carriers,sep = '_'):=
              lapply(carriers,function(x) as.numeric(UniqueCarrier == x))]
str(hflights_dt[,grep('^carrier',names(hflights_dt)),with=FALSE])

# 正则
select(iris,grep('^P',colnames(iris)))

# 采用dplyr包生成新变量
hflights <- hflights %>% mutate(DistanceKMs = Distance / 0.62137)

# 数据集合并
# dplyr/join
# data.table调用[ 操作符的mult参数
(wdays <- data.frame(
  DayOfWeek = 1:7,
  DayOfWeekString = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
))

system.time(merge(hflights,wdays))
system.time(merge(hflights_dt,wdays,by = 'DayOfWeek'))

# 相同结构 rbind cbind
# 稀疏矩阵 rBind cBind
# do.call命令，对list对象的所有元素执行rbind或cbind命令
# rbindlist 合并data.table对象

# 4.数据整形
# 将宽表转换为长表 melt
library(reshape2)
hflights_melted <- melt(hflights,id.vars = 0,measure.vars = c('ActualElapsedTime','AirTime'))
str(hflights_melted)
# ggplot2绘图需要

# 将长表转换为宽表 cast
hflights_melted <- melt(hflights,id.vars = 'Month',measure.vars = c('ActualElapsedTime','AirTime'))
head(hflights_melted)

(df <- dcast(hflights_melted,Month ~ variable,fun.aggregate = mean,na.rm = TRUE))

library(ggplot2)
ggplot(melt(df,id.vars = 'Month')) + 
  geom_line(aes(x = Month,y = value,color = variable)) + 
  scale_x_continuous(breaks = 1:12) + 
  theme_bw() + 
  theme(legend.position = 'top')


# tidyr包  gather和spread
library(tidyr)
str(gather(hflights[,c('Month','ActualElapsedTime','AirTime')],variable,value,-Month))