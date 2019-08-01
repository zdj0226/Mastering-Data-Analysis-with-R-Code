#导入文本文件的子集
#数据导入时进行筛选
df <- read.csv.sql('hflights.csv',sql = "select * from file where Dest = '\"BNA\"'")
#注意，sql默认为"select * from file"，且sqldf不能自动识别双引号

#在导入到R会话前筛选平面文件
#从数据库中导入数据
#•数据筛选
#subset which [ [[ sqldf 
library(sqldf)
sqldf("select * from mtcars where am = 1 and vs = 1")

#sqldf的row.names参数默认为FALSE

identical(
  sqldf("select * from mtcars where am = 1 and vs = 1",
        row.names = TRUE),
  subset(mtcars,am == 1 & vs == 1)
)

##subset函数挑选数据，subset（数据，行==x，select=列）
subset(mtcars,am==1&vs==1,select = hp:wt)
## -c(hp:wt)排除
subset(mtcars,select = -c(hp:wt))
library(hflights)
library(dplyr)
##也可以使用filter函数
filter(hflights,Dest=='BNA')
##数据聚集
##选择数据，划分依据，计算指标
##with函数选择数据
aggregate(hflights$Diverted,by=list(hflights$DayOfWeek),FUN=mean)
with(hflights,aggregate(Diverted,by=list(hflights$DayOfWeek),FUN=mean))
##公式化标记,字符少，行名称正确，速度快
aggregate(Diverted~DayOfWeek,data=hflights,FUN=mean)
##利用tapply函数来快速聚集数据，返回对象是数列格式
tapply(hflights$Diverted,hflights$DayOfWeek,mean)
##用plyr包调用ddply函数，再辅助summarise函数来替代，显示指定列名
library(plyr)
ddply(hflights,.(DayOfWeek),summarise,Diverted=mean(Diverted))
##汇总函数

#dplyr
hflights_DayofWeek <- group_by(hflights,DayOfWeek)
str(attributes(hflights_DayofWeek))

# summarise
dplyr::summarise(hflights_DayofWeek,mean(Diverted))


#使用data.table实现聚集
hflights_dt[,mean(Diverted),by = DayOfWeek]

# 列命名
hflights_dt[,.('mean'=mean(Diverted)),by = DayOfWeek]


#测试

#汇总函数

#统计子分组样例数
ddply(hflights,.(DayOfWeek),summarise,n = length(Diverted))

ddply(hflights,.(DayOfWeek),nrow)

table(hflights$DayOfWeek)

# plyr的count函数
count(hflights,'DayOfWeek')

# dplyr
dplyr::summarise(hflights_DayofWeek,n())

# hflights_DayOfWeek的结构
attr(hflights_DayofWeek,'group_sizes')

# data.table
hflights_dt[,.N,by =.(DayOfWeek)]
