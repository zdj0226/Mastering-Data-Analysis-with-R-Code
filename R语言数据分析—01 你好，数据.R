library('hflights')
write.csv(hflights,'hflights.csv',row.names = FALSE)
str(hflights)
system.time(read.csv('hflights.csv'))
colClasses<-sapply(hflights,class)
system.time(read.csv('hflights.csv',colClasses = colClasses))
library(microbenchmark)
f<-function()read.csv('hflights.csv')
g<-function()read.csv('hflights.csv',colClasses = colClasses,
                      nrows = 227496,comment.char = '')
res<-microbenchmark(f(),g())
res
boxplot(res,xlab='',
     main=expression(paste('Benchmarking',italic('read.table'))))
library(sqldf)
system.time(read.csv.sql('hflights.csv'))
library(bigmemory)
system.time(read.big.matrix('hflights.csv',header=TRUE))
library(data.table)
system.time(dt<-fread('hflights.csv'))
df<-as.data.frame(dt)
is.data.frame(dt)
.read.csv.orig<-function()read.csv('hflights.csv')
.read.csv.opt<-function()read.csv('hflights.csv',colClasses = colClasses,
              nrows = 227496,comment.char = '',stringsAsFactors = FALSE)
.read.csv.sql<-function()read.csv.sql('hflights.csv')
.read.big.matrix<-function()read.big.matrix('hflights.csv',header=TRUE)
.fread<-function()fread('hflights.csv')
res<-microbenchmark(.read.csv.orig(),.read.csv.opt(),.read.csv.sql(),
                    .read.big.matrix(),.fread(),times=10)
print(res,digits=6)
df<-read.csv.sql('hflights.csv',sql = 
                   "select * from file where Dest='\"BNA\"'")
system.time(read.csv.sql('hflights.csv'))
system.time(read.csv.sql('hflights.csv',sql = 
              "select*from file where Dest='\"BNA\"'"))
