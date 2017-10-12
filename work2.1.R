rm(list = ls())
Sys.setlocale("LC_TIME","english")
library(tseries)
library(quantmod)
library(moments)

####################read data######################
getSymbols("000300.SS",from="2005-04-08",to="2017-03-25")
#HSSB=read.table("D:/桌面/文件夹/学习/大三下/Financial Econometrics/作业/第一次/daily.txt",header=TRUE)


###################################################
#stationarity and volatility clustering
###################################################

#dprice = ts(HSSB$AdjClose,frequency = 365,start=c(2005,4,8))
#ts.plot(dprice,gpars=list(xlab = "Year", ylab="Daily Price",main="SCI 300 Index [2005/4/8-2017/3/25]",col="blue"))

#par(mfrow=c(4,1))
HSSB.dprice = `000300.SS`$`000300.SS.Adjusted`
dprice = ts(HSSB.dprice,frequency = 365,start=c(2005,4,8))
ts.plot(dprice,gpars=list(xlab = "Year", ylab="Daily Price",main="SCI 300 Index [2005/4/8-2017/3/25]",col="blue"))
#daily price

par(mfrow=c(3,1))
HSSB.dlogrtn = periodReturn(`000300.SS`,period='daily',type='log')
dlogreturn = ts(HSSB.dlogrtn, frequency = 365,start=c(2005,4,8))
ts.plot(dlogreturn,gpars=list(xlab = "Year", ylab="Daily Log Return",main="SCI 300 Index [2005/4/8-2017/3/25]",col="blue"))
#daily log return

HSSB.wlogrtn = periodReturn(`000300.SS`,period='weekly',type='log')
wlogreturn = ts(HSSB.wlogrtn, frequency = 52,start=c(2005,4,8))
ts.plot(wlogreturn,gpars=list(xlab = "Year", ylab="Weekly Log Return",main="SCI 300 Index [2005/4/8-2017/3/25]",col="blue"))
#weekly log return

HSSB.mlogrtn = periodReturn(`000300.SS`,period='monthly',type='log')
mlogreturn = ts(HSSB.mlogrtn, frequency = 12,start=c(2005,4,8))
ts.plot(mlogreturn,gpars=list(xlab = "Year", ylab="Monthly Log Return",main="SCI 300 Index [2005/4/8-2017/3/25]",col="blue"))
#monthly log return


####################################################
#heavy tails, asymetry and aggregational gaussianity
####################################################

#par(mfrow=c(2,3))
hist(HSSB.dlogrtn,breaks=100,prob=TRUE,col="yellow",xlab='',main='Daily Returns')
nrx = seq(min(HSSB.dlogrtn),max(HSSB.dlogrtn),by=0.001)
nry = dnorm(nrx,mean(HSSB.dlogrtn),sd(HSSB.dlogrtn))
lines(nrx,nry,col="blue")

hist(HSSB.wlogrtn,breaks=100,prob=TRUE,col="yellow",xlab='',main='Weekly Returns')
wnrx = seq(min(HSSB.wlogrtn),max(HSSB.wlogrtn),by=0.001)
wnry = dnorm(wnrx,mean(HSSB.wlogrtn),sd(HSSB.wlogrtn))
lines(wnrx,wnry,col="blue")

hist(HSSB.mlogrtn,breaks=100,prob=TRUE,col="yellow",xlab='',main='Monthly Returns')
mnrx = seq(min(HSSB.mlogrtn),max(HSSB.mlogrtn),by=0.001)
mnry = dnorm(mnrx,mean(HSSB.mlogrtn),sd(HSSB.mlogrtn))
lines(mnrx,mnry,col="blue")

qqnorm(HSSB.dlogrtn,col="brown",ylab='Quantile of Daily Returns',xlab='Normal Quantile')
qqline(HSSB.dlogrtn,col="blue")

qqnorm(HSSB.wlogrtn,col="brown",ylab='Quantile of Weekly Returns',xlab='Normal Quantile')
qqline(HSSB.wlogrtn,col="blue")

qqnorm(HSSB.mlogrtn,col="brown",ylab='Quantile of Monthly Returns',xlab='Normal Quantile')
qqline(HSSB.mlogrtn,col="blue")


####################################################
#long range dependence
####################################################

par(mfrow=c(3,3))
acf(HSSB.dlogrtn,lag=40,main="Daily Returns")
acf(HSSB.wlogrtn,lag=32,main="Weekly Returns")
acf(HSSB.mlogrtn,lag=24,main="Monthly Returns")

acf(HSSB.dlogrtn^2,lag=40,main="Squared Daily Returns")
acf(HSSB.wlogrtn^2,lag=32,main="Squared Weekly Returns")
acf(HSSB.mlogrtn^2,lag=24,main="Squared Monthly Returns")

acf(abs(HSSB.dlogrtn),lag=40,main="Absolute Daily Returns")
acf(abs(HSSB.wlogrtn),lag=32,main="Absolute Weekly Returns")
acf(abs(HSSB.mlogrtn),lag=24,main="Absolute Monthly Returns")


####################################################
#stationary(ADF test)
####################################################
adf.test(HSSB.dprice)
adf.test(HSSB.dlogrtn)
adf.test(HSSB.wlogrtn)
adf.test(HSSB.mlogrtn)

####################################################
#heavy tails and asymmetry(ADF test)
####################################################
kurtosis(HSSB.dlogrtn)
kurtosis(HSSB.wlogrtn)
kurtosis(HSSB.mlogrtn)

skewness(HSSB.dlogrtn)
skewness(HSSB.wlogrtn)
skewness(HSSB.mlogrtn)

####################################################
#aggregational gaussianity(JB test)
####################################################
jarque.bera.test(HSSB.dlogrtn)
jarque.bera.test(HSSB.wlogrtn)
jarque.bera.test(HSSB.mlogrtn)



