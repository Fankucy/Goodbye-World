rm(list = ls())
library(quantmod)

dat <- getSymbols('GOOGL', from = '2012-09-27', to = '2017-09-27')
dim(GOOGL)
head(GOOGL)
chartSeries(GOOGL, theme = 'white')
chartSeries(GOOGL, theme = 'white', TA = NULL) #without volumn

GOOGL.simrtn <- diff(GOOGL[,6])/GOOGL[1:1256,6]
GOOGL.logrtn <- diff(log(GOOGL[,6]))
head(GOOGL.simrtn)
head(GOOGL.logrtn)
chartSeries(GOOGL.simrtn, theme = 'white')
chartSeries(GOOGL.logrtn, theme = 'white')

library(fBasics)
basicStats(GOOGL.logrtn)#ͼ��֮���˵����������������������
#mean()
#var()
#sd()
#skewness()
#kurtosis()
#normalTest( ,method=��jb��)

#visualization
rtn <- as.numeric(GOOGL.logrtn[2:1257])
#�ֲ�ֱ��ͼ
hist(rtn, breaks = 60, col = 'grey')
#�����ܶ�ͼ����̬�����ܶ�ͼ�ĶԱ�
range(rtn)
x <- seq(-0.16, 0.16, 0.001)
dens <- density(rtn)
y2 <- dnorm(x, mean = mean(rtn), sd = sd(rtn))
plot(dens$x, dens$y, xlab = 'rtn', ylab = 'density', type = 'l')
lines(x, y2, lty = 2)
#����ֱ��ͼ����̬�����ܶ�ͼ�ĶԱ�
hist(rtn, breaks = 60)
#ʱ������ͼ
tdx <- c(1:length(rtn))/250 + 2012.75
plot(tdx, rtn, xlab = 'year', ylab = 'rtn', type = 'l')
grid()

# Q-Q plot

# ACF plot


#da <- read.csv("C:/Users/Administrator/Desktop/zj/lesson 1 reference/GOOGL.csv")
#simrtn <- diff(da$Adj.Close)/da$Adj.Close[1:(length(da$Adj.Close)-1)]
#logrtn <- diff(log(da$Adj.Close))
