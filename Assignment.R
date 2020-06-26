finData = read.csv("all_stocks_5yr.csv") 

fin_data = finData[finData$Name == "EBAY",]
sapply(fin_data,function(x)sum(is.na(x))) #no missing data or NA values. 
fin_data$date = as.Date(fin_data$date, "%Y-%m-%d") #changing date format.
row.names(fin_data) = fin_data$date
qqnorm(fin_data$close)

#Log returns of closing price
plot(fin_data$close~fin_data$date, type = 'o', main='Time Series Plot'
     , ylab='daily closing prices', xlab='Time') #time series plot. 
log_returns = diff(log(fin_data$close))       #calculating log return on clsoing prices 
decompose(log_returns)
hist(log_returns)                              #histogram of returns. 
quantile(log_returns,probs=c(0.01,0.025,0.05)) # vaR at 99%, 97.5% and 95%
fin_data$log_returns = c(c(0), log_returns)
plot(fin_data$log_returns~fin_data$date, type = "l")

#Testing Stationarity for closing price
library(stats)
library(TSA)
acf(fin_data$close)
Box.test(fin_data$close,type="Ljung-Box")
adf.test(fin_data$close)
kpss.test(fin_data$close)


#Testing Stationarity for LogReturns
acf(fin_data$log_returns)
Box.test(fin_data$log_returns,type="Ljung-Box")
adf.test(fin_data$log_returns)
kpss.test(fin_data$log_returns)

#Testing Normality for Data
m = mean(fin_data$close)
m
stdev = var(fin_data$close)
stdev
skewness(fin_data$close)
kurtosis(fin_data$close)
shapiro.test(fin_data$close)
x=rnorm(120,m, sqrt(stdev))
ks.test(fin_data$close,x)
#p-value is smaller than 0.05 the disturbution of data is significanlty different from normal distribution. 

#Testing Normality for LogReturns
m = mean(fin_data$log_returns)
stdev = var(fin_data$log_returns)
skewness(fin_data$log_returns)
kurtosis(fin_data$log_returns)
shapiro.test(fin_data$log_returns)
x=rnorm(120,m, sqrt(stdev))
ks.test(fin_data$log_returns,x)  
#pvalue is greater than 0.05 the distributions of returs is not significanly different from normal distribution.

#Value at Risk for LogReturns
#Negative signs show amount of loss. 
library(PerformanceAnalytics)

confidence_intervals = seq(0.80,0.99,0.01) #CI from 80% to 99%
historical_VaR=c()
Modified_VaR =c()
Gaussian_Var=c()
for (i in confidence_intervals) {
  val=VaR(fin_data[,"log_returns"], p=i, method="historical")
  historical_VaR=c(historical_VaR, val)
  val = VaR(fin_data[,"log_returns"], p=i, "modified")
  Modified_VaR = c(Modified_VaR , val)
  val=VaR(fin_data[,"log_returns"], p=i, method="gaussian")
  Gaussian_Var=c(Gaussian_Var, val)
} 
plot(historical_VaR~confidence_intervals, type = "p", ylab = "Value at Risk for historical ", xlab = "Confidence interval")
plot(Modified_VaR~confidence_intervals, type = "p", ylab = "Value at Risk for modified", xlab = "Confidence interval")
plot(Gaussian_Var~confidence_intervals, type = "p", main = "Value at Risk for gaussian", xlab = "Confidence interval")

#Identifying and Fitting the best Time Series Model
library(tseries)
#1. Identify white noise
Box.test(fin_data$log_returns,  type="Ljung-Box")
#pvalue is less than 0.05 hence the data is not white noise. 

#2. ACF and PACF
w=acf(fin_data$log_returns, lag.max = 50, ci.type = 'ma')
w
#PACF
x=pacf(fin_data$log_returns,50)
x
ord=ar(fin_data$log_returns)
ord$aic
ord$order
# possible AR VALUES= 3,42,48
#possible MA VALUES= 1,3,42,48
z=arima(fin_data$log_returns, order = c(3,0,1))
tsdiag(z) 

#Testing Log Returns Volatility
par(mfrow=c(1,2))
g = garch(fin_data$log_returns, order=c(5,5))
summary(g)
plot(g$residuals, ylim=c(-10,10))
qqnorm(g$residuals,ylim=c(-10,10)); qqline(g$residuals, col=2, )

                            