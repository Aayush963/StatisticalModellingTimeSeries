setwd("C://Users//udays//Downloads")

Nike <- read.csv("NKE.csv")
View(Nike)
Skechers <- read.csv("SKX.csv")
View(Skechers)
Adidas <- read.csv("ADDYY (1).csv")
View(Adidas)
COLM <- read.csv("COLM.csv")
View(COLM)
UA <- read.csv("UAA.csv")
View(UA)


####

library(data.table)

#### Adjusting for the names since we need to form a table with all the values. 


oldnames = c("Open","High","Low","Close","Adj.Close","Volume")


Nike_Name = c("Nike_Open","Nike_High","Nike_Low","Nike_Close","Nike_Adj.Close","Nike_Volume")
setnames(Nike, oldnames , Nike_Name )
SK_Name = c("SK_Open","SK_High","SK_Low","SK_Close","SK_Adj.Close","SK_Volume")
setnames(Skechers, oldnames , SK_Name )
AD_Name = c("AD_Open","AD_High","AD_Low","AD_Close","AD_Adj.Close","AD_Volume")
setnames(Adidas, oldnames , AD_Name )
UA_Name = c("UA_Open","UA_High","UA_Low","UA_Close","UA_Adj.Close","UA_Volume")
setnames(UA, oldnames , UA_Name )
COL_Name = c("COL_Open","COL_High","COL_Low","COL_Close","COL_Adj.Close","COL_Volume")
setnames(COLM, oldnames , COL_Name )


# Nike_Updated <- Nike[-1]
# View(Nike_Updated)



e = merge.data.frame(Nike,Skechers, by = "Date")
View(e)
f = merge.data.frame(e,Adidas, by = "Date")
View(f)
g = merge.data.frame(f,UA, by = "Date")
View(UA)
View(g)
DF = merge.data.frame(g,COLM, by = "Date")
View(DF)



## Collated plot of Various players. 
require(lattice)
xyplot(DF$Nike_Adj.Close + DF$SK_Adj.Close + DF$AD_Adj.Close +DF$COL_Adj.Close +DF$UA_Adj.Close ~ DF$Date  , data=DF, type = c('l','l','l','l','l'), col = c("blue", "red","green","gray","orange"),main = " Date Vs Adjusted Close for Various Companies ", xlab = "Date" ,ylab = "Stocks " ,auto.key=T)

## Simultaneous Plots 

# 5 figures arranged in 5 rows and 1 column
attach(mtcars)
par(mfrow=c(5,1), par(mar=c(1,1,1,1))) 
plot(DF$Nike_Adj.Close,type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "NIKE")
plot(DF$SK_Adj.Close,type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "Sketchers")
plot(DF$AD_Adj.Close,type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "Adidas")
plot(DF$COL_Adj.Close,type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "Puma")
plot(DF$UA_Adj.Close,type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "Under Armor")


###### Modeling Adjusted Closure of one of the Time Series on that of others. 

M1 = lm(data=DF,UA_Adj.Close ~ Nike_Adj.Close+SK_Adj.Close+AD_Adj.Close+COL_Adj.Close)
summary(M1)

library(Hmisc)
test<-Lag(DF$Nike_Close,-1)
cor(DF$Nike_Adj.Close,test)
acf(DF$Nike_High)
acf(M1$residuals)
acf(DF$UA_Adj.Close, plot=FALSE)$acf[2]
par(mfrow=c(2,1), par(mar=c(1,1,1,1))) 
hist(M1$residuals)
qqnorm(M1$residuals,col = "Red")
qqline(M1$residuals)
shapiro.test(M1$residuals)
####################################################################
############## STOCK PRICE Prediction for All the stocks ##########

### In progress .... Only Nike is done. 

setwd("C://Users//udays//Downloads")
nike = read.csv("NKE.csv")
View(nike)
nrow(nike)

agg = aggregate(nike, by = as.yearmon(Date))
#Studying the P Values


M1 = lm(data=nike,Volume~nike$Open+nike$High+nike$Low+nike$Close+nike$Adj.Close)
summary(M1)

plot(x = nike$Date,y = nike$Volume,data = nike)

ua = read.csv("UAA.csv")
View(ua)


adidas = read.csv("ADDYY (1).csv") # Reading Adidas
View(adidas)

library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)

#install.library("xts")
#install.library("xts")

# Lets look at the trends in the Stock Market #

plot(nike[,5],type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "NIKE")

plot(ua[,5],type = "l",xlab = "Time Range", ylab = "Adjusted Close", main = "Under Armor") ## Plot of Under armor. 

## It appears that post the 600 days there is a decline in the Share price for Under Armor. 
## We want to know what went wrong during that time ? 

plot(adidas[,5],type = "l",xlab = "Time Range", ylab = "Adjusted Close", main = "Adidas")

## Summary Statistics for each of the variables : 

summary(nike)
summary(adidas)
summary(ua)
par(mfrow=c(1,1))
# From the summary I want to know which date gave the Maximum and Minmum share prices : 

boxplot(ua$Adj.Close) # Observe no outliers here
boxplot(nike$Adj.Close) # Outliers are present -- Big company , and for obvious reasons.  
boxplot(adidas$Adj.Close) # observe no outliers here

##### Time Series Starts here Nike Analysis

## Lets get the Returns and Log Returns : 

n1=length(nike$Adj.Close)
n1
Ret1 = nike$Adj.Close[-1]/nike$Adj.Close[-n1]-1 # Returns
ret1 = diff(log(nike$Adj.Close)) # log returns
plot(x = nike$Date[-1],y = 100*Ret1,type = "l",col=1,main = "Log Differencing Vs time",xlab = "Time",ylab = "Log(diff)")
lines(nike$Date[-1],100*Ret1,col ="green")

#points(nike$Date[-1],100*ret1,col=2,cex=0.1 )

plot(x = nike$Date[-1],y = 100*ret1,type = "l",xlab="Time",ylab="Log-Return",col=1)
lines(nike$Date[-1],10*ret1,col ="black")

#legend(16600,-4.5,legend=c("Ret","Log-Return"),lty=1,col=c(1:2))


summary(ret1)   # Summary For Log Returns

##Spectrum Analysis

raw1 = spectrum(ret1)
smooth = spectrum(ret1,spans=c(25,5,25),main="Smoothed periodogram",ylim=c(9e-4,8e-5))

# From the smoothed periodogram, we can see that there is no significant dominant frequency, which means
# there is no significant cycles.Although there are some small peaks in the spectrum, but when we move the
# crossbar to each peak alongthe estimated spectrum, it gives pointwise 95% confidence intervals, and we 
# can see that all the peaks are insignificant.This is not contrary to our common sense that the stock 
# price and return is a kind like random walk, one can hardly find cycles in such a few years.


############ FITTING ARIMA Model

# Null Hypothesis : There is no Trend. 
# Alternate Hypothese : There is trend. 

# First we need to Decide which Auto Regressive Moving Average Model Needs to be used to answer ? 

# Code to print the AR MA values as a matrix. 

#https://rstudio-pubs-static.s3.amazonaws.com/345790_3c1459661736433382863ed19c30ea55.html
#https://www.quantstart.com/articles/Autoregressive-Moving-Average-ARMA-p-q-Models-for-Time-Series-Analysis-Part-3
#http://www-stat.wharton.upenn.edu/~stine/insr260_2009/lectures/arma_forc.pdf

# to be explaned by naman 
aic_table_nike = function(nike,P,Q){
  table_nike = matrix(NA,(P+1),(Q+1))
  for (p in 0:P){
    for (q in 0:Q){
      table_nike[p+1,q+1] = arima(nike,order=c(p,0,q))$aic
    }
  }
  dimnames(table_nike) = list(paste("<b> AR",0:P,"</b>",sep=""),paste("MA",0:Q,sep=""))
  table_nike
}

ret_aic_table_nike = aic_table_nike(ret1,4,4)

require(knitr)
kable(ret_aic_table_nike,digits=2)


## Here we need to choose the lowest AR vs MA value from the AIC table (Recall the learning).. 

min(ret_aic_table_nike) # This gives that the AR - 3 and MA - 3 is the best model.


# However, we also see that the AR1 vs MA1 is the third lowest. We will analyse for these two models. 

nike_arma33 = arima(ret1,order = c(4,0,4))
nike_arma33

nike_arma11 = arima(ret1,order = c(1,0,1))
nike_arma11

# We observe that the log likelihood of ARMA Model with p = 4 and q = 4 is the most apt model.
# Since the model with p = 3 and q = 3 happens to suffer with overfitting we choose the next best one 
# i.e p =1,q=1
# Hence we will try to model this for the moment. 
# the model looking at the parameter is : (1-0.7080B)(x(n)-0.0007) = (1-0.7733B)e(n)
# e(n) ~ N[0.2267]

# Confidence Interval for the parameters for nike : 
# [0.4324,0.9835]
# [-1.0085 , -.5381]

#### Test for Residuals : 

acf(resid(nike_arma11))
pacf(resid(nike_arma11))

####Finally, we perform the Ljung-Box test for 20 lags to confirm this:

Box.test(resid(nike_arma11), lag=20, type="Ljung-Box")
## ACF of Residuals is near to zero which means that model is relatively suitable

##### Forecasting
library(quantmod)
library(fArma)
install.packages("Arma")


## partition into train and test
train_series=ret1[1:5000]
View(train_series)
test_series=ret1[5001:5033]

## make arima models
arima_ret1_1=arima(train_series, order=c(1,0,1))
arima_ret1_2=arima(train_series, order=c(3,0,3))


#require(forecast)
library(forecast)
future_101 = forecast(arima_ret1_1, h = 300)
future_303 = forecast(arima_ret1_2, h = 300)
plot(future_101,col = "orange")
plot(future_303,col = "blue")


## Use Predict here to check for any date - Pending
?arma
library(forecast)
arima_nike_1 = arma(nike,order = c(1,0,1))
print(arima_nike_1)
future_nike_101 = forecast(nike[,5],h=1000)
plot(future_nike_101,col="purple")

ten_days = predict(arima_nike_1,n.ahead = 100)
ten_days



install.packages("forecast")
print(arima_nike_1)
print(arima_nike_2)
### the graph shows an upward trend and it appears that the stock price will take a further upward turn.
## Light Gray interval shows 99 % confidence interval. Dark grey shows 95% confidence interval. 

### IMPLEMENTING VARS ###


install.packages("vars")
library(vars)
head(DF)

nik_adj = diff(DF$Nike_Adj.Close)
Add_adj = diff(DF$AD_Adj.Close)
COL_adj = diff(DF$COL_Adj.Close)
Sk_adj  = diff(DF$SK_Adj.Close)
UA_adj  = diff(DF$UA_Adj.Close)

net = cbind(nik_adj,Add_adj,COL_adj,Sk_adj,UA_adj)
View(net)

sport_VAR = VAR(net, type = "const", lag.max = 10,ic = "AIC")
sport_VAR

summary(sport_VAR)


## Predicting VARS for UA only : 

UA_op = diff(DF$UA_Open)
UA_Cl = diff(DF$UA_Close)
UA_Hi = diff(DF$UA_High)
UA_Lo = diff(DF$UA_Low)
UA_Vol= diff(DF$UA_Volume)

net_UA = cbind(UA_op,UA_Hi,UA_Lo,UA_Cl,UA_adj)
View(net_UA)

UA_VAR = VAR(net_UA, type = "both", lag.max = 10,p = 2)
summary(UA_VAR)

