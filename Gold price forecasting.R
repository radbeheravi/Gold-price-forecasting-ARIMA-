# Radbeh Heravi
# 1401-10-...
# Financial Time Series
# Article

# packages
library(lubridate)
library(forecast)
library(urca)
library(ggplot2)

# remove previous stuff
rm(list=ls(all=TRUE))
ls()

# set path
setwd("C:/Users/asus/Desktop/R projects")
getwd()

# import data
mydata <- read.csv("C:/Users/asus/Desktop/goldp.csv",sep=",",header = TRUE)

# preparing data
mydata <- as.data.frame(mydata)
class(mydata)
mydata$goldp <- as.ts(mydata$goldp)
mydata$date <- as.Date(mydata$date)

# plot data for goldp
ggplot(mydata, aes(x = date, y = goldp)) +
  geom_line(col="red" , size=1.2) + 
  ggtitle("Gold Price")

# plot acf for goldp
layout(matrix(c(1,2),1,2))
Acf(mydata$goldp,lag.max=32,ann=FALSE, main="ACF of Gold Price", col= "blue", lwd=3)  
Pacf(mydata$goldp,lag.max=32,ann=FALSE, main="PACF of Gold Price", col= "blue", lwd=3)


# step 1: is the data stationary?---------------------------------------------------------------
# load my unit root tests
source("urtests.r")

# unit root test for goldp 
test1 <- ur.test(mydata$goldp,trend="ct",method="adf.gls")
print.ur.test(test1)
# result: it is not. Thus it is I(1) with drift

# first diff of goldp
mydata$fdgoldp <- c(NA, diff(mydata$goldp))

# unit root test of first diff of goldp
test2 <- ur.test(mydata$fdgoldp[2:123],trend="c",method="adf.gls")
print.ur.test(test2)
# result: it is stationary at the 5 percent level

# plot data for fdgoldp
ggplot(mydata[2:123,], aes(x = date, y = fdgoldp)) +
  geom_line(col="dark green", lwd=1.2) + 
  geom_hline(yintercept=0) +
  ggtitle("First Difference of Gold Price")

# Step 2: finding the best ARIMA model----------------------------------------------------------
# estimate ARIMA(1,0,1)
model101 <- Arima(mydata$goldp,order=c(1,0,1),method="ML",include.drift = TRUE)
model101
# result: it is not verified, because ma(1) and intercept are not statistically significant.

# estimate ARIMA(1,0,2)
model102 <- Arima(mydata$goldp,order=c(1,0,2),method="ML",include.drift = TRUE)
model102
# result: it is not verified, because ma(1), ma(2) and intercept are not statistically significant.

# estimate ARIMA(1,0,3)
model103 <- Arima(mydata$goldp,order=c(1,0,3),method="ML",include.drift = TRUE)
model103
# result: it is not verified, because ma(1), ma(2) and intercept are not statistically significant.

# estimate ARIMA(1,1,1)
model111 <- Arima(mydata$goldp,order=c(1,1,1),method="ML",include.drift = TRUE)
model111
# result: Goooooood

# estimate ARIMA(1,1,2)
model112 <- Arima(mydata$goldp,order=c(1,1,2),method="ML",include.drift = TRUE)
model112
# result: it is not verified, because ma(2) and intercept are not statistically significant.

# estimate ARIMA(1,1,3)
model113 <- Arima(mydata$goldp,order=c(1,1,3),method="ML",include.drift = TRUE)
model113
# result: it is not verified, because ma(2) and ma(3) are not statistically significant.
# final result of this part: ARIMA(1,1,1) is picked.

# step 3: forecasting --------------------------------------------------------------------------
# add a column of fitted values to mydata
mydata$fit <- model111$fitted

# plot goldp and fit values
ggplot(mydata, aes(x = date)) +
  geom_line(aes(y = goldp), col="red", size=1.2) +
  geom_line(aes(y = fit), col="blue", size=1.2) +
  ggtitle("Gold Price (Red Line), Fitted Values (Blue Line)")

# 6-step ahead forecasts
# ARIMA(1,0,1) 
pred101 <- forecast(model101,h=6)
pred101 <- as.ts(pred101$mean,start=c(2014,2),end=c(2014,7),frequency=12) 

# ARIMA(1,0,2)
pred102 <- forecast(model102,h=6)
pred102 <- as.ts(pred102$mean,start=c(2014,2),end=c(2014,7),frequency=12) 

# ARIMA(1,0,3)
pred103 <- forecast(model103,h=6)
pred103 <- as.ts(pred103$mean,start=c(2014,2),end=c(2014,7),frequency=12) 

# ARIMA(1,1,1) <---- This is our selected model
pred111 <- forecast(model111,h=6)
pred111 <- as.ts(pred111$mean,start=c(2014,2),end=c(2014,7),frequency=12) 

# ARIMA(1,1,2)
pred112 <- forecast(model112,h=6)
pred112 <- as.ts(pred112$mean,start=c(2014,2),end=c(2014,7),frequency=12) 

# ARIMA(1,1,3)
pred113 <- forecast(model113,h=6)
pred113 <- as.ts(pred113$mean,start=c(2014,2),end=c(2014,7),frequency=12)



# making a suitable data frame for ARIMA(1,1,1) to plot goldp, fitted and predicted values.
date <- c(mydata$date,"2014-02-01"
          ,"2014-03-01"
          ,"2014-04-01"
          ,"2014-05-01"
          ,"2014-06-01"
          ,"2014-07-01"
          ,"2014-08-01"
          ,"2014-09-01"
          ,"2014-10-01"
          ,"2014-11-01"
          ,"2014-12-01"
          ,"2015-01-01"
          ,"2015-02-01"
)
date <- as.Date(date)
goldp <- ts(c(mydata$goldp, rep(NA,13)), start = c(2003,11), end = c(2015,2),
            frequency = 12)
fit <- ts(c(mydata$fit, rep(NA,13)), start = c(2003,11), end = c(2015,2),
          frequency = 12)
prediction <- ts(c(rep(NA,123), pred111, rep(NA,7)), start = c(2003,11), end = c(2015,2),
               frequency = 12)
mydata1 <- data.frame(date, goldp, fit, prediction)

# plot goldp, fitted and forecasted values
ggplot(mydata1, aes(x = date)) +
  geom_line(aes(y = goldp), col="red", size=1.2) +
  geom_line(aes(y = fit), col="blue", size=1.2) +
  geom_line(aes(y = prediction), col="black", size=1.5) +
  ggtitle("Gold Price (Red Line), Fitted Values (Blue Line) and Predicted Values (Black Line)")

# step 4: fit statistics (appendix)-------------------------------------------------------------
# r-squared
e101 <- mydata$goldp - model101$fitted
r101 <- 1-(sum(e101^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

e102 <- mydata$goldp - model102$fitted
r102 <- 1-(sum(e102^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

e103 <- mydata$goldp - model103$fitted
r103 <- 1-(sum(e103^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

e111 <- mydata$goldp - model111$fitted
r111 <- 1-(sum(e111^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

e112 <- mydata$goldp - model112$fitted
r112 <- 1-(sum(e112^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

e113 <- mydata$goldp - model113$fitted
r113 <- 1-(sum(e113^2)/sum((mydata$goldp-mean(mydata$goldp))^2))

r <- rbind(r101,r102,r103,r111,r112,r113)

# rmse, mape, mae
# observed values during 2014-02 till 2014-07
obs <- c(29482.91,29670.43,28514.64,27812.81,26813.15,27867.11)

ac101 <- accuracy(pred101, obs)
ac102 <- accuracy(pred102, obs)
ac103 <- accuracy(pred103, obs)
ac111 <- accuracy(pred111, obs)
ac112 <- accuracy(pred112, obs)
ac113 <- accuracy(pred113, obs)

ac <- rbind(ac101,ac102,ac103,ac111,ac112,ac113)

# normalized bic
bic101 <- log(sum(model101$residuals^2)/123) + (length(model101$coef)/123)*log(123)
bic102 <- log(sum(model102$residuals^2)/123) + (length(model102$coef)/123)*log(123)
bic103 <- log(sum(model103$residuals^2)/123) + (length(model103$coef)/123)*log(123)
bic111 <- log(sum(model111$residuals^2)/123) + (length(model111$coef)/123)*log(123)
bic112 <- log(sum(model112$residuals^2)/123) + (length(model112$coef)/123)*log(123)
bic113 <- log(sum(model113$residuals^2)/123) + (length(model113$coef)/123)*log(123)

bic <- rbind(bic101,bic102,bic103,bic111,bic112,bic113)

# Ljung q statistics
bt101 <- Box.test(model101$resid,lag=18,type="Ljung-Box",fitdf=length(model101$coef))
bt102 <- Box.test(model102$resid,lag=18,type="Ljung-Box",fitdf=length(model102$coef))
bt103 <- Box.test(model103$resid,lag=18,type="Ljung-Box",fitdf=length(model103$coef))
bt111 <- Box.test(model111$resid,lag=18,type="Ljung-Box",fitdf=length(model111$coef))
bt112 <- Box.test(model112$resid,lag=18,type="Ljung-Box",fitdf=length(model112$coef))
bt113 <- Box.test(model113$resid,lag=18,type="Ljung-Box",fitdf=length(model113$coef))

bt <- rbind(bt101$p.value,bt102$p.value,bt103$p.value,bt111$p.value,bt112$p.value,bt113$p.value)

# gather all the fit statistics
fitstats <- cbind(r,ac,bic,bt) 
fitstats <- as.data.frame(fitstats)
rownames(fitstats) <- c("ARIMA(1,0,1)","ARIMA(1,0,2)","ARIMA(1,0,3)",
                        "ARIMA(1,1,1)","ARIMA(1,1,2)","ARIMA(1,1,3)")
colnames(fitstats) <- c("R-squared","ME","RMSE","MAE","MPE","MAPE","Normalized BIC",
                        "Ljung-Box p-value")
fitstats







