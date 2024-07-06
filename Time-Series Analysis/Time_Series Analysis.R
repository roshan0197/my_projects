#Loading Packages
library(lubridate)
library(dplyr)
library(zoo)
library(forecast)
library(smooth)
library(tsutils)
library(tstools)
library(tseries)
library(ggplot2)
library(openxlsx)
library(data.table)
#Import Data
group3 <- read.xlsx("/Users/roshanshekhar/Documents/M4-Daily-Assignment-1.xlsx", sheet = 1, rows = c(1,4,32,60))
info <- read.csv("/Users/roshanshekhar/Documents/M4-info.csv")

group3_info <- transpose(merge(group3,info, by.x =  "M4.Label",by.y = "M4id"))
    #3_1
group3_1 <- group3_info[c(4:3442,4418),1]
group3_1_ts <- ts(as.numeric(group3_1[1:3439]), frequency = 365, 
                  start = c(as.Date(group3_1[3440],format = "%d/%m/%Y")))
group3_1_date <- seq(as.Date(group3_1[3440],format = "%d/%m/%Y"), 
                     by = "day", length.out = length(group3_1)-1)
    #3_2
group3_2 <- group3_info[c(4:3323,4418),2]
group3_2_ts <- ts(as.numeric(group3_2[1:3320]), frequency = 365)
group3_2_date <- seq(as.Date(group3_2[3321],format = "%d/%m/%Y"), 
                     by = "day", length.out = length(group3_2)-1)
    #3_3
group3_3 <- group3_info[c(4:3323,4418),3]
group3_3_ts <- ts(as.numeric(group3_3[1:3320]), frequency = 365, 
                  start = c(as.Date(group3_3[3321],format = "%d/%m/%Y")))
group3_3_date <- seq(as.Date(group3_3[3321],format = "%d/%m/%Y"), 
                     by = "day", length.out = length(group3_3)-1)
####################################################################################
datats <- group3_1_ts
datadate <-group3_1_date
#blue for series3 "#0066CC"
#green for series 31"#4C9900"
#red for series59 "#CC0000"
####################################################################################
#1.Visualize Raw Data
plot(x = group3_1_date, y = group3_1_ts, main = "Time series for original data", 
     xlab = "Date", ylab = "Value", col = "#0066CC", type = "l",
     xlim = as.Date(c(group3_1_date[1],group3_2_date[length(group3_2_date)])) )
lines(x = group3_2_date, y = group3_2_ts, col = "#4C9900")
lines(x = group3_3_date, y = group3_3_ts, col = "#CC0000")
legend("topleft", legend=c("Series 3", "Series 31", "Series 59"),
       col=c("#0066CC", "#4C9900", "#CC0000"), lty=1, cex=0.8,
       title="Time series")
plot(x = group3_1_date, y = group3_1_ts, main = "Original Series 3", 
     xlab = "Date", ylab = "Value", col = "#0066CC", type = "l",
     xlim = as.Date(c(group3_1_date[1],group3_1_date[length(group3_1_date)])) )
plot(x = group3_2_date, y = group3_2_ts, main = "Original Series 31", 
     xlab = "Date", ylab = "Value", col = "#4C9900", type = "l",
     xlim = as.Date(c(group3_2_date[1],group3_2_date[length(group3_2_date)])) )
plot(x = group3_3_date, y = group3_3_ts, main = "Original Series 59", 
     xlab = "Date", ylab = "Value", col = "#CC0000", type = "l",
     xlim = as.Date(c(group3_3_date[1],group3_3_date[length(group3_3_date)])) )
#2.Identify Trend
cma_7 <- cmav(datats, ma = 7, fill = FALSE)
cma_30 <- cmav(datats, ma = 30, fill = FALSE)
cma_90 <- cmav(datats, ma = 90, fill = FALSE)
cma_365 <- cmav(datats, ma = 365, fill = FALSE)
plot(x = datadate, y = datats, col="grey",main = "Centred Moving Average (Series 3)", 
     xlab = "Date", ylab = "Value",type = "l",
     xlim = as.Date(c(datadate[1],datadate[length(datadate)])) )
lines(x= datadate, y = cma_7, col = "red")
lines(x= datadate, y = cma_30, col = "yellow")
lines(x= datadate, y = cma_90, col = "green")
lines(x= datadate, y = cma_365, col = "blue")
legend("topleft", legend=c("cma_7", "cma_30", "cma_90","cma_365"),
       col=c("red","yellow","green","blue"), lty=1, cex = 1,
       title="Length of CMA")
#3.Aggregation (quarterly, monthly, weekly) by sum
data_table_3_1 <- cbind(data.frame(group3_1_date),data.frame(group3_1_ts))
data_table_3_1$year_quarter <- as.yearqtr(group3_1_date,format="%Y-%m-%d")
data_table_3_1$year_month <- as.yearmon(group3_1_date,format="%Y-%m")
data_table_3_1$year_week <- strftime(group3_1_date,"%G-%V") 
data_table_3_1_quarter <- aggregate(group3_1_ts~year_quarter, data_table_3_1, FUN = sum)
data_table_3_1_month <- aggregate(group3_1_ts~year_month, data_table_3_1, FUN = sum)
data_table_3_1_week <- aggregate(group3_1_ts~year_week, data_table_3_1, FUN = sum)
data_table_3_1_quarter$datats <- ts(data_table_3_1_quarter$group3_1_ts,frequency = 4,start = data_table_3_1_quarter$year_quarter[1])
data_table_3_1_month$datats <- ts(data_table_3_1_month$group3_1_ts, frequency = 12,start = data_table_3_1_month$year_month[1])
startdt <- as.numeric(substr(data_table_3_1_week$year_week[1],1,4))
stopdt <- as.numeric(substr(data_table_3_1_week$year_week[1],6,7))
data_table_3_1_week$datats <- ts(data_table_3_1_week$group3_1_ts,frequency = 52,start = c(startdt,stopdt))

data_table_3_2 <- cbind(data.frame(group3_2_date),data.frame(group3_2_ts))
data_table_3_2$year_quarter <- as.yearqtr(group3_2_date,format="%Y-%m-%d")
data_table_3_2$year_month <- as.yearmon(group3_2_date,format="%Y-%m")
data_table_3_2$year_week <- strftime(group3_2_date,"%G-%V") 
data_table_3_2_quarter <- aggregate(group3_2_ts~year_quarter, data_table_3_2, FUN = sum)
data_table_3_2_month <- aggregate(group3_2_ts~year_month, data_table_3_2, FUN = sum)
data_table_3_2_week <- aggregate(group3_2_ts~year_week, data_table_3_2, FUN = sum)
data_table_3_2_quarter$datats <- ts(data_table_3_2_quarter$group3_2_ts,frequency = 4,start = data_table_3_2_quarter$year_quarter[1])
data_table_3_2_month$datats <- ts(data_table_3_2_month$group3_2_ts, frequency = 12,start = data_table_3_2_month$year_month[1])
startdt <- as.numeric(substr(data_table_3_2_week$year_week[1],1,4))
stopdt <- as.numeric(substr(data_table_3_2_week$year_week[1],6,7))
data_table_3_2_week$datats <- ts(data_table_3_2_week$group3_2_ts,frequency = 52,start = c(startdt,stopdt))

data_table_3_3 <- cbind(data.frame(group3_3_date),data.frame(group3_3_ts))
data_table_3_3$year_quarter <- as.yearqtr(group3_3_date,format="%Y-%m-%d")
data_table_3_3$year_month <- as.yearmon(group3_3_date,format="%Y-%m")
data_table_3_3$year_week <- strftime(group3_3_date,"%G-%V") 
data_table_3_3_quarter <- aggregate(group3_3_ts~year_quarter, data_table_3_3, FUN = sum)
data_table_3_3_month <- aggregate(group3_3_ts~year_month, data_table_3_3, FUN = sum)
data_table_3_3_week <- aggregate(group3_3_ts~year_week, data_table_3_3, FUN = sum)
data_table_3_3_quarter$datats <- ts(data_table_3_3_quarter$group3_3_ts,frequency = 4,start = data_table_3_3_quarter$year_quarter[1])
data_table_3_3_month$datats <- ts(data_table_3_3_month$group3_3_ts, frequency = 12,start = data_table_3_3_month$year_month[1])
startdt <- as.numeric(substr(data_table_3_3_week$year_week[1],1,4))
stopdt <- as.numeric(substr(data_table_3_3_week$year_week[1],6,7))
data_table_3_3_week$datats <- ts(data_table_3_3_week$group3_3_ts,frequency = 52,start = c(startdt,stopdt))

###Quarterly###
plot(data_table_3_1_quarter$datats, 
     main = "Time series for Quarterly Data", type = "l",
     xlab = "Date", ylab = "Value", col = "#0066CC", 
     xlim = c(2000,2010),
     ylim = c(min(data_table_3_3_quarter$datats),max(data_table_3_1_quarter$datats)))
lines(x = data_table_3_2_quarter$year_quarter, y = data_table_3_2_quarter$datats, col = "#4C9900")
lines(x = data_table_3_3_quarter$year_quarter, y = data_table_3_3_quarter$datats, col = "#CC0000")
legend("topleft", legend=c("Series 3", "Series 31", "Series 59"),
       col=c("#0066CC", "#4C9900", "#CC0000"),lty=1,cex=0.8,title="Time series")
###Monthly###
plot(data_table_3_1_month$datats, 
     main = "Time series for Monthly Data", type = "l",
     xlab = "Date", ylab = "Value", col = "#0066CC", 
     xlim = c(2000,2010),
     ylim = c(min(data_table_3_3_month$datats),max(data_table_3_1_month$datats)))
lines(x = data_table_3_2_month$year_month, y = data_table_3_2_month$datats, col = "#4C9900")
lines(x = data_table_3_3_month$year_month, y = data_table_3_3_month$datats, col = "#CC0000")
legend("topleft", legend=c("Series 3", "Series 31", "Series 59"),
       col=c("#0066CC", "#4C9900", "#CC0000"),lty=1,cex=0.8,title="Time series")
###Weekly###
plot(data_table_3_1_week$datats,xlab = "Date",ylab = "Value",col = "#0066CC",
     main = "Time series for Weekly Data", type = "l",
     xlim = c(2000,2010),
     ylim = c(min(data_table_3_2_week$datats),max(data_table_3_1_week$datats)))
lines(data_table_3_2_week$datats, col = "#4C9900")
lines(data_table_3_3_week$datats, col = "#CC0000")
legend("topleft", legend=c("Series 3", "Series 31", "Series 59"),
       col=c("#0066CC", "#4C9900", "#CC0000"),lty=1,cex=0.8,title="Time series")

#4.Identify Seasonality --> decide using which time series
###Quarterly###
seas_quarter_3_1 <- seasplot(data_table_3_1_quarter$datats)
seas_quarter_3_2 <- seasplot(data_table_3_2_quarter$datats) 
seas_quarter_3_3 <- seasplot(data_table_3_3_quarter$datats) 
###Monthly###
seas_month_3_1 <- seasplot(data_table_3_1_month$datats)
seas_month_3_2 <- seasplot(data_table_3_2_month$datats) 
seas_month_3_3 <- seasplot(data_table_3_3_month$datats) 
###Weekly###
seas_week_3_1 <- seasplot(data_table_3_1_week$datats)
seas_week_3_2 <- seasplot(data_table_3_2_week$datats) 
seas_week_3_3 <- seasplot(data_table_3_3_week$datats) 

#5.Use decomposition to decide type of model --> decide using which decomposition model
###Monthly###
data_table_3_1_month_seas_add <- decomp(data_table_3_1_month$datats,decomposition = "additive", outplot = TRUE) 
data_table_3_1_month_seas_mul <- decomp(data_table_3_1_month$datats, decomposition = "multiplicative", outplot = TRUE) 
data_table_3_2_month_seas_add <- decomp(data_table_3_2_month$datats,decomposition = "additive", outplot = TRUE) 
data_table_3_2_month_seas_mul <- decomp(data_table_3_2_month$datats, decomposition = "multiplicative", outplot = TRUE) 
data_table_3_3_month_seas_add <- decomp(data_table_3_3_month$datats,decomposition = "additive", outplot = TRUE) 
data_table_3_3_month_seas_mul <- decomp(data_table_3_3_month$datats, decomposition = "multiplicative", outplot = TRUE) 

autoplot(decompose(data_table_3_1_month$datats,type = "additive"))
autoplot(decompose(data_table_3_2_month$datats,type = "additive"))
autoplot(decompose(data_table_3_3_month$datats,type = "additive"))

autoplot(decompose(data_table_3_1_month$datats,type = "multiplicative"))
autoplot(decompose(data_table_3_2_month$datats,type = "multiplicative"))
autoplot(decompose(data_table_3_3_month$datats,type = "multiplicative"))

mean(abs(data_table_3_1_month_seas_add$irregular))
100* mean(abs(data_table_3_1_month_seas_add$irregular)/mean(data_table_3_1_month$datats))
mean(abs(data_table_3_1_month_seas_mul$irregular))
100* mean(abs(data_table_3_1_month_seas_mul$irregular)/mean(data_table_3_1_month$datats))

mean(abs(data_table_3_2_month_seas_add$irregular))
100* mean(abs(data_table_3_2_month_seas_add$irregular)/mean(data_table_3_2_month$datats))
mean(abs(data_table_3_2_month_seas_mul$irregular))
100* mean(abs(data_table_3_2_month_seas_mul$irregular)/mean(data_table_3_2_month$datats))

mean(abs(data_table_3_3_month_seas_add$irregular))
100* mean(abs(data_table_3_3_month_seas_add$irregular)/mean(data_table_3_3_month$datats))
mean(abs(data_table_3_3_month_seas_mul$irregular))
100* mean(abs(data_table_3_3_month_seas_mul$irregular)/mean(data_table_3_3_month$datats))

#6.Statistical Test - Stationary Test
kpss.test(data_table_3_1_month$datats)  #not
adf.test(data_table_3_1_month$datats) #not

kpss.test(data_table_3_2_month$datats)  #not
adf.test(data_table_3_2_month$datats)  #not

kpss.test(data_table_3_3_month$datats) #yes
adf.test(data_table_3_3_month$datats) #not

#7.ACF/PACF Plot
tsdisplay(data_table_3_1_month$datats)
tsdisplay(data_table_3_2_month$datats)
tsdisplay(data_table_3_3_month$datats)

#8.Differencing Data if not stationary
######3_1 (d) = 2
data_table_3_1_month_df1 <- diff(data_table_3_1_month$datats)
kpss.test(data_table_3_1_month_df1) #yes
adf.test(data_table_3_1_month_df1) #not

data_table_3_1_month_df2 <- diff(data_table_3_1_month_df1,lag=12)
kpss.test(data_table_3_1_month_df2) #yes
adf.test(data_table_3_1_month_df2) #yes
######3_2 (d) = 2
data_table_3_2_month_df1 <- diff(data_table_3_2_month$datats)
kpss.test(data_table_3_2_month_df1) #not
adf.test(data_table_3_2_month_df1) #not

data_table_3_2_month_df2 <- diff(data_table_3_2_month_df1)
kpss.test(data_table_3_2_month_df2) #yes
adf.test(data_table_3_2_month_df2) #not

######3_3 (d) = 2
data_table_3_3_month_df1 <- diff(data_table_3_3_month$datats)
kpss.test(data_table_3_3_month_df1) #not
adf.test(data_table_3_3_month_df1) #not

data_table_3_3_month_df2 <- diff(data_table_3_3_month_df1)
kpss.test(data_table_3_3_month_df2) #yes
adf.test(data_table_3_3_month_df2) #yes

#9.Fit ARIMA Model
tsdisplay(data_table_3_1_month_df1)
tsdisplay(data_table_3_2_month_df1)
tsdisplay(data_table_3_3_month_df1)
tsdisplay(data_table_3_1_month_df2)
tsdisplay(data_table_3_2_month_df2)
tsdisplay(data_table_3_3_month_df2)

######Due to the seasonality in ACF/PACF, difference the seasonal data with lag
data_table_3_1_month_df3 <- diff(data_table_3_1_month_df2,lag=12)
tsdisplay(data_table_3_1_month_df3)
data_table_3_2_month_df3 <- diff(data_table_3_2_month_df2,lag=12)
tsdisplay(data_table_3_2_month_df3)
data_table_3_3_month_df3 <- diff(data_table_3_3_month_df2,lag=12)
tsdisplay(data_table_3_3_month_df3)

#9-1 Manual
#Series3
#ARIMA(1,2,0)(0,1,0)[12] AICc=2436.11   BIC=2441.19
fit1 <- Arima(data_table_3_1_month$datats, order=c(1,2,0), seasonal=c(0,1,0)) 
fit1 
Box.test(fit1$residuals) #p-value = 0.6096
tsdisplay(residuals(fit1))
# ARIMA(1,2,0)(3,1,2)[12] AICc=2429.08  BIC=2446.1
fit2 <- Arima(data_table_3_1_month$datats, order=c(1,2,0), seasonal=c(3,1,2)) 
fit2
Box.test(fit2$residuals) #p-value = 0.5774
tsdisplay(residuals(fit2))

#Series31
#ARIMA(0,2,0)(0,1,0)[12] AICc=2342.76   BIC=2345.29
fit1 <- Arima(data_table_3_2_month$datats, order=c(0,2,0), seasonal=c(0,1,0)) 
fit1
Box.test(fit1$residuals) #p-value = 0.237
tsdisplay(residuals(fit1))
# ARIMA(1,2,0)(0,1,0)[12] AICc=2339.61   BIC=2344.63
fit2 <- Arima(data_table_3_2_month$datats, order=c(1,2,0), seasonal=c(0,1,0)) 
fit2
Box.test(fit2$residuals) #p-value = 0.6269
tsdisplay(residuals(fit2))

#Series59
#ARIMA(1,2,0)(0,1,0)[12] AICc=2335.27   BIC=2340.3
fit1 <- Arima(data_table_3_3_month$datats, order=c(1,2,0), seasonal=c(0,1,0)) 
fit1 
Box.test(fit1$residuals) #p-value = 0.3
tsdisplay(residuals(fit1))
# ARIMA(1,1,0)(0,1,0)[12] AICc=2344.06   BIC=2349.1
fit2 <- Arima(data_table_3_3_month$datats, order=c(1,1,0), seasonal=c(0,1,0)) 
fit2
Box.test(fit2$residuals) #p-value = 0.4314
tsdisplay(residuals(fit2))

#9-2 Auto.arima()
auto.arima(data_table_3_1_month$datats) #ARIMA(0,1,0)(1,0,0)[12] BIC=2722.27   AICc=2716.93
auto.arima(data_table_3_2_month$datats) #ARIMA(0,2,1)(1,0,0)[12] BIC=2616.38   AICc=2608.53
auto.arima(data_table_3_3_month$datats) #ARIMA(1,0,0)(1,0,0)[12] BIC=2649.98   AICc=2639.52

Box.test(auto.arima(data_table_3_1_month$datats)$residual) #p-value = 0.8928, it's just white noise
Box.test(auto.arima(data_table_3_2_month$datats)$residual) #p-value = 0.9523, it's just white noise
Box.test(auto.arima(data_table_3_3_month$datats)$residual) #p-value = 0.1902, it's just white noise

auto.ssarima(data_table_3_1_month$datats) #SARIMA(3,0,3)[1](3,0,0)[12] BIC=2782.751   AICc=2755.242
auto.ssarima(data_table_3_2_month$datats) #SARIMA(0,1,3)[1](1,0,3)[12] BIC=2665.655   AICc=2643.051
auto.ssarima(data_table_3_3_month$datats) #SARIMA(0,1,3)[1](1,0,3)[12] BIC=2630.034   AICc=2607.430

Box.test(auto.ssarima(data_table_3_1_month$datats)$residual) #p-value = 0.7157, it's just white noise
Box.test(auto.ssarima(data_table_3_2_month$datats)$residual) #p-value = 0.9261, it's just white noise
Box.test(auto.ssarima(data_table_3_3_month$datats)$residual) #p-value = 0.9847, it's just white noise
