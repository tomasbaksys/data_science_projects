library(fpp2)
library(readr)
library(ggplot2)
library(strucchange)
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)
library(TSA)
df <- read_csv("C:/Users/Tomas/OneDrive/Desktop/2nd year/2nd semester/Forecasting/final_project/final_forecast.csv")
summary(df)
print(df)
# Basic Statistical Properties
summary(room)
summary(living_cost)
boxplot(room, main='Boxplot of Room Price')
# Time Series Plot
room <- ts(df$`Room price`, start=c(2006,1),frequency = 12)
plot.ts(room, xlab="Time", ylab="Room price", main='Room price in Finland')
# Decomposition
properties<-stl(room, s.window="period")
plot(properties)
plot(properties$time.series[, "seasonal"], main='Seasonal Pattern of Room Price in Finland', ylab='Seasonal Component of Decomposition')
plot(decompose(room))
plot(decompose(room, type="multiplicative"))
adf.test(properties$time.series[,"trend"])
adf.test(properties$time.series[,"seasonal"])
adf.test(properties$time.series[,"remainder"])
# Stationarity and normality
adf.test(room)
adf.test(living_cost)
log_living_cost<-log(living_cost)
adf.test(log_living_cost)
diff_living_cost <-diff(log_living_cost)
adf.test(diff_living_cost)
sec_diff_living_cost<-diff(diff_living_cost)
adf.test(sec_diff_living_cost)
shapiro.test(sec_diff_living_cost)
hist(sec_diff_living_cost)
shapiro.test(room)
diff_room <-diff(room)
shapiro.test(diff_room)
log_room <-log(room)
shapiro.test(log_room)
log_log_room<-log(log_room)
shapiro.test(log_log_room)
diff_room<-diff(log_log_room)
sec_diff_room<-diff(diff_room)
shapiro.test(sec_diff_room)
hist(room)
hist(sec_diff_living_cost)
#plot(x=log_log_room, y=sec_diff_living_cost)
#cor.test(x=log_log_room, y=sec_diff_living_cost)

plot(sec_diff_room, sec_diff_living_cost)
adf.test(sec_diff_room)
adf.test(sec_diff_living_cost)
shapiro.test(sec_diff_living_cost)
shapiro.test(sec_diff_room)

# Dependency on external variables
cor(df$`Room price`, df$`Cost of living`, method='spearman')
cor(df$`Room price`, df$`Turnover of trade`, method='spearman')
cor(df$`Room price`, df$`Producer price index`, method='spearman')
cor(df$`Room price`, df$`New orders in manufacturing`, method='spearman')
cor(df$`Room price`, df$`Unemployed job seekers`, method='spearman')
cor(df$`Room price`, df$`Interest rates`, method='spearman')

model <- lm(`Room price` ~`Cost of living`+`Turnover of trade`+`Producer price index`+`Producer price index`+`New orders in manufacturing`+`Turnover in construction (Positive/Negative)`+`Turnover in construction (Positive/Negative)`+`Unemployed job seekers`+`Interest rates`, data = df)
summary(model)

# Seasonal Naive Forecast
train <- window(room, end=c(2021, 8))
test <- window(room, start=c(2021, 9))
fit1<-snaive(train, h=20)
plot(fit1, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit1, test)
# Random Walk with Drift
fit2<- rwf(train, drift=TRUE, h=20)
plot(fit2, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit2, test)
# Forecast with decomposition
decomp<-stl(train, s.window="period")
fit3<-forecast(decomp, h=20)
plot(fit3, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit3, test)
# SES forecasting
alpha <-seq(0.01, 1, by=0.01)
RMSE<-NA
for(i in seq_along(alpha)) {
  fit<-ses(train, h=20, initial="simple", alpha=alpha[i])
  RMSE[i]<-accuracy(fit, test)[2,2]
}
alpha.fit<-data_frame(alpha, RMSE)
alpha.min<-filter(alpha.fit, RMSE==min(RMSE))
ggplot(alpha.fit, aes(alpha, RMSE))+
  geom_line()+
  geom_point(data=alpha.min, aes(alpha, RMSE), size=2, color="blue")+
  labs(title = "Comparison of RMSE by Alpha")+
  theme(plot.title = element_text(hjust = 0.5))
alpha.min
fit4<-ses(train, h=20, initial="simple", alpha=.72)
plot(fit4, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit4, test)
#HES forecasting
beta<- seq(0.0001, 1, by=0.001)
RMSE <-NA
for(i in seq_along(beta)) {
  fit <-holt(train, beta=beta[i], h=20)
  RMSE[i] <-accuracy(fit, test)[2,2]
}
beta.fit<-data_frame(beta, RMSE)
beta.min<-filter(beta.fit, RMSE==min(RMSE))
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line()+
  geom_point(data=beta.min, aes(beta, RMSE), size=2, color="blue")+
  labs(title = "Comparison of RMSE by Beta")+
  theme(plot.title = element_text(hjust = 0.5))
beta.min
fit5<-holt(train, h=20, beta=0.569)
plot(fit5, ylim=c(50, 150), PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit5, test)

#HWES forecasting
alpha <-seq(0.01, .5, by=0.01)
RMSE<-NA
for(i in seq_along(alpha)) {
  fit<-hw(train, h=20, alpha=alpha[i], seasonal="additive")
  RMSE[i]<-accuracy(fit, test)[2,2]
}
alpha.fit<-data_frame(alpha, RMSE)
alpha.min<-filter(alpha.fit, RMSE==min(RMSE))
ggplot(alpha.fit, aes(alpha, RMSE))+
  geom_line()+
  geom_point(data=alpha.min, aes(alpha, RMSE), size=2, color="blue")
alpha.min

beta<- seq(0.0001, .2, by=0.001)
RMSE <-NA
for(i in seq_along(beta)) {
  fit <-hw(train, beta=beta[i], h=20, seasonal="additive")
  RMSE[i] <-accuracy(fit, test)[2,2]
}
beta.fit<-data_frame(beta, RMSE)
beta.min<-filter(beta.fit, RMSE==min(RMSE))
ggplot(beta.fit, aes(beta, RMSE)) +
  geom_line()+
  geom_point(data=beta.min, aes(beta, RMSE), size=2, color="blue")
beta.min
gamma <- seq(0.01, 0.5, by = 0.01)
RMSE <- numeric(length(gamma))  

for(i in seq_along(gamma)) {
  fit <- hw(train, h = 20, seasonal = "additive", gamma = gamma[i])
  RMSE[i] <- accuracy(fit, test)[2, "RMSE"]  
}

gamma.fit <- tibble(gamma = gamma, RMSE = RMSE)

gamma.min <- filter(gamma.fit, RMSE == min(RMSE))
p <- ggplot(gamma.fit, aes(x = gamma, y = RMSE)) +
  geom_line() +
  geom_point(data = gamma.min, aes(x = gamma, y = RMSE), size = 2, color = "blue") +
  labs(title = "Gamma vs. RMSE", x = "Gamma", y = "RMSE")
print(p)
print(gamma.min)

fit6<-hw(train, beta=0.158, h=20, seasonal="additive")
accuracy(fit6, test)
plot(fit6, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
forecast<-hw(room, beta=0.158, h=20, seasonal="additive")
plot(forecast, PI=FALSE)
lines(room)
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)


# ARIMAX
adf.test(room)
acf(room, lag.max=70)
pacf(room, lag.max=70)
pacf(room)
arima<-auto.arima(train, trace = TRUE, seasonal = TRUE)
summary(arima)
checkresiduals(arima)
fit7<-forecast(arima, h=20)
accuracy(fit7,test)
plot(fit7, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
# Creating time series independent variables
cost_liv <- ts(df$`Cost of living`, start=c(2006,1),frequency = 12)
trade <- ts(df$`Turnover of trade`, start=c(2006,1),frequency = 12)
ppi <- ts(df$`Producer price index`, start=c(2006,1),frequency = 12)
manu <- ts(df$`New orders in manufacturing`, start=c(2006,1),frequency = 12)
unem <- ts(df$`Unemployed job seekers`, start=c(2006,1),frequency = 12)
ir <- ts(df$`Interest rates`, start=c(2006,1),frequency = 12)
# Creating training set of independent variables
xreg_train <- cbind(
  window(cost_liv, end=c(2021, 8)),
  window(trade, end=c(2021, 8)),
  window(ppi, end=c(2021, 8)),
  window(manu, end=c(2021, 8)),
  window(unem, end=c(2021, 8)),
  window(ir, end=c(2021, 8))
)
#Creating test set of independent variables
xreg_test <- cbind(
  window(cost_liv, start=c(2021, 9),end=c(2023, 4)),
  window(trade, start=c(2021, 9),end=c(2023, 4)),
  window(ppi, start=c(2021, 9),end=c(2023, 4)),
  window(manu, start=c(2021, 9),end=c(2023, 4)),
  window(unem, start=c(2021, 9),end=c(2023, 4)),
  window(ir, start=c(2021, 9),end=c(2023, 4))
)
colnames(xreg_train) <- c("cost_liv", "trade", "ppi", "manu", "unem", "ir")
colnames(xreg_test) <- c("cost_liv", "trade", "ppi", "manu", "unem", "ir")
# Running ARIMAX model with the best ARIMA model
arimax_model <- Arima(train, order=c(1,0,0), seasonal=list(order=c(0,1,1)), xreg=xreg_train)
summary(arimax_model)
checkresiduals(arimax_model)
# Running ARIMAX with auto arima
arimax<-auto.arima(train, trace = TRUE, seasonal = TRUE, xreg=xreg_train)
summary(arimax)
checkresiduals(arimax)
fit9<-forecast(arimax, h=20, xreg=xreg_test)
plot(fit9, PI=FALSE)
lines(test, col="red")
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)
accuracy(fit9, test)


# Our own approach
# HES + Decomposition mix
holtforecast <- holt(train, h=20, beta=0.569) 
decomp <- stl(train, s.window = "periodic") 
decompforecast <- forecast(decomp, h=20) 
mixedforecast <- (holtforecast$mean + decompforecast$mean) / 2 
plot(room, main='Holt exponential smoothing and decomposition forecast mix')
lines(mixedforecast, col='red')
lines(test, col='blue')
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)

# HWES + Seasonal Naive mix
hwforecast <- holt(train, h=20, beta=0.158, seasonal='additive')
snaiveforecast <- snaive(train, h=20)
mixedforecast <-(hwforecast$mean + snaiveforecast$mean) / 2
plot(room, main='Holt Winters exponential smoothing and seasonal naive')
lines(mixedforecast, col='red')
lines(test, col='blue')
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)

# Seasonal Naive + Holt mix
snaiveforecast <- snaive(train, h=20)
holtforecast <- holt(train, h=20, beta=0.569, alpha=0.72) 
mixedforecast <-(snaiveforecast$mean + holtforecast$mean) / 2
plot(room, main='Holt exponential smoothing and seasonal naive')
lines(mixedforecast, col='red')
lines(test, col='blue')
legend("topleft", legend = c("Forecasted", "Actual"), col = c("blue", "red"), lty = 1)

# Holt with residuals forecasted with naive seasonal
snaivef <- snaive(holtforecast$residuals, h=20)
mixedforecast <-(snaivef$mean + holtforecast$mean)
plot(room, main='Holt with residuals forecasted using seasonal naive')
lines(mixedforecast, col='red')
lines(test, col='blue')
legend("topleft", legend = c("Forecasted", "Actual"), col = c("red", "blue"), lty = 1)
accuracy(mixedforecast, test)