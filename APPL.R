library(fpp)
library(TSA)
library(xts)
library(FinTS)
library(rugarch)
library(rmgarch)
library(Rmpfr)
library(quantmod)
library(DataCombine)
library(tseries)
library(urca)
library(dplyr)
library(fGarch)
APPL = "APPL.csv"
data <- read.csv(APPL)
head(data)
#data$Date <- as.POSIXct(data$Date)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
head(data)
# 计算对数收益率并添加为新列
data <- data %>%
  mutate(log_return = log(Close / lag(Close, default = first(Close)))) 
# 使用first(Price)处理第一天没有前一日价格的情况
#data <- data %>%
#  mutate(log_return = data$Price) 
# 查看结果
head(data)
# 提取起始日期
start_date <- as.Date(min(data$Date))
# 计算总的观测天数
n_days <- nrow(df)
# 转换为ts对象，这里假设数据是每日的，所以frequency为365
log_xts <- xts(data$log_return, order.by = data$Date)
summary(log_xts)
plot(data$Close,pch =16 , col = "RED", cex = 0.2)
plot(data$log_return,pch =16 , col = "RED", cex = 0.2)
plot(log_xts)
#pdf("QQ.pdf",width =5,height = 5)
qqnorm(log_xts,pch =4 , col = "RED", cex = 0.2);qqline(log_xts)
jb_test <- jarque.bera.test(data$Close)
print(jb_test)
adf=adf.test(log_ts)
print(adf)
acf(log_xts)
pacf(log_xts)
trend_lm <- lm(log_ts ~ time(log_ts))
summary(trend_lm)
plot(log_ts, main = "BTC Time Series with Linear Trend", ylab = "Price")
abline(trend_lm, col = "red")
eacf(log_xts)
#log_xts_1 <- na.omit(log_xts)
arima_model <- Arima(log_ts, order = c(2,0,2))
summary(arima_model)
forecast_values <- forecast(arima_model,h=30)
plot(forecast_values)
#lines(data$log_return, col = "red")
residuals <- residuals(arima_model)
# 绘制残差-时间图
plot(residuals, xlab = "Time", ylab = "Residuals", type = "o", pch = 32, col = "blue")
abline(h = 0, col = "red", lty = 2)
qqnorm(residuals)
qqline(residuals)
McLeod.Li.test(y=log_xts)
#正常
m1<-auto.arima(log_xts)
# 拟合GARCH模型
arma_residuals <- residuals(m1)
garch_fit <- garchFit(formula=~garch(1,2),data = arma_residuals^2, trace = FALSE)
summary(garch_fit)
#残差检验
shapiro.test(residuals(garch_fit, standardize = TRUE))
# t分布？
m1<-auto.arima(log_xts)
# 拟合GARCH模型
arma_residuals <- residuals(m1)
garch_fit_t <- garchFit(formula=~garch(1,2),data = arma_residuals^2, trace = FALSE,distribution.model = "std")
summary(garch_fit)
#TGARCH
tgarch_spec <- ugarchspec(variance.model = list(model = "fGARCH", 
                                                submodel = "TGARCH", 
                                                garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(2,2), include.mean = TRUE),
                          distribution.model = "ged")
# 拟合 TGARCH 模型
tgarch_fit <- ugarchfit(spec = tgarch_spec, data = log_xts)

# 显示 TGARCH 模型拟合结果
print(tgarch_fit)
#EGARCH
egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                          distribution.model = "ged")

# 拟合 EGARCH 模型
egarch_fit <- ugarchfit(spec = egarch_spec, data = log_xts)

# 显示 EGARCH 模型拟合结果
print(egarch_fit)
#预测
forecast_values <- ugarchforecast(garch_fit_t,h=10)
































