# Train data: Time series # analyse log(sales) as there is normal assumption in error


day_grp = cleaned_data %>% arrange(Order.Date) %>% group_by(Order.Date) %>%
  summarise(daily_avg = mean(Sales), percentile_95 = quantile(Sales, 0.95), max_sale = max(Sales))
day_grp 

summary(day_grp$daily_avg)

ggplot(day_grp, aes(x= Order.Date, y = daily_avg)) + geom_line() +
  xlab("") + labs(y = "Average Sale", title = "Daily average Sales")

day_data_org = cleaned_data %>% arrange(Order.Date) %>% group_by(Order.Date) %>%
  summarise(daily_avg = mean(log_sales)) # log_sales
no_row = nrow(day_data_org)
no_row
no = 50
day_data = day_data_org[1:(no_row - no), ]
dim(day_data)

ggplot(day_data, aes(x= Order.Date, y = daily_avg)) + geom_line() +
  xlab("") + labs(y = "Average log Sale", title = "Daily average of log of Sales")


ts_data = ts(day_data$daily_avg, start = min(Order.Date), end = max(Order.Date), frequency = 30) 
decom_ts = decompose(ts_data)
plot(decom_ts)

y = day_data$daily_avg

MannKendall(y) # Fail to reject H0 =>  there is no monotonic trend in the series


kruskal.test(log_sales ~ mon, data = cleaned_data)
# fail to reject =>  null that the location parameters of the distribution of x are the same in each group (sample)

auto.arima(y) # ARIMA(1,0,2) with non-zero mean 
ts_fit = arima(y, order = c(1, 0 ,2))
AIC(ts_fit)
x11()
tsdiag(ts_fit)

ts_pred = fitted(ts_fit) ##predicted value
ts_grp = cbind(y,ts_pred)
ts_grp

par(mfrow = c(2,2))
plot(day_data$Order.Date, resid(ts_fit), main="Residual of ARIMA", ylab="Residual", xlab = "Time")
abline(h = 0, col = "red")
qqnorm(resid(ts_fit), main = "QQ plot of Residual")
acf(resid(ts_fit), main="Correlogram (ACF)")
pacf(resid(ts_fit), main="Partial Correlogram (PACF)")


# Ljung-Box test
Box.test(resid(ts_fit), lag=20, type = 'Ljung-Box')
# H0: The data are independently distributed vs H1:  exhibit serial correlation.
# fail to reject H0
# No need to go for Arch or garch

s = sd(resid(ts_fit))
q = qnorm(0.975)
cont_up = ts_pred + s * q
cont_lo = ts_pred - s * q

dev.off()
plot(day_data$Order.Date, day_data$daily_avg, main = "Fitted value with CI",
     xlab = "Time", ylab = "Log(Sales)")
lines(day_data$Order.Date, ts_pred, col = "blue")
lines(day_data$Order.Date, cont_up, col = "red")
lines(day_data$Order.Date, cont_lo, col = "red") 
legend("top", legend = c("Fitted", "95% CI"), col = c("blue", "red"), cex = 0.6, lty = 1)


ts_forecast = forecast(ts_data, h = no)
ts_forecast = as.data.frame(ts_forecast)

test_data = day_data_org[(no_row - no +1):no_row, ]
dim(test_data)
y_test = as.vector(test_data$daily_avg)

ts_rmse = sqrt(mean((exp(y_test) - exp(ts_forecast[,1]))^2))
ts_rmse

plot(test_data$Order.Date, ts_forecast[,1], main = "Forecast with CI", type = "l", ylab = "Log(Sales)", xlab = "Time",
      ylim = c(min(ts_forecast[,4]), max(ts_forecast[,5])))
#lines(test_data$Order.Date,  ts_forecast[,2], col = "red", lty = 2)
#lines(test_data$Order.Date,  ts_forecast[,3], col = "red", lty = 2)
lines(test_data$Order.Date,  ts_forecast[,4], col = "blue", lty = 3)
lines(test_data$Order.Date,  ts_forecast[,5], col = "blue", lty = 3)
lines(test_data$Order.Date, y_test, col = "green", lty = 4)

legend("topleft", legend = c("Actual", "95% CI"), lty = c(4, 3), col = c("green", "blue"), cex = 0.5 )

cbind(ts_forecast[,1], y_test)

