# série em logaritmo
diesel_log_train
diesel_log_test


# 1
arima1 <- Arima(diesel_log_train, order = c(6, 1, 3), method = "ML")
arima2 <- Arima(diesel_log_train, order = c(6, 1, 3), seasonal = list(order = c(0, 0, 1)), method = "ML")
arima3 <- Arima(diesel_log_train, order = c(5, 1, 2), seasonal = list(order = c(1, 1, 1)), method = "ML")

arima1_fc <- forecast(arima1, h = 12)
arima2_fc <- forecast(arima2, h = 12)
arima3_fc <- forecast(arima3, h = 12)

arima1_fc$method
arima2_fc$method
arima3_fc$method

arima1_fc$model
arima2_fc$model
arima3_fc$model


accuracy(arima1_fc, diesel_log_test)
accuracy(arima2_fc, diesel_log_test)
accuracy(arima3_fc, diesel_log_test)


m1 <- accuracy(arima1_fc, diesel_log_test)
m2 <- accuracy(arima2_fc, diesel_log_test)
m3 <- accuracy(arima3_fc, diesel_log_test)

comp <- cbind(m1[2,], m2[2,], m3[2,])

comp
