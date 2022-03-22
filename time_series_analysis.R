library(forecast)
library(lubridate)
library(TSstudio)
library(ggplot2)
library(scales)
library(zoo)
library(strucchange)
library(tseries)
library(plotly)

source("get_data.R")
df_rs <- oil_data_state() 

### OBJETO TIME SERIES

date_min_y <- year(min(df_rs$ANO_MES))
date_min_m <- month(min(df_rs$ANO_MES))
date_min <- c(date_min_y, date_min_m)

date_max_y <- year(max(df_rs$ANO_MES))
date_max_m <- month(max(df_rs$ANO_MES))
date_max <- c(date_max_y, date_max_m)

diesel_ts <- ts(df_rs$VENDAS, start = date_min, end = date_max, frequency = 12)
diesel_ts_log <- ts(log(df_rs$VENDAS), start = date_min, end = date_max, frequency = 12)
ts_info(diesel_ts)


######### MODELOS #########

# SPLIT
#n_train <- floor(length(diesel_ts)*0.2)
diesel_split <- ts_split(diesel_ts, sample.out = 12)
diesel_train <- diesel_split$train
diesel_test <- diesel_split$test

diesel_log_split <- ts_split(diesel_ts_log, sample.out = 12)
diesel_log_train <- diesel_log_split$train
diesel_log_test <- diesel_log_split$test

## 1 - SAZONAL COM TENDÊNCIA SIMPLES

# Série normal
reg_seas_tren <- tslm(diesel_train ~ season + trend) # MESES SÃO SIGNIFICATIVOS?
AIC(reg_seas_tren)
BIC(reg_seas_tren)
reg1_fc <- forecast(reg_seas_tren, h = 12)
accuracy(reg1_fc, diesel_test)

plot_forecast(reg1_fc)
test_forecast(actual = diesel_ts,
              forecast.obj = reg1_fc,
              test = diesel_test)

# 2 - SAZONAL COM TENDÊNCIA SIMPLES EM SÉRIE COM LOGARITMO
reg_seas_tren <- tslm(diesel_log_train ~ season + trend)
AIC(reg_seas_tren)
BIC(reg_seas_tren)
reg2_fc <- forecast(reg_seas_tren, h = 12)
accuracy(reg2_fc, diesel_log_test)

plot_forecast(reg1_fc)
test_forecast(actual = diesel_ts_log,
              forecast.obj = reg1_fc,
              test = diesel_log_test)

# 3 - ARIMA (6, 1, 3)
ggAcf(diesel_train)
ggPacf(diesel_train)
adf.test(diesel_train) # presença de raiz unitária

# série diferenciada
diesel_train_diff <- diff(diesel_train)

ggAcf(diesel_train_diff)
ggPacf(diesel_train_diff)
adf.test(diesel_ts_diff) # ausência de raiz unitária

arima1 <- Arima(diesel_train, order = c(6, 1, 3), method = "ML")
arima1$aic
arima1$bic
checkresiduals(arima1)
arima1_fc <- forecast(arima1, h = 12)
accuracy(arima1_fc, diesel_test)

plot_forecast(arima1_fc)
test_forecast(actual = diesel_ts,
              forecast.obj = arima1_fc,
              test = diesel_test)

# 4 - SARIMA (6, 1, 3) (0, 0, 1)
arima2 <- Arima(diesel_train, order = c(6, 1, 3), seasonal = list(order = c(0, 0, 1)), method = "ML")
arima2$aic
arima2$bic
checkresiduals(arima2)
arima2_fc <- forecast(arima2, h = 12)
accuracy(arima2_fc, diesel_test)

plot_forecast(arima2_fc)
test_forecast(actual = diesel_ts,
              forecast.obj = arima2_fc,
              test = diesel_test)

# 5 - SARIMA (5, 1, 1) (1, 1, 1) SÉRIE APARENTA DECAIMENTO SAZONAL
ggAcf(diesel_train_diff, lag.max = 48)

diesel_diff_seas <- diff(diesel_train_diff, 12) # diferença sazonal
ggAcf(diesel_diff_seas, lag.max = 48)
ggPacf(diesel_diff_seas, lag.max = 48)

arima3 <- Arima(diesel_train, order = c(5, 1, 1), seasonal = list(order = c(1, 1, 1)), method = "ML")
arima3$aic
arima3$bic
checkresiduals(arima3)
arima3_fc <- forecast(arima3, h = 12)
accuracy(arima3_fc, diesel_test)

plot_forecast(arima3_fc)
test_forecast(actual = diesel_ts,
              forecast.obj = arima3_fc,
              test = diesel_test)

# 6: MODELO 5 COM TRANSFORMAÇÃO LOGARITMICA

diesel_log_train
diesel_log_train_diff <- diff(diesel_log_train)
ggAcf(diesel_log_train)
ggPacf(diesel_log_train)

ggAcf(diesel_log_train_diff, lag.max = 48)
ggPacf(diesel_log_train_diff, lag.max = 48)

ggAcf(diff(diesel_log_train_diff,12), lag.max = 48)
ggPacf(diff(diesel_log_train_diff,12), lag.max = 48)

arima4 <- Arima(diesel_log_train, order = c(5, 1, 2), seasonal = list(order = c(1, 1, 1)), method = "ML")
arima4$aic
arima4$bic
checkresiduals(arima4)
arima4_fc <- forecast(arima4, h = 12)
accuracy(arima4_fc, diesel_log_test)
accuracy(exp(arima4_fc$mean), diesel_test) # valores transformados


plot_forecast(arima4_fc)
test_forecast(actual = diesel_ts_log,
              forecast.obj = arima4_fc,
              test = diesel_log_test
              ) %>%
  layout(title = "Vendas de óleo diesel - Observado vs Estimado e Projetado",
         yaxis = list(title = "log de m³"))


#comparando modelos 5 e 6
#arima3$aic NÃO SE COMPARAM MODELOS COM AIC E BIC QUANDO DIFEREM NOS DADOS (NORMAL VS. LOG)
#arima3$bic
#arima4$aic
#arima4$bic

accuracy(arima3_fc, diesel_test)
accuracy(arima4_fc, diesel_log_test)
accuracy(exp(arima4_fc$mean), diesel_test) # com transormação logaritmica é melhor


# 7 - HOLT WINTERS
shallow_grid <- ts_grid(diesel_ts,
                      model = "HoltWinters",
                      periods = 6,
                      window_space = 6,
                      window_test = 12,
                      hyper_params = list(alpha = seq(0,1,0.1),
                                          beta = seq(0,1,0.1),
                                          gamma = seq(0,1,0.1)),
                      parallel = TRUE,
                      n.cores = 8)
plot_grid(shallow_grid) # alfa: entre 0,1 e 0,5; beta: entre 0 e 0,1; gama: entre 0,1 e 0,3
plot_grid(shallow_grid, type = "3D", top = 250)

 
deep_grid <- ts_grid(diesel_ts,
                   model = "HoltWinters",
                   periods = 6,
                   window_space = 6,
                   window_test = 12,
                   hyper_params = list(alpha = seq(0.1,0.5,0.01),
                                       beta = seq(0,0.1,0.01),
                                       gamma = seq(0.1,0.3,0.01)),
                    )
plot_grid(deep_grid)
plot_grid(deep_grid, type = "3D", top = 250)
 
md_hw_grid <- HoltWinters(diesel_ts,
                         alpha = deep_grid$alpha,
                         beta = deep_grid$beta,
                         gamma = deep_grid$gamma)

accuracy(md_hw_grid$fitted, diesel_ts)

plot(diesel_ts)
lines(md_hw_grid$fitted[,1], col = "red")
plot(md_hw_grid$fitted[,2]) # nível
plot(md_hw_grid$fitted[,3]) # tendência
plot(md_hw_grid$fitted[,4]) # sazonalidade

# PROJEÇÃO COM MODELO 6 (LOG SARIMA)
arima4$coef
arima4_fc$method

arima_final <- Arima(log(diesel_ts), order = c(5, 1, 2), seasonal = list(order = c(1, 1, 1)), method = "ML")
arima_final$coef

ts_info(diesel_ts)

arima_fc <- forecast(arima_final, h = 15)

# projeções

# 2021 (meses faltantes)
p1 <- window(diesel_ts, start = c(2021,1))
p2 <- window(exp(arima_fc$mean), end = c(2021, 12))
vendas_2021 <- c(p1,p2)
sum(vendas_2021)
# 2022
proj_2022 <- window(exp(arima_fc$mean), start = c(2022, 1))
sum(proj_2022) # 3.866.497 METROS CÚBICOS DE ÓLEO DIESEL NO RS EM 2022
