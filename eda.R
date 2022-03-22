library(lubridate)
library(TSstudio)
library(ggplot2)



# DADOS
source("get_data.R")
df_rs <- oil_data_state()

head(df_rs)
tail(df_rs)

date_min_y <- year(min(df_rs$ANO_MES))
date_min_m <- month(min(df_rs$ANO_MES))
date_min <- c(date_min_y, date_min_m)

date_max_y <- year(max(df_rs$ANO_MES))
date_max_m <- month(max(df_rs$ANO_MES))
date_max <- c(date_max_y, date_max_m)

diesel_ts <- ts(df_rs$VENDAS, start = date_min, end = date_max, frequency = 12)
diesel_ts_log <- ts(log(df_rs$VENDAS), start = date_min, end = date_max, frequency = 12)
ts_info(diesel_ts)

ts_plot(diesel_ts,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

ts_plot(log(diesel_ts),
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

trend <- rollapply(diesel_ts, width = 12, FUN = mean)
df <- cbind(diesel_ts, trend)
colnames(df) <- c("VENDAS", "MÉDIA MÓVEL 12 MESES")
ts_plot(df,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

ts_plot(log(df),
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

dec <- decompose(diesel_ts)
plot(dec)

### ANÁLISE DE SAZONALIDADE

ggseasonplot(diesel_ts, continuous = TRUE) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

ggsubseriesplot(diesel_ts) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

diesel_ts_diff <- diff(diesel_ts) # cria série diferenciada

ggseasonplot(diesel_ts_diff) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

ggsubseriesplot(diesel_ts_diff) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

ts_seasonal(diesel_ts, type = "box")
ts_seasonal(diesel_ts_diff, type = "box")

reg_seas <- tslm(diesel_ts_log ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas)
