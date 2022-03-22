library(lubridate)
library(TSstudio)
library(ggplot2)
library(dplyr)


############## EXTRAÇÃO DOS DADOS
source("get_data.R")
df_rs <- oil_data_state()
head(df_rs)
tail(df_rs)


############## PREPARAÇÃO DOS DADOS
df_rs$ANO <- year(df_rs$ANO_MES)
df_rs_anual <- group_by(df_rs, ANO) %>% 
                  summarise(
                          vendas = sum(VENDAS)
                            )

date_min_y <- year(min(df_rs$ANO_MES))
date_min_m <- month(min(df_rs$ANO_MES))
date_min <- c(date_min_y, date_min_m)

date_max_y <- year(max(df_rs$ANO_MES))
date_max_m <- month(max(df_rs$ANO_MES))
date_max <- c(date_max_y, date_max_m)


# SÉRIE ANUAL
diesel_ts_year <- ts(df_rs_anual$vendas, start = date_min_y, end = date_max_y -1, frequency = 1)
ts_info(diesel_ts_year)

# SÉRIE MENSAL
diesel_ts <- ts(df_rs$VENDAS, start = date_min, end = date_max, frequency = 12)
ts_info(diesel_ts)

# SÉRIE MENSAL EM LOGARITMO
diesel_ts_log <- ts(log(df_rs$VENDAS), start = date_min, end = date_max, frequency = 12)
ts_info(diesel_ts_log)

ts_info(diesel_ts)


############## VISUALIZAÇÃO

# SÉRIE ANUAL
ts_plot(diesel_ts_year,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS ANUAIS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")


# SÉRIE MENSAL COM RENDÊNCIA
trend <- rollapply(diesel_ts, width = 12, FUN = mean) #OPÇÃO 1
#trend <- decompose(diesel_ts)$trend # OPÇÃO 2
df <- cbind(diesel_ts, trend)
colnames(df) <- c("VENDAS MENSAIS", "MÉDIA MÓVEL 12 MESES")
ts_plot(df,
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")

# SÉRIE EM LOGARITMO
ts_plot(log(df),
        color = "green",
        Ygrid = TRUE,
        Xgrid = TRUE,
        title = "VENDAS DE ÓLEO DIESEL NO RIO GRANDE DO SUL (METROS CÚBICOS)")


############## ANÁLISE DE SAZONALIDADE

ggseasonplot(diesel_ts, continuous = TRUE) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

ggsubseriesplot(diesel_ts) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL")

# SÉRIE DIFERENCIADA
diesel_ts_diff <- diff(diesel_ts) # cria série diferenciada
ggseasonplot(diesel_ts_diff) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

ggsubseriesplot(diesel_ts_diff) +
  theme_bw() +
  scale_y_continuous(labels = scales::number) +
  labs(title = "SAZONALIDADE DAS VENDAS DE ÓLEO DIESEL (SÉRIE DIFERENCIADA")

# SIGNIFICÂNCIA DA SAZONALIDADE MENSAL
ts_seasonal(diesel_ts, type = "box")
reg_seas <- tslm(diesel_ts ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas)

ts_seasonal(diesel_ts_diff, type = "box")
reg_seas <- tslm(diesel_ts_diff ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas)

ts_seasonal(diesel_ts_log, type = "box")
reg_seas <- tslm(diesel_ts_log ~ season) # MESES SÃO SIGNIFICATIVOS?
summary(reg_seas)
