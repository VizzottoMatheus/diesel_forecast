library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(scales)
library(e1071)
library(gridExtra)

model_str <- function(model) {
  arma <- model$arma
  model_str <- paste0("ARIMA (", arma[1], ",", arma[6], ",", arma[2], ")", "(", arma[3], ",", arma[7], ",", arma[4], ")")
  return(model_str)
}

############### DADOS DE VENDA DE DIESEL EM DE 1990 A 2020  #################

# Fonte: https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/vendas-de-derivados-de-petroleo-e-biocombustiveis



diesel_19902020 <- read.csv2("C:/Users/matheus.vizzotto/Desktop/Faculdade/Econometria Aplicada/Trabalho/Dados/vendas-oleo-diesel-m3-1990-2020.csv", encoding = "UTF-8")
diesel_rs_19902020 <- diesel_19902020[-19] %>%
  subset(ESTADO == "RIO GRANDE DO SUL" | ESTADO == " RIO GRANDE DO SUL ") %>%
  select(c("ANO", "ESTADO", "JANEIRO", "FEVEREIRO", "MARÇO", "ABRIL", "MAIO",
           "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO",
           "DEZEMBRO")) %>%
  gather(key="month", value = "m³ vendidos", -c("ANO","ESTADO"))

############### DADOS DE VENDA DE DIESEL EM 2021  ###########################

diesel_2021 <- read.csv2("C:/Users/matheus.vizzotto/Desktop/Faculdade/Econometria Aplicada/Trabalho/Dados/vendas-oleo-diesel-m3-2021.csv", encoding = "UTF-8")
diesel_rs_2021 <- diesel_2021 %>%
  subset(ESTADO == "RIO GRANDE DO SUL" | ESTADO == " RIO GRANDE DO SUL ") %>%
  select(c("ANO", "ESTADO", "Jan", "Fev")) %>%
  gather(key="month", value = "m³ vendidos", -c("ANO","ESTADO"))

################ CONCATENANDO PERÍODOS #####################################

diesel_rs_19902021 <- rbind(diesel_rs_19902020, diesel_rs_2021)
#diesel_rs_19902021 <- "RIO GRANDE DO SUL"

################ WRANGLING ################################################

diesel_rs_19902021$Data <- paste0("01/",diesel_rs_19902021$month, "/",diesel_rs_19902021$ANO)


diesel_rs_19902021$Data <- gsub("JANEIRO", "01", diesel_rs_19902021$Data) %>%
  gsub("Jan", "01", .) %>%
  gsub("FEVEREIRO", "02", .) %>%
  gsub("Fev", "02", .) %>%
  gsub("MARÇO", "03", .) %>%
  gsub("ABRIL", "04", .) %>%
  gsub("MAIO", "05", .) %>%
  gsub("JUNHO", "06", .) %>%
  gsub("JULHO", "07", .) %>%
  gsub("AGOSTO", "08", .) %>%
  gsub("SETEMBRO", "09", .) %>%
  gsub("OUTUBRO", "10", .) %>%
  gsub("NOVEMBRO", "11", .) %>%
  gsub("DEZEMBRO", "12", .)

diesel_rs_19902021$Data <- as.Date(diesel_rs_19902021$Data, format = "%d/%m/%Y")

diesel_rs_19902021 <- arrange(diesel_rs_19902021, Data)


##############################################################################
###############         MODELAGEM     ########################################
##############################################################################


##### transforma dados em série temporal ###########

diesel <- select(diesel_rs_19902021, Data, `m³ vendidos`)

diesel_ts <- ts(diesel$`m³ vendidos`, start = c(1990,1), end = c(2021, 2), freq = 12)

autoplot(diesel_ts) + ggtitle("Vendas de óleo diesel no RS (m³)")        # série original
autoplot(diff(diesel_ts))   # série estacionária

###### análise de componentes ############

dec <- decompose(diesel_ts)
plot(dec)

#### sazonalidade

ggseasonplot(diesel_ts)

boxplot(diesel_ts ~ cycle(diesel_ts))

ggsubseriesplot(diesel_ts)

ggsubseriesplot(diff(diesel_ts))


###########################################################################
### AGRICULTURA ESTÁ RELACIONADA COM SAZONALIDADE DAS VENDAS DE DIESEL? ###
###########################################################################

df <- read_excel("C:/Users/matheus.vizzotto/Desktop/Faculdade/Econometria Aplicada/Trabalho/Dados/regressao/tabela5457.xlsx",
                 sheet = "AP")


########### DISTRIBUIÇÃO ENTRE OS ANOS ############


df_dist <- select(df, "Município", "Ano", "Total", "Diesel_l")
df_dist <- pivot_longer(df_dist, c("Total", "Diesel_l"), "Categoria")
df_dist <- drop_na(df_dist, "value")

ggplot(df_dist, aes(x=value, fill = as.factor(Ano))) + geom_density(alpha = .3) + 
  facet_wrap(. ~ Categoria, ncol = 2, scales = "free") + theme_bw() +
  scale_x_continuous(label = scales::number) + theme(axis.text.y = element_blank(),
                                                     axis.ticks.y = element_blank())
e1071::skewness(drop_na(df)$Total)
e1071::skewness(drop_na(df)$Diesel_l)

# VENDAS DE DIESEL E ÁREA PLANTADA TOTAL COM ASSIMETRIA (curtose > 0) -- NECESSÁRIA TRANFORMAÇÃO EM LOG


ggplot(df_dist, aes(y=value, x = as.factor(Ano), color = as.factor(Ano))) + 
  geom_boxplot(outlier.size = 2) + 
  facet_wrap(. ~ Categoria, ncol = 2, scales = "free") + theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# CANOAS TEM VENDAS ELEVADAS EM DECORRÊNCIA DA REFINARIA -- OUTLIER 

df2 <- subset(df, Município != "Canoas (RS)")  
ggplot(df2, aes(log(Soja), log(Diesel_l), color = as.factor(Ano), alpha = 0.8)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "black")


# REGRESSÃO
df2_2019 <- subset(df2, Ano == 2019)
diesel_total2 <- lm(log(df2_2019$Diesel_l) ~ log(df2_2019$Total))
summary(diesel_total2)
#stargazer(diesel_total2)
sink("C:/Users/matheus.vizzotto/Downloads/teste.txt")
print(summary(diesel_total2))
sink()


df2_2018 <- subset(df2, Ano == 2018)
diesel_total2 <- lm(log(df2_2018$Diesel_l) ~ (df2_2018$Total))
summary(diesel_total2)

# 2018 e 2019 se mostram ambos significativos


#########################################################################
# VOLUME DE DIESEL ESTÁ RELACIONADO COM A ÁREA PLANTADA NO MUNICÍPIO ####
#########################################################################



###### DE VOLTA À SÉRIE TEMPORAL

####### FORECASTING COM AUTOARIMA   ###############

#aa <- auto.arima(diesel_ts, stepwise = FALSE, approximation = FALSE, trace = TRUE) # stepwise = FALSE: testa todas opções
# approximation = FALSE: não arredonda critérios
# trace: retorna modelos sendo executados

#fc <- forecast(aa, h = 60)  # prevê 60 períodos à frente
#autoplot(fc, include = 120)  # inclui apenas últimos 120 períodos no gráfico

#autoplot(diesel_ts, linetype = "dashed") + geom_line(y = aa$fitted, color = "red") + 
#  labs(title = "Vendas de óleo diesel no RS (m³)", subtitle = model_str(aa))


#### IDENTIFICAÇÃO


ggAcf(diesel_ts)    # FAC indica presença de tendência --> necessário diferenciar a série
ggPacf(diesel_ts)

# diferenciação                                                      # I = 1

acf_dif <- ggAcf(diff(diesel_ts), 48) + labs(title = element_blank())    # Q = 3
pacf_dif <- ggPacf(diff(diesel_ts), 48) + labs(title = element_blank())  # P = 6
grid.arrange(acf_dif, pacf_dif)

# Decaimento no componente sazonal --> diferenciação sazonal
# SAZONALIDADE FAC E FACP  (VER MATERIAL SABINO)
# https://medium.com/@kfoofw/seasonal-lags-sarima-model-fa671a858729


ggAcf(diff(diff(diesel_ts), 12))




#### MODELO

fit <- Arima(diesel_ts, order = c(6, 1, 3), seasonal = list(order = c(1,1,1)), include.drift = FALSE, method = "ML")

summary(fit)

checkresiduals(fit)

autoplot(diesel_ts, linetype = "dashed") + geom_line(y=fitted(fit), color = "red") + 
  labs(title = "Vendas de óleo diesel no RS (m³)", subtitle = model_str(fit))



## FORECASTING

fc <- forecast(fit, h = 24)
autoplot(fc, include = 120)

#write.csv2(fc, "C:/Users/matheus.vizzotto/Desktop/Faculdade/Econometria Aplicada/Trabalho/Forecast.csv")
