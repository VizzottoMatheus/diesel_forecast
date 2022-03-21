library(dplyr)
library(jsonlite)

# FAZ DOWNLOAD DE DADOS DE VENDA DE DIESEL E IMPROTA DADOS DE SOJA, MILHO E 
# TRIGO, ESTES TRÊS DIRETAMENTE AO AMBIENTE


######  FONTE  #####

# https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/vendas-de-derivados-de-petroleo-e-biocombustiveis



##############   DADOS POR MUNICÍPIO

oil_data_cities <- function() {
  # 2000 - 2018
  path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/oleo-diesel/vendas-oleodiesel-municipio-"
  diesel_2000_2018 <- NULL
  for (ano in 2000:2018) {
    url <- paste0(path, ano, ".csv")
    df <- read.csv(url, sep = ";", encoding = "UTF-8", dec = ",")
    colnames(df) <- c("ANO", "ESTADO", "CODIBGE", "MUNICIPIO", "VENDAS")
    diesel_2000_2018 <- bind_rows(diesel_2000_2018, df)
  }
  diesel_2000_2018$VENDAS <- as.numeric(diesel_2000_2018$VENDAS)
  
  
  # 2019
  path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/oleo-diesel/vendas-oleo-diesel-municipio-2019.csv"
  diesel_2019 <- read.csv(path, sep = ";", col.names = c("ANO", "ESTADO", "CODIBGE", "MUNICIPIO", "VENDAS"))
  diesel_2019$VENDAS <-  as.numeric(gsub("\\.", "", diesel_2019$VENDAS))
  
  # CONCATENANDO
  
  diesel_municipios <- bind_rows(diesel_2000_2018, diesel_2019)
  
  # MUNICÍPIOS DO RIO GRANDE DO SUL
  unique(diesel_municipios$ESTADO)
  
  diesel_mun_rs <- filter(diesel_municipios, ESTADO == "Rio Grande do Sul")
  
  return(diesel_mun_rs)
}


##############   DADOS POR ESTADO

oil_data_state <- function() {
  path <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/vdpb/vendas-derivados-petroleo-e-etanol/vendas-derivados-petroleo-etanol-m3-1990-2021.csv"
  derivados <- read.csv(path, sep = ";", dec = ",",  encoding = "UTF-8")
  colnames(derivados) <- c("ANO", "MES", "GRANDE.REGIAO", "UNIDADE.DA.FEDERACAO", "PRODUTO", "VENDAS")
  oleo_diesel <- tail(unique(derivados$PRODUTO),1)
  diesel_estados <- filter(derivados, PRODUTO == oleo_diesel)
  diesel_rs <- filter(diesel_estados, UNIDADE.DA.FEDERACAO == "RIO GRANDE DO SUL")
  
  diesel_rs$MES <- diesel_rs$MES %>%
        gsub('JAN', "01", .) %>%
        gsub('FEV', "02", .) %>%
        gsub('MAR', "03", .) %>%
        gsub('ABR', "04", .) %>%
        gsub('MAI', "05", .) %>%
        gsub('JUN', "06", .) %>%
        gsub('JUL', "07", .) %>%
        gsub('AGO', "08", .) %>%
        gsub('SET', "09", .) %>%
        gsub('OUT', "10", .) %>%
        gsub('NOV', "11", .) %>%
        gsub('DEZ', "12", .)
  
  diesel_rs$ANO_MES <- as.Date(paste0(diesel_rs$ANO, "-", diesel_rs$MES, "-01"))
  diesel_rs <- arrange(diesel_rs, ANO_MES)
  df_diesel_rs <- select(diesel_rs, c("ANO_MES", "VENDAS"))
  return(df_diesel_rs)
}

################# DADOS DE AGRICULTURA E PECUÁRIA

# FONTE: https://apisidra.ibge.gov.br/         # (IBGE)

url_cste <- "http://api.sidra.ibge.gov.br/values/"
soja<- paste0(url_cste, "t/5457/p/all/v/8331,216,214,215/c782/40124/n3/43")
milho<- paste0(url_cste, "t/5457/p/all/v/8331,216,214,215/c782/40122/n3/43")
trigo<- paste0(url_cste, "t/5457/p/all/v/8331,216,214,215/c782/40127/n3/43")

soja_df <- fromJSON(soja)
milho_df <- fromJSON(soja)
trigo_df <- fromJSON(soja)
