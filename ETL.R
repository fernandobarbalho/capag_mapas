library(readxl)
library(geobr)
library(rio)



url_original<- "https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/86636c19-b38a-4b9e-8fff-30fc4208dd04/download/CAPAG-Municipios---Ano-Base-2022.xlsx"

download.file(url_original, destfile = "dados_capag_2022_original.xlsx", mode = "wb")

dados_capag_2022_original <- read_excel("dados_capag_2022_original.xlsx")

dados_capag_2022_original <- janitor::clean_names(dados_capag_2022_original)


url<- "https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/6a218451-f1b4-4fce-ac2a-00a3675bf4eb/download/CAPAG-Oficial-Municipios-2023-02-23-corrigido.xlsx"

download.file(url, destfile = "dados_capag_2022.xlsx", mode = "wb")

dados_capag_2022 <- read_excel("dados_capag_2022.xlsx")

dados_capag_2022 <- janitor::clean_names(dados_capag_2022)



url<- "https://www.tesourotransparente.gov.br/ckan/dataset/9ff93162-409e-48b5-91d9-cf645a47fdfc/resource/31ed778a-9115-419c-b18e-c9131a978aef/download/CAPAG-Oficial-23.xlsx"

download.file(url, destfile = "dados_capag_2022.xlsx", mode = "wb")

dados_capag_2022 <- read_excel("dados_capag_2022.xlsx")

dados_capag_2022 <- janitor::clean_names(dados_capag_2022)



mapa_municipios <- geobr::read_municipality(simplified = FALSE)

estados<- geobr::read_state()
sedes_municipios<- geobr::read_municipal_seat()
brasil<- geobr::read_country()

gera_tabela_ibge_municipios<- function(){
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)
  library(colorspace)

  # API endpoint URL
  api_url <- "https://apisidra.ibge.gov.br/values/t/4714/n6/all/v/all/p/all/d/v614%202"

  data_list <- fromJSON(api_url)


  names_df<- data_list[1,]
  data_list <- data_list[-1,]

  names(data_list) <- names_df

  data_list <- janitor::clean_names(data_list)

  ibge_municipios<-
    data_list %>%
    mutate(valor =as.numeric(valor)) %>%
    select(municipio_codigo,
           municipio,
           variavel,
           valor,
           ano) %>%
    pivot_wider(id_cols = c(municipio_codigo, municipio), names_from = variavel, values_from = valor) %>%
    separate(municipio, into = c("municipio", "uf"), sep = " - ")


  ibge_municipios<- janitor::clean_names(ibge_municipios)

  ibge_municipios


}

#Exemplo de uso
ibge2022<-
  gera_tabela_ibge_municipios()

rio::export(mapa_municipios, "mapa_municipios.RDS")
rio::export(sedes_municipios,"sedes_municipios.RDS")
rio::export(estados, "estados.RDS")
rio::export(brasil,"brasil.rds")

rio::export(ibge2022,"ibge2022.RDS")
rio::export(dados_capag_2022,"dados_capag_2022.RDS")
