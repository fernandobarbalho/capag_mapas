# pacotes -----------------------------------------------------------------
library(dplyr)
library(httr)
library(jsonlite)
library(data.table)

# funções -----------------------------------------------------------------

# Carrega função que pega o json do powerbi e transforma em data frame
source("source/funcoes/powerBi_table_2_dataframe.R")


# variáveis ---------------------------------------------------------------
url_app <- paste0("https://wabi-brazil-south-b-primary-api.analysis.windows.net/", 
                  "public/reports/querydata?synchronous=true")

path_json_request <- "dados/brutos/template.json"

sheets_id <- "1hoPgCd70ViHdoFa1ERXjljpKZ551QXb86lUsKHRPiU8"
aba_nome <-  "tabela-pac"

# obtem o json resposta ----------------------------------------------------
headers <- httr::add_headers(
  "Accept" = paste0("text/html,application/xhtml+xml,application/", 
                    "xml;q=0.9,image/avif,image/webp,*/*;q=0.8"),
  "Accept-Encoding" =  "gzip, deflate, br",
  "Accept-Language" = "pt-BR,en-US;q=0.7,en;q=0.3",
  "Host" = "wabi-brazil-south-b-primary-api.analysis.windows.net",
  "Origin" = "https://app.powerbi.com",
  "Referer" = "https://app.powerbi.com",
  "Sec-Fetch-Dest" = "empty",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Site" = "cross-origin",
  "Upgrade-Insecure-Requests" = 1,
  "ActivityId" = "c53be8d2-dd88-4cbd-b8c9-99726d029f8c",
  "X-PowerBI-ResourceKey" = "2efd1d40-e256-4280-af9a-f2bc0f3adba8",
  
  "User-Agent" = paste0("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) ", 
                        "Gecko/20100101 Firefox/115.0"))

json_string <- readLines(path_json_request, encoding = "UTF-8")

r_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE, 
                             simplifyDataFrame = FALSE, simplifyMatrix = FALSE)

resposta_json <- POST(url_app, body = r_list, headers, encode = "json") %>% 
  httr::content() %>% 
  jsonlite::fromJSON(simplifyVector = FALSE, simplifyDataFrame = FALSE, 
                     simplifyMatrix = FALSE)


# faz o data frame e salva ------------------------------------------------
df <- extract_powerBi(resposta_json)
df%>%
  readr::write_csv("obras_pac.csv")

# googlesheets4::write_sheet(data = df, 
#                            ss = sheets_id, 
#                            sheet = aba_nome)


df%>%
  summarise(.by = EIXO,
            n())
