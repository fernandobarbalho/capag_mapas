library(tidyverse)


mapa_municipios<- rio::import("mapa_municipios.RDS")
sedes_municipios<- rio::import("sedes_municipios.RDS")
estados<- rio::import("estados.RDS")
brasil<- rio::import("brasil.RDS")
ibge2022<- rio::import("ibge2022.RDS")
dados_capag_2022 <- rio::import("dados_capag_2022.RDS")


### Mapa com os territórios dos municípios exibindo as fronteiras

mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf(aes(fill= capag_2022),color=NA)


### Mapa com os territórios dos municípios sem exibir as fronteiras
mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf(aes(fill= capag_2022))


### Mapa com sedes dos municípios
sedes_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf(aes(color= capag_2022)) +
  geom_sf(data= estados, fill=NA)

