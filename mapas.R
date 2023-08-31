library(tidyverse)
library(colorspace)
library(patchwork)


mapa_municipios<- rio::import("mapa_municipios.RDS")
sedes_municipios<- rio::import("sedes_municipios.RDS")
estados<- rio::import("estados.RDS")
brasil<- rio::import("brasil.RDS")
ibge2022<- rio::import("ibge2022.RDS")
dados_capag_2022 <- rio::import("dados_capag_2022.RDS")


gera_mapa_categoria<- function(categoria, point_color="red", titulo){

  sedes<-
    sedes_municipios %>%
    inner_join(
      dados_capag_2022 %>%
        filter(capag_2022== categoria) %>%
        rename(code_muni = cod_ibge)
    )

    sedes_municipios %>%
    # inner_join(
    #   dados_capag_2022 %>%
    #     rename(code_muni = cod_ibge)
    # ) %>%
    ggplot()+
    #geom_sf( pch=21, fill="#808080",  color="#808080", size= 0.5) +
    geom_sf(data = sedes,pch=21,  fill=point_color, color = point_color, size= 0.5 )+
    geom_sf(data= estados, fill=NA, color="white") +
    theme_light() +
    theme(
      #text = element_text(size=20),
      panel.background = element_rect(fill = "black"),
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "#505050"),
      strip.text = element_text(color = "white"),
      axis.text = element_blank(),
      legend.key = element_rect(fill = "black")

    ) +
      labs(title = titulo)


}


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
      #mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf( aes(fill= capag_2022), pch=21,  color="black", size= 0.9) +
  geom_sf(data= estados, fill=NA, color="#808080") +
  #scale_fill_discrete_qualitative(palette= "Dark 2")+
  scale_fill_discrete_sequential(palette= "Heat 2", rev= FALSE) +

  theme_light() +
  theme(
    #text = element_text(size=20),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    axis.text = element_blank(),
    legend.key = element_rect(fill = "black")

  )+ guides(fill = guide_legend(override.aes = list(size=2)))





### Gera obejtos Mapas com sedes dos municípios com destaque para cada categoria separadamente


mapa_a<-
  gera_mapa_categoria("A","#ff6600","Cidades capag A")

mapa_b<-
  gera_mapa_categoria("B","#ff6600","Cidades capag B")

mapa_c<-
  gera_mapa_categoria("C","#ff6600","Cidades capag C")


mapa_d<-
  gera_mapa_categoria("D","#ff6600","Cidades capag D")

mapa_nd<-
  gera_mapa_categoria("n.d.","#ff6600","Cidades com informações não declaradas")


(mapa_a+mapa_b)/(mapa_c+mapa_nd)


### Mapa com sedes dos municípios com facet
sedes_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      filter(capag_2022 != "D") %>%
      #mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf( fill = "#ff6600", pch=21,  color="#ff6600", size= 0.1) +
  geom_sf(data= estados, fill=NA, color="white") +
  #scale_fill_discrete_qualitative(palette= "Dark 2")+
  scale_fill_discrete_sequential(palette= "Heat 2", rev= FALSE) +

  theme_light() +
  theme(
    #text = element_text(size=20),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill = "#505050"),
    strip.text = element_text(color = "white"),
    axis.text = element_blank(),
    legend.key = element_rect(fill = "black")

  )+
  facet_wrap(capag_2022~.)
