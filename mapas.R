library(tidyverse)
library(colorspace)
library(patchwork)


mapa_municipios<- rio::import("mapa_municipios.RDS")
sedes_municipios<- rio::import("sedes_municipios.RDS")
estados<- rio::import("estados.RDS")
brasil<- rio::import("brasil.RDS")
ibge2022<- rio::import("ibge2022.RDS")
dados_capag_2022 <- rio::import("dados_capag_2022.RDS")


source("analise_exploratoria.R")


paleta_sequencial<- function(){
  color_vector <- c("#ffffff", "#ffddbb", "#ffaa66", "#ff8800", "#ff6600")
  color_vector
}

paleta_qualitativa<- function(){
  qualitative_contrast_vector <- c("#FF6347", "#32CD32", "#1E90FF", "#FFD700", "#BA55D3")
  qualitative_contrast_vector
}

gera_mapa_categoria<- function(categoria, point_color="#ff6600", titulo){

  sedes<-
    sedes_municipios %>%
    inner_join(
      dados_capag_2022 %>%
        filter(capag_oficial== categoria) %>%
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


gera_mapa_destaque_categoria<- function(.data,categoria, point_color="#ff6600", titulo,  var_cod_mun = "cod_ibge"){

  uf_sel<-
    unique(.data$uf)


  sedes<-
    sedes_municipios %>%
    inner_join(
      .data %>%
        filter(capag_oficial %in%  categoria) %>%
        rename(code_muni = !!sym(var_cod_mun))
    )

  estados_sel<-
    estados %>%
    filter(abbrev_state %in% uf_sel)



  sedes_municipios %>%
    inner_join(
       .data %>%
         rename(code_muni = !!sym(var_cod_mun))
     ) %>%
    ggplot()+
    geom_sf( pch=21, fill="#808080",  color="#808080", size= 0.5) +
    geom_sf(data = sedes,pch=21,  fill=point_color, color = point_color, size= 0.5 )+
    geom_sf(data= estados_sel, fill=NA, color="white") +
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


gera_mapa_destaque_varias_categoria<- function(.data,categoria,  titulo,  var_cod_mun = "cod_ibge", a_size=0.6){

  uf_sel<-
    unique(.data$uf)


  sedes<-
    sedes_municipios %>%
    inner_join(
      .data %>%
        filter(capag_oficial %in% categoria) %>%
        rename(code_muni = !!sym(var_cod_mun))
    )

  estados_sel<-
    estados %>%
    filter(abbrev_state %in% uf_sel)



  sedes_municipios %>%
    inner_join(
      .data %>%
        rename(code_muni = !!sym(var_cod_mun))
    ) %>%
    ggplot()+
    geom_sf( pch=21, fill="#808080",  color="#808080", size= a_size) +
    geom_sf(data = sedes,pch=21, aes(fill=capag_oficial, color= capag_oficial), size=a_size )+
    geom_sf(data= estados_sel, fill=NA, color="white") +
    scale_fill_manual(values= paleta_qualitativa())+
    scale_color_manual(values= paleta_qualitativa())+
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



gera_mapa_nota_indicador<- function(.data,a_indicador, mid_point, com_facet=TRUE, a_size=1 ){

  indicador<- str_c("indicador_",a_indicador)


  graph<-
  sedes_municipios %>%
    inner_join(
      .data %>%
        rename(code_muni = cod_ibge)
    ) %>%
    ggplot()+
    geom_sf( aes(fill = !!sym(indicador)), pch=21,  color="black", size= a_size) +
    geom_sf(data= estados, fill=NA, color="white") +
    scale_fill_continuous_divergingx(palette = "Zissou 1", mid=mid_point) +
    #scale_fill_continuous_sequential(palette= "Heat 2") +
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

    )

  if (com_facet){

    graph<-
    graph+
    facet_wrap(capag_oficial~.)

  }

  graph

}






### Mapa com os territórios dos municípios exibindo as fronteiras

mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      mutate(capag_oficial = ifelse(capag_oficial=="n.d.",NA,capag_oficial) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf(aes(fill= capag_oficial),color=NA)


### Mapa com os territórios dos municípios sem exibir as fronteiras
mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      mutate(capag_oficial = ifelse(capag_oficial=="n.d.",NA,capag_oficial) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf(aes(fill= capag_oficial))


### Mapa com sedes dos municípios
sedes_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      #mutate(capag_2022 = ifelse(capag_2022=="n.d.",NA,capag_2022) ) %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf( aes(fill= capag_oficial), pch=21,  color="black", size= 0.9) +
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
      #filter(capag_oficial != "D") %>%
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
  facet_wrap(capag_oficial~.)


sedes_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1") %>%
      rename(code_muni = cod_ibge)
  ) %>%
  ggplot()+
  geom_sf( aes(fill = indicador_1), pch=21,  color="#ff6600", size= 0.1) +
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
  facet_wrap(capag_oficial~.)


dados_capag_2022 %>%
  filter_outliers("indicador_1") %>%
  gera_mapa_nota_indicador("1",1)

dados_capag_2022 %>%
  filter_outliers("indicador_2") %>%
  gera_mapa_nota_indicador("2",0.95, com_facet = FALSE, a_size= 1.2)

dados_capag_2022 %>%
  filter_outliers("indicador_2") %>%
  gera_mapa_nota_indicador("2",0.95,  a_size= 1.8)



dados_capag_2022 %>%
  filter_outliers("indicador_3") %>%
  gera_mapa_nota_indicador("3",1,  a_size= 1.8)



####Mapas de estados específicos para categorias específicas

mapa_a1<-
dados_capag_2022 %>%
  filter(uf %in% c("RS")) %>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios Nota A")


mapa_a2<-
  dados_capag_2022 %>%
  filter(uf %in% c("AL")) %>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios Nota A")

mapa_a3<-
  dados_capag_2022 %>%
  filter(uf %in% c("BA")) %>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios Nota A")

mapa_a1+mapa_a3

dados_capag_2022 %>%
  filter(uf %in% c("AL","BA","RS")) %>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios Nota A")


mapa_nd1<-
  dados_capag_2022 %>%
  filter(uf %in% c("GO")) %>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios não avaliados")


mapa_nd2<-
  dados_capag_2022 %>%
  filter(uf %in% c("TO")) %>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios não avaliados")


mapa_nd3<-
  dados_capag_2022 %>%
  filter(uf %in% c("PA")) %>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios não avaliados")


mapa_nd4<-
  dados_capag_2022 %>%
  filter(uf %in% c("RS")) %>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios não avaliados")

mapa_nd1 + mapa_nd2 + mapa_nd3 + mapa_nd4

dados_capag_2022 %>%
  filter(uf %in% c("GO","TO","PA", "RS")) %>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios não avaliados")


dados_capag_2022 %>%
  filter(uf %in% c("BA", "AL", "RS")) %>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios nota A")


dados_capag_2022 %>%
  filter(uf %in% c("BA", "PE", "CE","MG","PI")) %>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C")

fab<-
  capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0")

mun_0a<-
capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0")%>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios nota A", var_cod_mun = "cod_cidade")


mun0c<-
capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0")%>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")


mun_0a +mun0c


mun_0asp<-
  capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf == "RS")%>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios nota A", var_cod_mun = "cod_cidade")


mun0csp<-
  capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf == "RS")%>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")



mun_0ars<-
  capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf == "RS")%>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios nota A", var_cod_mun = "cod_cidade")


mun0crs<-
  capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf == "RS")%>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0")%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","C"), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")





capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5")%>%
  gera_mapa_destaque_categoria(categoria = "A", titulo = "Munícipios nota A", var_cod_mun = "cod_cidade")



capag_regic_trabalho %>%
  filter( nivel_hierarquia %in% c("1","2B","3B","3A"))%>%
  gera_mapa_destaque_categoria(categoria = "B", titulo = "Munícipios nota B", var_cod_mun = "cod_cidade")


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5")%>%
  gera_mapa_destaque_categoria(categoria = "B", titulo = "Munícipios nota B", var_cod_mun = "cod_cidade")



capag_regic_trabalho %>%
  filter( nivel_hierarquia %in% c("0"))%>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5")%>%
  gera_mapa_destaque_categoria(categoria = "C", titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")


capag_regic_trabalho %>%
  filter( nivel_hierarquia %in% c("5"))%>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios sem nota", var_cod_mun = "cod_cidade")


capag_regic_trabalho %>%
  filter( nivel_hierarquia %in% c("2C","3A"))%>%
  gera_mapa_destaque_categoria(categoria = "n.d.", titulo = "Munícipios sem nota", var_cod_mun = "cod_cidade")



capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0")%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade")

capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf %in% c("RS"))%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 1)


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "0",
          uf %in% c("GO"))%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 1)



capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5")%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 0.3)


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5",
          uf =="GO")%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 1)


capag_regic_trabalho %>%
  filter( nivel_hierarquia == "5",
          uf =="CE")%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 1)


capag_regic_trabalho %>%
  filter( nivel_hierarquia %in% c("1","2B","3A","3B","4A"))%>%
  gera_mapa_destaque_varias_categoria (categoria = c("A","B", "C", "D", "n.d."), titulo = "Munícipios nota C", var_cod_mun = "cod_cidade", a_size = 1)
