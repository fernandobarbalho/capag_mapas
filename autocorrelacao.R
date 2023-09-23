library(sf)
library(spdep)
library(tidyverse)
library(colorspace)
library(patchwork)

#parte das análises contaram com a colaboração desse diálogo realizado no chatGPT
##https://chat.openai.com/share/2c6582dd-e42b-45e4-9ba1-91f9b13c14ed


gera_clusters_espaciais<- function(.data, nome_coluna){



  # select column to work with
  s <- subset(.data, select=nome_coluna)

  names(s)[1]<-"indicador"



  s %>%
    ggplot() +
    geom_histogram(aes(x=!!sym(nome_coluna)),color="white")

  s %>%
    mutate(tipo_indicador= nome_coluna) %>%
    ggplot() +
    geom_boxplot(aes(x=tipo_indicador, y=!!sym(nome_coluna)))



  # define neighbor
  nb <- poly2nb(s, queen=TRUE) # here nb list all ID numbers of neighbors;

  # assign weights to neighbors
  lw <- nb2listw(nb, style="W", zero.policy=TRUE) # Row-standardized weights


  # # compute neighbor average
  # inc.lag <- lag.listw(lw, s$indicador_1, zero.policy=TRUE)
  #
  # # plot polygons vs lags
  # plot(inc.lag ~ s$indicador_1, pch=16, asp=1)
  # M1 <- lm(inc.lag ~ s$indicador_1)
  # abline(M1, col="blue")
  #
  # # access Moran's coeff
  # coef(M1)[2]
  #
  # # calculating Moran coeff with one line
  # I <- moran(s$indicador_1, lw, length(nb), Szero(lw), zero.policy=TRUE)[1]
  #
  # I
  #
  #
  #
  #
  # # hypothesis test with moran.test function
  # moran.test(s$indicador_1,lw, alternative="greater", zero.policy=TRUE)
  #
  # # using Monte-Carlo simulation
  # MC<- moran.mc(s$indicador_1, lw, nsim=999, alternative="greater", zero.policy=TRUE)
  #
  # # View results (including p-value)
  # MC# plot Null distribution
  # plot(MC)



  lisa<- spdep::localmoran(s$indicador, lw, zero.policy=TRUE)

  df_lisa<- as_tibble(lisa)



  df_lisa<- janitor::clean_names(df_lisa)


  df_lisa_significante<-
    df_lisa %>%
    mutate(id = row_number())%>%
    filter(pr_z_e_ii <=0.05)


  df_vizinhos<-
    purrr::map_dfr(df_lisa_significante$id, function(a_id){


      tibble(id= c(a_id, lw[["neighbours"]][[a_id]]),
             id_referencia= rep(a_id,NROW(lw[["neighbours"]][[a_id]])+1),
             ii=  rep(df_lisa_significante$ii[which(df_lisa_significante$id== a_id)], NROW(lw[["neighbours"]][[a_id]])+1),
             z_ii=  rep(df_lisa_significante$z_ii[which(df_lisa_significante$id== a_id)], NROW(lw[["neighbours"]][[a_id]])+1))

    })



  id_central<-
    unique(df_vizinhos$id_referencia)

  ###Seleciona os clusters em que o id_referencia se mantém como referência do próprio cluster
  clusters_selecionado<-
    df_vizinhos %>%
    filter(id %in% id_central) %>%
    group_by(id) %>%
    filter(abs(z_ii) == max(abs(z_ii))) %>%
    ungroup() %>%
    filter(id_referencia == id)



  id_referencia_selecao<- unique(clusters_selecionado$id_referencia)

  vizinhos_selecao<-
    df_vizinhos %>%
    filter(id_referencia %in% id_referencia_selecao)%>%
    group_by(id) %>%
    filter(abs(z_ii) == max(abs(z_ii))) %>%
    ungroup()

  ids_referencias_finais<-
    (vizinhos_selecao %>%
       summarise(.by = id_referencia,
                 quantidade = n()) %>%
       filter(quantidade >=2))$id_referencia


  df_clusters_espaciais<-
    .data %>%
    mutate(id = row_number())%>%
    inner_join(
      vizinhos_selecao %>%
        filter(id_referencia %in% ids_referencias_finais )
    )

  return(list(df_clusters_espaciais= df_clusters_espaciais,lw=lw))

}




agrupa_mapa_id_referencia<- function(.data, a_uf="", sinal=0, indicador){


  clusters_espaciais_trabalho<-
    .data[["df_clusters_espaciais"]]





  if (a_uf!=""){
    clusters_espaciais_trabalho <-
      clusters_espaciais_trabalho %>%
      filter(abbrev_state == a_uf)

  }

  if (sinal!=0){
    clusters_espaciais_trabalho <-
      clusters_espaciais_trabalho %>%
      filter(sign(z_ii) == sinal)

  }




  ids_referencia<- unique(clusters_espaciais_trabalho$id_referencia)
  vizinhos<-.data$lw$neighbours

  position<- which(names(clusters_espaciais_trabalho) == indicador)



  df_clusters_espaciais<- as_tibble(clusters_espaciais_trabalho)

  names(df_clusters_espaciais)[position] <- "indicador_selecionado"




  purrr::map_dfr(ids_referencia, function(a_id){



    z_ii_objeto<- unique(clusters_espaciais_trabalho$z_ii[clusters_espaciais_trabalho$id_referencia==a_id])
    ii_objeto<- unique(clusters_espaciais_trabalho$ii[clusters_espaciais_trabalho$id_referencia==a_id])
    media_indice<- mean(df_clusters_espaciais$indicador_selecionado[df_clusters_espaciais$id_referencia==a_id])
    cv<- sd(df_clusters_espaciais$indicador_selecionado[df_clusters_espaciais$id_referencia==a_id])/
        mean(df_clusters_espaciais$indicador_selecionado[df_clusters_espaciais$id_referencia==a_id])


    clusters_espaciais_trabalho %>%
      filter(id %in% c(a_id,vizinhos[[a_id]]))%>%
      group_by(id_referencia) %>%
      st_union() %>%
      as_tibble() %>%
      mutate(id_referencia =a_id,
             z_ii = z_ii_objeto,
             ii = ii_objeto,
             media_indice = media_indice,
             multiplicador = 1+runif(n=1),
             cv =cv)


  })


}



#######Análises com clusters com dados no intervalo interquartil

clusters_indicador_1<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_1")



fab<-
  clusters_indicador_1 %>%
  agrupa_mapa_id_referencia("CE",1)

mun_ce<-
  mapa_municipios %>%
  filter(abbrev_state == "CE")

mun_ce_clusters<-
  clusters_indicador_1$df_clusters_espaciais%>%
  filter(abbrev_state == "CE")


mun_ce_clusters_positivo<-
  clusters_indicador_1$df_clusters_espaciais%>%
  filter(abbrev_state == "CE",
         z_ii>0)



#opção de colorir usando alphas aleatórios

clusters_indicador_1 %>%
  agrupa_mapa_id_referencia("CE",1)%>%
  ggplot() +
  #geom_sf(data= mun_ce, color="lightgray" , fill= NA)+
  geom_sf(aes(geometry = geometry, alpha= multiplicador -1, fill= z_ii), color=NA,  show.legend = FALSE) +
  #geom_sf(aes(geometry = geometry, color= z_ii),  show.legend = FALSE) +
  #geom_sf(data= mun_ce_clusters, aes(fill=sign(z_ii)*indicador_1) , color = NA)+
  scale_fill_continuous_divergingx(palette= "Zissou 1", rev= TRUE,alpha=0.8) +
  #scale_color_continuous_divergingx(palette= "Zissou 1", rev= TRUE) +
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )



#opção de colorir usando médias



clusters_indicador_1 %>%
  agrupa_mapa_id_referencia("CE",1)%>%
  ggplot() +
  geom_sf(data= mun_ce, color="lightgray" , fill= NA)+
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  #geom_sf(aes(geometry = geometry, color= z_ii),  show.legend = FALSE) +
  #geom_sf(data= mun_ce_clusters, aes(fill=sign(z_ii)*indicador_1) , color = NA)+
  #scale_fill_continuous_divergingx(palette= "Zissou 1", rev= TRUE,alpha=0.8) +
  #scale_color_continuous_divergingx(palette= "Zissou 1", rev= TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  geom_sf(data= mun_ce_clusters_positivo,  fill = NA, color = "lightgray")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )





#opção de colorir usando o indicador

ggplot() +
  geom_sf(data= mun_ce, color="lightgray" , fill= NA)+
  #geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  #geom_sf(aes(geometry = geometry, color= z_ii),  show.legend = FALSE) +
  #geom_sf(data= mun_ce_clusters, aes(fill=sign(z_ii)*indicador_1) , color = NA)+
  #scale_fill_continuous_divergingx(palette= "Zissou 1", rev= TRUE,alpha=0.8) +
  #scale_color_continuous_divergingx(palette= "Zissou 1", rev= TRUE) +
  geom_sf(data= mun_ce_clusters_positivo,aes(fill = indicador_1), color = "lightgray")+
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


fab<-
  clusters_indicador_1 %>%
  agrupa_mapa_id_referencia()

fab2<-
  clusters_indicador_1 %>%
  filter(abbrev_state %in% c("CE"),
         z_ii>0) %>%
  agrupa_mapa_id_referencia()



clusters_indicador_1 %>%
  agrupa_mapa_id_referencia(sinal = 1) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  geom_sf(data=estados, fill= NA) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )



#######Análises com clusters com dados menores que extremo


###Indicador 1
clusters_indicador_1_amplo<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_1<10) %>%
      #filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_1")


clusters_indicador_1_amplo %>%
  agrupa_mapa_id_referencia(sinal = 1) %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )




##Indicador 2
clusters_indicador_2_amplo<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_2<10) %>%
      #filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_2")


dados_clusters_agrupados<-
mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_2<10) %>%
      #filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_2")%>%
  agrupa_mapa_id_referencia(sinal = 1, indicador = "indicador_2")

dados_clusters_agrupados %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


###Indicador 3
mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_3<10) %>%
      #filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_3") %>%
  agrupa_mapa_id_referencia(sinal = 1, indicador = "indicador_3") %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


#######Análises com clusters com dados menores que extremo com grágicos combinados


##Clusters convergentes indicador 1


#Estatísticas descritiva dos clusters

clusters_espaciais_1<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_1<10) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_1")



df_mapa_agrupado_convergentes_1<-
  clusters_espaciais_1%>%
  agrupa_mapa_id_referencia(sinal = 1, indicador = "indicador_1")




##histogramas clusters convergentes




df_mapa_agrupado_convergentes_1 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_convergentes_1 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters convergentes
g1<- df_mapa_agrupado_convergentes_1 %>%
  mutate(grupo  = "convergente 1") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<- df_mapa_agrupado_convergentes_1 %>%
  mutate(grupo  = "convergente 1") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))






##mapas

m1<-
df_mapa_agrupado_convergentes_1 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_convergentes_1 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2) / (m1+m2)





##Clusters divergentes indicador 1

df_mapa_agrupado_divergentes_1<-
  clusters_espaciais_1%>%
  agrupa_mapa_id_referencia(sinal = -1, indicador = "indicador_1")

##Histogramas clusters divergentes

df_mapa_agrupado_divergentes_1 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_divergentes_1 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters divergentes
g1<- df_mapa_agrupado_divergentes_1 %>%
  mutate(grupo  = "divergente 1") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<-df_mapa_agrupado_divergentes_1 %>%
  mutate(grupo  = "divergente 1") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))



m1<-
  df_mapa_agrupado_divergentes_1 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_divergentes_1 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2)/(m1+m2)


##Clusters convergentes indicador 2


#Estatísticas descritiva dos clusters

clusters_espaciais_2<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_2<10) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_2")



df_mapa_agrupado_convergentes_2<-
  clusters_espaciais_2%>%
  agrupa_mapa_id_referencia(sinal = 1, indicador = "indicador_2")




##histogramas clusters convergentes




df_mapa_agrupado_convergentes_2 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_convergentes_2 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters convergentes
g1<- df_mapa_agrupado_convergentes_2 %>%
  mutate(grupo  = "convergente 2") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<- df_mapa_agrupado_convergentes_2 %>%
  mutate(grupo  = "convergente 2") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))






##mapas

m1<-
  df_mapa_agrupado_convergentes_2 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_convergentes_2 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2) / (m1+m2)





##Clusters divergentes indicador 2

df_mapa_agrupado_divergentes_2<-
  clusters_espaciais_2%>%
  agrupa_mapa_id_referencia(sinal = -1, indicador = "indicador_2")

##Histogramas clusters divergentes

df_mapa_agrupado_divergentes_2 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_divergentes_2 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters divergentes
g1<- df_mapa_agrupado_divergentes_2 %>%
  mutate(grupo  = "divergente 2") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<-df_mapa_agrupado_divergentes_2 %>%
  mutate(grupo  = "divergente 2") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))



m1<-
  df_mapa_agrupado_divergentes_2 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_divergentes_2 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2)/(m1+m2)



##Clusters convergentes indicador 3

#Estatísticas descritiva dos clusters

clusters_espaciais_3<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter(indicador_3<10) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais(nome_coluna = "indicador_3")



df_mapa_agrupado_convergentes_3<-
  clusters_espaciais_3%>%
  agrupa_mapa_id_referencia(sinal = 1, indicador = "indicador_3")


##histogramas clusters convergentes


df_mapa_agrupado_convergentes_3 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_convergentes_3 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters convergentes
g1<- df_mapa_agrupado_convergentes_3 %>%
  mutate(grupo  = "convergente 3") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<- df_mapa_agrupado_convergentes_3 %>%
  mutate(grupo  = "convergente 3") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))






##mapas


m1<-
  df_mapa_agrupado_convergentes_3 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_convergentes_3 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2)/(m1+m2)



##Clusters divergentes indicador 3

df_mapa_agrupado_divergentes_3<-
  clusters_espaciais_3%>%
  agrupa_mapa_id_referencia(sinal = -1, indicador = "indicador_3")


##histogramas clusters divergentes


df_mapa_agrupado_divergentes_3 %>%
  ggplot() +
  geom_histogram(aes(x=media_indice), color = "white")


df_mapa_agrupado_divergentes_3 %>%
  ggplot() +
  geom_histogram(aes(x=cv), color = "white")


##box-plot clusters convergentes
g1<- df_mapa_agrupado_divergentes_3 %>%
  mutate(grupo  = "divergente 3") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=media_indice))


g2<- df_mapa_agrupado_divergentes_3 %>%
  mutate(grupo  = "divergente 3") %>%
  ggplot() +
  geom_boxplot(aes(x=grupo, y=cv))


m1<-
  df_mapa_agrupado_divergentes_3 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )


m2<-
  df_mapa_agrupado_divergentes_3 %>%
  ggplot() +
  geom_sf(data=estados, fill= NA) +
  geom_sf(aes(geometry = geometry,  fill= cv), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )

(g1+g2)/(m1+m2)





##Teste para identificar coordenadas com problemas


centroides<-
  purrr::map_dfr(2261:NROW(mapa_municipios), function(i){
    print(i)


    tibble(lon= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,1],
           lat= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,2])

  })





###########Laboratórios diversos



# Access the shapefile


fab<-
  df_clusters_espaciais %>%
  filter(id_referencia == 878) %>%
  st_union(by_feature = FALSE)

mun_rmf<-
  df_clusters_espaciais %>%
  filter(id_referencia == 878)


fab %>%
  ggplot() +
  geom_sf() +
  geom_sf(data=mun_rmf)


fab_2<-
  df_clusters_espaciais %>%
  group_by(id_referencia) %>%
  st_union()


fab_2 %>%
  ggplot() +
  geom_sf()

df_clusters_espaciais %>%
  ggplot() +
  geom_sf(aes(fill=ii),color=NA)


df_clusters_espaciais %>%
  ggplot() +
  geom_sf(aes(fill=z_ii),color=NA)


df_clusters_espaciais %>%
  ggplot() +
  geom_sf(aes(fill=indicador_1),color=NA) +
  geom_sf(data = estados,fill=NA) +
  scale_fill_continuous_sequential(palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


df_clusters_espaciais %>%
  ggplot() +
  geom_sf(aes(fill=z_ii),color=NA) +
  geom_sf(data = estados,fill=NA) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g1<-
df_clusters_espaciais %>%
  filter(abbrev_state=="CE") %>%
  ggplot() +
  geom_sf(aes(fill=z_ii)) +
  geom_sf(data = estados[estados$abbrev_state == "CE",],fill=NA) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g2<-
df_clusters_espaciais %>%
  filter(abbrev_state=="CE") %>%
  ggplot() +
  geom_sf(aes(fill=indicador_1)) +
  geom_sf(data = estados[estados$abbrev_state == "CE",],fill=NA) +
  scale_fill_continuous_sequential (palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g1+g2


map_centros<-
  df_clusters_espaciais %>%
  filter(abbrev_state == "PA",
         id %in% id_referencia_selecao )


g1<-
  df_clusters_espaciais %>%
  filter(abbrev_state=="PA") %>%
  ggplot() +
  geom_sf(aes(fill=z_ii)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  geom_sf_text(data= map_centros, aes(label = name_muni), color ="white" ) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g2<-
  df_clusters_espaciais %>%
  filter(abbrev_state=="PA") %>%
  ggplot() +
  geom_sf(aes(fill=indicador_1)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  scale_fill_continuous_sequential (palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )




g1+g2

map_centros_selecao<-
  map_centros %>%
  filter(id_referencia == 135 )

g1<-
  df_clusters_espaciais%>%
  filter(id %in% c(135,lw[["neighbours"]][[135]]))%>%
  ggplot() +
  geom_sf(aes(fill=z_ii)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  geom_sf_text(data= map_centros_selecao, aes(label = name_muni), color ="white" ) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g2<-
  df_clusters_espaciais %>%
  filter(id %in% c(135,lw[["neighbours"]][[135]]))%>%
  ggplot() +
  geom_sf(aes(fill=indicador_1)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  scale_fill_continuous_sequential (palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )




g1+g2


g1+g2

map_centros_selecao<-
  map_centros %>%
  filter(id_referencia == 134 )

g1<-
  df_clusters_espaciais%>%
  filter(id %in% c(134,lw[["neighbours"]][[134]]))%>%
  ggplot() +
  geom_sf(aes(fill=z_ii)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  geom_sf_text(data= map_centros_selecao, aes(label = name_muni), color ="white" ) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g2<-
  df_clusters_espaciais %>%
  filter(id %in% c(134,lw[["neighbours"]][[134]]))%>%
  ggplot() +
  geom_sf(aes(fill=indicador_1)) +
  geom_sf(data = estados[estados$abbrev_state == "PA",],fill=NA) +
  scale_fill_continuous_sequential (palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )




g1+g2




g1<-
  df_clusters_espaciais %>%
  filter(id %in% c(878,lw[["neighbours"]][[878]]))%>%
  filter(abbrev_state=="CE") %>%
  group_by(id_referencia) %>%
  st_union() %>%
  ggplot() +
  geom_sf(fill= aes(fill=id_referencia) , color =NA) +
  geom_sf(data = estados[estados$abbrev_state == "CE",],fill=NA) +
  #scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  scale_fill_discrete_qualitative(palette= "Dark 1") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g2<-
  df_clusters_espaciais %>%
  filter(abbrev_state=="CE") %>%
  ggplot() +
  geom_sf(aes(fill=indicador_1)) +
  geom_sf(data = estados[estados$abbrev_state == "CE",],fill=NA) +
  scale_fill_continuous_sequential (palette="Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


g1+g2


