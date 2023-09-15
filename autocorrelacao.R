library(sf)
library(spdep)
library(tidyverse)
library(colorspace)
library(patchwork)

#parte das análises contaram com a colaboração desse diálogo realizado no chatGPT
##https://chat.openai.com/share/2c6582dd-e42b-45e4-9ba1-91f9b13c14ed


gera_clusters_espaciais<- function(.data){



  # select column to work with
  s <- subset(.data, select=c("indicador_1"))


  # check data skewness
  hist(s$indicador_1, main=NULL)
  # check for outliers
  boxplot(s$indicador_1, horizontal = TRUE)

  s %>%
    ggplot() +
    geom_histogram(aes(x=indicador_1),color="white")

  s %>%
    mutate(tipo_indicador= "indicador 1") %>%
    ggplot() +
    geom_boxplot(aes(x=tipo_indicador, y=indicador_1))



  # define neighbor
  nb <- poly2nb(s, queen=TRUE) # here nb list all ID numbers of neighbors;

  # assign weights to neighbors
  lw <- nb2listw(nb, style="W", zero.policy=TRUE) # Row-standardized weights


  # compute neighbor average
  inc.lag <- lag.listw(lw, s$indicador_1, zero.policy=TRUE)

  # plot polygons vs lags
  plot(inc.lag ~ s$indicador_1, pch=16, asp=1)
  M1 <- lm(inc.lag ~ s$indicador_1)
  abline(M1, col="blue")

  # access Moran's coeff
  coef(M1)[2]

  # calculating Moran coeff with one line
  I <- moran(s$indicador_1, lw, length(nb), Szero(lw), zero.policy=TRUE)[1]

  I




  # hypothesis test with moran.test function
  moran.test(s$indicador_1,lw, alternative="greater", zero.policy=TRUE)

  # using Monte-Carlo simulation
  MC<- moran.mc(s$indicador_1, lw, nsim=999, alternative="greater", zero.policy=TRUE)

  # View results (including p-value)
  MC# plot Null distribution
  plot(MC)


  lisa<- spdep::localmoran(s$indicador_1, lw, zero.policy=TRUE)

  df_lisa<- as_tibble(lisa)

  names(df_lisa)



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


  clusters_selecionado<-
    df_vizinhos %>%
    filter(id %in% id_central) %>%
    group_by(id) %>%
    filter(abs(z_ii) == max(abs(z_ii))) %>%
    ungroup()

  id_referencia_selecao<- unique(clusters_selecionado$id_referencia)

  vizinhos_selecao<-
    df_vizinhos %>%
    filter(id_referencia %in% id_referencia_selecao)%>%
    group_by(id) %>%
    filter(abs(z_ii) == max(abs(z_ii))) %>%
    ungroup()



  df_clusters_espaciais<-
    .data %>%
    mutate(id = row_number())%>%
    inner_join(
      vizinhos_selecao

    )

  return(list(df_clusters_espaciais= df_clusters_espaciais,lw=lw))

}


clusters_indicador_1<-
  mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      #filter(nota_1 %in% c("A","B")) %>%
      rename(code_muni = cod_ibge)
  )%>%
  gera_clusters_espaciais()


agrupa_mapa_id_referencia<- function(.data, a_uf="", sinal=0){


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

  purrr::map_dfr(ids_referencia, function(a_id){


    z_ii_objeto<- unique(clusters_espaciais_trabalho$z_ii[clusters_espaciais_trabalho$id_referencia==a_id])
    ii_objeto<- unique(clusters_espaciais_trabalho$ii[clusters_espaciais_trabalho$id_referencia==a_id])
    media_indice<- mean(clusters_espaciais_trabalho$indicador_1[clusters_espaciais_trabalho$id_referencia==a_id])


    clusters_espaciais_trabalho %>%
      filter(id %in% c(a_id,vizinhos[a_id]))%>%
      group_by(id_referencia) %>%
      st_union() %>%
      as.tibble() %>%
      mutate(id_referencia =a_id,
             z_ii = z_ii_objeto,
             ii = ii_objeto,
             media_indice = media_indice,
             multiplicador = 1+runif(n=1))


  })


}






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
  agrupa_mapa_id_referencia() %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,  fill= media_indice), color=NA,  show.legend = TRUE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "black")
  )



##Teste para identificar coordenadas com problemas


centroides<-
  purrr::map_dfr(2261:NROW(mapa_municipios), function(i){
    print(i)


    tibble(lon= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,1],
           lat= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,2])

  })





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


