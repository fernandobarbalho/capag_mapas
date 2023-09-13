library(sf)
library(spdep)
library(tidyverse)
library(colorspace)


##Teste para identificar coordenadas com problemas


centroides<-
  purrr::map_dfr(2261:NROW(mapa_municipios), function(i){
    print(i)


    tibble(lon= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,1],
           lat= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,2])

  })





# Access the shapefile
df_shape<-
mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      rename(code_muni = cod_ibge)
  )





# select column to work with
s <- subset(df_shape, select=c("indicador_1"))

s %>%
  slice_sample(prop=0.1) %>%
  readr::write_csv("dados_autocorrelacao_espacial.csv")

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

df_lisa %>%
  mutate(id= as.character(row_number())) %>%
  readr::write_csv("lisa.csv")


df_lisa<- janitor::clean_names(df_lisa)


df_lisa_significante<-
  df_lisa %>%
  mutate(id = row_number())%>%
  filter(pr_z_e_ii <=0.05)


df_vizinhos<-
purrr::map_dfr(df_lisa_significante$id, function(a_id){


  tibble(id=lw[["neighbours"]][[a_id]],
         id_referencia= a_id,
         ii= rep(df_lisa_significante$ii[which(df_lisa_significante$id== a_id)], NROW(lw[["neighbours"]][[a_id]]) ),
         z_ii = rep(df_lisa_significante$z_ii[which(df_lisa_significante$id== a_id)], NROW(lw[["neighbours"]][[a_id]])))

})


df_clusters_espaciais<-
  df_shape %>%
  mutate(id = row_number())%>%
  inner_join(
    df_vizinhos %>%
      group_by(id) %>%
      filter(z_ii == max(z_ii)) %>%
      ungroup()

  )


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


df_clusters_espaciais %>%
  filter(abbrev_state=="CE") %>%
  ggplot() +
  geom_sf(aes(fill=z_ii),color=NA) +
  geom_sf(data = estados[estados$abbrev_state == "CE",],fill=NA) +
  scale_fill_continuous_divergingx (palette="Zissou 1", rev= TRUE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )
