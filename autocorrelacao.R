library(sf)
library(spdep)
library(tidyverse)



set.seed(13) # because we are randomizing part of the process# Access the shapefile


# Access the shapefile
s<-
mapa_municipios[-2260,] %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      #filter(indicador_1!=0) %>%
      rename(code_muni = cod_ibge)
  )




# select column to work with
s <- subset(s, select=c("indicador_1"))

# check data skewness
hist(s$indicador_1, main=NULL)# check for outliers
boxplot(s$indicador_1, horizontal = TRUE)


# define neighbor
nb <- poly2nb(s, queen=TRUE) # here nb list all ID numbers of neighbors;

# assign weights to neighbors
lw <- nb2listw(nb, style="W", zero.policy=TRUE) # equal weights

# compute neighbor average
inc.lag <- lag.listw(lw, s$indicador_1, zero.policy = TRUE)

# plot polygons vs lags
plot(inc.lag ~ s$indicador_1, pch=16, asp=1)
M1 <- lm(inc.lag ~ s$indicador_1)
abline(M1, col="blue")

# access Moran's coeff
coef(M1)[2]

# calculating Moran coeff with one line
I <- moran(s$indicador_1, lw, length(nb), Szero(lw))[1]

I




##Teste para identificar coordenadas com problemas


centroides<-
purrr::map_dfr(2261:NROW(mapa_municipios), function(i){
  print(i)


  tibble(lon= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,1],
         lat= sf::st_coordinates(sf::st_centroid(mapa_municipios$geom[i]))[,2])

})



