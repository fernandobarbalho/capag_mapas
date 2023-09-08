library(sf)
library(spdep)
library(tidyverse)



set.seed(13) # because we are randomizing part of the process# Access the shapefile


# Access the shapefile
s<-
mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      filter(uf =="RO" ) %>%
      mutate(capag_oficial = ifelse(capag_oficial=="n.d.",NA,capag_oficial) ) %>%
      rename(code_muni = cod_ibge)
  )


sf::st_coordinates(sf::st_centroid(s$geom))[,1]
sf::st_coordinates(sf::st_centroid(s$geom))[,2]


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
inc.lag <- lag.listw(lw, s$estimate)

# plot polygons vs lags
plot(inc.lag ~ s$estimate, pch=16, asp=1)
M1 <- lm(inc.lag ~ s$estimate)
abline(M1, col="blue")

# access Moran's coeff
coef(M1)[2]

# calculating Moran coeff with one line
I <- moran(s$estimate, lw, length(nb), Szero(lw))[1]




library(sf)
library(ggplot2)

fab<-
  mapa_municipios %>%
  inner_join(
    dados_capag_2022 %>%
      filter_outliers("indicador_1", type="E") %>%
      mutate(capag_oficial = ifelse(capag_oficial=="n.d.",NA,capag_oficial) ) %>%
      rename(code_muni = cod_ibge)
  )


# Assuming 'fab$geom' is the problematic geometry
coords <- st_coordinates(st_geometry(fab$geom))

# Plot the entire polygon
plot <- ggplot(data = data.frame(coords), aes(x = X, y = Y)) +
  #geom_polygon(fill = "lightblue", color = "black") +
  theme_minimal()

# Add all segments that involve points 46 to 49
for (i in 46:49) {
  segment_coords <- coords[i:(i+1), ]
  plot <- plot + geom_segment(data = data.frame(segment_coords),
                              aes(x = segment_coords[1,1], y = segment_coords[1,2],
                                  xend = segment_coords[2,1], yend = segment_coords[2,2]),
                              color = "red", size = 1.5)
}

print(plot)



