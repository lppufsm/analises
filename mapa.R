library(tidyverse)
library(geobr)
library(sf)

bairro <- geobr::read_neighborhood() %>% 
  filter(code_muni == '4316907')

health <- read_health_facilities() %>% 
  filter(code_muni == '431690')

sm <- read_municipality(code_muni = 4316907)

bairro %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data = health[bairro, ])+
  theme_void()

dist <- raster::distanceFromPoints(object = bairro, xy = health)

bairro %>% 
  ggplot()+
  geom_sf(aes(fill = dist))
  