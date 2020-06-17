

library(rgdal)
library(dplyr)
library(tidyverse)
library(stringi)
library(RColorBrewer)
library(leaflet)
library(maptools)
library(sp)
library(raster)

shp_mun <- readOGR("./br_municipios/BR_Municipios_2019.shp", 
               stringsAsFactors=TRUE, encoding="UTF-8")
class(shp_mun)
head(shp_mun$CD_MUN)
# usando somente os 6 primeiros dígitos do código dos municípios
# para deixar como estão nos dados do Min. da Saúde
shp_mun$CD_MUN <- str_sub(as.character(shp_mun$CD_MUN),
                          start = 1,end = 6)

mun <- readRDS("./Dados/dados_por_municipio.rds")
#View(mun)

# selecionando as variáveis que vão ser usadas
mun <- mun %>% 
 dplyr::select(Estado,Município,Codigo,codDRS,nomDRS) %>% 
  dplyr::distinct()

# adding the variables in mun to the spatial object shp_mun
mun_rs <- sp::merge(shp_mun,mun, by.x = "CD_MUN", by.y = "Codigo")
class(mun_rs)
View(mun_rs)
rm(shp_mun) # deleting the shp_mun variable

# Aggregate a SpatialPolygon* object, optionally by combining 
# polygons that have the same attributes for one or more variables
mun_rs = raster::aggregate(mun_rs, by = "codDRS")
plot(mun_rs)

mun_rs <- merge(mun_rs, mun %>% dplyr::select(codDRS,nomDRS,Estado) %>% 
                  distinct(),
      by.x = "codDRS", by.y = "codDRS")

# passar as coordenadas de latitude e longitude para o objeto
proj4string(mun_rs) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
saveRDS(mun_rs, "mun_rs_shp.rds")

# Lendo os dados já salvos

mun_rs <- readRDS("./mun_rs_shp.rds")


state_popup <- paste0("<strong>Estado: </strong>", 
                      mun_rs$Estado, 
                      "<br><strong>RS: </strong>", 
                      mun_rs$nomDRS)
leaflet(data = mun_rs) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(popup = state_popup) 

# forma de ler as variáveis referentes a algum ponto
# de lat e lon do mapa
tmp <- coordinates(mun_rs)
head(tmp)
tmp2 <- raster::extract(mun_rs,matrix(tmp[1,],ncol=2))
tmp2


