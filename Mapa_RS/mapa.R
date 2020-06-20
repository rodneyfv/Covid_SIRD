

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
head(shp_mun$CD_MUN)
shp_mun@data <- shp_mun@data %>% dplyr::mutate(Codigo = CD_MUN) %>%
  dplyr::select(-CD_MUN)
View(shp_mun@data)

mun <- readRDS("./dados_por_municipio.rds")
head(mun$Codigo)

# selecionando as variáveis que vão ser usadas
mun <- mun %>% 
 dplyr::select(Estado,Município,Codigo,codDRS,nomDRS) %>% 
  dplyr::distinct()
#View(mun)

mun_rs <- sp::merge(shp_mun,mun, by.x = "Codigo", by.y = "Codigo")
class(mun_rs)
View(mun_rs)
plot(mun_rs)
View(mun_rs@data)
rm(shp_mun) # deleting the shp_mun variable

# Aggregate a SpatialPolygon* object, optionally by combining 
# polygons that have the same attributes for one or more variables
mun_rs = raster::aggregate(mun_rs, by = "codDRS")
plot(mun_rs)
View(mun_rs@data)

mun_rs <- merge(mun_rs, mun %>% dplyr::select(codDRS,nomDRS,Estado) %>% 
                  distinct(),
                by.x = "codDRS", by.y = "codDRS")

# passar as coordenadas de latitude e longitude para o objeto
proj4string(mun_rs) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

mun_rs@data <- mun_rs@data %>% dplyr::mutate(codDRS = as.character(codDRS))
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

