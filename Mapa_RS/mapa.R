

library(rgdal)
library(dplyr)
library(tidyverse)
library(stringi)
library(RColorBrewer)
library(leaflet)
library(maptools)
library(sp)
library(raster)

shp_mun <- readOGR("/media/rodney/Arquivos/Datasets/Dados_Municipios_BR/br_municipios/BR_Municipios_2019.shp", 
                   stringsAsFactors=TRUE, encoding="UTF-8")
class(shp_mun)
#head(shp_mun$CD_MUN)
# usando somente os 6 primeiros dígitos do código dos municípios
# para deixar como estão nos dados do Min. da Saúde
shp_mun$CD_MUN <- str_sub(as.character(shp_mun$CD_MUN),
                          start = 1,end = 6)
#head(shp_mun$CD_MUN)
shp_mun@data <- shp_mun@data %>% dplyr::mutate(Codigo = CD_MUN) %>%
  dplyr::select(-CD_MUN)
#View(shp_mun@data)

mun <- readRDS("../dados_por_municipio.rds")
#head(mun$Codigo)

# selecionando as variáveis que vão ser usadas
mun <- mun %>% 
 dplyr::select(Estado,Município,Codigo,codDRS,nomDRS) %>% 
  dplyr::distinct()
#View(mun)

mun_rs <- sp::merge(shp_mun,mun, by.x = "Codigo", by.y = "Codigo")
#plot(mun_rs)
#View(mun_rs@data)
rm(shp_mun) # deleting the shp_mun variable

# Aggregate a SpatialPolygon* object, optionally by combining 
# polygons that have the same attributes for one or more variables
mun_rs = raster::aggregate(mun_rs, by = "codDRS")
#plot(mun_rs)
#View(mun_rs@data)

mun_rs <- merge(mun_rs, mun %>% dplyr::select(codDRS,nomDRS,Estado) %>% 
                  distinct(),
                by.x = "codDRS", by.y = "codDRS")

# passar as coordenadas de latitude e longitude para o objeto
proj4string(mun_rs) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

mun_rs@data <- mun_rs@data %>% dplyr::mutate(codDRS = as.character(codDRS))

# tomando as coordenadas das regiões em mun_rs
tmp <- raster::coordinates(mun_rs)
#View(tmp)
#dim(tmp)
#dim(mun_rs@data)
#head(mun_rs@data)
#head(tmp)
#raster::extract(mun_rs,head(tmp))

# montando um dataframe com as coordenadas de mun_rs e suas
# variáveis correspondentes
mun_rs_coord <- data.frame(lat = tmp[,1], lon = tmp[,2],
                           codDRS = mun_rs@data$codDRS,
                           nomDRS = mun_rs@data$nomDRS,
                           Estado = mun_rs@data$Estado)
mun_rs_coord <- mun_rs_coord %>% dplyr::mutate(codDRS = as.character(codDRS))
saveRDS(mun_rs_coord, "./Rt_regsaude/mun_rs_coord.rds")


