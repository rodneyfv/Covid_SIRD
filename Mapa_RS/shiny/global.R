# arquivos/pacotes necessários

library(pacman)

# Carregando os pacotes
library(rgdal)
library(dplyr)
library(tidyverse)
library(stringi)
library(RColorBrewer)
library(leaflet)
library(maptools)
library(sp)
library(raster)

# lendo os dados das RSs em SpatialPolygonsDataFrame
mun_rs <- readRDS("../mun_rs_shp.rds")
# a variável código precisa ser caractere pra não dar
# problema quando for usada como id pra identificar cliques
mun_rs$codDRS <- as.character(mun_rs$codDRS)

# lendo as curvas já salvas para as RSs com pelo menos
# duas semanas epidemiológicas
estim_drs_df <- readRDS("../curvas_drs_df.rds")
# deixamos o código das RSs aqui também para poder compara
# com os códigos em mun_rs
estim_drs_df <- estim_drs_df %>% mutate(codDRS = as.character(codDRS))

# checando para quais DRs temos curvas estimadas e
# deixando só estas no spatialdataframe
codDRS_tem_curva <- mun_rs$codDRS %in% estim_drs_df$codDRS
mun_rs <- mun_rs[codDRS_tem_curva,]

# variável com texto com nome do estado e da RS, para
# ser usada na popup do mapa
state_popup <- paste0("<strong>Estado: </strong>", 
                      mun_rs$Estado, 
                      "<br><strong>RS: </strong>", 
                      mun_rs$nomDRS)

