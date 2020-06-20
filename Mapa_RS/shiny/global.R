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
library(plotly)
library(openxlsx)

# lendo os dados das RSs em SpatialPolygonsDataFrame
mun_rs <- readRDS("../Rt_regsaude/mun_rs_shp.rds")
# View(mun_rs@data)

file_names <- list.files("../Rt_regsaude") %>% substring(1,10)
file_names <- file_names[which(file_names!="mun_rs_shp")]
num_files <- length(file_names) + 1


# lendo as curvas já salvas para as RSs com pelo menos
# duas semanas epidemiológicas
estim_drs_df <- readRDS(paste("../Rt_regsaude/",
                              max(file_names),"_Rt_drs.rds",sep=""))
estim_drs_df <- left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,
                          by="codDRS")
# View(estim_drs_df)

# indices de mun_rs para os quais temos curvas estimadas
codDRS_tem_curva <- which(mun_rs$codDRS %in% estim_drs_df$codDRS)

# variável com texto com nome do estado e da RS, para
# ser usada na popup do mapa
state_popup <- paste0("<strong>Estado: </strong>", 
                      mun_rs[codDRS_tem_curva,]$Estado, 
                      "<br><strong>RS: </strong>", 
                      mun_rs[codDRS_tem_curva,]$nomDRS)


