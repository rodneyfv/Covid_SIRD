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
library(shinyWidgets)

# lendo os dados das RSs em um dataframe com as coordenadas e
# codDRS, nomDRS e estado correspondentes
mun_rs <- readRDS("./shiny/Rt_regsaude/mun_rs_coord.rds")
# View(mun_rs)

# bando de dados com nome de estados, municípios e códigos
# de RS correspondentes
est_mun_rs <- readRDS("./shiny/Rt_regsaude/est_mun_rs.rds")

# lendo os nomes dos arquivos na pasta com as curvas para
# diferentes datas
file_names <- list.files("./shiny/Rt_regsaude") %>% substring(1,10)
file_names <- file_names[str_detect(file_names,"2020")]
num_files <- length(file_names) + 1

# lendo as curvas já salvas para as RSs com pelo menos
# duas semanas epidemiológicas
estim_drs_df <- readRDS(paste("./shiny/Rt_regsaude/",
                              max(file_names),"_Rt_drs.rds",sep=""))
estim_drs_df <- left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,
                          by="codDRS")
# View(estim_drs_df)


estim_drs_df_esp <- estim_drs_df %>% filter(Estado == "MARANHAO") %>% 
  filter(date == max(date))
#View(estim_drs_df_esp)

mun_rs_esp <- mun_rs %>% filter(Estado == "MARANHAO")
# indices de mun_rs para os quais temos curvas estimadas
codDRS_tem_curva <- which(mun_rs_esp$codDRS %in% estim_drs_df_esp$codDRS)

state_popup_esp <- paste0(round(estim_drs_df_esp$Rt,2))

data_leaflet <- data.frame(x = mun_rs_esp[codDRS_tem_curva,]$lat,
               y = mun_rs_esp[codDRS_tem_curva,]$lon,
               id=mun_rs_esp[codDRS_tem_curva,]$codDRS,
               estado_id=mun_rs_esp[codDRS_tem_curva,]$Estado,
               popup_id=state_popup_esp)


leaflet() %>%
  setView(lng=-45 , lat =-5, zoom=5.5) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
  addMarkers(data=data_leaflet,
    lng = ~x, lat = ~y,
    label = ~popup_id,
    labelOptions = labelOptions(noHide = TRUE, textsize='9px'))
  # addCircleMarkers(data=data_leaflet, ~x , ~y, layerId=~id, 
  #                  popup=~popup_id, 
  #                  radius=8 , color="black",  fillColor="red", 
  #                  stroke = TRUE, fillOpacity = 0.8)

