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
mun_rs <- readRDS("./Rt_regsaude/mun_rs_coord.rds")
# View(mun_rs)
mun_rs$nomDRS <- as.character(mun_rs$nomDRS)

# bando de dados com nome de estados, municípios e códigos
# de RS correspondentes
est_mun_rs <- readRDS("./Rt_regsaude/est_mun_rs.rds")

# lendo os nomes dos arquivos na pasta com as curvas para
# diferentes datas
file_names <- list.files("./Rt_regsaude") %>% substring(1,10)
file_names <- file_names[str_detect(file_names,"2020")]
num_files <- length(file_names) + 1

# lendo as curvas já salvas para as RSs com pelo menos
# duas semanas epidemiológicas
estim_drs_df <- readRDS(paste("./Rt_regsaude/",
                              max(file_names),"_Rt_drs.rds",sep=""))
estim_drs_df <- left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,
                          by="codDRS")
# View(estim_drs_df)

# indices de mun_rs para os quais temos curvas estimadas
codDRS_tem_curva <- which(mun_rs$codDRS %in% estim_drs_df$codDRS)

# # variável com texto com nome do estado e da RS, para
# # ser usada na popup do mapa
# state_popup <- paste0("<strong>Estado: </strong>", 
#                       mun_rs[codDRS_tem_curva,]$Estado, 
#                       "<br><strong>RS: </strong>", 
#                       mun_rs[codDRS_tem_curva,]$nomDRS)

ultima_data <- max(estim_drs_df$date)
tmp <- estim_drs_df %>% filter(date == ultima_data) %>%
  select(codDRS,Rt)
tmp <- mun_rs[codDRS_tem_curva,] %>% 
  left_join(tmp, by=c("codDRS"))

# variável com texto com nome do estado e da RS, para
# ser usada na popup do mapa
state_popup <- paste0("<strong>Estado: </strong>", 
                      tmp$Estado, 
                      "<br><strong>RS: </strong>", 
                      tmp$nomDRS,
                      paste("<br>Rt (",ultima_data,"): ",sep=""),
                      round(tmp$Rt,2))
