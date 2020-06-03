library(shiny)
library("tidyverse")
library("lubridate")
library("writexl")
library("httr")
library("readr")
library("readxl")
library("plotly")


# Lendo os dados dos municípios
mun <- readRDS("../dados_por_municipio.rds")
#mun <- mun %>% filter(Estado %in% c('SAO PAULO','RIO DE JANEIRO'))
# removendo municípios cuja soma de recuperados é NA
tmp <- mun %>% group_by(Estado,Município) %>% 
  summarise(soma = sum(confirmed), mort = sum(deaths),idh = IDHM_Renda[1]) %>%
  filter(is.na(soma) | is.na(idh) | is.na(mort))
mun <- mun %>% group_by(Estado,Município) %>%
  filter(!(is.element(Estado, tmp$Estado) & is.element(Município,tmp$Município))) %>%
  ungroup

# Definindo a UI para o aplicativo das curvas e fanova por estado
shinyUI(pageWithSidebar(
  
  # Título da aplicação
  headerPanel("Curvas"),
  
  # Barra lateral que controla qual estado e qual curva queremos
  # analisar e se desejamos fazer o teste fanova dos grupos
  # separados como > ou < que o IDH mediano
  sidebarPanel(
    selectInput("estado", "Estado:",
                as.list(unique(mun$Estado))),
    selectInput("curva", "Curva:",
                list("nu_t", "beta_t", "mu_t", "R_e")),
    
    checkboxInput("fanova", "FANOVA", FALSE)
  ),

  # mostra no caption qual estado e qual curva estão sendo analisados,
  # o gráfico dessa curva para os municípios desse estado e, quando
  # indicado e possível, o sumário do teste fanova para os grupos
  # feitos pelo IDH
  mainPanel(
    h3(textOutput("caption")),
    # o output aqui vai ser um gráfico plotly
    plotlyOutput("mpgPlot"),
    
    verbatimTextOutput("summary")
  )
  
))