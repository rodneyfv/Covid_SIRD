
library(shiny)
library(leaflet)

ui <- fluidPage(
#  title = "Mapa das RSs do Brasil",
  br(),
  # mapa leaflet
  column(8,leafletOutput("mapa", height="600px")),
  # gráfico plotly
  column(4,plotlyOutput("plot", height="300px")),
  br(),
  # caixa que deixa o usuário escolher a data dos dados, sendo o
  # default a última data na pasta com esses arquivos, cujos nomes
  # estão armazenados na variável global file_names
  column(4,dateInput("dateuser", "Data:", value = max(file_names),
                       max=max(file_names),min=min(file_names))),
  column(4,pickerInput(
    inputId = "estado_rs",
    label = "Estado",
    selected = NULL,
    choices = est_mun_rs %>% dplyr::select(Estado) %>% distinct()
    )),
  column(4,uiOutput("get_mun")),
  # link para download dos dados. está com uiOutput porque ele
  # usa renderUI, para aparecer somente quando um ponto é clicado
  uiOutput("get_the_item"),
  
  # column(4,verbatimTextOutput("comandos")),
  # caixa com opções de curvas que podem ser analisadas
  # column(4,  selectInput("curva", "Curva:",
  #                        list("Rt","beta_hat","nu_hat","mu_hat"))),
  br()
)
