
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
  column(4,dateInput("dateuser", "Data:", value = Sys.Date(),
                     max=Sys.Date(),min=Sys.Date()-num_files+2)),
  #downloadLink('downloaddata', 'Baixar dados da RS'),
  #column(4,verbatimTextOutput("comandos")),
  uiOutput("get_the_item"),
  # caixa com opções de curvas que podem ser analisadas
  # column(4,  selectInput("curva", "Curva:",
  #                        list("Rt","beta_hat","nu_hat","mu_hat"))),
  br()
)
