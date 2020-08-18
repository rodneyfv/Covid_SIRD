
library(shiny)
library(leaflet)

ui <- fluidPage(
  # permite usar o show e hide do shinyjs pra esconder o
  # link pra download quando necessário
  shinyjs::useShinyjs(),
#  title = "Mapa das RSs do Brasil",
actionButton(inputId='ab1', label="Ir para o Painel de Previsões",
             icon = icon("th"),
             onclick ="location.href='https://insightdataanalysis.shinyapps.io/covidforecast/'",
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

p(),


actionButton(inputId='ab2', label="Ir para o Painel de Subnotificação",
             icon = icon("th"),
             onclick ="location.href='https://insightdataanalysis.shinyapps.io/reportacao/'",
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

  p(),

actionButton(inputId='ab3', label="Ir para o Painel de Atividade",
             icon = icon("th"),
             onclick ="location.href='https://insightdataanalysis.shinyapps.io/appatividade/'",
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
p(),

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
  # barra com opções de estados para escolher
  column(4,pickerInput(
    inputId = "estado_rs",
    label = "Estado", 
    options = pickerOptions(noneSelectedText = "Nenhum estado selecionado"),
    choices = est_mun_rs %>% dplyr::select(Estado) %>% distinct()
    )),
  # barra com opções de municípios para o estado escolhido
  column(4,uiOutput("get_rs")),
  # link para download dos dados
  column(4,downloadLink('downloaddata', 'Baixar dados da RS')),
  # lista dos municípios na RS escolhida
  column(4,verbatimTextOutput("RS_municipios")),
  # br(),
  # column(4,paste("Última atualização:",max(file_names))),
  br(),
  column(12,p(paste("Última atualização dos dados:",max(file_names))) ),
  br(),
  column(12,p("Informações: Neste aplicativo, o número de reprodução Rt é 
  calculado para diferentes regiões de saúde (RS), que são grupos de
  municípios apresentados nos dados fornecidos pelo Ministério
  da Saúde. A RS pode ser escolhida clicando nos círculos vermelhos
    do mapa ou através da barra lateral. As informações no gráfico
    correspondem ao modelo que usa os dados mais recentes disponíveis,
    mas que mostra valores até sete dias anteriores à última data mais recente 
    por razões técnicas. Uma planilha com os dados da RS escolhida pode ser 
              baixada pelo usuário no link destacado.")),
  # column(4,verbatimTextOutput("comandos")),
  # caixa com opções de curvas que podem ser analisadas
  # column(4,  selectInput("curva", "Curva:",
  #                        list("Rt","beta_hat","nu_hat","mu_hat"))),
  br()
)
