
library(shiny)
library(leaflet)

server <- function(input, output){
  
  # os dados das curvas é reactive porque vai mudar conforme o 
  # usuário escolhe datas diferentes
  estim_drs_df <- reactive({
    req(input$dateuser) # uma data precisa estar selecionada
    estim_drs_df <- readRDS(paste("../Rt_regsaude/",input$dateuser,"_Rt_drs.rds",sep=""))
    estim_drs_df <- left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,
                              by="codDRS")
    estim_drs_df <- estim_drs_df %>% mutate(codDRS = as.character(codDRS))
    estim_drs_df # retornando os resultatos para a data escolhida
  })
  # vetor que indica quais para quais RS's em mun_rs temos
  # curvas estimadas
  codDRS_tem_curva <- reactive({
    req(input$dateuser)
    # usamos o isolate porque essa parte depende dos valores
    # em estim_drs_df, que é variável
    isolate({
      codDRS_tem_curva <- which(mun_rs$codDRS %in% estim_drs_df()$codDRS)      
    })
    codDRS_tem_curva
  })
  # variável com texto com nome do estado e da RS, para
  # ser usada na popup do mapa
  state_popup <- reactive({
    req(input$dateuser)
    # usamos o isolate porque essa parte depende dos valores
    # em estim_drs_df, que é variável
    isolate({
      state_popup <- paste0("<strong>Estado: </strong>",
                            mun_rs[codDRS_tem_curva(),]$Estado,
                            "<br><strong>RS: </strong>",
                            mun_rs[codDRS_tem_curva(),]$nomDRS)
    })
    state_popup
  })
  
  # dados com coordenadas e nomes das RSs
  # o código da RS será usado como id pra identificar cliques
  data <- reactive({
    req(input$dateuser)
    isolate({
      data.frame(x=coordinates(mun_rs[codDRS_tem_curva(),])[,1],
                 y=coordinates(mun_rs[codDRS_tem_curva(),])[,2],
                 id=mun_rs[codDRS_tem_curva(),]$codDRS,
                 estado_id=mun_rs[codDRS_tem_curva(),]$Estado,
                 popup_id=state_popup())
    })
  })

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
  # mapa que coloca um círculo nas coordenadas das RSs para
  # as quais temos 2 semanas epidemiológicas e curvas estimadas
  output$mapa <- renderLeaflet({
      leaflet() %>%
      setView(lng=-62 , lat =-11, zoom=4) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(data=data(), ~x , ~y, layerId=~id, 
                     popup=~popup_id, 
                     radius=8 , color="black",  fillColor="red", 
                     stroke = TRUE, fillOpacity = 0.8)
    
  })
  
  # salvando o ponto no mapa onde ocorreu o clique
  observeEvent(input$mapa_marker_click,{
    # IMPORTANTE: note que a variável que guarda o mapa no
    # output se chama "mapa" e que é no argumento 
    # "_marker_click" dela que vamos obter os dados do clique
    data_of_click$clickedMarker <- input$mapa_marker_click
  })
  
  # criando o link que permite baixar os dados. usamos o
  # renderUI para deixar esse link aparecer somente depois
  # que algum ponto no mapa for selecionado
  output$get_the_item <- renderUI({
    if(is.null(data_of_click$clickedMarker)) return()
    req(input$mapa_marker_click)
    downloadLink('downloaddata', 'Baixar dados da RS') })
  
  # comando que torna nula a variável que armazena dados de
  # clique quando a data é alterada
  observeEvent(input$dateuser,{
    data_of_click$clickedMarker <- NULL
  })
  
  # Gera o gráfico usando o pacote plotly
  output$plot <- renderPlotly({
    # checa se algum ponto já foi selecionado antes de 
    # fazer o plot, se não, nada aparece
    req(input$mapa_marker_click)
    # se a variável com dados sobre clique for nula, a data foi
    # alterada, logo deixamos um vazio no espaço do gráfico
    if(is.null(data_of_click$clickedMarker)) return()
    # checando o código da RS onde o clique ocorreu
    my_place=data_of_click$clickedMarker$id
    # fazendo o gráfico da curva escolhida para a RS
    # identificada pelo código correspondente ao clique
    isolate({ # usa isolate porque depende de estim_drs_df, que é reactive
      p <- estim_drs_df() %>% filter(codDRS==my_place) %>%
        ggplot( aes(x=date, y=Rt)) +
        ylab("Rt") + xlab("Data") +
        labs(title=as.character(input$dateuser)) +
        geom_line() +
        theme_light()
    })
    ggplotly(p)
  })
  
  # criando o output com um link para download dos dados
  # sobre o ponto escolhido no mapa
  output$downloaddata <- downloadHandler(
    filename = function() {
      file = paste("data.xlsx", sep = "")
      return(file)
    },
    content = function(file) {
      req(input$mapa_marker_click)
      if(is.null(data_of_click$clickedMarker)) return()
      my_place=data_of_click$clickedMarker$id
      isolate({
        tmp <- estim_drs_df() %>% filter(codDRS==my_place)
      })
      openxlsx::write.xlsx(tmp,file,row.names = TRUE)
    })
  
  # output$comandos <- renderPrint({
  #   req(input$mapa_marker_click)
  #   my_place=data_of_click$clickedMarker$id
  #   #if(is.null(my_place)){my_place="35072"}
  #   print(my_place)
  #   print(data_of_click$clickedMarker)
  # })
  
}


