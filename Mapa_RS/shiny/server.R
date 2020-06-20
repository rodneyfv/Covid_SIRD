
library(shiny)
library(leaflet)

server <- function(input, output){
  
  observeEvent(input$dateuser,{
    estim_drs_df <- readRDS(paste("../Rt_regsaude/",input$dateuser,"_Rt_drs.rds",sep=""))
    estim_drs_df <- left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,
                              by="codDRS")
    estim_drs_df <- estim_drs_df %>% mutate(codDRS = as.character(codDRS))
    codDRS_tem_curva <- mun_rs$codDRS %in% estim_drs_df$codDRS
    mun_rs <- mun_rs[codDRS_tem_curva,]
    
    # variável com texto com nome do estado e da RS, para
    # ser usada na popup do mapa
    state_popup <- paste0("<strong>Estado: </strong>", 
                          mun_rs$Estado, 
                          "<br><strong>RS: </strong>", 
                          mun_rs$nomDRS)
  })

  
  # dados com coordenadas e nomes das RSs
  # o código da RS será usado como id pra identificar cliques
  data=data.frame(x=coordinates(mun_rs)[,1],
                  y=coordinates(mun_rs)[,2],
                  id=mun_rs$codDRS,
                  estado_id=mun_rs$Estado,
                  popup_id=state_popup)

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
  # mapa que coloca um círculo nas coordenadas das RSs para
  # as quais temos 2 semanas epidemiológicas e curvas estimadas
  output$mapa <- renderLeaflet({
      leaflet() %>%
      setView(lng=-62 , lat =-11, zoom=4) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    addCircleMarkers(data=data, ~x , ~y, layerId=~id, 
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
  
  # Gera o gráfico usando o pacote plotly
  output$plot <- renderPlotly({
    # checa se algum ponto já foi selecionado antes de 
    # fazer o plot, se não, nada aparece
    req(input$mapa_marker_click)
    # checando o código da RS onde o clique ocorreu
    my_place=data_of_click$clickedMarker$id
    # fazendo o gráfico da curva escolhida para a RS
    # identificada pelo código correspondente ao clique
    p <- estim_drs_df %>% filter(codDRS==my_place) %>%
      ggplot( aes(x=date, y=Rt)) +
      ylab("Rt") + xlab("Data") +
      labs(title=as.character(input$dateuser)) +
      geom_line()
    ggplotly(p)
  })
  
  output$downloaddata <- downloadHandler(
    req(input$mapa_marker_click),
    filename = function() {
      file = paste("data.xlsx", sep = "")
      return(file)
    },
    content = function(file) {
      my_place=data_of_click$clickedMarker$id
      tmp <- filter(estim_drs_df, codDRS==my_place)
      openxlsx::write.xlsx(tmp,file,row.names = TRUE)
    })
  # output$comandos <- renderPrint({
  #   my_place=data_of_click$clickedMarker$id
  #   if(is.null(my_place)){my_place="35072"}
  #   print(my_place)
  #   print(data_of_click$clickedMarker)
  # })
  
}


