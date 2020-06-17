
library(shiny)
library(leaflet)

server <- function(input, output){
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
    # checando o código da RS onde o clique ocorreu
    my_place=data_of_click$clickedMarker$id
    # fazendo o gráfico da curva escolhida para a RS
    # identificada pelo código correspondente ao clique
    p <- estim_drs_df %>% filter(codDRS==my_place) %>%
      dplyr::mutate_(vcurva = input$curva) %>%
      ggplot( aes(x=date, y=vcurva)) +
      ylab(as.character(input$curva)) +
      geom_line()
    ggplotly(p)
  })
  
  # output$comandos <- renderPrint({
  #   my_place=data_of_click$clickedMarker$id
  #   if(is.null(my_place)){my_place="35072"}
  #   print(my_place)
  #   print(data_of_click$clickedMarker)
  # })
  
}


