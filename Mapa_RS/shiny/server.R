
library(shiny)
library(leaflet)

server <- function(input, output, session){
  
  # os dados das curvas é reactive porque vai mudar conforme o 
  # usuário escolhe datas diferentes
  estim_drs_df <- reactive({
    req(input$dateuser) # uma data precisa estar selecionada
    if(input$dateuser %in% file_names){
      ultima_data0 <- input$dateuser
    }else{
      ultima_data0 <- max(file_names[which(file_names<=input$dateuser)])
    }
    estim_drs_df <- readRDS(paste("./Rt_regsaude/",
                                  ultima_data0,"_Rt_drs.rds",sep=""))
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
      ultima_data0 <- max(estim_drs_df()$date)
      tmp <- estim_drs_df() %>% filter(date == ultima_data0) %>%
        select(codDRS,Rt)
      tmp <- mun_rs[codDRS_tem_curva(),] %>% 
        left_join(tmp, by=c("codDRS"))
      
      state_popup <- paste0("<strong>Estado: </strong>",
                            mun_rs[codDRS_tem_curva(),]$Estado,
                            "<br><strong>RS: </strong>",
                            mun_rs[codDRS_tem_curva(),]$nomDRS,
                            paste("<br>Rt (",ultima_data0,"): ",sep=""),
                            round(tmp$Rt,2))
    })
    state_popup
  })
  
  # dados com coordenadas e nomes das RSs
  # o código da RS será usado como id pra identificar cliques
  data <- reactive({
    req(input$dateuser)
    isolate({
      data.frame(x = mun_rs[codDRS_tem_curva(),]$lat,
                 y = mun_rs[codDRS_tem_curva(),]$lon,
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
      setView(lng = -62 , lat = -11, zoom = 4) %>%
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
  
  # comando que torna nula a variável que armazena dados de
  # clique quando a data é alterada
  observeEvent(input$dateuser,{
    data_of_click$clickedMarker <- NULL
    output$plot <- renderPlotly({
      return()
    })
    # limpa os campos nas barras para selecionar estado e RS
    updateSliderInput(session, "selected_rs", value = "")
    updateSliderInput(session, "estado_rs", value = "")
    # lista os municípios que pertencem à RS escolhida
    output$RS_municipios <- renderPrint({
      cat(' ')
    })
    shinyjs::hide("downloaddata")
  })
  
  
  observeEvent(input$mapa_marker_click,{
    shinyjs::show("downloaddata")
    # Gera o gráfico usando o pacote plotly
      output$plot <- renderPlotly({
        # se a variável abaixo for nula, o gráfico sendo mostrdo
        # é referente ao estado e município nas caixas
        if(is.null(data_of_click$clickedMarker)) return()
        # checando o código da RS onde o clique ocorreu
        my_place=data_of_click$clickedMarker$id
        # limpa os campos nas barras para selecionar estado e RS
        updateSliderInput(session, "selected_rs", value = "")
        updateSliderInput(session, "estado_rs", value = "")
        # lista os municípios que pertencem à RS escolhida
        output$RS_municipios <- renderPrint({
          vec_mun <- est_mun_rs %>% filter(codDRS==my_place) %>%
            dplyr::select(Município) %>% distinct()
          cat('Municípios da RS escolhida:\n')  
          cat(paste(as.character(vec_mun$Município), sep="' '", collapse=", "))
        })
        
        # variável que vai ser usada para pegar o nome da RS
        tmp2 <- mun_rs %>% filter(codDRS==my_place)
        # aqui pegamos a curva da RS clicada. Antes checamos se
        # tem algo em my_place, pra evitar que dê erro quando
        # se seleciona estado/município nas caixas, o que deixa
        # my_place vazio automaticamente
        isolate({
          req(length(my_place)>0)
          tmp <- estim_drs_df() %>% filter(codDRS==my_place)
          })
        # fazendo o gráfico da curva escolhida para a RS
        # identificada pelo código correspondente ao clique
        p <- tmp %>%
          ggplot( aes(x=date, y=Rt)) +
          ylab("Rt") + xlab("Data") +
          ylim(0,4) + geom_hline(yintercept=1,size=0.8) +
          labs(title=paste(tmp2$Estado,": ",tmp2$nomDRS)) +
          geom_line(color="red") +
          theme_light()
        ggplotly(p)
  })
      
  })
  
    
  # variável reativa que guarda os municípios do estado selecionado
  # na primeira caixa
  vec_rs <- reactive({
    req(input$estado_rs)
    isolate({
      vec_rs <- mun_rs %>% filter(Estado==input$estado_rs) %>%
        filter(codDRS %in% mun_rs[codDRS_tem_curva(),]$codDRS) %>%
        dplyr::select(nomDRS) %>% distinct()
    })
    vec_rs
  })
  
  # output que formará a caixa com as RSs do estado
  # selecionado na primeira caixa
  output$get_rs <- renderUI({
    if(is.null(input$estado_rs)) return()
    req(input$dateuser)
    isolate({
      pickerInput(
        inputId = "selected_rs",
        label = "Região de Saúde (RS)",
        #selected = NULL,
        choices = vec_rs(),
      )
  })
  })
    
  # toda vez que a RS selecionada na segunda caixa
  # mudar, um novo gráfico irá aparecer. Se o estado for
  # alterado também mudará o gráfico pois o município da
  # segunda caixa mudará automaticamente
  observeEvent(input$selected_rs,{
    shinyjs::show("downloaddata")
    # anulamos a variável do clique, para tratar só com
    # os dados do estaddo/RS selecionados
    data_of_click$clickedMarker <- NULL
    # checando o codDRS referente ao estado e RS
    # selecionados nas caixas
    tmp <- mun_rs %>% filter(Estado==input$estado_rs) %>%
      filter(nomDRS==input$selected_rs)
    # variável que terá o nomDRS referente a variável acima
    tmp2 <- mun_rs %>% filter(codDRS==tmp$codDRS)
    # a variável de dados de clique será nula quando o usuário
    # alterar estado ou RS, e nesse caso, plotamos o
    # gráfico da RS correspondente
    if(is.null(data_of_click$clickedMarker)){
      output$plot <- renderPlotly({
        isolate({ # usa isolate porque depende de estim_drs_df, que é reactive
          p <- estim_drs_df() %>% filter(codDRS==tmp2$codDRS) %>%
            ggplot( aes(x=date, y=Rt)) +
            ylab("Rt") + xlab("Data") +
            ylim(0,4) + geom_hline(yintercept=1,size=0.8) +
            labs(title=paste(tmp2$Estado,": ",tmp2$nomDRS)) +
            geom_line(color="red") +
            theme_light()
        })
        ggplotly(p)
      })
      # lista os municípios que pertencem à RS escolhida
      output$RS_municipios <- renderPrint({
        vec_mun <- est_mun_rs %>% filter(Estado==input$estado_rs) %>%
          filter(codDRS==tmp2$codDRS) %>%
          dplyr::select(Município) %>% distinct()
        cat('Municípios da RS escolhida:\n')  
        cat(paste(as.character(vec_mun$Município), sep="' '", collapse=", "))
      })
    }
  })
  
  # criando o output com um link para download dos dados
  # sobre o ponto escolhido no mapa ou estado/município
  # escolhido nas caixas
  output$downloaddata <- downloadHandler(
    filename = function() {
      file = paste("data.xlsx", sep = "")
      return(file)
    },
    content = function(file) {
      # se a variável do clique for nula, o gráfico mostrado é
      # referente ao estado/município nas caixas, caso contrário,
      # foi escolhido após algum clique no mapa
      if(is.null(data_of_click$clickedMarker)){
        tmp <- mun_rs %>% filter(Estado==input$estado_rs) %>%
          filter(nomDRS==input$selected_rs)
        my_place <- tmp$codDRS
      }else{
        my_place <- data_of_click$clickedMarker$id
      }
      isolate({
        tmp <- estim_drs_df() %>% filter(codDRS==my_place)
      })
      openxlsx::write.xlsx(tmp,file,row.names = TRUE)
    })
  
  # output$comandos <- renderPrint({
  #   req(input$mapa_marker_click)
  #   print(input$mapa_marker_click)
  #   #my_place=data_of_click$clickedMarker$id
  #   #if(is.null(my_place)){my_place="35072"}
  #   #print(my_place)
  #   #print(data_of_click$clickedMarker)
  # })

}


