library(shiny)
library(datasets)
library("tidyverse")
library("lubridate")
library("writexl")
library("httr")
library("readr")
library("readxl")
library("plotly")
library("fdANOVA")

calcula_estimador <- function(t_range, k_range, vetor_x, vetor_y, H) {
  out_ <- rep(NA, t_range)
  for(t in 1:t_range) {
    numerador <- 0
    denominador <- 0
    for(k in 1:k_range) {
      numerador <- numerador + gaussian_kernel((t - k)/H) * vetor_y[k] * vetor_x[k]
      denominador <- denominador + gaussian_kernel((t - k)/H) * vetor_x[k] * vetor_x[k]
    }
    out_[t] <- numerador/denominador
  }
  return(out_)
}

gaussian_kernel <- function(x) {
  (1 / (sqrt(2 * pi))) * exp(-(x^2/2))
}

estima_parametros <- function(dados, populacao, caso_corte = 50, kc = 1, kd = 1, kr = 1, expoente_H = 0.3, recuperados_sintetico = TRUE) {
  
  y <- (dados %>% dplyr::select(x = recovered, r = recovered, d = deaths, c = confirmed) %>% as.matrix)
  
  linha_corte <- which(y[,"c"] >= caso_corte)[1]
  
  N <- populacao
  
  n <- nrow(y) - linha_corte
  
  H <- n^expoente_H
  
  dt <- y[,"d"]/kd
  ct <- y[,"c"]/kc
  st <- N - ct
  
  if(recuperados_sintetico) {
    rt <- dplyr::lag(ct, 14) - dt
    rt <- ifelse(is.na(rt), 0, rt)
  } else {
    rt <- y[,"r"]/kr
  }
  
  xt <- ct - dt - rt
  
  dt <- dt[linha_corte:(n+linha_corte)]
  st <- st[linha_corte:(n+linha_corte)]
  xt <- xt[linha_corte:(n+linha_corte)]
  rt <- rt[linha_corte:(n+linha_corte)]
  ct <- ct[linha_corte:(n+linha_corte)]
  
  delta_dt <- diff(dt)
  delta_rt <- diff(rt)
  delta_xt <- diff(xt)
  delta_st <- diff(st)
  
  mu_est <- calcula_estimador(n, n, xt[-length(xt)], delta_dt, H)
  nu_est <- calcula_estimador(n, n, xt[-length(xt)], delta_rt, H)
  beta_est <- - calcula_estimador(n, n, xt[-length(xt)] * st[-length(st)]/N, delta_st, H)
  
  R_e <- beta_est / (mu_est + nu_est) * st[-length(st)]/N
  
  return(list(nu_t = nu_est,
              beta_t = beta_est, 
              mu_t = mu_est,
              R_e = R_e,
              xt = xt, 
              rt = rt, 
              st = st,
              dt = dt,
              primeira_data = first(dados$Data),
              datas = dados$Data[linha_corte:(n+linha_corte)][-length(dados$Data[linha_corte:(n+linha_corte)])]
  ))
  
}

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

# selecionando e criando as variáveis que vão ser usadas no modelo
mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered
  ) %>% ungroup %>% select(Estado,Município,Data,confirmed,deaths,recovered,
                           Populacao_estimada,IDHM_Renda)

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25
# tabela contendo os municípios separados por estado
EstMun <- mun %>% group_by(Estado,Município) %>% dplyr::summarise(count = n())
# lista que irá armazenar as curvas
estimadores_mun <- vector(mode = "list", length = dim(EstMun)[1])

for(i in 1:dim(EstMun)[1]){
  dados_mun <- mun %>% filter(Estado==EstMun$Estado[i] & Município==EstMun$Município[i])
  linha_corte <- which(dados_mun$confirmed >= caso_corte)
  # se o município não tiver 25 ou mais casos confirmados em dia algum,
  # linha_corte será NA, mas para obter as curvas para as duas últimas
  # semanas, precisamos de pelo menos 15 dias epidemiológicos, pois uma
  # diferença é tomada durante a estimação do modelo
  if(is.na(linha_corte[1]) | length(linha_corte)<15){
    estimadores_mun[[i]] <- list(Estado = EstMun$Estado[i], Município = EstMun$Município[i])
  }else{
    tmp <- estima_parametros(dados_mun, 
                             populacao = dados_mun$Populacao_estimada[1], 
                             caso_corte = 25, 
                             expoente_H = 0.3, 
                             recuperados_sintetico = TRUE)[c("nu_t", "beta_t", "mu_t", "R_e","datas")] %>%
      lapply(function(x) tail(x,14))
    estimadores_mun[[i]] <- append(tmp,list(Estado = rep(EstMun$Estado[i],14), Município = rep(EstMun$Município[i],14)))
  }
}

# checando para quantos municípios o modelo foi estimado
tmp <- lapply(estimadores_mun, length) %>% unlist(use.names=FALSE)
# usando a lista com as curvas, montamos um tibble contendo somente
# os municípios para os quais o modelo foi estimado
tmp2 <- lapply(estimadores_mun[which(tmp == 7)], as.data.frame)
estim_mun_df <- do.call("rbind",tmp2)
estim_mun_df <- mutate(estim_mun_df, Data=estim_mun_df$datas) %>%
  select(-datas)
suppressWarnings(
  estim_mun_df <- dplyr::left_join(estim_mun_df,mun,
                                 by=c("Estado","Município","Data")) %>%
    tibble
)


# definindo o server da aplicação
shinyServer(function(input, output) {
  
  # forma o texto que indicará qual estado e curva estão sendo
  # analisado na aplicação
  formulaText <- reactive({
    paste(input$curva, ": ", input$estado)
  })
  
  # Retorna o texto que será impresso como caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Gera o gráfico usando o pacote plotly
  output$mpgPlot <- renderPlotly({
    
    # criando variável que classifica os municípios de acordo com os quartis de IDH
    grup_quartis <- estim_mun_df %>% dplyr::filter(Estado==input$estado) %>%
      group_by(Município) %>% dplyr::summarize(med = unique(IDHM_Renda)) %>%
      dplyr::summarize(Q1 = quantile(med,.25)[[1]],
                       Q2 = quantile(med,.5)[[1]],
                       Q3 = quantile(med,.75)[[1]])
    mun_df_grupo <- estim_mun_df %>% dplyr::filter(Estado==input$estado) %>%
      group_by(Município) %>% dplyr::mutate(idh_grupo = ifelse(unique(IDHM_Renda)<grup_quartis$Q1,"ind<Q1",
                                                               ifelse(unique(IDHM_Renda)<grup_quartis$Q2,"Q1<ind<Q2",
                                                                      ifelse(unique(IDHM_Renda)<grup_quartis$Q3,"Q2<ind<Q3","ind>Q3"))))
    suppressWarnings(suppressMessages(
    tmp <- estim_mun_df %>% left_join(mun_df_grupo) %>% 
      dplyr::filter(Estado==input$estado) %>%
      dplyr::mutate_(vcurva = input$curva) %>%
      group_by(Estado,Data) %>%
      dplyr::summarize(vcurva = mean(vcurva))
    ))
    # curva dos municípios do estado usado no filtro e a respectiva
    # curva média
    suppressWarnings(suppressMessages(
    p <- estim_mun_df %>% left_join(mun_df_grupo) %>% 
      dplyr::filter(Estado==input$estado) %>%
      # usando standard evaluation com o mutate_ pra passar o nome da
      # variável como argumento
      dplyr::mutate_(vcurva = input$curva) %>%
      ggplot( aes(x=Data, y=vcurva,color=idh_grupo)) +
      geom_line(aes(group=Município), alpha = .4) +
      geom_line(data=tmp, alpha = .8, size = 1.5,color="black")
    ))
    ggplotly(p)
  })
  
  # essa variável funciona como uma função que será interativa,
  # para realizar ou não o teste fanova conforme o usuário
  # indica através do checkbox
  tmp <- reactive(
    # checando se o usuário deseja ou não realizar o teste fanova
    # nas medidas de IDH
    if(input$fanova){
      idh_g <- estim_mun_df %>% dplyr::filter(Estado==input$estado) %>%
        group_by(Município) %>% dplyr::summarize(med = median(IDHM_Renda)) %>%
        dplyr::summarize(median(med))
      
      mun_df_grupo <- estim_mun_df %>% dplyr::filter(Estado==input$estado) %>%
        group_by(Município) %>% dplyr::mutate(idh_grupo = ifelse(median(IDHM_Renda)>idh_g,1,0))
      
      grupos_fanova <- mun_df_grupo %>% select(Município,idh_grupo) %>%
        dplyr::group_by(Município) %>% unique %>% as.matrix
      
      # checando se todos os grupos tem mais de duas observações (condição
      # necessária para usar o pacote fdanova)
      if(prod(table(grupos_fanova[,2])>1)){
        dados_fanova <- mun_df_grupo %>% 
          dplyr::mutate_(vcurva = input$curva) %>%
          select(Município,Data,vcurva) %>%
          tidyr::spread(key = Município, value = vcurva) %>% 
          select(-Data) %>% as.matrix
        
        # Avaliando o efeito de IHHM_Renda
        mod <- fanova.tests(x = dados_fanova,
                            group.label = grupos_fanova[,2], test = "FP",
                            parallel = FALSE)
        return(summary(mod))
      }else{
        return("Não há observações suficientes nos dois grupos para fazer o teste")
      }
      
    }else{
      return(" ")
    }
  )
  output$summary <- renderPrint({tmp()})

  
})