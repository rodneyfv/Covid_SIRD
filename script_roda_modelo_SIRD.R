pacote <- function(p){
  if (!is.element(p, installed.packages()[,1])){
    message('Pacote ',p,' nao encontrado. Instalando..')
    install.packages(p, dep = TRUE)
  }  
  message('Carregando pacote ',p)
  require_worked <- try(require(p, character.only = TRUE))
  if(!require_worked) {
    install.packages(p, dep = TRUE)
  }
}

pacote("tidyverse")
pacote("lubridate")
pacote("writexl")
pacote("httr")
pacote("readr")
pacote("readxl")
pacote("plotly")

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

# Rodando a funcao para Brasil, IDH alto e IDH baixo

lista_dados <- readRDS("./lista_dados.rds")
mun <- readRDS("./dados_por_municipio.rds")
lista_populacoes <- readRDS("./lista_populacoes.rds")
ms_br <- readRDS("./dados_MS_brasil.rds") %>% 
  dplyr::select(Data = Data, confirmed = confirmed, deaths = deaths)

jh_br <- lista_dados$dados_originais %>% dplyr::filter(Country_Region == "Brazil")
ms_br <- ms_br %>% left_join(jh_br %>% dplyr::select(Data, recovered))

mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered
  ) %>% ungroup 

mun_belgica <- mun %>% dplyr::filter(Belgica == 1) %>% 
  dplyr::group_by(Data) %>% 
  dplyr::summarise(deaths = sum(deaths), confirmed = sum(confirmed), recovered = sum(recovered), pop = sum(Populacao_estimada), infected = sum(infected))

mun_india <- mun %>% dplyr::filter(Belgica == 0) %>% 
  dplyr::group_by(Data) %>% 
  dplyr::summarise(deaths = sum(deaths), confirmed = sum(confirmed), recovered = sum(recovered), pop = sum(Populacao_estimada), infected = sum(infected))

estimadores_belgica <- estima_parametros(mun_belgica, populacao = lista_populacoes$Brasil_IDH_alto, caso_corte = 50)[c("datas", "R_e")] %>% as.data.frame %>% dplyr::filter(datas > ymd("2020-04-14")) %>% dplyr::select(datas, Rt_IDH_alto = R_e)
estimadores_india <- estima_parametros(mun_india, populacao = lista_populacoes$Brasil_IDH_baixo, caso_corte = 50)[c("datas", "R_e")] %>% as.data.frame %>% dplyr::filter(datas > ymd("2020-04-14")) %>% dplyr::select(datas, Rt_IDH_baixo = R_e)
estimadores_br <- estima_parametros(ms_br, populacao = lista_populacoes$Brasil, caso_corte = 50, expoente_H = 0.3)[c("datas", "R_e")] %>% as.data.frame %>% dplyr::filter(datas > ymd("2020-04-14")) %>% dplyr::select(datas, Rt_Brasil = R_e)

estimadores_br_recuperados_divulgados <- estima_parametros(ms_br, populacao = 1, caso_corte = 50, expoente_H = 0.3, recuperados_sintetico = FALSE)[c("datas", "R_e")] %>% as.data.frame %>% dplyr::filter(datas > ymd("2020-04-14")) %>% dplyr::select(datas, Rt_Brasil_recuperados_divulgados = R_e)

plot_ly(x = estimadores_belgica$datas) %>% 
  add_trace(y = estimadores_br$Rt_Brasil, name = "Rt Brasil",
            mode = 'lines') %>% 
  add_trace(y = estimadores_belgica$Rt_IDH_alto, name = "Rt Brasil IDH alto",
            mode = 'lines') %>% 
  add_trace(y = estimadores_india$Rt_IDH_baixo, name = "Rt Brasil IDH baixo",
            mode = 'lines') %>% 
  layout(title = "Número efetivo de reprodução",  legend = list(orientation = "h"))

