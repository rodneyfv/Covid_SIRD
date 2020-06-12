
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

#********************#
# Funcoes definidas 
gaussian_kernel <- function(x) {
  (1 / (sqrt(2 * pi))) * exp(-(x^2/2))
}

# Estimador por Kernel
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

estima_parametros <- function(dados, populacao, caso_corte = 50, kc = 1, kd = 1, kr = 1, expoente_H = 0.3, 
                              recuperados_sintetico = TRUE) {
  y <- (dados %>% dplyr::select(r = recovered, d = deaths, c = confirmed) %>% as.matrix)
  
  linha_corte <- which(y[,"c"] >= caso_corte)[1]
  
  N <- populacao
  
  n <- nrow(y) - linha_corte
  
  H <- n^expoente_H
  
  dt <- y[,"d"]/kd # dead
  ct <- y[,"c"]/kc # confirmed
  st <- N - ct # susceptible
  # recovered
  if(recuperados_sintetico) {
    # recuperados hoje := confirmados 14 atrás menos o numero de mortos hoje
    rt <- dplyr::lag(ct, 14) - dt
    rt <- ifelse(is.na(rt), 0, rt)
  } else {
    rt <- y[,"r"]/kr
  }
  # infected
  xt <- ct - dt - rt
  
  dt <- dt[linha_corte:(n+linha_corte)] # dead
  st <- st[linha_corte:(n+linha_corte)] # susceptible
  xt <- xt[linha_corte:(n+linha_corte)] # infected
  rt <- rt[linha_corte:(n+linha_corte)] # recovered
  ct <- ct[linha_corte:(n+linha_corte)] # confirmed
  
  delta_dt <- diff(dt)
  delta_rt <- diff(rt)
  delta_xt <- diff(xt)
  delta_st <- diff(st)
  
  mu_est <- calcula_estimador(n, n, xt[-length(xt)], delta_dt, H)
  mu_est <- pmin(pmax(mu_est, 1/21 * 0.06), 1/7 * 0.06)
  mu_est_fixo <- rep(1/14 * 0.06, n)
  mu_est <- 0.75 * mu_est_fixo + 0.25 * mu_est
  #mu_est <- mu_est_fixo
  
  nu_est <- calcula_estimador(n, n, xt[-length(xt)], delta_rt, H)
  nu_est <- pmin(pmax(nu_est, 1/28 * 0.94), 1/7 * 0.94)
  nu_est_fixo <- rep(1/14 * 0.94, n)
  nu_est <- 0.75 * nu_est_fixo + 0.25 * nu_est
  #nu_est <- nu_est_fixo
  
  beta_est <- - calcula_estimador(n, n, xt[-length(xt)] * st[-length(st)]/N, delta_st, H)
  
  beta_est <- ifelse(beta_est < 0, 0, beta_est)
  
  R_e <- beta_est / (mu_est + nu_est) * st[-length(st)]/N
  
  nu_ocorrido <- delta_rt/xt[-length(xt)]
  mu_ocorrido <- delta_dt/xt[-length(xt)]
  beta_ocorrido <- (delta_xt + delta_rt)/xt[-length(xt)]
  
  R_ocorrido <- beta_ocorrido / (mu_ocorrido + nu_ocorrido) * st[-length(st)]/N
  
  return(list(nu_t = nu_est,
              beta_t = beta_est, 
              mu_t = mu_est,
              R_e = R_e,
              nu_ocorrido = nu_ocorrido,
              mu_ocorrido = mu_ocorrido,
              beta_ocorrido = beta_ocorrido,
              R_ocorrido = R_ocorrido,
              xt = xt, 
              rt = rt, 
              st = st,
              dt = dt,
              ct = ct,
              primeira_data = first(dados$Data),
              datas = dados$Data[linha_corte:(n+linha_corte)][-length(dados$Data[linha_corte:(n+linha_corte)])]
  ))
  
}
#********************#

# Lendo os dados dos municípios
mun <- readRDS("../dados_por_municipio.rds")
#View(mun)

# removendo municípios cuja soma de recuperados é NA
tmp <- mun %>% group_by(Estado,Município) %>% 
  summarise(soma = sum(confirmed), mort = sum(deaths),idh = IDHM[1]) %>%
  filter(is.na(soma) | is.na(idh) | is.na(mort))
#View(tmp) 
table(tmp$Estado)
dim(tmp)[1] # número de municípios removidos
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
                           Populacao_estimada,IDHM)

#******************************#

# filtrando o estado e município desejados
dados_mun <- mun %>% filter(Estado=="PARA" & Município=="Santarém")
estimadores_mun <- estima_parametros(dados_mun, 
                         populacao = dados_mun$Populacao_estimada[1], 
                         caso_corte = 25, 
                         expoente_H = 0.3, 
                         recuperados_sintetico = TRUE)[c("nu_t", "beta_t", "mu_t", "R_e","datas")]

# usando a lista com as curvas, montamos um dataframe contendo somente
# o município desejado
tmp2 <- estimadores_mun %>% as.data.frame
estim_mun_df <- do.call("cbind",tmp2)
estim_mun_df <- as.data.frame(estim_mun_df)
estim_mun_df <- mutate(estim_mun_df, Data=estimadores_mun$datas) %>%
  select(-datas)

# retirando as estimativas da última semana devido ao efeito
# de borda
ultima_data_menos_7 <- max(estim_mun_df$Data) - 7
estim_mun_df <- estim_mun_df %>% 
  filter(Data<=ultima_data_menos_7)

p <- estim_mun_df %>%
  ggplot( aes(x=Data, y=R_e)) +
  geom_line() + 
  labs(title="R_e")
#  coord_cartesian( ylim = c(0, 20))
ggplotly(p)

p_Re <- estim_mun_df %>%
  ggplot( aes(x=Data, y=R_e)) +
  geom_line() + 
  labs(title="R_e")
p_beta <- estim_mun_df %>%
  ggplot( aes(x=Data, y=beta_t)) +
  geom_line() + 
  labs(title="beta_t")
p_nu <- estim_mun_df %>%
  ggplot( aes(x=Data, y=nu_t)) +
  geom_line() + 
  labs(title="nu_t")
p_mu <- estim_mun_df %>%
  ggplot( aes(x=Data, y=mu_t)) +
  geom_line() + 
  labs(title="mu_t")

library(gridExtra)
grid.arrange(p_Re,p_beta,p_nu,p_mu, nrow=2)




