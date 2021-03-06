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
table(tmp)
# usando a lista com as curvas, montamos um tibble contendo somente
# os municípios para os quais o modelo foi estimado
tmp2 <- lapply(estimadores_mun[which(tmp == 7)], as.data.frame)
estim_mun_df <- do.call("rbind",tmp2)
estim_mun_df <- mutate(estim_mun_df, Data=estim_mun_df$datas) %>%
  select(-datas)
estim_mun_df <- dplyr::left_join(estim_mun_df,mun,by=c("Estado","Município","Data")) %>%
  tibble
#View(estim_mun_df)
rm(EstMun); rm(estimadores_mun)

# checando o número de municípios que sobraram por estado
tmp <- estim_mun_df %>% group_by(Estado,Município) %>% 
  dplyr::summarise(count = n())
table(tmp$Estado)

# curva dos municípios do estado usado no filtro e a respectiva
# curva média, usando a função ggploty
tmp <- estim_mun_df %>% dplyr::filter(Estado=='SAO PAULO') %>%
  group_by(Estado,Data) %>%
  dplyr::summarize(R_e = mean(R_e))
p <- estim_mun_df %>% dplyr::filter(Estado=='SAO PAULO') %>%
  ggplot( aes(x=Data, y=R_e,color=Estado)) +
  geom_line(aes(group=Município), alpha = .4) +
  geom_line(data=tmp, alpha = .8, size = 1.5,color="black")
#  coord_cartesian( ylim = c(0, 20))
ggplotly(p)

# criando uma variável dummy nos dados que assume 1 se o município
# tem IDH maior que a mediana e 0 caso contrário
tmp <- estim_mun_df %>% dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  group_by(Município) %>% dplyr::summarize(med = median(IDHM)) %>%
  dplyr::summarize(median(med))
mun_df_grupo <- estim_mun_df %>% dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  group_by(Município) %>% dplyr::mutate(idh_grupo = ifelse(median(IDHM)>tmp,1,0))
View(mun_df_grupo)

# criando uma matriz para ser usada na função fanova.tests, tendo as
# curvas observadas nas colunas e pontos de discretização nas linhas
tmp <- mun_df_grupo %>% select(Município,Data,R_e) %>%
  tidyr::spread(key = Município, value = R_e)
dados_fanova <- tmp %>% select(-Data) %>% as.matrix
rownames(dados_fanova) <- tmp$Data %>% as.character
# vetor com os grupos referente a cada curva observada
grupos_fanova <- mun_df_grupo %>% select(Município,idh_grupo) %>%
  dplyr::group_by(Município) %>% unique %>% as.matrix
# checando se todos os grupos tem mais de duas observações (condição
# necessária para usar o pacote fdanova)
prod(table(grupos_fanova[,2])>1)

# Avaliando o efeito de IHHM_Renda
library("fdANOVA")
plotFANOVA(x = dados_fanova, group.label = grupos_fanova[,2],
           means = TRUE)
fanova <- fanova.tests(x = dados_fanova,
                       group.label = grupos_fanova[,2], test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)


grupos_fanova <- mun_df_grupo %>% select(Município,idh_grupo) %>%
  dplyr::group_by(Município) %>% unique %>% as.matrix
# checando se todos os grupos tem mais de duas observações (condição
# necessária para usar o pacote fdanova)
prod(table(grupos_fanova[,2])>1)


# criando variável que classifica os municípios de acordo com os quartis de
# IDHM para todos os municípios do Brasil
grup_quartis <- mun %>%
  group_by(Estado,Município) %>% dplyr::distinct(IDHM)
grup_quartis <- tibble(Q1 = quantile(grup_quartis$IDHM,.25)[[1]],
                   Q2 = quantile(grup_quartis$IDHM,.5)[[1]],
                   Q3 = quantile(grup_quartis$IDHM,.75)[[1]])
mun_df_grupo <- estim_mun_df %>% dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  group_by(Município) %>% dplyr::mutate(idh_grupo = ifelse(unique(IDHM)<grup_quartis$Q1,"idh<Q1",
                                                           ifelse(unique(IDHM)<grup_quartis$Q2,"Q1<idh<Q2",
                                                                  ifelse(unique(IDHM)<grup_quartis$Q3,"Q2<idh<Q3","idh>Q3"))))
View(mun_df_grupo)

# vetor com os grupos referente a cada curva observada
grupos_fanova <- mun_df_grupo %>% select(Município,idh_grupo) %>%
  dplyr::group_by(Município) %>% unique %>% as.matrix
# checando se todos os grupos tem mais de duas observações (condição
# necessária para usar o pacote fdanova)
prod(table(grupos_fanova[,2])>1)

# gráfico das curvas dos municípios separadas pelos grupo e com a curva
# média plotada junto
tmp1 <- estim_mun_df %>% left_join(mun_df_grupo) %>% 
  dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  group_by(Data) %>%
  dplyr::summarize(R_e = mean(R_e))
tmp2 <- estim_mun_df %>% left_join(mun_df_grupo) %>% 
  dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  group_by(Data,idh_grupo) %>%
  dplyr::summarize(R_e = mean(R_e))
p <- estim_mun_df %>% left_join(mun_df_grupo) %>% 
  dplyr::filter(Estado=='RIO DE JANEIRO') %>%
  ggplot( aes(x=Data, y=R_e,color=idh_grupo)) +
  geom_line(aes(group=Município), alpha = .5, size = .5,linetype="dotted") +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=1))) +
  geom_line(data=tmp2, alpha = 1, size = 1.5) +
  geom_line(data=tmp1, alpha = 1, size = 2,color="black")
ggplotly(p)

plotFANOVA(x = dados_fanova, group.label = grupos_fanova[,2],
           means = TRUE)
fanova <- fanova.tests(x = dados_fanova,
                       group.label = grupos_fanova[,2], test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)

#*********************************#

estim_mun_df %>% filter(Estado=="PARA") %>%
  select(Município) %>% distinct() %>% View()

p <- estim_mun_df %>% 
  dplyr::filter(Estado=='PARA',Município=="Santarém") %>%
  ggplot( aes(x=Data, y=R_e)) +
  geom_line(aes(group=Município))
#  coord_cartesian( ylim = c(0, 20))
ggplotly(p)

