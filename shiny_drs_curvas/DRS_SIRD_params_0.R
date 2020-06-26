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
pacote("stringi")
library("fdANOVA")

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
  ) %>% ungroup %>% select(Estado,Município,codDRS, Data,confirmed,deaths,
                           recovered, Populacao_estimada, IDHM)

# agregando os dados por DRS, o que vai remover a variável Município.
mun <- mun %>% group_by(Estado,Data,codDRS) %>%
  dplyr::summarise(confirmed=sum(confirmed), deaths=sum(deaths),
                   recovered=sum(recovered), 
                   populacao=sum(Populacao_estimada),
                   IDHM=mean(IDHM)) %>% ungroup %>% arrange(Estado,codDRS)
View(mun)

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25
# tabela contendo os municípios separados por estado
EstMun <- mun %>% group_by(Estado,codDRS) %>% dplyr::summarise(count = n())
# lista que irá armazenar as curvas
estimadores_mun <- vector(mode = "list", length = dim(EstMun)[1])

for(i in 1:dim(EstMun)[1]){
  dados_mun <- mun %>% filter(Estado==EstMun$Estado[i] & codDRS==EstMun$codDRS[i])
  linha_corte <- which(dados_mun$confirmed >= caso_corte)
  # se o município não tiver 25 ou mais casos confirmados em dia algum,
  # linha_corte será NA, mas para obter as curvas para as duas últimas
  # semanas, precisamos de pelo menos 15 dias epidemiológicos, pois uma
  # diferença é tomada durante a estimação do modelo
  if(is.na(linha_corte[1]) | length(linha_corte)<15){
    estimadores_mun[[i]] <- list(Estado = EstMun$Estado[i], 
                                 codDRS = EstMun$codDRS[i])
  }else{
    tmp <- estima_parametros(dados_mun, 
                             populacao = dados_mun$populacao[1], 
                             caso_corte = 25, 
                             expoente_H = 0.3, 
                             recuperados_sintetico = TRUE)[c("nu_t", "beta_t", 
                                                             "mu_t", "R_e",
                                                             "datas")] %>%
      lapply(function(x) tail(x,14))
    estimadores_mun[[i]] <- append(tmp,list(Estado = rep(EstMun$Estado[i],14), 
                                            codDRS = rep(EstMun$codDRS[i],14)))
  }
}

# checando para quantas DRSs o modelo foi estimado
tmp <- lapply(estimadores_mun, length) %>% unlist(use.names=FALSE)
table(tmp)
# usando a lista com as curvas, montamos um tibble contendo somente
# as DRSs para as quais o modelo foi estimado
tmp2 <- lapply(estimadores_mun[which(tmp == 7)], as.data.frame)
estim_mun_df <- do.call("rbind",tmp2)
estim_mun_df <- mutate(estim_mun_df, Data=estim_mun_df$datas) %>%
  select(-datas)
estim_mun_df <- dplyr::left_join(estim_mun_df,mun,by=c("Estado","codDRS","Data")) %>%
  tibble
#View(estim_mun_df)
rm(EstMun); rm(estimadores_mun)

# checando o número de DRSs que sobraram por estado
tmp <- estim_mun_df %>% group_by(Estado,codDRS) %>% 
  dplyr::summarise(count = n())
table(tmp$Estado)
# removendo o Distrito Federal, porque esse só tem uma DRS
estim_mun_df <- estim_mun_df %>% filter(!Estado=="DISTRITO FEDERAL")

# curva dos municípios do estado usado no filtro e a respectiva
# curva média, usando a função ggploty
tmp <- estim_mun_df %>% dplyr::filter(Estado=='SAO PAULO') %>%
  group_by(Estado,Data) %>%
  dplyr::summarize(R_e = mean(R_e))
p <- estim_mun_df %>% dplyr::filter(Estado=='SAO PAULO') %>%
  ggplot( aes(x=Data, y=R_e,color=Estado)) +
  geom_line(aes(group=codDRS), alpha = .4) +
  geom_line(data=tmp, alpha = .8, size = 1.5,color="black")
#  coord_cartesian( ylim = c(0, 20))
ggplotly(p)

# criando uma matriz para ser usada na função fanova.tests, tendo as
# curvas observadas nas colunas e pontos de discretização nas linhas
tmp <- estim_mun_df %>% filter(!Estado=="DISTRITO FEDERAL") %>%
  select(codDRS,Data,R_e) %>%
  tidyr::spread(key = codDRS, value = R_e)
dados_fanova <- tmp %>% select(-Data) %>% as.matrix
rownames(dados_fanova) <- tmp$Data %>% as.character
# vetor com os grupos referente a cada curva observada
grupos_fanova <- estim_mun_df %>% 
  filter(!Estado=="DISTRITO FEDERAL") %>%
  select(codDRS, Estado) %>%
  dplyr::group_by(codDRS) %>% unique %>% as.matrix
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

#************************************************#
#************************************************#

# Parte do códgigo onde são feitas algumas tabelas de
# contingência

mun <- readRDS("../dados_por_municipio.rds")

mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered
  ) %>% ungroup %>% select(Estado,Município,codDRS, Data,confirmed,deaths,
                           recovered, Populacao_estimada, IDHM)

# criando variável que classifica os municípios de acordo com os quartis de
# IDHM para todos os municípios do Brasil
quartis_idhm <- mun %>%
  group_by(Estado,Município) %>% dplyr::distinct(IDHM)
quartis_idhm <- tibble(Q1 = quantile(quartis_idhm$IDHM,.25)[[1]],
                       Q2 = quantile(quartis_idhm$IDHM,.5)[[1]],
                       Q3 = quantile(quartis_idhm$IDHM,.75)[[1]])

mun_2SemEpd <- mun %>% select(Estado,Município,codDRS,IDHM,confirmed) %>%
  mutate(grupIDHM = NA, grupSemEpd = NA)

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25

tmp <- mun %>% select(Estado,Município) %>% distinct()
dim(tmp)
for(i in 1:nrow(tmp)){
  # grupo do IDHM
  tmp2 <- mun_2SemEpd %>% filter(Estado==tmp[i,]$Estado,Município==tmp[i,]$Município) %>%
    dplyr::summarise(grupIDHM = ifelse(unique(IDHM)<quartis_idhm$Q1,"D",
                             ifelse(unique(IDHM)<quartis_idhm$Q2,"C",
                                    ifelse(unique(IDHM)<quartis_idhm$Q3,"B","A"))))
  # vetor com dias epidemiológicos (depois que passou de caso_corte casos)
  tmp3 <- mun_2SemEpd %>% 
    filter(Estado==tmp[i,]$Estado,Município==tmp[i,]$Município) %>%
    select(confirmed)
  tmp3 <- which(tmp3$confirmed >= caso_corte)
  # alterando valores das variáveis que indicam o grupo do IDHM municipal
  # e se o município tem 2 semanas epidemiológicas
  tmp4 <- which(mun_2SemEpd$Estado==tmp[i,]$Estado & mun_2SemEpd$Município==tmp[i,]$Município)
  mun_2SemEpd$grupIDHM[tmp4] <- tmp2$grupIDHM
  mun_2SemEpd$grupSemEpd[tmp4] <- ifelse(is.na(tmp3) || length(tmp3)<15,FALSE,TRUE)
}
View(mun_2SemEpd)

# Tabelas de contingência
tmp <- mun_2SemEpd %>% group_by(Estado,Município) %>% 
  select(grupIDHM,grupSemEpd) %>%
  distinct()
table(tmp$grupIDHM,tmp$grupSemEpd)
prop.table(table(tmp$grupIDHM,tmp$grupSemEpd))

# número de municípios dentro de cada estado que tem ou não 2 
# semanas epidemiológicas
mun_2SemEpd %>% select(Estado, Município, grupSemEpd) %>%
  distinct() %>% group_by(Estado, grupSemEpd) %>%
  dplyr::summarise(n = n()) %>% spread(key = grupSemEpd, value = n) %>%
  print(n=30)
# número de municípios dentro de cada estado que pertencem a cada
# um dos grupos de IDHM
mun_2SemEpd %>% select(Estado, Município, grupIDHM) %>%
  distinct() %>% group_by(Estado, grupIDHM) %>%
  dplyr::summarise(n = n()) %>% spread(key = grupIDHM, value = n) %>%
  print(n=30)

# dentre os municípios que tem 2 semanas epidemiológicas, quantos destes
# dentro de cada estado pertencem a cada um dos grupos de IDHM
mun_2SemEpd %>% filter(grupSemEpd==TRUE) %>%
  select(Estado, Município, grupIDHM) %>%
  distinct() %>% group_by(Estado, grupIDHM) %>%
  dplyr::summarise(n = n()) %>% spread(key = grupIDHM, value = n) %>%
  print(n=30)

#*****************************#

# Parte do código onde incluímos a variável classe de 
# urbanização dos municípios na análise

mun <- readRDS("../dados_por_municipio.rds")

# selecionando e criando as variáveis que vão ser usadas
mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered
  ) %>% ungroup %>% select(Estado,Município,Codigo,codDRS, Data,confirmed,deaths,
                           recovered, Populacao_estimada, IDHM)

# Classificação e Caracterização dos Espaços Rurais e Urbanos do Brasil 
urbanizacao <- read_excel("../Grau_de_urbanizacao.xlsx", guess_max = 100000,
                          col_names = TRUE,sheet = 2) %>%
  dplyr::select(Estado = NM_UF, Município = NM_MUN, Codigo = CD_GCMUN, 
                pop_area_densa = POP_AREA_DENSA,
                pop_area_nao_densa = POP_AREA_NAO_DENSA,
                pop_total = POP_TOTAL_UNID_POP, grau_urb = GR_URB,
                classe_urb = CLASSES)
View(urbanizacao)
# renomeando as classes
urbanizacao <- urbanizacao %>% 
  mutate(classe_urb = ifelse(classe_urb=="Unidade populacional com moderado grau de urbanizacao","moderado",
                             ifelse(classe_urb=="Unidade populacional com alto grau de urbanizacao","alto","baixo")))
# eliminando o último dígito do código nesses dados do IBGE, para deixar ele
# com 6 dígitos, como está nos dados do Ministério da Saúde
urbanizacao <- urbanizacao %>% mutate(Codigo = str_sub(as.character(Codigo), start = 1,end = 6))
# tirando acentos e deixando em caixa alta os nomes dos estados
urbanizacao <- urbanizacao %>% mutate(Estado = stri_trans_general(str = Estado, 
                                                          id = "Latin-ASCII")) %>%
  mutate(Estado = stri_trans_general(str = Estado, 
                                     id = "upper"))

mun %>% select(Codigo) %>% distinct() %>% dim()
urbanizacao %>% select(Codigo) %>% distinct() %>% dim()
# número de municípios em urbanizacao que não estão em mun
urbanizacao %>% select(Estado,Município,Codigo) %>% 
  filter(!(Codigo %in% mun$Codigo)) %>% distinct() %>% dim()
urbanizacao %>% select(Estado,Município,Codigo) %>% 
  filter(!(Codigo %in% mun$Codigo)) %>% distinct() %>% View()

# adicionando os dados sobre urbanização dos municípios ao dataframe mun
mun_2SemEpd <- mun %>% group_by(Estado,Codigo) %>%
  dplyr::inner_join(urbanizacao %>% select(-Município),
                    by=c("Estado","Codigo")) %>%
  ungroup()

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25

mun_2SemEpd <- mun_2SemEpd %>% mutate(grupIDHM = NA, grupSemEpd = NA)
tmp <- mun_2SemEpd %>% select(Estado,Município) %>% distinct()
dim(tmp)
for(i in 1:nrow(tmp)){
  # grupo do IDHM
  tmp2 <- mun_2SemEpd %>% filter(Estado==tmp[i,]$Estado,Município==tmp[i,]$Município) %>%
    dplyr::summarise(grupIDHM = ifelse(unique(IDHM)<quartis_idhm$Q1,"D",
                                       ifelse(unique(IDHM)<quartis_idhm$Q2,"C",
                                              ifelse(unique(IDHM)<quartis_idhm$Q3,"B","A"))))
  # vetor com dias epidemiológicos (depois que passou de caso_corte casos)
  tmp3 <- mun_2SemEpd %>% 
    filter(Estado==tmp[i,]$Estado,Município==tmp[i,]$Município) %>%
    select(confirmed)
  tmp3 <- which(tmp3$confirmed >= caso_corte)
  # alterando valores das variáveis que indicam o grupo do IDHM municipal
  # e se o município tem 2 semanas epidemiológicas
  tmp4 <- which(mun_2SemEpd$Estado==tmp[i,]$Estado & mun_2SemEpd$Município==tmp[i,]$Município)
  mun_2SemEpd$grupIDHM[tmp4] <- tmp2$grupIDHM
  mun_2SemEpd$grupSemEpd[tmp4] <- ifelse(is.na(tmp3) || length(tmp3)<15,FALSE,TRUE)
}
View(mun_2SemEpd)

#write.csv(mun_2SemEpd,"mun_2SemEpd.csv",row.names = FALSE, col.names = TRUE)

# agregando os dados por DRS, o que vai remover a variável Município.
mun_2SemEpd <- mun_2SemEpd %>% group_by(Estado,Data,codDRS) %>%
  dplyr::summarise(confirmed=sum(confirmed), deaths=sum(deaths),
                   recovered=sum(recovered), 
                   populacao=sum(Populacao_estimada),
                   classe_urb = unique(classe_urb)[[1]],
                   grupIDHM = unique(grupIDHM)[[1]],
                   grupSemEpd = unique(grupSemEpd)[[1]],
                   IDHM=mean(IDHM)) %>% ungroup %>% arrange(Estado,codDRS)

# tabela contendo os municípios separados por estado
EstMun <- mun_2SemEpd %>% group_by(Estado,codDRS) %>% dplyr::summarise(count = n())
# lista que irá armazenar as curvas
estimadores_mun <- vector(mode = "list", length = dim(EstMun)[1])

for(i in 1:dim(EstMun)[1]){
  dados_mun <- mun_2SemEpd %>% filter(Estado==EstMun$Estado[i] & codDRS==EstMun$codDRS[i])
  # checando se a DRS possui duas semanas epidemiológicas
  if(unique(dados_mun$grupSemEpd)[[1]]){
    tmp <- estima_parametros(dados_mun, 
                             populacao = dados_mun$populacao[1], 
                             caso_corte = 25, 
                             expoente_H = 0.3, 
                             recuperados_sintetico = TRUE)[c("nu_t", "beta_t", 
                                                             "mu_t", "R_e",
                                                             "datas")] %>%
      lapply(function(x) tail(x,14))
    estimadores_mun[[i]] <- append(tmp,list(Estado = rep(EstMun$Estado[i],14), 
                                            codDRS = rep(EstMun$codDRS[i],14)))
  }else{
    estimadores_mun[[i]] <- list(Estado = EstMun$Estado[i], 
                                 codDRS = EstMun$codDRS[i])
  }
}

# checando para quantas DRSs o modelo foi estimado
tmp <- lapply(estimadores_mun, length) %>% unlist(use.names=FALSE)
table(tmp)
# usando a lista com as curvas, montamos um tibble contendo somente
# as DRSs para as quais o modelo foi estimado
tmp2 <- lapply(estimadores_mun[which(tmp == 7)], as.data.frame)
estim_mun_df <- do.call("rbind",tmp2)
estim_mun_df <- mutate(estim_mun_df, Data=estim_mun_df$datas) %>%
  select(-datas)
estim_mun_df <- dplyr::left_join(estim_mun_df,mun_2SemEpd,by=c("Estado","codDRS","Data")) %>%
  tibble
#View(estim_mun_df)
rm(EstMun); rm(estimadores_mun)

# checando o número de DRSs que sobraram por estado
tmp <- estim_mun_df %>% group_by(Estado,codDRS) %>% 
  dplyr::summarise(count = n())
table(tmp$Estado)
# total de DRSs por Estado
mun %>% select(Estado,codDRS) %>% distinct() %>% group_by(Estado) %>% 
  dplyr::summarise(n = n()) %>% print(n=30)

# curva dos municípios do estado usado no filtro e a respectiva
# curva média, usando a função ggploty
tmp <- estim_mun_df %>%
  group_by(Data) %>%
  dplyr::summarize(R_e = mean(R_e))
p <- estim_mun_df %>%
  ggplot( aes(x=Data, y=R_e,color=grupIDHM)) +
  geom_line(aes(group=codDRS), alpha = .4) +
  geom_line(data=tmp, alpha = .8, size = 1.5,color="black")
ggplotly(p)

tmp <- estim_mun_df %>%
  group_by(Data) %>%
  dplyr::summarize(R_e = mean(R_e))
p <- estim_mun_df %>%
  ggplot( aes(x=Data, y=R_e,color=classe_urb)) +
  geom_line(aes(group=codDRS), alpha = .4) +
  geom_line(data=tmp, alpha = .8, size = 1.5,color="black")
ggplotly(p)

# checando municípios na DRS observada
mun %>% filter(codDRS == 21001) %>% select(Estado,Município) %>% distinct()

# criando uma matriz para ser usada na função fanova.tests, tendo as
# curvas observadas nas colunas e pontos de discretização nas linhas
tmp <- estim_mun_df %>%
  select(codDRS,Data,R_e) %>%
  tidyr::spread(key = codDRS, value = R_e)
dados_fanova <- tmp %>% select(-Data) %>% as.matrix
rownames(dados_fanova) <- tmp$Data %>% as.character

# vetor com os grupos referente a cada curva observada
grupos_fanova <- estim_mun_df %>% 
  select(codDRS, grupIDHM) %>%
  dplyr::group_by(codDRS) %>% unique %>% as.matrix
grupos_fanova <- rep(NA,ncol(dados_fanova))
tmp <- colnames(dados_fanova)
for(i in 1:length(tmp)){
  tmp2 <- estim_mun_df %>% filter(codDRS==tmp[i]) %>%
    select(grupIDHM) %>% unique()
  grupos_fanova[i] <- tmp2$grupIDHM
}
# checando se todos os grupos tem mais de duas observações
table(grupos_fanova)
plotFANOVA(x = dados_fanova, group.label = grupos_fanova,
           means = TRUE)
fanova <- fanova.tests(x = dados_fanova,
                       group.label = grupos_fanova, 
                       #test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)


# vetor com os grupos referente a cada curva observada
grupos_fanova <- estim_mun_df %>% 
  select(codDRS, classe_urb) %>%
  dplyr::group_by(codDRS) %>% unique %>% as.matrix
grupos_fanova <- rep(NA,ncol(dados_fanova))
tmp <- colnames(dados_fanova)
for(i in 1:length(tmp)){
  tmp2 <- estim_mun_df %>% filter(codDRS==tmp[i]) %>%
    select(classe_urb) %>% unique()
  grupos_fanova[i] <- tmp2$classe_urb
}
# checando se todos os grupos tem mais de duas observações
table(grupos_fanova)
# Avaliando o efeito de IHHM_Renda
plotFANOVA(x = dados_fanova, group.label = grupos_fanova,
           means = TRUE)
fanova <- fanova.tests(x = dados_fanova,
                       group.label = grupos_fanova, 
                       #test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)

