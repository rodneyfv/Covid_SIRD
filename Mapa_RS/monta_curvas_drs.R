
source("./time_varying_SIRD-master/load_packages.R")
source("./time_varying_SIRD-master/gaussian_kernel.R")
source("./time_varying_SIRD-master/estimator_calculation.R")
source("./time_varying_SIRD-master/run_SIRD.R")
library("stringi")
library("fdANOVA")

#************************************************#
#************************************************#

# Parte do código onde incluímos a variável classe de 
# urbanização dos municípios na análise

mun <- readRDS("./dados_por_municipio.rds")

# selecionando e criando as variáveis que vão ser usadas
mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered,
    date = Data,
  ) %>% ungroup %>% select(Estado,Município,Codigo,codDRS,nomDRS, date,confirmed,
                           deaths,recovered, Populacao_estimada, IDHM)

# quartis de IDHM para todos os municípios do Brasil
quartis_idhm <- mun %>%
  group_by(Estado,Município) %>% dplyr::distinct(IDHM)
quartis_idhm <- tibble(Q1 = quantile(quartis_idhm$IDHM,.25)[[1]],
                       Q2 = quantile(quartis_idhm$IDHM,.5)[[1]],
                       Q3 = quantile(quartis_idhm$IDHM,.75)[[1]])

# checando os grupos de IDHM dos Municípios 
mun <- mun %>% mutate(grupIDHM = NA)
tmp <- mun %>% select(Estado,Município) %>% distinct()
for(i in 1:nrow(tmp)){
  # grupo do IDHM
  tmp2 <- mun %>% filter(Estado==tmp[i,]$Estado,Município==tmp[i,]$Município) %>%
    dplyr::summarise(grupIDHM = ifelse(unique(IDHM)<quartis_idhm$Q1,"D",
                                       ifelse(unique(IDHM)<quartis_idhm$Q2,"C",
                                              ifelse(unique(IDHM)<quartis_idhm$Q3,"B","A"))))
  # alterando valores das variáveis que indicam o grupo do IDHM municipal
  tmp4 <- which(mun$Estado==tmp[i,]$Estado & mun$Município==tmp[i,]$Município)
  mun$grupIDHM[tmp4] <- tmp2$grupIDHM
}

# Classificação e Caracterização dos Espaços Rurais e Urbanos do Brasil 
urbanizacao <- read_excel("./Grau_de_urbanizacao.xlsx", guess_max = 100000,
                          col_names = TRUE,sheet = 2) %>%
  dplyr::select(Estado = NM_UF, Município = NM_MUN, Codigo = CD_GCMUN, 
                pop_area_densa = POP_AREA_DENSA,
                pop_area_nao_densa = POP_AREA_NAO_DENSA,
                pop_total = POP_TOTAL_UNID_POP, grau_urb = GR_URB,
                classe_urb = CLASSES)
#View(urbanizacao)

# renomeando as classes de urbanização
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

# adicionando os dados sobre urbanização dos municípios ao dataframe mun
mun <- mun %>% group_by(Estado,Codigo) %>%
  dplyr::inner_join(urbanizacao %>% select(-Município),
                    by=c("Estado","Codigo")) %>%
  ungroup()
rm(urbanizacao) # descartando a variável urbanizacao

# agregando os dados por DRS, o que vai remover a variável Município.
drs <- mun %>% group_by(Estado,date,codDRS) %>%
  dplyr::summarise(confirmed=sum(confirmed), deaths=sum(deaths),
                   recovered=sum(recovered), 
                   populacao=sum(Populacao_estimada),
                   pop_area_densa = sum(pop_area_densa),
                   pop_area_nao_densa = sum(pop_area_nao_densa)
  ) %>% ungroup %>% arrange(Estado,codDRS)

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25

tmp <- mun %>% select(codDRS) %>% distinct()
drs <- drs %>% mutate(drs_idhm = NA, dsem_epd = NA, classe_urb = NA)
for(i in 1:nrow(tmp)){
  # grupo do IDHM
  tmp2 <- mun %>% filter(codDRS==tmp[i,]$codDRS) %>% group_by(Município) %>%
    select(Município, grupIDHM) %>% distinct()
  # vetor com dias epidemiológicos
  tmp3 <- drs %>% filter(codDRS==tmp[i,]$codDRS) %>%
    select(confirmed)
  tmp3 <- which(tmp3$confirmed >= caso_corte)
  # checando a classe de urbanização da DRS
  tmp5 <- drs %>% filter(codDRS==tmp[i,]$codDRS) %>%
    select(pop_area_densa,pop_area_nao_densa) %>% distinct()
  tmp5 <- tmp5$pop_area_densa/(tmp5$pop_area_densa + tmp5$pop_area_nao_densa)
  # adicionando esses dados ao dataframe drs
  tmp4 <- which(drs$codDRS==tmp[i,]$codDRS)
  drs$drs_idhm[tmp4] <- names(which.max(table(tmp2$grupIDHM)))
  drs$dsem_epd[tmp4] <- ifelse(is.na(tmp3) || length(tmp3)<15,FALSE,TRUE)
  drs$classe_urb[tmp4] <- ifelse(tmp5>.75,"alto",ifelse(tmp5>.5,"moderado","baixo"))
}

#************************************#

# Análise de curvas médias no país para diferentes variávies

# tabela contendo as DRSs separados por estado
EstDRS <- drs %>% group_by(Estado, codDRS) %>% dplyr::summarise(count = n())
# lista que irá armazenar as curvas
estimadores_drs <- vector(mode = "list", length = dim(EstDRS)[1])

for(i in 1:dim(EstDRS)[1]){
  dados_drs <- drs %>% filter(codDRS==EstDRS$codDRS[i])
  linha_corte <- which(dados_drs$confirmed >= caso_corte)
  # checando se a DRS possui pelo menos duas semanas epidemiológicas
  if(!(is.na(linha_corte[1]) | length(linha_corte)<14)){
    tmp <- run_SIRD(df = dados_drs, 
                    size_population = dados_drs$populacao[1], 
                    minimum_number_cases = caso_corte,
                    kc = 1, kd = 1, kr = 1, power_H = 0.4, 
                    recovered_synthetic = TRUE, remove_last = 6)[c("nu_hat", "beta_hat", 
                                                             "mu_hat", "Rt")]
    # númerode dias para os quais temos estimativas
    num_dias <- length(tmp$beta_hat)
    # vetor com as datas referentes às estimativas
    tmp2 <- names(tmp$beta_hat) 
    estimadores_drs[[i]] <- append(tmp,
                                   list(Estado = rep(EstDRS$Estado[i],num_dias), 
                                        codDRS = rep(EstDRS$codDRS[i],num_dias),
                                        date=tmp2))
  }else{
    estimadores_drs[[i]] <- list(Estado = EstDRS$Estado[i], 
                                 codDRS = EstDRS$codDRS[i])
  }
}

# checando para quantas DRSs o modelo foi estimado
tmp <- lapply(estimadores_drs, length) %>% unlist(use.names=FALSE)
table(tmp)
# usando a lista com as curvas, montamos um tibble contendo somente
# as DRSs para as quais o modelo foi estimado
tmp2 <- lapply(estimadores_drs[which(tmp == 7)], as.data.frame)
estim_drs_df <- do.call("rbind",tmp2)
estim_drs_df <- dplyr::mutate(estim_drs_df,date = as.Date(date))
estim_drs_df <- dplyr::left_join(estim_drs_df,drs,by=c("Estado","codDRS","date")) %>%
  tibble
#View(estim_mun_df)
rm(EstDRS); rm(estimadores_drs)

saveRDS(estim_drs_df, "curvas_drs_df.rds")


