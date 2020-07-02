
source("./time_varying_SIRD-master/load_packages.R")
source("./time_varying_SIRD-master/gaussian_kernel.R")
source("./time_varying_SIRD-master/estimator_calculation.R")
source("./time_varying_SIRD-master/run_SIRD.R")
library("stringi")

#************************************************#
#************************************************#

# Parte do código onde incluímos a variável classe de 
# urbanização dos municípios na análise

mun <- readRDS("../dados_por_municipio.rds")

# selecionando e criando as variáveis que vão ser usadas
mun <- mun %>% 
  dplyr::group_by(Codigo) %>% 
  dplyr::mutate(
    recovered = pmax(0, dplyr::lag(confirmed, 14) - dplyr::lag(deaths, 0)),
    recovered = ifelse(is.na(recovered), 0, recovered),
    infected = confirmed - deaths - recovered,
    date = Data,
  ) %>% ungroup %>% dplyr::select(Estado,Município,Codigo,codDRS,nomDRS, date,confirmed,
                           deaths,recovered, Populacao_estimada)

# agregando os dados por DRS, o que vai remover a variável Município.
drs <- mun %>% dplyr::group_by(Estado,date,codDRS,nomDRS) %>%
  dplyr::summarise(confirmed=sum(confirmed), deaths=sum(deaths),
                   recovered=sum(recovered), 
                   populacao=sum(Populacao_estimada)
  ) %>% ungroup %>% arrange(Estado,codDRS)

#************************************#

# Análise de curvas médias no país para diferentes variávies

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25
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
                    recovered_synthetic = TRUE, remove_last = 6)[c("Rt")]
    # númerode dias para os quais temos estimativas
    num_dias <- length(tmp$Rt)
    # vetor com as datas referentes às estimativas
    tmp2 <- names(tmp$Rt) 
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
#table(tmp)
# usando a lista com as curvas, montamos um tibble contendo somente
# as DRSs para as quais o modelo foi estimado
tmp2 <- lapply(estimadores_drs[which(tmp > 2)], as.data.frame)
estim_drs_df <- do.call("rbind",tmp2)
estim_drs_df <- dplyr::mutate(estim_drs_df,date = as.Date(date))
estim_drs_df <- dplyr::left_join(estim_drs_df,drs,by=c("Estado","codDRS","date")) %>%
  tibble
#View(estim_mun_df)
rm(EstDRS); rm(estimadores_drs)

# deixamos o código das RSs aqui também para poder compara
# com os códigos em mun_rs
estim_drs_df <- estim_drs_df %>% mutate(codDRS = as.character(codDRS))
# salvando os dados num formato que economiza mais espaço que
# o dataframe completo
tmp <- estim_drs_df %>% dplyr::select(Rt,date,codDRS)
tmp1 <- estim_drs_df %>% dplyr::select(Estado,codDRS,nomDRS) %>% distinct()
estim_drs_df = list(Rt_date=tmp,estado_nomDRS=tmp1)
saveRDS(estim_drs_df, 
        paste("./shiny/Rt_regsaude/",Sys.Date(),"_Rt_drs.rds",sep=""))


# maneira que os dados vão ser colocados na forma original
#tmp = left_join(estim_drs_df$Rt_date,estim_drs_df$estado_nomDRS,by="codDRS")
