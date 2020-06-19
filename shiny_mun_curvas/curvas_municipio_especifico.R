
source('../../time_varying_SIRD/estimator_calculation.R')
source('../../time_varying_SIRD/gaussian_kernel.R')
source('../../time_varying_SIRD/load_packages.R')
source('../../time_varying_SIRD/run_SIRD.R')

#********************#

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

#******************************#

# filtrando o estado e município desejados
dados_mun <- mun %>% filter(Estado=="ESPIRITO SANTO" & Município=="Vitória")

# número de casos confirmados que marca o primeiro dia epidemiológico
caso_corte = 25

# checando se o município tem pelo menos duas semanas epidemiológicas
tmp <- dados_mun %>% select(confirmed)
tmp <- which(tmp$confirmed >= caso_corte)
length(tmp)>14

estimadores_mun <- run_SIRD(df = dados_mun, 
         size_population = dados_mun$Populacao_estimada[1], 
         minimum_number_cases = caso_corte,
         kc = 1, kd = 1, kr = 1, power_H = 0.4, 
         recovered_synthetic = TRUE, remove_last = 6)[c("nu_hat", "beta_hat", 
                                                        "mu_hat", "Rt")]
# usando a lista com as curvas, montamos um dataframe contendo somente
# o município desejado
tmp2 <- estimadores_mun %>% as.data.frame
estim_mun_df <- do.call("cbind",tmp2)
estim_mun_df <- as.data.frame(estim_mun_df)
# vetor com as datas referentes às estimativas
tmp2 <- names(estimadores_mun$beta_hat)
estim_mun_df <- dplyr::mutate(estim_mun_df,Data = as.Date(tmp2))
View(estim_mun_df)

p <- estim_mun_df %>%
  ggplot( aes(x=Data, y=Rt)) +
  geom_line() + 
  labs(title="Rt")
#  coord_cartesian( ylim = c(0, 20))
ggplotly(p)

p_Re <- estim_mun_df %>%
  ggplot( aes(x=Data, y=Rt)) +
  geom_line() + 
  labs(title="Rt")
p_beta <- estim_mun_df %>%
  ggplot( aes(x=Data, y=beta_hat)) +
  geom_line() + 
  labs(title="beta_t")
p_nu <- estim_mun_df %>%
  ggplot( aes(x=Data, y=nu_hat)) +
  geom_line() + 
  labs(title="nu_t")
p_mu <- estim_mun_df %>%
  ggplot( aes(x=Data, y=mu_hat)) +
  geom_line() + 
  labs(title="mu_t")

library(gridExtra)
grid.arrange(p_Re,p_beta,p_nu,p_mu, nrow=2)




