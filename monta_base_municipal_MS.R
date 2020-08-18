
# Nome do arquivo do min. saude
arquivo_min_saude <- "HIST_PAINEL_COVIDBR_04ago2020.xlsx"
#arquivo_min_saude <- "HIST_PAINEL_COVIDBR_19mai2020.xlsx"

# Nome do arquivo a ser usado como historico
arquivo_historico <- "Dados_Municipais_wide.csv"

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

# Atualizando tidyr se necessario
if(installed.packages() %>% as.data.frame %>% dplyr::filter(Package == "tidyr") %>% pull(Version) %>% as.character < "1.0") {
  install.packages("tidyr")
  pacote("tidyr")
}

municipais_wide <- read_csv(arquivo_historico) %>% # Dados enviados por Henrique e Iuri
  dplyr::mutate(Codigo = as.character(Codigo)) 

df_caracteristicas_fixas <- municipais_wide %>% dplyr::select(- starts_with("casos"), - starts_with("mortes")) 
populacoes <- list()
populacoes[["Brasil"]] <- df_caracteristicas_fixas$Populacao_estimada %>% sum
populacoes[["Brasil_IDH_alto"]] <- df_caracteristicas_fixas %>% dplyr::filter(Belgica == 1) %>% dplyr::pull(Populacao_estimada) %>% sum
populacoes[["Brasil_IDH_baixo"]] <- df_caracteristicas_fixas %>% dplyr::filter(Belgica == 0) %>% dplyr::pull(Populacao_estimada) %>% sum

saveRDS(populacoes, "lista_populacoes.rds")

municipais_wide_casos <- municipais_wide %>% dplyr::select(Codigo, starts_with("casos."))
municipais_wide_mortes <- municipais_wide %>% dplyr::select(Codigo, starts_with("mortes."))

mun_casos <- municipais_wide_casos %>% pivot_longer(- c(Codigo), names_to = "Data", values_to = "confirmed") %>% 
  dplyr::mutate(Data = Data %>% str_replace_all("casos.", "") %>% ymd)

mun <- municipais_wide_mortes %>% pivot_longer(- c(Codigo), names_to = "Data", values_to = "deaths") %>% 
  dplyr::mutate(Data = Data %>% str_replace_all("mortes.", "") %>% ymd) %>% full_join(mun_casos)

df_msaude <- read_excel(paste0(arquivo_min_saude), guess_max = 100000) %>% dplyr::select(Data = data, Codigo = codmun, deaths = obitosAcumulado,
                                                             Municipio = municipio, 
                                                             confirmed = casosAcumulado) %>% 
  dplyr::mutate(Data = ymd(Data), 
                confirmed = as.numeric(confirmed), 
                deaths = as.numeric(deaths)) %>% 
  dplyr::filter(!is.na(Municipio)) %>% dplyr::select(-Municipio)

# Existem municípios com linhas duplicadas para uma mesma data de 30/05. 
# Vamos remover agora pra seguir com a análise
tmp <- sort(unique(df_msaude$Data),decreasing = TRUE)[c(1:2)]
df_msaude <- df_msaude %>% filter(!(Data %in% tmp))

ultima_data_disponivel <- df_msaude$Data %>% max

# Vendo quais municipios nao estao indo ate a ultima data da base (possiveis erros)
codigos_com_erro <- df_msaude %>% dplyr::group_by(Codigo) %>% dplyr::summarise(menor_data = min(Data), maior_data = max(Data)) %>% 
  dplyr::arrange(maior_data) %>% dplyr::filter(maior_data != max(maior_data)) %>% dplyr::pull(Codigo)

message("Numero de municipios que nao vao ate o final: ", length(codigos_com_erro))

message("Valores de casos acumulados para os municipios com esse erro: ",  df_msaude %>% dplyr::filter(Codigo %in% codigos_com_erro) %>% dplyr::pull(confirmed) %>% unique %>% sort %>% paste(collapse = ", "))

message("Vou completar esses municipios repetindo o ultimo valor de casos e obitos ate a ultima data")

# Loop pelos municipios com erro

for(cod_ in codigos_com_erro) {
  tbl_temp <- df_msaude %>% dplyr::filter(Codigo == cod_) %>% dplyr::filter(Data == max(Data)) %>% unique()
  ultima_data <- tbl_temp$Data

  datas_faltantes <- seq.Date(from = ultima_data + 1, to = ultima_data_disponivel, by = 1)
  tbl_completando <- tibble(Data = datas_faltantes, Codigo = cod_) %>% left_join(tbl_temp %>% dplyr::select(-Data), by = "Codigo") %>% 
    dplyr::select(colnames(tbl_temp))
  
  df_msaude <- rbind(df_msaude, tbl_completando)
}

df_msaude <- df_msaude %>% dplyr::arrange(Codigo, Data) # Essa base ja esta com todos os municipios indo ate a ultima data

# Agora vou completar as datas anteriores usando os dados do Brasil.IO

# Comparando primeiro dia do municipio no MSaude com o dia equivalente no Brasil.IO, pra ver se existe e se tem muita diferença

comparacao_primeiro_dia <- df_msaude %>% dplyr::group_by(Codigo) %>% 
  dplyr::filter(Data == min(Data)) %>% dplyr::select(everything(), deaths_MS = deaths, 
                                                     confirmed_MS = confirmed) %>% left_join(mun)

comparacao_primeiro_dia <- comparacao_primeiro_dia %>% dplyr::mutate(
  check_confirmed = confirmed_MS - confirmed,
  check_deaths = deaths_MS - deaths
)

#comparacao_primeiro_dia %>% dplyr::filter(!is.na(check_deaths)) %>% arrange(desc(abs(check_confirmed))) 

#mun %>% dplyr::filter(Codigo %in% codigos_com_NA) %>% dplyr::filter(confirmed > 0)

# Verificano se os municipios que nem entraram, ate hoje, no do MSaude possuem um numero relevante de casos segundo Brasil.IO
mun_apenas_BrasilIO <- mun %>% dplyr::filter(!Codigo %in% unique(df_msaude$Codigo)) %>% 
  dplyr::group_by(Codigo) %>% dplyr::filter(Data == max(Data))  %>% dplyr::arrange(desc(confirmed))
# Apenas 8 casos. Logo, vou desconsiera-los, confiando mais no dado do MS.
 

# Dentre os municipios que estao no MS, pegando as datas anteriores as disponiveis no MS
keys_fora_do_df_msaude <- anti_join(mun %>% dplyr::filter(Codigo %in% unique(df_msaude$Codigo)) %>% dplyr::select(Codigo, Data), df_msaude %>% dplyr::select(Codigo, Data)) %>% 
  left_join(mun) 

dados <- rbind(keys_fora_do_df_msaude, df_msaude)  
dados_sem_buracos <- expand.grid(Codigo = unique(dados$Codigo), Data = unique(dados$Data)) %>% 
  dplyr::mutate(Codigo = as.character(Codigo), Data = ymd(Data))
  
buracos <- anti_join(dados_sem_buracos, dados %>% dplyr::select(Codigo, Data))

# Vou substituir os buracos pelo ultimo dado. Sao poucos confirmados e obitos nesses casos.
buracos <- buracos %>% dplyr::mutate(confirmed = NA, deaths = NA)
for(i_b in 1:nrow(buracos)) {
    buracos$confirmed[i_b] <- mun %>% dplyr::filter(Codigo == buracos[i_b,]$Codigo, Data < buracos[i_b,]$Data)  %>% dplyr::pull(confirmed) %>% last %>% as.numeric
    buracos$deaths[i_b] <- mun %>% dplyr::filter(Codigo == buracos[i_b,]$Codigo, Data < buracos[i_b,]$Data)  %>% dplyr::pull(deaths) %>% last %>% as.numeric
  
}

dados <- rbind(dados, buracos) %>% dplyr::arrange(Codigo, Data)

# Check de que nao tem mais buracos:
nrow(dados) == nrow(dados_sem_buracos)

dados_finais <- dados %>% left_join(df_caracteristicas_fixas)


saveRDS(dados_finais, "dados_por_municipio.rds")

# 
dados_finais_agg <- dados_finais %>% dplyr::group_by(Data) %>%
  dplyr::summarise(confirmed = sum(confirmed), deaths = sum(deaths))

#### Comparando esses dados finais por municipio com os dados no nivel Brasil, tambem do proprio MS

MS_BR <- read_excel(arquivo_min_saude, guess_max = 1000000) %>% dplyr::filter(regiao == "Brasil") 
MS_BR <- MS_BR %>% 
  dplyr::select(data, casosAcumulado, obitosAcumulado, Recuperadosnovos) %>% 
  dplyr::mutate(Data = ymd(data), confirmed = as.numeric(casosAcumulado),
                deaths = as.numeric(obitosAcumulado), recovered = as.numeric(Recuperadosnovos),
                recovered = ifelse(is.na(recovered), 0, recovered)) %>% dplyr::select(Data, confirmed, deaths, recovered)

saveRDS(MS_BR, "dados_MS_brasil.rds")
