
require(dplyr)
require(readxl)
require(openxlsx)
require(stringi)
require(tidyverse)

# Lendo os dados dos municípios
mun <- readRDS("../dados_por_municipio.rds")
#View(mun)

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
                           deaths,recovered, Populacao_estimada, IDHM,
                           PIB_2015,Media_temperatura_2Q_2002,Altitude)


# Classificação e Caracterização dos Espaços Rurais e Urbanos do Brasil 
urbanizacao <- read_excel("../Grau_de_urbanizacao.xlsx", guess_max = 100000,
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

mun %>% select(Codigo) %>% distinct() %>% dim()
urbanizacao %>% select(Codigo) %>% distinct() %>% dim()
# número de municípios em urbanizacao que não estão em mun
urbanizacao %>% select(Estado,Município,Codigo) %>% 
  filter(!(Codigo %in% mun$Codigo)) %>% distinct() %>% dim()
# urbanizacao %>% select(Estado,Município,Codigo) %>% 
#   filter(!(Codigo %in% mun$Codigo)) %>% distinct() %>% View()

# adicionando os dados sobre urbanização dos municípios ao dataframe mun
mun <- mun %>% group_by(Estado,Codigo) %>%
  dplyr::inner_join(urbanizacao %>% select(-Município),
                    by=c("Estado","Codigo")) %>%
  ungroup()

vcapitais <- c("Belém", "Boa Vista", "Macapá", "Manaus", "Palmas", 
               "Porto Velho","Rio Branco", "Aracaju", "Fortaleza", 
               "João Pessoa", "Maceió", "Natal", "Recife", "Salvador", 
               "São Luís", "Teresina", "Brasília" , 
               "Campo Grande", "Cuiabá", "Goiânia",
               "Belo Horizonte", "Rio de Janeiro", "São Paulo",
               "Vitória","Curitiba", "Florianópolis", "Porto Alegre")
tmp <- mun %>% filter(Município %in% vcapitais) %>%
  filter(date>"2020-05-09")
tmp %>% select(Estado,Município) %>% distinct() %>% 
  group_by(Município) %>% summarise(count = n()) %>%
  filter(count>1)
tmp <- tmp %>% filter(!(Município=="Belém" & Estado!="PARA")) %>%
  filter(!(Município=="Boa Vista" & Estado!="RORAIMA")) %>%
  filter(!(Município=="Campo Grande" & Estado!="MATO GROSSO DO SUL")) %>%
  filter(!(Município=="Palmas" & Estado!="TOCANTINS")) %>%
  filter(!(Município=="Rio Branco" & Estado!="ACRE"))
View(tmp)

write.xlsx(x = tmp, file = "Covid_capitais.xlsx")

