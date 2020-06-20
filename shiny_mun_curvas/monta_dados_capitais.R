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

colnames(mun)
View(mun)

vcapitais <- c("Belém", "Boa Vista", "Macapá", "Manaus", "Palmas", 
               "Porto Velho","Rio Branco", "Aracaju", "Fortaleza", 
               "João Pessoa", "Maceió", "Natal", "Recife", "Salvador", 
               "São Luís", "Teresina", "Brasília" , 
               "Campo Grande", "Cuiabá", "Goiânia",
               "Belo Horizonte", "Rio de Janeiro", "São Paulo",
               "Vitória","Curitiba", "Florianópolis", "Porto Alegre")
tmp <- mun %>% filter(Município %in% vcapitais) %>%
  filter(date>"2020-05-17")
View(tmp)

require(openxlsx)
write.xlsx(x = tmp, file = "Covid_capitais.xlsx")

