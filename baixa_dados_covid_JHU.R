pacote <- function(p){
  if (!is.element(p, installed.packages()[,1])){
    message('Pacote ',p,' nao encontrado. Instalando..')
    if(p == "HDeconometrics") {
      devtools::install_github("gabrielrvsc/HDeconometrics")
    } else {
      install.packages(p, dep = TRUE)}  
  }
  message('Carregando pacote ',p)
  require_worked <- try(require(p, character.only = TRUE))
  if(!require_worked) {
    install.packages(p, dep = TRUE)
  }
}

pacote("tidyr")
pacote("tidyverse")
pacote("lubridate")
pacote("writexl")
pacote("readxl")
pacote("httr")

if(installed.packages() %>% as.data.frame %>% dplyr::filter(Package == "tidyr") %>% pull(Version) %>% as.character < "1.0") {
  install.packages("tidyr")
  pacote("tidyr")
}

baixa_dados <- function(tipo_dado) {
  
  base_tmp <- read_csv(str_c("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_", 
                             tipo_dado, "_global.csv")) %>% pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "Data", values_to = tipo_dado) %>% 
    dplyr::mutate(Data = mdy(Data)) %>% 
    dplyr::select(Data, 
                  Country_Region = `Country/Region`, 
                  Province_State = `Province/State`, 
                  Lat, Long, all_of(tipo_dado)) %>% dplyr::arrange(Country_Region, Province_State, Data)
  
  return(base_tmp)
  
}

cria_lista <- function() {
  
  tipos_dados <- c("confirmed", "deaths", "recovered")
  dados <- lapply(tipos_dados, baixa_dados)
  
  lista_dados <- list()
  lista_dados$dados_originais <- dados %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2, by = c("Data", "Country_Region", "Province_State", "Lat", "Long")),.)
  
  # Alterando o seguinte ponto: territórios de relativa autonomia (ex Groelândia vs Dinamarca) serão
  # separados
  pre_montagem<-lista_dados$dados_originais
  # Vamos trocar províncias NA para "espaço vazio" para nós conseguirmos fazer comparações
  # que gerem T/F e substituir no lugares adequados
  pre_montagem$Province_State[is.na(pre_montagem$Province_State)]<-" "
  # Separando Aruba da Holanda
  pre_montagem$Country_Region[pre_montagem$Province_State == "Aruba"]<-"Aruba"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Bermuda"]<-"Bermuda"
  pre_montagem$Country_Region[pre_montagem$Province_State == "British Virgin Islands"]<-"British Virgin Islands"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Cayman Islands"]<-"Cayman Islands"
  pre_montagem$Country_Region[pre_montagem$Province_State == "French Polynesia"]<-"French Polynesia"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Gibraltar"]<-"Gibraltar"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Greenland"]<-"Greenland"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Hong Kong"]<-"Hong Kong"
  pre_montagem$Country_Region[pre_montagem$Province_State == "New Caledonia"]<-"New Caledonia"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Turks and Caicos Islands"]<-"Turks and Caicos Islands"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Macau"]<-"Macau"
  pre_montagem$Country_Region[pre_montagem$Province_State == "Faroe Islands"]<-"Faroe Islands"
  
  lista_dados$agrupado_por_pais <- pre_montagem %>% dplyr::group_by(Data, Country_Region) %>% 
    dplyr::summarise(Lat = mean(Lat), Long = mean(Long), 
                     confirmed = sum(confirmed, na.rm=TRUE), deaths = sum(deaths, na.rm=TRUE), recovered = sum(recovered, na.rm=TRUE)) %>% ungroup() %>% 
    dplyr::arrange(Country_Region, Data)
  
  # datas_intervencoes <- read_xlsx("datas_intervencoes.xlsx")
  # 
  # 
  # if(!all(datas_intervencoes$Country %in% lista_dados$agrupado_por_pais$Country_Region)) {
  #   stop("Erro! Existem paises na planilha datas_intervencoes que nao constam (ou estao com nome diferente) nos dados que vem do GitHub.")
  # }
  # 
  # colnames(datas_intervencoes) <- gsub("-", "_", gsub(" ", "_", colnames(datas_intervencoes)))
  # intervencoes <- setdiff(colnames(datas_intervencoes), "Country")
  # 
  # lista_dados$agrupado_por_pais <- lista_dados$agrupado_por_pais %>% left_join(datas_intervencoes, by = c("Country_Region" = "Country")) %>% as.data.frame
  # for(interv_ in intervencoes) {
  #   lista_dados$agrupado_por_pais[,interv_] <- ifelse(lista_dados$agrupado_por_pais$Data >= lista_dados$agrupado_por_pais[,interv_], 
  #                                                     1, 0)
  # }
  # 
  lista_dados <- lista_dados[c(2, 1)]
  
  return(lista_dados)
}


lista_dados <- cria_lista()

## Juntando com dados da planliha da Oxford

url1 <- "https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
base_oxford <- read_excel(tf, 1)
base_oxford <- base_oxford %>% dplyr::select(-ends_with("_Notes"), -ConfirmedCases, -ConfirmedDeaths)
base_oxford <- base_oxford %>% dplyr::mutate(Data = ymd(Date)) %>% dplyr::select(-Date) %>% 
  dplyr::select(Country_Region = CountryName, CountryCode, Data, everything())

# Mudando a forma de match dos nomes de países - nomes nas duas listas não são exatamente iguais
# ex: EUA se chamam US na base do Git e se chamam United States em Oxford

# Número de países
length(unique(lista_dados$agrupado_por_pais$Country_Region)) # 185
length(unique(base_oxford$Country_Region)) # 190

difs1<-sort(setdiff(base_oxford$Country_Region, lista_dados$agrupado_por_pais$Country_Region))
difs2<-sort(setdiff(lista_dados$agrupado_por_pais$Country_Region, base_oxford$Country_Region))

# Checando na mão os países que estão nos dois, mas tem nomes diferentes
  # Substituindo nomes da Lista de Oxford para bater com o GitHub: (depois vamos checar esses nomes)
view(as.data.frame(difs1))
view(as.data.frame(difs2))

base_oxford$Country_Region[base_oxford$Country_Region == "Cape Verde"]<-"Cabo Verde"
base_oxford$Country_Region[base_oxford$Country_Region == "Congo"]<-"Congo (Kinshasa)"
base_oxford$Country_Region[base_oxford$Country_Region == "Democratic Republic of Congo"]<-"Congo (Brazzaville)"
base_oxford$Country_Region[base_oxford$Country_Region == "Czech Republic"]<-"Czechia"
base_oxford$Country_Region[base_oxford$Country_Region == "Kyrgyz Republic"]<-"Kyrgyzstan"
base_oxford$Country_Region[base_oxford$Country_Region == "Macedonia"]<-"North Macedonia"
base_oxford$Country_Region[base_oxford$Country_Region == "Myanmar"]<-"Burma"
base_oxford$Country_Region[base_oxford$Country_Region == "Palestine"]<-"West Bank and Gaza"
base_oxford$Country_Region[base_oxford$Country_Region == "Slovak Republic"]<-"Slovakia"
base_oxford$Country_Region[base_oxford$Country_Region == "South Korea"]<-"Korea, South"
base_oxford$Country_Region[base_oxford$Country_Region == "Taiwan"]<-"Taiwan*"
base_oxford$Country_Region[base_oxford$Country_Region == "Timor"]<-"Timor-Leste"
base_oxford$Country_Region[base_oxford$Country_Region == "United States"]<-"US"
base_oxford$Country_Region[base_oxford$Country_Region == "Macao"]<-"Macau"
base_oxford$Country_Region[base_oxford$Country_Region == "Faeroe Islands"]<-"Faroe Islands"
base_oxford$Country_Region[base_oxford$Country_Region == "Swaziland"]<-"Eswatini"

# Revendo os nomes sem correspondência entre os dois lados. Entrando na base original do GitHub
# vemos que muitos ds matchs não ocorridos foram por territórios - vamos abrir esses casos

difs1<-sort(setdiff(base_oxford$Country_Region, lista_dados$agrupado_por_pais$Country_Region))
difs2<-sort(setdiff(lista_dados$agrupado_por_pais$Country_Region, base_oxford$Country_Region))

difs1
# "Guam" "Puerto Rico" "United States Virgin Islands"

# Guam - território americano na Micronésia - não separa no GitHub
# Puerto Rico - território Americano que não está spearado no GitHub
# United States Virgin Islands - território Americano que não está spearado no GitHub

difs2
# "Diamond Princess" "Holy See" "Kosovo"               
# "Malawi" "MS Zaandam" "Sao Tome and Principe"
# "Sierra Leone" "South Sudan" "Western Sahara"       
# "Yemen"   

# Diamond Princess é um caso estranho porque existe na base d GitHub como território do Canadá e também
# como país separado
# Holy See é o Vaticano - não tem nada na de Oxford para lá
# MS Zaandam é um navio
# Kosovo, Malawi, Sao Tome and Principe, Sierra Leone, South Sudan,
# Western Sahara, Yemen não aparecem em Oxford

lista_dados$agrupado_por_pais <- lista_dados$agrupado_por_pais %>% left_join(base_oxford)

saveRDS(lista_dados, "lista_dados.rds")
write_xlsx(lista_dados, "planilha_dados.xlsx")

