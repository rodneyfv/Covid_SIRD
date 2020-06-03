library("tidyverse")

mun <- readRDS("./dados_por_municipio.rds")
View(mun)

length(unique(mun$Estado))
length(unique(mun$Município))
colnames(mun)
aux <- filter(mun, Estado=="SAO PAULO", Município=="São Paulo")
View(aux)

# curva de casos em Sao Paulo
ggplot(data = aux) +
  geom_point(mapping = aes(x = Data, y = confirmed))

# verificando quantos pontos no tempo temos para cada município
aux <- group_by(mun, Estado, Município) %>% summarize(count = n())
head(aux)
mean(aux$count)
table(aux$count)
# todos os municípios tem o mesmo número de observações

aux <- select(mun,Estado,Município,confirmed,Data,IDHM_Renda,
              IDHM_Longevidade,IDHM_Educação)
dim(aux)
View(aux)
aux2 <- spread(aux, key = Data, value = confirmed)
dim(aux2)
View(aux2)

ggplot(aux, aes(x=Data, y=confirmed, group=interaction(Estado, Município))) +
  geom_line() + theme_bw()

library("fdANOVA")
View(aux2)
aux2 <- filter(aux2, Estado!="DISTRITO FEDERAL")
table(aux2$Estado)

aux3_data <- t(as.matrix(aux2[,-c(1:5)]))
dim(aux3_data)
View(aux3_data)
aux3_groups <- as.matrix(aux2[,1]$Estado)
plotFANOVA(x = aux3_data)

plotFANOVA(x = aux3_data, group.label = aux3_groups)
plotFANOVA(x = aux3_data, group.label = aux3_groups,
           separately = TRUE)
plotFANOVA(x = aux3_data, group.label = aux3_groups,
           means = TRUE)
set.seed(123)
(fanova <- fanova.tests(x = aux3_data, group.label = aux3_groups))

which(aux2[,2]=="Fortaleza")
plot(1:84,diff(aux3_data[,1]))
dim(aux3_data)

aux4 <- apply(aux3_data, 2, diff)
dim(aux4)

plot(aux4[,1])
tmp <- smooth.spline(1:84,aux4[,1])
lines(1:84,tmp$y)

aux5 <- aux4
aux5[is.na(aux5)] <- 0
for (i in 1:dim(aux5)[2]){
  tmp <- smooth.spline(1:84,aux5[,i])
  aux5[,i] <- tmp$y
}
aux5[aux5<0] <- 0
matplot(aux5,type='l',lty=1,col=1)


aux_groups <- as.matrix(aux2$IDHM_Renda > 0.7)
plotFANOVA(x = aux5[,-c(which(is.na(aux2$IDHM_Renda)))], 
           group.label = aux_groups[-c(which(is.na(aux2$IDHM_Renda)))])

set.seed(123)
(fanova <- fanova.tests(x = aux5[,-c(which(is.na(aux2$IDHM_Renda)))],
                        group.label = aux_groups[-c(which(is.na(aux2$IDHM_Renda)))]))

# pacote pra ler planilha do excel
library(readxl)
library(tidyverse)
require(stringi)

aux <- read_xlsx("Anuario_Estatistico_de_Turismo_2020_-_Ano_base_2019.xlsx",
                 sheet = 90)
View(aux)
aux2 <- aux[c(7:23),c(1,3)]
View(aux2)
aux2 <- rename(aux2,Estado = `3. Síntese Brasil`) %>%
  rename(turistas2019 = `...3`)
aux2 <- mutate(aux2, Estado = toupper(Estado)) %>%
  mutate(Estado = stri_trans_general(str = Estado, id = "Latin-ASCII"))

aux3 <- aux2 %>% left_join(mun, by = "Estado")
View(aux3)


