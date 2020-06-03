library("tidyverse")
library("fdANOVA")

mun <- readRDS("./dados_por_municipio.rds")
View(mun)
dim(mun)

# selecionando numero de casos confirmados por municipio e IDHMs
mun_conf <- select(mun,Estado,Município,confirmed,Data,IDHM_Renda,
              IDHM_Longevidade,IDHM_Educação) %>%
  # deixando as datas nas colunas
  spread(key = Data, value = confirmed) %>%
  # removendo DF, pois pra usar fdANOVA os grupos precisam ter n>1
  filter(Estado!="DISTRITO FEDERAL") %>%
  # removendo municipios que tem NA em IDHM_Renda
  filter(!is.na(IDHM_Renda))
dim(mun_conf)

# removendo os NAs
mun_conf <- na.omit(mun_conf)

# matriz em que nas colunas teremos as curvas suavizadas de casos diarios
suav_curv_dia <- matrix(nrow=84,ncol=dim(mun_conf)[1])
for(i in 1:dim(mun_conf)[1]){
  tmp <- as.matrix(t(mun_conf[i,6:90]))
  tmp2 <- smooth.spline(1:84,diff(tmp))
  suav_curv_dia[,i] <- tmp2$y
}
suav_curv_dia[suav_curv_dia<0] <- 0

plot(1:84,suav_curv_dia[,which(mun_conf[,2]=="São Paulo")],type='l')
matplot(suav_curv_dia,type='l',lty=1,col=1)

set.seed(2020)
# Avaliando o efeito de IHHM_Renda
aux_groups <- as.matrix(mun_conf$IDHM_Renda > 0.7)
plotFANOVA(x = suav_curv_dia, group.label = aux_groups,
           means = TRUE)
fanova <- fanova.tests(x = suav_curv_dia,
                        group.label = aux_groups, test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)

# Avaliando o efeito de IHHM_Renda
aux_groups <- as.matrix(mun_conf$IDHM_Longevidade > 0.8)
plotFANOVA(x = suav_curv_dia, group.label = aux_groups,
           means = TRUE)
fanova <- fanova.tests(x = suav_curv_dia,
                       group.label = aux_groups, test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)

# Avaliando o efeito de IHHM_Renda
aux_groups <- as.matrix(mun_conf$IDHM_Educação > 0.6)
plotFANOVA(x = suav_curv_dia, group.label = aux_groups,
           means = TRUE)
fanova <- fanova.tests(x = suav_curv_dia,
                       group.label = aux_groups, test = "FP",
                       parallel = TRUE, nslaves = 2)
summary(fanova)


########


