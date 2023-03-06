library(vcd)
library(ggstatsplot)
library(tidyverse)
library(sjPlot)
library(sf)
library(corrplot)
library(readxl)
library(readr)
library(mfx)
library(caret)
library(pRoc)
library(ResourceSelection)
library(modEvA)
library(foreign)
library(stargazer)



#Inserindo base de dados

cadunico <- read_excel("data/CECAD - agosto 2020 - SEPLAN_Original.xlsx")
resultados_familias <- read_csv2("data/resultados_familias.csv")

resultados_individuos <- cadunico %>%
  left_join(resultados_familias, by = "Código familiar")

racahab <- table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`)
ftable(resultados_individuos$`Cor ou raça`,resultados_individuos$`Déficit ou Inadequação`)

round(prop.table(table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`),margin=2),2)
round(prop.table(table(resultados_individuos$`Cor ou raça`, resultados_individuos$`Déficit ou Inadequação`),margin=1),2)
       
racahab <- rename(racahab, Var1 = Deficit ou Inaquedacao)
colnames(racahab)[1] <- "Deficit ou Inaquedacao"
colnames(racahab)[2] <- "Cor ou Raca"

#Removendo Amarelos e Indígenas de Cor ou Raça

resultados_pb <- resultados_individuos[resultados_individuos$`Cor ou raça`!="Indígena" & 
                                            resultados_individuos$`Cor ou raça`!="Amarela",]

#Substituindo "Preta" e "Parda" por "Negra" (Negros = Pretos + Pardos)

resultados_pb$`Cor ou raça`[resultados_pb$`Cor ou raça` == "Preta"] <- "Negra"
resultados_pb$`Cor ou raça`[resultados_pb$`Cor ou raça` == "Parda"] <- "Negra"



#Teste Qui-Quadrado Homogeneidade para I01 (Densidade Excessiva)

#Removendo Sem Dados ou Não Aplicável das categorias de I01

resultados_I01 <- resultados_pb[resultados_pb$`I01`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I01)

table(resultados_I01$`Cor ou raça`, resultados_I01$`I01`)

racahab_I01 <- table(resultados_I01$`Cor ou raça`, resultados_I01$`I01`)

EiI01 <- outer(rowSums(racahab_I01), colSums(racahab_I01), "*")/sum(racahab_I01)
                                                                    
chisq.test(racahab_I01)$expected

chisq.test(racahab_I01, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I01)

ggstatsplot::ggbarstats(
  data = resultados_I01,
  x = `Cor ou raca`, 
  y = `I01`, 
  title = "Inadequação de moradia por densidade excessiva", 
  xlab = "Densidade excessiva", 
  legend.title = "Cor ou raça" 
)

#cálculo dos resíduos (Raça x I01)

residuos_I01 <- chisq.test(resultados_I01$`Cor ou raça`, resultados_I01$`I01`)
round(residuos_I01$residuals, 3)
corrplot(residuos_I01$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I01)

contrib_I01 <- 100*residuos_I01$residuals^2/residuos_I01$statistic
round(contrib_I01, 3)
corrplot(contrib_I01, is.cor = FALSE)


#Teste Qui-Quadrado Homogeneidade para I02 (Ausência de Banheiro)

#Removendo Sem Dados ou Não Aplicável das categorias de I02


resultados_corrplot(contrib_I03, is.cor = FALSE)
 <- resultados_pb[resultados_pb$`I02`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I02)

table(resultados_I02$`Cor ou raça`, resultados_I02$`I02`)

racahab_I02 <- table(resultados_I02$`Cor ou raça`, resultados_I02$`I02`)

EiI02 <- outer(rowSums(racahab_I02), colSums(racahab_I02), "*")/sum(racahab_I02)

chisq.test(racahab_I02)$expected

chisq.test(racahab_I02, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I02)

ggstatsplot::ggbarstats(
  data = resultados_I02,
  x = `I02`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por ausência de banheiro", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de banheiro" 
)

#cálculo dos resíduos (Raça x I02)

residuos_I02 <- chisq.test(resultados_I02$`Cor ou raça`, resultados_I02$`I02`)
round(residuos_I02$residuals, 3)
corrplot(residuos_I02$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I02)

contrib_I02 <- 100*residuos_I02$residuals^2/residuos_I02$statistic
round(contrib_I02, 3)
corrplot(contrib_I02, is.cor = FALSE)

#Regressão Logística (Raça x I01)

colnames(resultados_I01)[47] <- "Cor ou raca"

regressao_I01 <- resultados_I01

regressao_I01$`I01`[regressao_I01$`I01` == "Adequado"] <- 1
regressao_I01$`I01`[regressao_I01$`I01` == "Inadequado"] <- 0

colnames(regressao_I01)[47] <- "Negra"

colnames(regressao_I01)[151] <- "I01"
regressao_I01$`Branca` <- regressao_I01$Negra


regressao_I01$I01 <- as.numeric(as.character(regressao_I01$I01))
regressao_I01$I01 <- as.numeric(as.character(regressao_I01$I01))

regressao_I01$`Cor ou raca`[regressao_I01$`Cor ou raca` == "Negra"] <- 1 
regressao_I01$`Cor ou raca`[regressao_I01$`Cor ou raca` == "Branca"] <- 0 

regressao_I01$`Cor ou raca` <- factor(regressao_I01$`Cor ou raca`)

mylogit_I01 <- glm(`I01` ~ `Cor ou raca`, data = regressao_I01, family = binomial(link="logit")) 
summary(mylogit_I01)                      


ggplot(regressao_I01, aes(x=`Cor ou raca`, y=`I01`)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

logit=glm(I01~`Cor ou raca`, data=regressao_I01, family = binomial(link="logit"))
summary(logit)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I03 (Material de Piso Inadequado)

#Removendo Sem Dados ou Não Aplicável das categorias de I03

resultados_I03 <- resultados_pb[resultados_pb$`I03`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I03)

table(resultados_I03$`Cor ou raça`, resultados_I03$`I03`)

racahab_I03 <- table(resultados_I03$`Cor ou raça`, resultados_I03$`I03`)

EiI03 <- outer(rowSums(racahab_I03), colSums(racahab_I03), "*")/sum(racahab_I03)

chisq.test(racahab_I03)$expected

chisq.test(racahab_I03, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I03)

ggstatsplot::ggbarstats(
  data = resultados_I03,
  x = `I03`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Material de piso inadequado", 
  xlab = "Cor ou raça", 
  legend.title = "Material de Piso Inadequado" 
)

#cálculo dos resíduos (Raça x I03)

residuos_I03 <- chisq.test(resultados_I03$`Cor ou raça`, resultados_I03$`I03`)
round(residuos_I03$residuals, 3)
corrplot(residuos_I03$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I03)

contrib_I03 <- 100*residuos_I03$residuals^2/residuos_I03$statistic
round(contrib_I03, 3)
corrplot(contrib_I03, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I04 (Ausência de Água Canalizada)

#Removendo Sem Dados ou Não Aplicável das categorias de I04

resultados_I04 <- resultados_pb[resultados_pb$`I04`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I04)

table(resultados_I04$`Cor ou raça`, resultados_I04$`I04`)

racahab_I04 <- table(resultados_I04$`Cor ou raça`, resultados_I04$`I04`)

EiI04 <- outer(rowSums(racahab_I04), colSums(racahab_I04), "*")/sum(racahab_I04)

chisq.test(racahab_I04)$expected

chisq.test(racahab_I04, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I04)

ggstatsplot::ggbarstats(
  data = resultados_I04,
  x = `I04`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ausência de Água Canalizada", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de Água Canalizada" 
)

#cálculo dos resíduos (Raça x I04)

residuos_I04 <- chisq.test(resultados_I04$`Cor ou raça`, resultados_I04$`I04`)
round(residuos_I04$residuals, 3)
corrplot(residuos_I04$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I04)

contrib_I04 <- 100*residuos_I04$residuals^2/residuos_I04$statistic
round(contrib_I04, 3)
corrplot(contrib_I04, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I05 (Ausência de Coleta e Tratamento de Esgoto)

#Removendo Sem Dados ou Não Aplicável das categorias de I05

resultados_I05 <- resultados_pb[resultados_pb$`I05`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I05)

table(resultados_I05$`Cor ou raça`, resultados_I05$`I05`)

racahab_I05 <- table(resultados_I05$`Cor ou raça`, resultados_I05$`I05`)

EiI05 <- outer(rowSums(racahab_I05), colSums(racahab_I05), "*")/sum(racahab_I05)

chisq.test(racahab_I05)$expected

chisq.test(racahab_I05, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I05)

ggstatsplot::ggbarstats(
  data = resultados_I05,
  x = `I05`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ausência de Coleta e
Tratamento de Esgoto", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de Coleta e
Tratamento de Esgoto" 
)

#cálculo dos resíduos (Raça x I05)

residuos_I05 <- chisq.test(resultados_I05$`Cor ou raça`, resultados_I05$`I05`)
round(residuos_I05$residuals, 3)
corrplot(residuos_I05$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I05)

contrib_I05 <- 100*residuos_I05$residuals^2/residuos_I05$statistic
round(contrib_I05, 3)
corrplot(contrib_I05, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I06 (Ausência de Abastecimento de Água por Rede Pública)

#Removendo Sem Dados ou Não Aplicável das categorias de I06

resultados_I06 <- resultados_pb[resultados_pb$`I06`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I06)

table(resultados_I06$`Cor ou raça`, resultados_I06$`I06`)

racahab_I06 <- table(resultados_I06$`Cor ou raça`, resultados_I06$`I06`)

EiI06 <- outer(rowSums(racahab_I06), colSums(racahab_I06), "*")/sum(racahab_I06)

chisq.test(racahab_I06)$expected

chisq.test(racahab_I06, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I06)

ggstatsplot::ggbarstats(
  data = resultados_I06,
  x = `I06`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ausência de Abastecimento
de Água por Rede Pública", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de Abastecimento
de Água por Rede Pública" 
)

#cálculo dos resíduos (Raça x I06)

residuos_I06 <- chisq.test(resultados_I06$`Cor ou raça`, resultados_I06$`I06`)
round(residuos_I06$residuals, 3)
corrplot(residuos_I06$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I06)

contrib_I06 <- 100*residuos_I06$residuals^2/residuos_I06$statistic
round(contrib_I06, 3)
corrplot(contrib_I06, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I07 (Ausência de Fornecimento de Energia Elétrica)

#Removendo Sem Dados ou Não Aplicável das categorias de I07

resultados_I07 <- resultados_pb[resultados_pb$`I07`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I07)

table(resultados_I07$`Cor ou raça`, resultados_I07$`I07`)

racahab_I07 <- table(resultados_I07$`Cor ou raça`, resultados_I07$`I07`)

EiI07 <- outer(rowSums(racahab_I07), colSums(racahab_I07), "*")/sum(racahab_I07)

chisq.test(racahab_I07)$expected

chisq.test(racahab_I07, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I07)

ggstatsplot::ggbarstats(
  data = resultados_I07,
  x = `I07`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ausência de Fornecimento de
Energia Elétrica", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de Ausência de Fornecimento de
Energia Elétrica" 
)

#cálculo dos resíduos (Raça x I07)

residuos_I07 <- chisq.test(resultados_I07$`Cor ou raça`, resultados_I07$`I07`)
round(residuos_I07$residuals, 3)
corrplot(residuos_I07$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I07)

contrib_I07 <- 100*residuos_I07$residuals^2/residuos_I07$statistic
round(contrib_I07, 3)
corrplot(contrib_I07, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I08 (Ausência de Coleta de Lixo)

#Removendo Sem Dados ou Não Aplicável das categorias de I08

resultados_I08 <- resultados_pb[resultados_pb$`I08`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I08)

table(resultados_I08$`Cor ou raça`, resultados_I08$`I08`)

racahab_I08 <- table(resultados_I08$`Cor ou raça`, resultados_I08$`I08`)

EiI08 <- outer(rowSums(racahab_I08), colSums(racahab_I08), "*")/sum(racahab_I08)

chisq.test(racahab_I08)$expected

chisq.test(racahab_I08, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I08)

ggstatsplot::ggbarstats(
  data = resultados_I08,
  x = `I08`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ausência de Coleta de Lixo", 
  xlab = "Cor ou raça", 
  legend.title = "Ausência de Coleta de Lixo" 
)

#cálculo dos resíduos (Raça x I08)

residuos_I08 <- chisq.test(resultados_I08$`Cor ou raça`, resultados_I08$`I08`)
round(residuos_I08$residuals, 3)
corrplot(residuos_I08$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I08)

contrib_I08 <- 100*residuos_I08$residuals^2/residuos_I08$statistic
round(contrib_I08, 3)
corrplot(contrib_I08, is.cor = FALSE)

#------------------------------------------------------------------------------

#Teste Qui-Quadrado Homogeneidade para I09 (Ônus Excessivo com Aluguel)

#Removendo Sem Dados ou Não Aplicável das categorias de I09

resultados_I09 <- resultados_pb[resultados_pb$`I09`!= "Sem Dados ou Não Aplicável",] 

#Tabela de contigêcia (Raça x I09)

table(resultados_I09$`Cor ou raça`, resultados_I09$`I09`)

racahab_I09 <- table(resultados_I09$`Cor ou raça`, resultados_I09$`I09`)

EiI09 <- outer(rowSums(racahab_I09), colSums(racahab_I09), "*")/sum(racahab_I09)

chisq.test(racahab_I09)$expected

chisq.test(racahab_I09, correct = FALSE)

#Gráfico de proporções de frequência (Raça x I09)

ggstatsplot::ggbarstats(
  data = resultados_I09,
  x = `I09`, 
  y = `Cor ou raça`, 
  title = "Inadequação de moradia por Ônus Excessivo com Aluguel", 
  xlab = "Cor ou raça", 
  legend.title = "Ônus Excessivo com Aluguel" 
)

#cálculo dos resíduos (Raça x I09)

residuos_I09 <- chisq.test(resultados_I09$`Cor ou raça`, resultados_I09$`I09`)
round(residuos_I09$residuals, 3)
corrplot(residuos_I09$residuals, is.cor = FALSE)

#cálculo da contribuição das células (Raça x I09)

contrib_I09 <- 100*residuos_I09$residuals^2/residuos_I09$statistic
round(contrib_I09, 3)
corrplot(contrib_I09, is.cor = FALSE)

#------------------------------------------------------------------------------
