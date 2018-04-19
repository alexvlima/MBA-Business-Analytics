setwd("/cloud/project/Modelagem-Preditiva")

# install.packages("gmodels", dependencies = T)
# install.packages("readxl", dependencies = T)
# install.packages("arules", dependencies = T)
# install.packages("pROC", dependencies = T)
# install.packages("hmeasure", dependencies = T)

library(readxl)
library(gmodels)
library(arules)
library(pROC)
library(hmeasure)

TELEGV <- read_excel("Modelagem-Preditiva/TELEGV.xlsx")
# View(TELEGV)

tt <- TELEGV[,-1]

# exemplo de análise
m <- table(tt$TMPCLI,tt$PR0M0)
m
prop.table(m,1)

boxplot(tt$FUNC~tt$PR0M0,col=rainbow(4))

# dividir a amostra em duas partes
nrow(tt)

set.seed(982192167)
flag=sample(1:2100,1100,replace=F)

lrn <- tt[flag,] #arquivo learn
tst <- tt[-flag,] #arquivo teste

# rodar a regressão logística - ajuste do modelo
fit <- glm(data=lrn, formula = PR0M0~PROD_A+PROD_B+PROD_C+REGIAO+SETOR+TMPCLI+SEDE+FUNC,
           family = binomial())
# para incluir todas as variáveis no modelo colocar glm(lrn,formula=PR0M0~.,family=binomial())

summary(fit) # sumário do modelo
# em modelagem preditiva, trabalhamos com p-value inferior a 0.15 ou 0.20 para seleção de variáveis

# utilizando método stepwise para seleção de variáveis
fit2 <- step(fit)
summary(fit2)

# acrescentando a probabilidade gerada pelo modelo logístico no dataset de teste
tst$psim <- predict(fit2, newdata = tst, type = "response") 
# a resposta é a probabilidade de sim (prob(sim)), ou seja PR0M0 = 1

# classificando psim = 0.5 (ponto de corte = 50%)
tst$class <- ifelse(tst$psim > 0.5, "yes", "no")

# matriz de classificação (confusion matrix)
m <- CrossTable(tst$PR0M0, tst$class, prop.c = F, prop.t = F, prop.chisq = F)

# calcular o percentual de erro a partir da matriz de classificação
tx_erro_prob_0.5 <- (39+46)/1000
tx_erro_prob_0.5

# alterando o ponto de corte para 75% (psim = 0.75)
tst$class <- ifelse(tst$psim > 0.75, "yes", "no")

# matriz de classificação
m <- CrossTable(tst$PR0M0, tst$class, prop.c = F, prop.t = F, prop.chisq = F)

# percentual de classificação errada
tx_erro_prob_0.75 <- (22+111)/1000
tx_erro_prob_0.75

# avaliar a capacidade preditiva do modelo
# descritizar a variável psim em 5 classes de mesma frequência

kpsim <- discretize(tst$psim, method = "frequency", categories = 5) # pode utilizar o breaks
# verificando a categoria estimada com o observado (a probabilidade de sim observada em cada categoria tem que estar dentro da faixa)
m <- CrossTable(kpsim, tst$PR0M0, prop.c = F, prop.t = F, prop.chisq = F)
# pode fazer o teste de Hosmer-Lemeshow ou mais indicado atualmente o teste de Spiegelhalter

# calcular a curva ROC
roc_curve <- roc(tst$PR0M0, tst$psim)
roc_curve
plot(roc_curve) # Gráfico da curva ROC

HMeasure(tst$PR0M0, tst$psim)$metrics

