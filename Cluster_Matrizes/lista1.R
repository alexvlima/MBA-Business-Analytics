#################
### DIRETORIO ###
#################

getwd()
setwd("/Volumes/KINGSTON/FGV/Clusters e matrizes/Aula 02")


#################
### LIBRARIES ###
#################

library(dplyr)
library(hmeasure)
library(ResourceSelection)

#################
#### DATASET ####
#################

hbat400<-read.csv("hbat_clima.csv")


#################
### QUESTÃO 3 ###
#################

fit.min.hbat400<- glm(evasao~1,family = binomial(link = "logit"),
                      data=hbat400)


stp.hbat400<-step(fit.min.hbat400,
                  direction="both",
                  scope=(~
                           JS1 + JS2 + JS3 + JS4 + JS5 + 
                           OC1 + OC2 + OC3 + OC4 + 
                           EP1 + EP2 + EP3 + EP4 + 
                           AC1 + AC2 + AC3 + AC4 + 
                           SI1 + SI2 + SI3 + SI4
                  ))

summary(stp.hbat400)

# A função step utiliza o AIC como critério para a entrada/saída das variáveis no modelo.
# Como default o p-valor é de 0.15 (0.1573). 
# Para alterar para o p-valor para 0.10, basta modificar o parâmetro k para 2.7
# Fonte: https://stats.stackexchange.com/questions/97257/stepwise-regression-in-r-critical-p-value

stp.hbat400_2<-step(fit.min.hbat400,
                  direction="both",
                  scope=(~
                           JS1 + JS2 + JS3 + JS4 + JS5 + 
                           OC1 + OC2 + OC3 + OC4 + 
                           EP1 + EP2 + EP3 + EP4 + 
                           AC1 + AC2 + AC3 + AC4 + 
                           SI1 + SI2 + SI3 + SI4
                  ), k = 2.7)

summary(stp.hbat400_2)


#################
### QUESTÃO 4 ###
#################

summary(glm(evasao~SI1,family = binomial(link = "logit"),data=hbat400))
summary(glm(evasao~SI2,family = binomial(link = "logit"),data=hbat400))
summary(glm(evasao~SI3,family = binomial(link = "logit"),data=hbat400))
summary(glm(evasao~SI4,family = binomial(link = "logit"),data=hbat400))
cor(hbat400[,c(16,18,21)])


#################
### QUESTÃO 6 ###
#################


dataset_q6 <- select(hbat400, starts_with("SI"))
pca_q6 <- princomp(dataset_q6, cor = F) # utilizei a matriz de covariância, pois os dados já estão na mesma escala (Likert)
pca_q6

pca_q6$loadings # carga fatorial
pca_q6$sdev # desvio padrão das componentes
plot(pca_q6, type="l", main = "Variância das Componentes Principais")

componente1 <- pca_q6$scores[,1]
componente1
hbat_q6 <- as.data.frame(cbind(evasao=hbat400$evasao,componente1))
# head(hbat_q6)
tail(hbat_q6)
fit_q6<- glm(evasao~.,family = binomial(), # o link = logistic é default
                      data=hbat_q6)
summary(fit_q6)

# Avaliação do ajuste do modelo - Teste Hosmer-Lemeshow
# 1) Modelo de Regressão Logística com Stepwise
hoslem.test(stp.hbat400$y, fitted(stp.hbat400))

# 2) Modelo de Regressão Logística com Componente Principal (SI)
hoslem.test(fit_q6$y, fitted(fit_q6))


# Avaliação do grau de discriminação do modelo
# 1) Modelo de Regressão Logística com Stepwise
pred_evasao_stp <- NULL
pred_evasao_stp <- predict(stp.hbat400,newdata = pred_evasao_stp, type = "response")
HMeasure(hbat400$evasao, pred_evasao_stp)$metrics

# 2) Modelo de Regressão Logística com Componente Principal (SI)
pred_evasao_pca <- NULL
pred_evasao_pca <- predict(fit_q6,newdata = pred_evasao_pca, type = "response")
HMeasure(hbat400$evasao, pred_evasao_pca)$metrics


