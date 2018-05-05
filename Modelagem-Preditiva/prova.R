getwd()
setwd("/cloud/project/Modelagem-Preditiva")

library(readxl)
library(rpart)
library(rpart.plot)
library(gmodels)
library(neuralnet)
library(hmeasure)

base <- read_excel("base_de_dados_impar.xlsx")
View(base)

# Questao 1 - A

base_arvore <- base
tree <- rpart(data = base_arvore,PROMO~.)
prp(tree, type=1, extra=105, nn=T, fallen.leaves = T, branch.col = "red", 
    branch.lty = 5,box.col = c("white",'blue'))
printcp(tree)

ind <- data.frame(PROD_A = 19,
                  PROD_B = 7.98,
                  PROD_C = 8.36,
                  REGIAO = "SUL",
                  SETOR = "COM",
                  TMPCLI = "OLD",
                  SEDE = 0,
                  FUNC = 10)

predict(tree,newdata = ind)

base_arvore$prob=predict(tree, newdata=base_arvore, type="prob")
head(base_arvore)

HMeasure(base_arvore$PROMO, base_arvore$prob)$metrics

printcp(tree)
tree2 <- prune(tree,cp=.1) # podar = 2
printcp(tree2)
prp(tree2, type=1, extra=105, nn=T, fallen.leaves = T, branch.col = "red", 
    branch.lty = 5,box.col = c("white",'blue'))

predict(tree2,newdata = ind)

# Questao 1 - B

base_reg <- base[,c(1,2,3,4,9)]

base_reg$PROMO <- ifelse(base_reg$PROMO == "sim",1,0)

fit <- glm(data=base_reg, formula = PROMO~PROD_A+PROD_B+PROD_C+REGIAO,
           family = binomial())
summary(fit)

# calular a taxa de erro
base_reg$psim=predict(fit, newdata = base_reg,type = "response")

# classificando utilizando ponto de corte psim=0.6
base_reg$class=ifelse(base_reg$psim>.6, "yes","nao")
# matriz de classificacao
m=CrossTable(base_reg$PROMO,base_reg$class, prop.c = F,prop.t = F, prop.chisq = F )

taxadeerrocom.6=(211+346)/1507
taxadeerrocom.6

# Questao 2


base_net <- base[,c(1,2,3,9)]

var_ind <- model.matrix(data = base_net, ~.)
class(var_ind)

# mudar para data frame (era matrix)
var_ind <- as.data.frame(var_ind)
head(var_ind, 6)

var_ind <- var_ind[,-1] # Eliminar o intercept

# padronizar as variaveis - entre 0 e 1
min <- apply(var_ind, 2, min)
max <- apply(var_ind, 2, max)

var_ind_scale <- scale(var_ind, 
                       center = min, scale = (max-min))

var_ind_scale <- as.data.frame(var_ind_scale)
base_padr <- var_ind_scale
head(base_padr)


set.seed(1234)
rede2 <- neuralnet(PROMOsim~PROD_A+PROD_B+PROD_C,
                   data = base_padr, hidden = 2, lifesign = "minimal", linear.output = F)


head(rede2$net.result [[1]], 5)


out.tst <-rede2$net.result [[1]]
tail(out.tst)

resultado_teste <- cbind(base_padr, out.tst)


HMeasure(resultado_teste$PROMOsim,resultado_teste$out.tst)$metrics
