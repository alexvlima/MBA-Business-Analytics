# getwd()
setwd("/cloud/project/Modelagem-Preditiva")

#install.packages("rpart")
#install.packages("RColorBrewer")
#install.packages("rpart.plot")
#install.packages("partykit")

library(readxl)
library(rpart)
library(rpart.plot)
library(hmeasure)
library(partykit)


# Base de Dados

bux <- readxl::read_excel("DADOS EXERCICIOS MODELOS PREDITIVOS   2017 11 21.xlsx", 
                          sheet = "BUXshrt")

bux <- bux[,2:8]


# Retira as linhas com missing value
bux = na.omit(bux)

# Criar duas amostras
set.seed(123)
flag <- sample(1:3000, 1800, replace = F)
lrn <- bux[flag,]
tst <- bux[-flag,]

# Rodar árvore
tree <- rpart(data = lrn,STATUS~IDADE+UNIFED+FONE+RESTR+QUANTI+NET) #method=class para avisar que não é quantitativa
tree
# Plota a árvore
prp(tree, type=2, extra=104, nn=T, fallen.leaves = T, branch.col = "red", branch.lty = 5,box.col = c("white","green"))

# Análise para podar a árvore
printcp(tree)

# Caso seja identificada a necessidade de podar, com o aumento da xerror 
# (escolher o CP entre o split 1 e 2 se quiser fazer a poda no 2)
#        CP nsplit rel error  xerror     xstd
#1 0.092179      0   1.00000 1.00000 0.047305
#2 0.011173      2   0.81564 0.81564 0.043689
#3 0.010000      5   0.78212 0.82961 0.043988

tree2 = prune(tree,cp=.05)
prp(tree2, type=2, extra=104, nn=T, fallen.leaves = T, branch.col = "red", branch.lty = 5,box.col = c("white","green"))
printcp(tree2)

# Prever as probabilidades
prob <- predict(tree2, newdata=tst, type="prob")
head(prob)
tst$pbom <- prob[,1]

# Prever a classe (ponto de corte padrão .5)
tst$clas <- predict(tree2, newdata=tst, type="class")

# Capacidade de discriminação
HMeasure(tst$STATUS, tst$pbom)$metrics

# Discretização com ctree
tst$STATUS_a <- as.factor(tst$STATUS)
ctree(data = tst, STATUS_a~IDADE)