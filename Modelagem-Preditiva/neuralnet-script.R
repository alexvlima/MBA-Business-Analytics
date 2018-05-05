# getwd()
setwd("/cloud/project/Modelagem-Preditiva")
list.files() # Lista os arquivos do diretorio

bux <- readxl::read_excel("DADOS EXERCICIOS MODELOS PREDITIVOS   2017 11 21.xlsx", 
                   sheet = "BUXshrt")

bux <- bux[,2:8]

# gerar as variaveis dummy

var_ind <- model.matrix(data = bux, ~.)
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
# View(var_ind_scale)
class(var_ind_scale)

# mudar para data frame (era matrix)
var_ind_scale <- as.data.frame(var_ind_scale)
base_padr <- var_ind_scale

# gerar duas amostras (learn e test)
set.seed(123)
flag <- sample(1:3000,1800,replace = F)

base_padr.lrn <- base_padr[flag,]
base_padr.tst <- base_padr[-flag,]

# rodar a rede neural

# install.packages("neuralnet")
library(neuralnet)

set.seed(123)
# ?neuralnet
# rede neural com 2 neuronios
rede2 <- neuralnet(STATUSMAU~IDADE+UNIFEDRJ+UNIFEDSP+FONESIM+RESTRSIM+QUANTISIM+NETSIM,
                  data = base_padr.lrn, hidden = 2, lifesign = "minimal", linear.output = F)

# rede neural com 4 neuronios
rede4 <- neuralnet(STATUSMAU~IDADE+UNIFEDRJ+UNIFEDSP+FONESIM+RESTRSIM+QUANTISIM+NETSIM,
                  data = base_padr.lrn, hidden = 4, lifesign = "minimal", linear.output = F)

# rede neural com 2 camadas e 4 neuronios em cada
rede_2_4 <- neuralnet(STATUSMAU~IDADE+UNIFEDRJ+UNIFEDSP+FONESIM+RESTRSIM+QUANTISIM+NETSIM,
                   data = base_padr.lrn, hidden = c(4,4), lifesign = "minimal", linear.output = F)

# rede neural com 6 neuronios
rede6 <- neuralnet(STATUSMAU~IDADE+UNIFEDRJ+UNIFEDSP+FONESIM+RESTRSIM+QUANTISIM+NETSIM,
                   data = base_padr.lrn, hidden = 6, lifesign = "minimal", linear.output = F)
plot(rede6)
rede6$result.matrix # mostra os pesos da equacao

rede6$data # contem os dados originais. cuidado que sao os dados padronizados!
head(rede6$net.result [[1]], 5) # contem os outputs

out <- cbind(base_padr.lrn, rede6$net.result[[1]])
# class(out) # ja e data frame 

head(out,5)

colnames(out)[9] <- "prob_mau"

# calcular o output para a base de teste

prob_net <- compute(rede6, base_padr.tst[,-1]) # tem que retirar a variavel target!

out.tst <- prob_net$net.result
head(out.tst)

resultado_teste <- cbind(base_padr.tst, out.tst)
head(resultado_teste)
# class(resultado_teste)

View(resultado_teste)

library(hmeasure)
HMeasure(resultado_teste$STATUSMAU,resultado_teste$out.tst)$metrics
