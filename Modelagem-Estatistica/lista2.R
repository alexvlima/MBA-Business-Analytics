#################################
# Modelagem Estatitica Avancada  #
#           Lista 2              #
##################################

### QUESTÃƒO 1

set.seed(12345)
# Simular os dados (criar um data frame para exemplo)
x1 <- rnorm(100)
x2 <- .3*x1*rnorm(100)*.2
x3 <- .9*x1*rnorm(100)*.9
x4.1 <- rnorm(100)
x4.2 <- rnorm(100, sd = 100) # Gerar lista com DP = 100
x <- as.data.frame(cbind(x1,x2,x3,x4.1))
w <- as.data.frame(cbind(x1,x2,x3,x4.2)) # Esse data frame inclui a variÃ¡vel com DP = 100

# Calcular matrizes de correlaÃ§Ã£o de x e w
cor(x)
cor(w)

# Matrix de gráficos de dispersão
plot(x)
plot(w)

# Criar os objetos PCA com matriz de covariÃ¢ncias
pcax <- princomp(x,cor=FALSE)
pcaw <- princomp(w,cor=FALSE)
summary(pcax)
summary(pcaw)
pcax$loadings
pcaw$loadings


# Criar os objetos PCA com matriz de correlaÃ§Ãµes
pcaxb <- princomp(x,cor=TRUE)
pcawb <- princomp(w,cor=TRUE)
summary(pcaxb)
summary(pcawb)
pcaxb$loadings
pcawb$loadings

### QUESTÃƒO 2

# Carregar os pacotes a serem usados
install.packages("pixmap")
getwd()
library(pixmap)
image <- read.pnm("sak.ppm")

# Fazer a leitura da matriz verde
green.matrix <- matrix(image@green, nrow = image@size[1], ncol = image@size[2])

# Plotar a imagem
image(green.matrix, col = heat.colors(255))

# Realizar a decomposicÃ£o em valores singulares
green.matrix.svd <- svd(green.matrix)

d <- green.matrix.svd$d
u <- green.matrix.svd$u
v <- green.matrix.svd$v

par(mar=c(4,4,0.5,0.5))

plot(d/sum(d), xlab = "NÃºmero de componentes",
     ylab = "ProporÃ§Ã£o da variÃ¢ncia explicada",
     type = "b")
plot(cumsum(d)/sum(d), xlab = "NÃºmero de componentes",
     ylab = "Percentual da VariÃ¢ncia explicada",
     type = "b")
as.matrix(d)
diag(d)
sum(as.matrix(d[1:71]))/sum(as.matrix(d))

# Reconstruindo com número reduzido de variáveis

green.matrix.compressed <- as.matrix(u[,1]) %*% d[1:1] %*% as.matrix(t(v[,1]))
image(green.matrix.compressed, col = heat.colors(255))

i <- 20
green.matrix.compressed <- u[,1:i] %*% diag(d[1:i]) %*% t(v[,1:i])
image(green.matrix.compressed, col = heat.colors(255))


# QUESTÃO 3

install.packages("dplyr")
install.packages("corrplot")
library("dplyr")
library("ggplot2")
library("corrplot")
library("psych")

load("hbat.Rdata")
row.names(hbat) <- hbat$id
select1 <- select(hbat,num_range("x",6:18))
head(select1)
cor <- cor(select1)
corrplot(cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Rodar o MSA 
KMO(select1)
select2 <- select(select1,-x15) # Tirando o 15 porque o MSA Ã© menor que 0.5 e o mais baixo
KMO(select2)
select3 <- select(select2,-x17) # Jogando fora o 17
KMO(select3)
# select <- select(select3,-x11) # Em tese não precisaria tirar o 11, porque melhorou. Tirou apenas porque o livro tira os três de uma vez


### QUESTÃ•ES 4 E 5

install.packages("GPArotation")
library("GPArotation")

cor <- cor(select3)
corrplot(cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

EFA <- fa(select3, nfactors = 4, rotate = "varimax")#, fm = "pa")
EFA$loadings

### QUESTÃƒO 6

EFA2 <- fa(select3, nfactors = 4, rotate = "promax")
EFA2$loadings

cor(EFA$scores)
cor(EFA2$scores)
