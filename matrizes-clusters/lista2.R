##################################
# Modelagem Estatística Avançada #
#           Lista 2              #
##################################

### QUESTÃO 1

set.seed(12345)
# Simular os dados (criar um data frame para exemplo)
x1 <- rnorm(100)
x2 <- .3*x1*rnorm(100)*.2
x3 <- .9*x1*rnorm(100)*.9
x4.1 <- rnorm(100)
x4.2 <- rnorm(100, sd = 100) # Gerar lista com DP = 100
x <- as.data.frame(cbind(x1,x2,x3,x4.1))
w <- as.data.frame(cbind(x1,x2,x3,x4.2)) # Esse data frame inclui a variável com DP = 100

# Calcular matrizes de correlação de x e w
cor(x)
cor(w)

# Matrix de gr?ficos de dispers?o
plot(x)
plot(w)

# Criar os objetos PCA com matriz de covariâncias
pcax <- princomp(x,cor=FALSE)
pcaw <- princomp(w,cor=FALSE)
summary(pcax)
summary(pcaw)
pcax$loadings
pcaw$loadings


# Criar os objetos PCA com matriz de correlações
pcaxb <- princomp(x,cor=TRUE)
pcawb <- princomp(w,cor=TRUE)
summary(pcaxb)
summary(pcawb)
pcaxb$loadings
pcawb$loadings

### QUESTÃO 2

# Carregar os pacotes a serem usados
install.packages("pixmap")
getwd()
library(pixmap)
image <- read.pnm("sak.ppm")

# Fazer a leitura da matriz verde
green.matrix <- matrix(image@green, nrow = image@size[1], ncol = image@size[2])

# Plotar a imagem
image(green.matrix, col = heat.colors(255))

# Realizar a decomposicão em valores singulares
green.matrix.svd <- svd(green.matrix)

d <- green.matrix.svd$d
u <- green.matrix.svd$u
v <- green.matrix.svd$v

par(mar=c(4,4,0.5,0.5))

plot(d/sum(d), xlab = "Número de componentes",
     ylab = "Proporção da variância explicada",
     type = "b")
plot(cumsum(d)/sum(d), xlab = "Número de componentes",
     ylab = "Percentual da Variância explicada",
     type = "b")
as.matrix(d)
diag(d)
sum(as.matrix(d[1:71]))/sum(as.matrix(d))

# Reconstruindo com n?mero reduzido de vari?veis

green.matrix.compressed <- as.matrix(u[,1]) %*% d[1:1] %*% as.matrix(t(v[,1]))
image(green.matrix.compressed, col = heat.colors(255))

i <- 20
green.matrix.compressed <- u[,1:i] %*% diag(d[1:i]) %*% t(v[,1:i])
image(green.matrix.compressed, col = heat.colors(255))


# QUEST?O 3

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
select2 <- select(select1,-x15) # Tirando o 15 porque o MSA é menor que 0.5 e o mais baixo
KMO(select2)
select3 <- select(select2,-x17) # Jogando fora o 17
KMO(select3)
# select <- select(select3,-x11) # Em tese n?o precisaria tirar o 11, porque melhorou. Tirou apenas porque o livro tira os tr?s de uma vez


### QUESTÕES 4 E 5

install.packages("GPArotation")
library("GPArotation")

cor <- cor(select3)
corrplot(cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

EFA <- fa(select3, nfactors = 4, rotate = "varimax")#, fm = "pa")
EFA$loadings

### QUESTÃO 6

EFA2 <- fa(select3, nfactors = 4, rotate = "promax")
EFA2$loadings

cor(EFA$scores)
EFA$r.scores

cor(EFA2$scores)
EFA2$r.scores
