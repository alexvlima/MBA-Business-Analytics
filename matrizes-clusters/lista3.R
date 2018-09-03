###############
### LIBRARY ###
###############

library("dplyr")
library("ggplot2")
library("corrplot")
library("psych")

#####################
### BASE DE DADOS ###
#####################

load('./Aula 04/hbat.Rdata')

#################
### QUESTAO 2 ###
#################

q2 <- hbat[,7:19]

# Validacao da amostra
KMO(q2)
# Exclusao das variaveis que nao tiveram uma boa aderencia da amostragem
q2 <- select(q2,-x11,-x15,-x17) 

distancia_mahalanobis <- mahalanobis(q2, colMeans(q2), 
                                     cov(q2))

max(distancia_mahalanobis)

#################
### QUESTAO 5 ###
#################

q5 <- q2
q5_dist <- dist(q5, method = "euclidean")

cluster <- hclust(q5_dist, method = "centroid")
plot(cluster)

abline(h = 1.2, col = "red", lty = 5)
cluster$height <- sort(cluster$height)
grupos <- cutree(cluster, h = 1.2)
table(grupos)

grupos <- cutree(cluster, k =  10)
table(grupos)
rect.hclust(cluster, k=10, border="red")

q5$grupo <- grupos
q5 %>%
  group_by(grupo) %>%
  summarise(media = mean(x6)) %>%
  arrange(media)

#################
### QUESTAO 6 ###
#################

q6 <- select(hbat,x19:x22)
head(q6)

pca <- prcomp(q6, scale. = T) 
pca$rotation
pca$scale

componentes <- as.data.frame(pca$x)
componentes$grupo <- grupos

ggplot(componentes, aes(PC1, PC2)) +
  geom_point(aes(color = grupo)) +
  theme_minimal()
