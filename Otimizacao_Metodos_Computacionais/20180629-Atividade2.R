#########################################
### ALUNO: ALEXANDRE VASCONCELOS LIMA ###
######### MATRICULA: 126704/2017 ########
#########################################

#################
### DIRETORIO ###
#################

# getwd()
setwd('/Volumes/KINGSTON/FGV/Rodrigo Togneri')

###################
### BIBLIOTECAS ###
###################

# if(!require(markochain,quietly = TRUE))
# install.packages('markovchain',dependencies=TRUE)

library(markovchain)
library(dplyr)
library(tidyr)
library(ggplot2)

###############
### DATASET ###
###############

# Tabela de retornos por periodo
retornos <- 
       matrix(c(0.006, 0.089, 0.004,
                0.028, 0.056, 0.012,
                0.035, 0.040, 0.015,
                0.014, 0.045, 0.017,
                0.026, 0.029, 0.008,
                0.032, 0.056, 0.018,
                0.028, 0.030, 0.021,
                0.010, 0.006, 0.030,
                0.020, 0.034, 0.013,
                0.026, 0.038, 0.017),
              nrow = 10,ncol = 3, byrow = TRUE)

colnames(retornos) <- c(paste("Papel", LETTERS[1:3]))
print(retornos)

carteira <- c(0.50, 0.35, 0.15)

#################
### QUESTAO A ###
#################

media_retorno_papel <- apply(retornos, 2, mean)
media_retorno_papel

media_retorno_carteira <- sum(media_retorno_papel * carteira)
media_retorno_carteira

#################
### QUESTAO B ###
#################

variancia_papel <- apply(retornos, 2, var)
variancia_papel

matriz_covariancia <- cov(retornos)
matriz_covariancia

#################
### QUESTAO C ###
#################

carteira <- t(as.matrix(carteira))
var_carteira <- carteira %*% matriz_covariancia %*% t(carteira)
var_carteira

risco <- var_carteira ^ (1/2)
risco
