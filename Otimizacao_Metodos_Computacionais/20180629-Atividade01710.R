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


###############
### DATASET ###
###############

# Matriz de Transicoa
tm <- matrix(c(0.00, 0.10, 0.10, 0.20, 0.60,
               0.15, 0.00, 0.05, 0.35, 0.45,
               0.20, 0.10, 0.00, 0.13, 0.57,
               0.00, 0.00, 0.00, 1.00, 0.00,
               0.00, 0.00, 0.00, 0.00, 1.00),
             nrow = 5, ncol = 5, byrow = TRUE)

print(tm)

####################
### MARKOV CHAIN ###
####################

dtmc <- new("markovchain", 
            transitionMatrix = tm,
            states = c("Site","Hotpage","Call Center","Venda", "Saída"),
            name="MarkovChain para o e-commerce")

print(dtmc)
plot(dtmc)

#################
### QUESTAO A ###
#################

# Inicio pela Hotpage - vetor inicial (0,1,0,0,0)
inicialState <- c(0, 1, 0, 0, 0)
steps <- c(1:1000)

finalState_1 <- inicialState*(dtmc^steps[1]) # Passo 1
print(finalState_1)

finalState_2 <- inicialState*(dtmc^steps[2]) # Passo 2
print(finalState_2)


#################
### QUESTAO B ###
#################

# Inicio pelo Site
initialState_i <- c(1, 0, 0, 0, 0) # Pelo Site
finalState_i <- initialState_i*(dtmc^steps[1000])
print(finalState_i)
finalState_i[,'Venda']

# Inicio pela Hotpage
initialState_ii <- c(0, 1, 0, 0, 0) 
finalState_ii <- initialState_ii*(dtmc^steps[1000])
print(finalState_ii)
finalState_ii[,'Venda']

# Inicio pelo Call Center
initialState_iii <- c(0, 0, 1, 0, 0) 
finalState_iii <- initialState_iii*(dtmc^steps[1000])
print(finalState_iii)
finalState_iii[,'Venda']


#################
### QUESTAO D ###
#################

initialState <- c(0.5, 0.2, 0.3, 0, 0)
finalState <- initialState*(dtmc^steps[1000])
finalState
finalState[,'Venda']

