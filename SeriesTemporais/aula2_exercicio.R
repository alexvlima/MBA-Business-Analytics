############
### PATH ###
############

setwd('Y://Material Alunos//Series_Temporais_Chela/')

#################
### LIBRARIES ###
#################

library(tidyverse)
library(TTR)
library(forecast)

#####################
### INADIMPLENCIA ###
#####################

inad <- readxl::read_excel('Exercicio_Final.xlsx', 
                           sheet = 'INAD', skip = 11)
glimpse(inad)
colnames(inad) <- c('data','inad')

inad_ts <- ts(data = inad$inad, start = (2017),frequency = 12) 
inad_ts

inad_mod <- auto.arima(inad_ts)
inad_mod

predict(object = inad_mod, n.ahead = 4)
forecast(object = inad_mod, h = 4)

plot(forecast(object = inad_mod, h = 4))

##############
### VENDAS ###
##############

vendas <- readxl::read_excel('Exercicio_Final.xlsx', 
                           sheet = 'VENDAS', skip = 11)
glimpse(vendas)
colnames(vendas) <- c('data','vendas')

vendas_ts <- ts(data = vendas$vendas, start = c(2010,9),frequency = 12) 
vendas_ts

vendas_mod <- auto.arima(vendas_ts)
vendas_mod

predict(object = vendas_mod, n.ahead = 4)
forecast(object = vendas_mod, h = 4)

plot(forecast(object = vendas_mod, h = 4))

##############
### VENDAS ###
##############

prod <- readxl::read_excel('Exercicio_Final.xlsx', 
                             sheet = 'PROD', skip = 4)
glimpse(prod)
colnames(prod) <- c('ano','t','producao')

prod_ts <- ts(data = prod$producao, start = (1968), frequency = 1) 
prod_ts

prod_mod <- auto.arima(prod_ts)
prod_mod

predict(object = prod_mod, n.ahead = 4)
forecast(object = prod_mod, h = 4)

plot(forecast(object = prod_mod, h = 4))
