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

########################
### BCB TIME SERIES ###
########################

### PREDICT IGPM for OCT/2018 ###
### ARMA MODEL ###

igpm <- readr::read_csv2('IGPM2018.csv')
glimpse(igpm)
colnames(igpm) <- c('data','igpm')

igpm <- gsub(pattern = ',', replacement = '\\.', x = igpm$igpm)
igpm <- as.numeric(igpm)
hist(igpm)

igpm_ts <- ts(data = igpm, start = (2003),frequency = 12) 
igpm_ts
igpm_ts <- igpm_ts[1:189]

acf(igpm_ts)
pacf(igpm_ts)

### ARMA MODEL ### 
igpm_model <- arima(x = igpm_ts, order = c(1,0,1))
igpm_model

predict(object = igpm_model, n.ahead = 1)
forecast(object = igpm_model, h = 1)

plot(forecast(object = igpm_model, h = 6))

###########################
### AUTO.ARIMA FUNCTION ###
###########################

igpm_model <- auto.arima(igpm_ts)
igpm_model
accuracy(igpm_model)

predict(object = igpm_model, n.ahead = 1)
forecast(object = igpm_model, h = 1)

plot(forecast(object = igpm_model, h = 6))

### PREDICT INCC for OCT/2018 ###

incc <- readr::read_csv2('INCC2018.csv')
glimpse(incc)
colnames(incc) <- c('data','incc')

incc <- gsub(pattern = ',', replacement = '\\.', x = incc$incc)
incc <- as.numeric(incc)
hist(incc)

incc_ts <- ts(data = incc, start = (2003),frequency = 12) 
incc_ts
incc_ts <- incc_ts[1:189]

acf(incc_ts)
pacf(incc_ts)

incc_model <- auto.arima(incc_ts)
incc_model
accuracy(incc_model)

predict(object = incc_model, n.ahead = 1)
forecast(object = incc_model, h = 1)

plot(forecast(object = incc_model, h = 6))
