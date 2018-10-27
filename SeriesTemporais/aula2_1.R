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

###############
### DATASET ###
###############

rm(list = ls())
igpm <- readxl::read_excel('IGPM.xls')
glimpse(igpm)

### TIME SERIES ###
igpm_ts <- ts(data = igpm, frequency = 12)
plot.ts(igpm_ts)

#######################
### AUTOCORRELATION ###
#######################

acf(igpm_ts)
pacf(igpm_ts) ### CHOOSING MODEL PARAMETER P ###

################
### AR MODEL ###
################

igpm_ar_model <- arima(x = igpm_ts, order = c(1,0,0))
igpm_ar_model

################
### FORECAST ###
################

predict(igpm_ar_model, n.ahead = 4)
plot(forecast(igpm_ar_model, h = 4))

########################
### BCB TIME SERIES ###
########################

### PREDICT IGPM for OCT/2018 ###

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

igpm_model <- arima(x = igpm_ts, order = c(1,0,0))
igpm_model

predict(object = igpm_model, n.ahead = 1)
forecast(object = igpm_model, h = 1)

plot(forecast(object = igpm_model, h = 6))

### STATIONARY MODEL ###
# E(Yt) = intercept / (1 - Beta)
# Var(Yt) = Var(e) / (1 - Beta^2), where e is the white noise

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

incc_model <- arima(x = incc_ts, order = c(1,0,0))
incc_model

predict(object = incc_model, n.ahead = 1)
forecast(object = incc_model, h = 1)

plot(forecast(object = incc_model, h = 6))
