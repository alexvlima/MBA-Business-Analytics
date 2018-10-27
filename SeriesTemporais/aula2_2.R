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

ma1 <- arima.sim(model = list(ma = 0.8), n = 1000000)

#######################
### AUTOCORRELATION ###
#######################

acf(ma1)
pacf(ma1)

### COMPARE VERSUS AR MODEL ###
ar1 <- arima.sim(model = list(ar = 0.8), n = 1000000)

par(mfrow=c(1,2), mar=c(1,1,1,1))

acf(ma1)
acf(ar1)

### ANOTHER LAG IN MA MODEL ###
par(mfrow=c(1,1), mar=c(1,1,1,1))

ma2 <- arima.sim(model = list(ma = c(0.32,0.47)), n = 1000000)

acf(ma2)
pacf(ma2)

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

### MA(1) MODEL - EVEN WE KNOW THERE IS A AR MODEL (TESTING) ### 
igpm_model <- arima(x = igpm_ts, order = c(0,0,1))
igpm_model

predict(object = igpm_model, n.ahead = 1)
forecast(object = igpm_model, h = 1)

plot(forecast(object = igpm_model, h = 6))

### STATIONARY MODEL ###
# E(Yt) = intercept 
# Var(Yt) = Var(e)*(1 + Var(θ1))
