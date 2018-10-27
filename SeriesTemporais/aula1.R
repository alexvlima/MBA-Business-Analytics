###############
### LIBRARY ###
###############

# install.packages('TTR')
library(TTR)
library(dplyr)

###############
### DATASET ###
###############

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
glimpse(births)

###################
### TIME SERIES ###
###################

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
# par(mar=c(1,1,1,1))
plot.ts(birthstimeseries)

### DECOMPOSE ###
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

### DIFF TRANSFORM ###
plot(diff(births))

#############################
### TIMES SERIES GENERATE ###
#############################

ar1 <- arima.sim(model = list(ar=0.8), n = 10000)
plot(ar1)

ar2 <- arima.sim(model = list(ar=-0.8), n = 10000)
plot(ar2)

#########################
### STATIONARY VERIFY ###
#########################

### TIME SERIES AUTOCORRELATION ### 
acf(ar1)
acf(ar2)

### VERIFY IN BIRTHS TIME SERIES ###
acf(birthstimeseries)
acf(diff(birthstimeseries))

######################
### VENDAS DATASET ###
######################

getwd()
setwd('Y:\\Material Alunos\\Series_Temporais_Chela')

library(readxl)

vendas <- read_excel('Vendas.xlsx')
glimpse(vendas)
colnames(vendas) <- 'vendas'

acf(vendas$vendas)
acf(diff(vendas$vendas))

#######################
### AUTOCORRELATION ###
#######################

pacf(ar1)
pacf(diff(birthstimeseries))

ar3 <- arima.sim(n = 100000, 
                 model = list(ar = c(0.8, -0.5)))
acf(ar3)
pacf(ar3)

# install.packages('forecast')
library(forecast)

mod_ar3 <- auto.arima(ar3)
mod_ar3

mod_vendas <- auto.arima(vendas)
mod_vendas

predict(mod_vendas, n.ahead=4)

plot(forecast(mod_vendas), 
     n.ahead = 4)
