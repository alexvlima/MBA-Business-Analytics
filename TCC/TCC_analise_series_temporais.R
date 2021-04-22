#################################################
#                                               #
#          ANÁLISE DAS SÉRIES TEMPORAIS         #
#                                               #
#################################################


# LIMPAR AMBIENTE E MEMÓRIA
rm(list=ls())
gc(reset = TRUE)


# BIBLIOTECAS
library(tidyverse)
library(data.table)
library(lubridate)
library(tseries)
library(stringr)
library(scales)

# DEFINIR DIRETÓRIO
getwd()
setwd("C:/")
list.files()

# CARREGAR OS DADOS

mei <- read_csv2("mei_mensal.csv")
deslig <- read_csv2("caged_desligamentos_mensal.csv")

# OBSERVAR OS DADOS

glimpse(mei)
glimpse(deslig)

mei %>%
  mutate(data = paste0(mes,"-",ano)) %>%
  mutate(data = dmy(paste("01/",data,sep=""))) %>%
  slice(-1) %>%
  slice(-n()) %>%
  ggplot(aes(x = data, y = qtde)) + 
  scale_y_continuous("Quantidade", labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date("Data", date_breaks = "1 year",
               date_labels = "%Y") +
  geom_line(color = "#00AFBB", size = 1)  +
  theme_bw()

deslig %>%
  mutate(data = paste0(mes,"-",ano)) %>%
  mutate(data = dmy(paste("01/",data,sep=""))) %>%
  slice(-1) %>%
  slice(-n()) %>%
  ggplot(aes(x = data, y = qtde_ajustada)) + 
  scale_y_continuous("Quantidade", labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date("Data", date_breaks = "1 year",
               date_labels = "%Y") +
  geom_line(color = "#00AFBB", size = 1) +
  theme_bw()

# FORMATAR COMO SÉRIE TEMPORAL

mei_ts <- ts(data = mei$qtde[3:nrow(mei)-1], start = c(2010, 2), frequency = 12)
mei_ts

deslig_ts <- ts(data = deslig$qtde_ajustada[3:nrow(deslig)-1], start = c(2010, 2), frequency = 12)
deslig_ts

# plot(mei_ts)
# plot(deslig_ts)

# DECOMPOSIÇÃO DA SÉRIE

mei_dec <- decompose(mei_ts)
deslig_dec <- decompose(deslig_ts)

# ANÁLISE E DECOMPOSIÇÃO DA SÉRIE
# plot(mei_dec)
# plot(deslig_dec)

# FORMATAR DADOS PARA USAR COM GGPLOT

# MEI
mei_0 <- as.data.frame(mei[3:nrow(mei)-1,])
mei_0$mes <- str_pad(mei_0$mes, width = 2, side = "left", pad = "0")
mei_0$mes <- paste(mei_0$mes, "01", sep = "-")
mei_0 <- mei_0 %>%
  unite(col = "data", c(ano, mes), sep = "-")
mei_0$data <- as.Date(mei_0$data, format = c("%Y-%m-%d"))
str(mei_0)
mei_0$ten <- mei_dec$trend
mei_0$saz <- mei_dec$seasonal
mei_0$rui <- mei_dec$random
mei_1 <- mei_0 %>%
  gather(key = "dado", value = "valor", qtde, ten, saz, rui) # Para poder usar facet_grid
mei_1$dado <- factor(mei_1$dado, levels = c('qtde', 'ten', 'saz', 'rui')) # Para que o grid do plot fique ordenado

mei_label <- c(
  'qtde' = "Série original",
  'ten' = "Tendência",
  'rui' = "Ruído",
  'saz' = "Sazonalidade")

ggplot(mei_1, aes(x = data, y = valor)) +
  geom_line(size = 0.7) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  facet_grid(dado ~ .,
             scales = "free",
             labeller = as_labeller(mei_label))

# DESLIGAMENTOS

deslig_0 <- as.data.frame(deslig[3:nrow(deslig)-1,])
deslig_0$mes <- str_pad(deslig_0$mes, width = 2, side = "left", pad = "0")
deslig_0$mes <- paste(deslig_0$mes, "01", sep = "-")
deslig_0 <- deslig_0 %>%
  unite(col = "data", c(ano, mes), sep = "-")
deslig_0$data <- as.Date(deslig_0$data, format = c("%Y-%m-%d"))
str(deslig_0)
deslig_0$ten <- deslig_dec$trend
deslig_0$saz <- deslig_dec$seasonal
deslig_0$rui <- deslig_dec$random
deslig_1 <- deslig_0 %>%
  gather(key = "dado", value = "valor", qtde_ajustada, ten, saz, rui) # Para poder usar facet_grid
deslig_1$dado <- factor(deslig_1$dado, levels = c('qtde_ajustada', 'ten', 'saz', 'rui')) # Para que o grid do plot fique ordenado

deslig_label <- c(
  'qtde_ajustada' = "Série original",
  'ten' = "Tendência",
  'rui' = "Ruído",
  'saz' = "Sazonalidade")

ggplot(deslig_1, aes(x = data, y = valor)) +
  geom_line(size = 0.7) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme_bw() +
  facet_grid(dado ~ .,
             scales = "free",
             labeller = as_labeller(deslig_label))


# LOG DIFERENÇA DA SÉRIE

mei_ts_log <- diff(log(mei_ts))
deslig_ts_log <- diff(log(deslig_ts))

# TESTE DE ESTACIONARIEDADE DA NOVA SÉRIE 

adf.test(mei_ts_log)
adf.test(deslig_ts_log)

par(mfrow=c(1,2))

acf(mei_ts_log,
    na.action = na.pass,
    lag.max = 100,
    main = "Autocorrelações da série de novos registros de MEI",
    ylab = "Autocorrelação (ACF)")

acf(deslig_ts_log,
    na.action = na.pass,
    lag.max = 100,
    main = "Autocorrelações da série de desligamentos",
    ylab = "Autocorrelação (ACF)")

# CORRELAÇÃO CRUZADA DAS SÉRIES

par(mfrow=c(1,1))

ccf1<-ccf(deslig_ts_log,mei_ts_log, 
          plot = T, 
          main = "",
          na.action = na.pass,
          lag.max = 12)
ccf1








             