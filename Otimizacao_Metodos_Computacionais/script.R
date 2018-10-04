
#################
### QUESTAO 1 ###
#################

################
### PATH DIR ###
################

# getwd()
setwd('/Volumes/KINGSTON/FGV/Rodrigo Togneri/04 Trabalho em Grupo/01 Problema 1/01 Dados')

#################
### LIBRARIES ###
#################

library(dplyr)
library(ggplot2)

###############
### DATASET ###
###############

df1 <- read.csv2("estudo_hist_resultados.csv", stringsAsFactors = FALSE)
glimpse(df1)

#########################
### DATA MANIPULATION ###
#########################

# Calculando a diferença entre retorno esperado e retorno real
df1$dif <- df1$retorno_esp_modelo - df1$retorno_real

# Elevando ao quadrado para eliminar os valores negativos
df1$dif2 <- df1$dif^2

################
### ANALYSIS ###
################

# Calculando a média das diferenças para cada tamanho de histórico
df1 %>%
  group_by(qt_sem_hist) %>%
  summarise(diferenca_media = mean(dif2)) %>%
  ggplot(aes(x=qt_sem_hist, y=diferenca_media)) +
  geom_line() +
  geom_smooth() +
  xlab('Qtde de semanas') +
  ylab('Diferença Média ao Quadrado') +
  ggtitle('Média das Diferenças ao Quadrado entre o Retorno Real e o Esperado') +
  scale_x_continuous(breaks = unique(df1$qt_sem_hist)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 6, color = 'red')

#################
### QUESTAO 2 ###
#################

################
### PATH DIR ###
################

# getwd()
setwd('/Volumes/KINGSTON/FGV/Rodrigo Togneri/04 Trabalho em Grupo/02 Problema 2')

#################
### LIBRARIES ###
#################

library(dplyr)
library(ggplot2)

###############
### DATASET ###
###############

arquivos <- list.files()
arquivos
df2 <- read.csv2(arquivos[1], stringsAsFactors = FALSE)

glimpse(df2)

#########################
### DATA MANIPULATION ###
#########################

df2 <- 
  apply(df2[,2:10], 2, as.numeric) %>%
  as.data.frame() %>%
  glimpse()

################
### ANALYSIS ###
################

df2 %>% 
  ggplot(aes(x=tempo_execucao_media, y=funcao_objetivo_media, color = factor(popsize))) +
  geom_point(position = 'jitter', size = 2) +
  ggtitle('Dispersão da Função Objetivo e o Tempo de Execução') +
  xlab('Tempo Médio da Execução') +
  ylab('Média da Função Objetivo') +
  labs(color = 'Tamanho da População') +
  theme(plot.title = element_text(hjust = 0.5))

df2 %>% 
  ggplot(aes(x=factor(popsize), y=funcao_objetivo_media)) +
  geom_boxplot(fill='red') +
  ggtitle('Boxplot da Função Objetivo segundo o Tamanho da População') +
  xlab('Tamanho da População') +
  ylab('Média da Função Objetivo') +
  theme(plot.title = element_text(hjust = 0.5))

df2 %>% 
  ggplot(aes(x=factor(popsize), y=tempo_execucao_media)) +
  geom_boxplot(fill='red') +
  ggtitle('Boxplot do Tempo de Execução segundo o Tamanho da População') +
  xlab('Tamanho da População') +
  ylab('Tempo Médio de Execução') +
  theme(plot.title = element_text(hjust = 0.5))


df2 %>%
  filter(popsize == 100) %>%
  ggplot(aes(x=tempo_execucao_media, y=funcao_objetivo_media, color = factor(pmutation))) +
  geom_point(position = 'jitter', size = 2) +
  ggtitle('Dispersão da Função Objetivo e o Tempo de Execução') +
  xlab('Tempo Médio da Execução') +
  ylab('Média da Função Objetivo') +
  labs(color = 'Mutação') +
  theme(plot.title = element_text(hjust = 0.5))

df2 %>% 
  filter(popsize == 100) %>%
  ggplot(aes(x=factor(pmutation), y=funcao_objetivo_media)) +
  geom_boxplot(fill='red') +
  ggtitle('Boxplot da Função Objetivo segundo a Mutação') +
  xlab('Mutação') +
  ylab('Média da Função Objetivo') +
  theme(plot.title = element_text(hjust = 0.5))

df2 %>% 
  filter(popsize == 100) %>%
  ggplot(aes(x=factor(pmutation), y=tempo_execucao_media)) +
  geom_boxplot(fill='red') +
  ggtitle('Boxplot do Tempo de Execução segundo a Mutação') +
  xlab('Mutação') +
  ylab('Tempo Médio de Execução') +
  theme(plot.title = element_text(hjust = 0.5))


df2 %>% 
  filter(popsize == 100 & pmutation == 0.05) %>%
  ggplot(aes(x=factor(pcrossover), y=funcao_objetivo_media)) +
  geom_col(fill='blue') +
  geom_errorbar(aes(ymin=funcao_objetivo_media - funcao_objetivo_desvpad, 
                    ymax=funcao_objetivo_media + funcao_objetivo_desvpad)) +
  ggtitle('Função Objetivo segundo o Crossover') +
  xlab('Crossover') +
  ylab('Média da Função Objetivo') +
  theme(plot.title = element_text(hjust = 0.5))

df2 %>% 
  filter(popsize == 100 & pmutation == 0.05) %>%
  ggplot(aes(x=factor(pcrossover), y=tempo_execucao_media)) +
  geom_col(fill='red') +
  geom_errorbar(aes(ymin=tempo_execucao_media - tempo_execucao_desvpad, 
                    ymax=tempo_execucao_media + tempo_execucao_desvpad)) +
  ggtitle('Boxplot do Tempo de Execução segundo o Crossover') +
  xlab('Crossover') +
  ylab('Tempo Médio de Execução') +
  theme(plot.title = element_text(hjust = 0.5))

