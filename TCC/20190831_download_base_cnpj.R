#################
### DIRETORIO ###
#################

getwd()
setwd("/Volumes/Macintosh HD 1/Usua??rios/alexandrevasconceloslima/Documents/RFB")

########################
### LIMPANDO MEMORIA ###
########################

rm(list = ls())
dev.off()
gc(reset = TRUE)

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
# devtools::install_github("georgevbsantiago/qsacnpj")
# https://github.com/georgevbsantiago/qsacnpj
library(qsacnpj)

####################
### DOWNLOAD RFB ###
####################

# Site RFB #
# https://receita.economia.gov.br/orientacao/tributaria/cadastros/cadastro-nacional-de-pessoas-juridicas-cnpj/dados-publicos-cnpj

num_arquivo <- c("01","02","03","04","05","06","07","08","09","10",
                 "11","12","13","14","15","16","17","18","19","20")

for(i in 1:length(num_arquivo)){
  download.file(url = paste0("http://200.152.38.155/CNPJ/DADOS_ABERTOS_CNPJ_",num_arquivo[i],".zip"),
                destfile = paste0("rfb_cnpj",num_arquivo[i],".zip"))
}

##########################
### TRATAMENTO ARQUIVO ###
##########################

list.files("./01_Dados_Brutos/")

# Extrair os arquivos do zip
for (i in 1:length(num_arquivo)) {
  unzip(paste0("./01_Dados_Brutos/DADOS_ABERTOS_CNPJ_",num_arquivo[i],".zip"),
        exdir = "./02_Dados_Tratados/")
}

# Renomear arquivo acrescentando .txt
for (i in 1:length(num_arquivo)) {
  file.rename(paste0("./02_Dados_Tratados/K3241.K03200DV.D90805.L000",num_arquivo[i]),
              paste0("./02_Dados_Tratados/K3241.K03200DV.D90805.L000",num_arquivo[i],".txt"))
}

#######################
### GERAR BASE CNPJ ###
#######################

setwd("/Volumes/Macintosh HD 1/Usua??rios/alexandrevasconceloslima/Documents/RFB/02_Dados_Tratados/")

gerar_bd_cnpj(path_arquivos_txt = "/Volumes/Macintosh HD 1/Usua??rios/alexandrevasconceloslima/Documents/RFB/02_Dados_Tratados",
              localizar_cnpj = "NAO",
              n_lines = 10000,
              armazenar = "csv")
