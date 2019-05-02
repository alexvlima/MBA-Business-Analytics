
#############
### OPCAO ###
#############
options(scipen = 14)


###################
### LER ARQUIVO ###
###################

install.packages('readxl')
library(readxl)

base <- read_excel('base_automoveis_recodificada.xlsx', 
				sheet = 'AUTOMÓVEIS RECODIFICADA')
str(base)

#base <- base[,-c(12:16)] 


##################
### HISTOGRAMA ###
##################

install.packages('munsell')
install.packages('ggplot2')
library(ggplot2)

ggplot(base, aes(Imagem)) +
geom_histogram()

ggplot(base, aes(Utilitário)) +
geom_histogram()

ggplot(base, aes(Preço)) +
geom_histogram()


#############################
### DIAGRAMA DE DISPERSÃO ###
#############################

plot(base$Imagem, base$Utilitário)
plot(base$Imagem, base$Preço)
plot(base$Utilitário, base$Preço)


#########################
### SUMÁRIO DOS DADOS ###
#########################

install.packages("magrittr")
install.packages("dplyr")
library(magrittr)
library(dplyr)

fill <- "#4271AE"
line <- "#1F3552"

base %>% group_by(Protótipo) %>% summarise(media = mean(Imagem))

ggplot(base, aes(x = prototipo, y = Imagem)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Imagem") +
        scale_x_discrete(name = "Protótipo") +
        ggtitle("Boxplot da Imagem segundo Protótipo") +
	  theme_bw()

base %>% group_by(Protótipo) %>% summarise(media = mean(Utilitário))

ggplot(base, aes(x = prototipo, y = Utilitário)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Utilitário") +
        scale_x_discrete(name = "Protótipo") +
        ggtitle("Boxplot da Utilitário segundo Protótipo") +
	  theme_bw()

base %>% group_by(Protótipo) %>% summarise(media = mean(Preço))

ggplot(base, aes(x = prototipo, y = Preço)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Preço") +
        scale_x_discrete(name = "Protótipo") +
        ggtitle("Boxplot do Preço segundo Protótipo") +
	  theme_bw()


#####################################
### CRUZAMENTO DE VARIÁVEIS QUALI ###
#####################################

install.packages('gmodels')
library(gmodels)

base$sexo <- factor(base$G?nero, 
		levels=c("F","M"),
		labels=c("Masculino","Feminino"))

base$prototipo <- factor(base$Protótipo,
		levels=c(1,2,3),
		labels=c("tipo1","tipo2","tipo3"))

with(base, 
	CrossTable(prototipo, sexo, 
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

#with(base, 
#	CrossTable(prototipo, sexo, 
#			digits=1,
#			prop.r=FALSE, prop.c=TRUE,
#			prop.t=FALSE, prop.chisq=FALSE, 
#			chisq =TRUE))

ggplot(data=base, aes(x=prototipo, fill=sexo)) + 
    geom_bar(colour="black", stat="count",
             position=position_dodge(),
             size=.3) +                        
    scale_fill_manual(values=c("light blue", "pink")) +     
    xlab("Protótipo") + ylab("Freq.") +  
    ggtitle("Frequência da preferência do protótipo segundo sexo") +     
    theme_bw()

colnames(base)[6] <- paste("Estado_civil")
colnames(base)

with(base, 
	CrossTable(prototipo, Estado_civil, 
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

with(base, 
	CrossTable(prototipo, Estado_civil, 
			digits=1,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

ggplot(data=base, aes(x=prototipo, fill=Estado_civil)) + 
    geom_bar(colour="black", stat="count",
             position=position_dodge(),
             size=.3) +                        
    scale_fill_manual(values=c("blue", "green")) +     
    xlab("Protótipo") + ylab("Freq.") +  
    ggtitle("Frequ?ncia da preferência do protótipo segundo estado civil") +     
    theme_bw()

#base$Renda <- factor(base$Renda, 
#		levels=c(1,2,3,4,5,6),
#		labels=c("at? 100","100 a 150", "150 a 200", "200 a 250", "250 a 300", "acima de 300"))

with(base, 
	CrossTable(Renda, prototipo, 
			digits=2,
			prop.r=FALSE, prop.c=TRUE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

with(base, 
	CrossTable(Filhos, prototipo, 
			digits=2,
			prop.r=FALSE, prop.c=TRUE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

with(base, 
	CrossTable(prototipo, Pequeno,
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

#base$Idade <- factor(base$Idade, 
#		levels=c(1,2,3,4,5,6),
#		labels=c("Menor 25","25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 ou mais"))

with(base, 
	CrossTable(Idade, prototipo, 
			digits=2,
			prop.r=FALSE, prop.c=TRUE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))


#############
### ANOVA ###
#############

fit <- aov(Imagem ~ prototipo+Error(prototipo),data=base)	
fit


###############
### CLUSTER ###
###############

cluster_data <- subset(base, select = c('Imagem', 'Utilitário', 'Preço'))
str(cluster_data)

# Ward Hierarchical Clustering
ward_cluster <- dist(cluster_data, method = "euclidean") # distance matrix
fit <- hclust(ward_cluster, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit, k=4, border="red")

base$grupo <- cutree(fit, k=4)
str(base)

with(base, 
	CrossTable(grupo, sexo, 
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

with(base, 
	CrossTable(grupo, Estado_civil, 
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

with(base, 
	CrossTable(grupo, Idade, 
			digits=2,
			prop.r=TRUE, prop.c=FALSE,
			prop.t=FALSE, prop.chisq=FALSE, 
			chisq =TRUE))

###############
### TESTE T ###
###############

t.test (base$Imagem~base$sexo)

fill <- c("#4271AE","#FF0000")
line <- "#1F3552"

ggplot(base, aes(x = sexo, y = Imagem)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Imagem") +
        scale_x_discrete(name = "Sexo") +
        ggtitle("Boxplot da Imagem segundo Sexo") +
	  theme_bw()

ggplot(base, aes(x = sexo, y = Preço)) +
        geom_boxplot(fill = fill, colour = line) +
        scale_y_continuous(name = "Preço") +
        scale_x_discrete(name = "Sexo") +
        ggtitle("Boxplot da Preço segundo Sexo") +
	  theme_bw()


#######################
### NÃO-PARAMÉTRICO ###
#######################

shapiro.test(base$Imagem)
wilcox.test(base$Imagem~base$sexo)

########################
### REGRESSÃO LINEAR ###
########################

base_2 <- read_excel('base_automoveis_recodificada.xlsx', 
				sheet = 'iogurte')

str(base_2)
base_2 <- base_2[,-c(8:10)] 

colnames(base_2)[6] <- paste("Nota_Iogurte")
colnames(base_2)

fit <- lm(Nota_Iogurte ~ Genero + consistência + doçura + acidez + fruta, data=base_2)
fit
aov(fit)
