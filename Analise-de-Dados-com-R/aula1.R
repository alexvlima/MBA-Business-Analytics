#Para rodar cada linha, usar CRTL+ENTER

2+2
a <- 10 #Atribui o valor 10 a variavel 'a'
a
erros <- c(2,3,0,1,0,0,2,2,1) #Concatena os numeros e atribui a um vetor
erros[3]
erros+1 #Soma 1 a todos os elementos
erros
erros[-3] #Todos os elementos menos o da terceira posicao
erros[3:6] #Elementos na posicao de 3 a 6
mean(erros) #Media
var(erros)

sd(erros) == sqrt(var(erros)) #Testando se o desvio padrao e igual a raiz quadrada da vari?ncia

max(erros) #Apresenta o maior valor do vetor
erros == max(erros) #Testa, numero a numero, se o valor e igual ao valor maximo do vetor
which(erros == max(erros)) #Mostra posicao do valor TRUE

getwd() #Mostra qual em qual pasta estamos
write.csv2(erros,"erros.csv") #Cria arquivo csv na pasta que estamos
erros2 <- read.csv2("erros.csv")
View(erros2) #Visualiza a tabela
erros2[3,2] #Apresenta o elemento que est? na terceira linha, segunda coluna
erros2[3:5,1] #Terceira a quinta linha, coluna 1

#install.packages("ggplot2") #Instalar o pacote grafico ggplot2
library(ggplot2) #Carregar o pacote para usa-lo

View(mpg) #Apresenta uma base de dados exemplo, de miles per galoon
hist(mpg$displ)

ggplot(mpg, aes(x=cty,y=hwy)) + geom_point(aes(color=cyl)) + geom_smooth() + facet_wrap(~class)
ggplot(mpg, aes(x=class,y=hwy)) + geom_boxplot(aes(fill=class))

ggplot(mpg, aes(x=displ,y=hwy)) + #Apenas prepara os dados do gráfico (não exibe)
  geom_point(aes(color=class))  + #Gráfico de dispersão com cores por classe de automóvel
  geom_smooth(aes(linetype=drv)) + #Mostra outra dimensão, pelo tipo de tração (traseira, frontal ou 4x4)
  facet_wrap(~manufacturer) #Divide o gráfico em vários, um por fabricante

View(PlantGrowth)
hist(PlantGrowth$weight) #Histograma da variável weight - Acessa a coluna do dataset com $
plant <- unstack(PlantGrowth) #Transforma as categorias em colunas
View(plant)
boxplot(plant) # gráfico boxplot

ggplot(PlantGrowth,aes(x=group, y=weight)) + geom_boxplot() #Cria o gráfico boxplot
ggplot(PlantGrowth,aes(x=group, y=weight)) + geom_boxplot(color="blue") #Cor da borda em azul
ggplot(PlantGrowth,aes(x=group, y=weight)) + geom_boxplot(fill="blue") #Preenche de azul
ggplot(PlantGrowth,aes(x=group, y=weight)) + geom_boxplot(aes(fill=group)) #Preenchimento segue o grupo

View(InsectSprays)

ggplot(InsectSprays,aes(x=spray, y=count)) + geom_boxplot(aes(fill=spray)) #Preenchimento segue o grupo

oneway.test(formula = count ~ spray, data = InsectSprays) #ANOVA: Testa a correlação de count e spray (se p-value for menor que 0,05, o teste e forte)

iris[1:12,] #Faz um recorte do dataset iris
pairs(iris) #Faz graficos cruzando todas as variaveis
pairs(iris[1:4], main="Titulo do grafico", pch=21) #pch e a largura dos pontos

cor = c("blue","green","red")
cor[2]
cor[c(1,1,1,2,2,3)]
iris$Species
unclass(iris$Species)
  cor[unclass(iris$Species)]

pairs(iris[1:4], main="Titulo do grafico", pch=21, bg=cor[unclass(iris$Species)]) #bg colore os pontos

#install.packages("GGally")
library(GGally)
ggpairs(iris,columns=1:4) #pairs melhorado
ggpairs(iris, columns=1:4, ggplot2::aes(color=Species))
ggpairs(iris, ggplot2::aes(color=Species)) #Acrescentando a coluna Species nos cruzamentos


