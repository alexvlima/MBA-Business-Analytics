####################################################################################
####################################################################################
## Prova de Valor de An�lise de Cesta de Mercado para um e-commerce X.
####################################################################################
####################################################################################


####################################################################################
## Setando o WD.
####################################################################################

getwd()

setwd("C:/Users/rodto/OneDrive/08 Atividades letivas/05 FGV MBA Analytics/05_II - Met_Comp_Aplic_v4/03 T�picos Av. Estrut. de Dados/01 Matrizes Esparsas/Case_ecommerceX")

getwd()

####################################################################################
## An�lise de Cesta de Mercado (Market Basket Analysis) para itens (lineitems).
####################################################################################

# Instalando e ativando o pacote arules (association rules, nome formal para an�lise de cesta de mercado)
#install.packages("arules")
require(arules)

# Lendo a base de dados de transa��es de itens. Este comando de leitura j� traz a 
# base em formato de matriz esparsa.
order_lineitems_2_R_4 <- read.transactions("C:/Users/rodto/OneDrive/08 Atividades letivas/05 FGV MBA Analytics/05_II - Met_Comp_Aplic_v4/03 T�picos Av. Estrut. de Dados/01 Matrizes Esparsas/Case_ecommerceX/order_lineitems_2_R_4.csv",sep=",")

# Explorando a base de dados de transa��es de itens.
order_lineitems_2_R_4
summary(order_lineitems_2_R_4)
inspect(order_lineitems_2_R_4[1:3])
itemFrequencyPlot(order_lineitems_2_R_4,support=0.05) # N�o houve plotagem porque n�o
# h� caso acima do suporte m�nimo de 0.05 designado.
itemFrequencyPlot(order_lineitems_2_R_4,topN=5)

# Construindo o modelo de An�lise de Cesta de Mercado (Market Basket Analysis) para
# itens.
mba_product <- apriori(order_lineitems_2_R_4,parameter=list(support=0.0001,confidence=0.10,minlen=2))
# Para as especifica��es designadas, o m�todo n�o encontrou regras de associa��o.


####################################################################################
## An�lise de Cesta de Mercado (Market Basket Analysis) para categorias de produto 
## (product categories).
####################################################################################

# Lendo a base de dados de transa��es de categorias de produtos. Este comando de 
# leitura j� traz a base em formato de matriz esparsa.
order_productcat_2_R_4 <- read.transactions("C:/Users/rodto/Google Drive/10 Atividades letivas/05 FGV MBA Analytics/05_II - M�todos Computacionais Aplicados_v3/02 T�picos Av. Estrut. de Dados/01 Matrizes Esparsas/Case_ecommerceX/order_productcat_2_R_4.csv",sep=",")

# Explorando a base de dados de transa��es de categorias de produtos.
order_productcat_2_R_4
summary(order_productcat_2_R_4)
inspect(order_productcat_2_R_4[1:3])
itemFrequencyPlot(order_productcat_2_R_4,support=0.05)
itemFrequencyPlot(order_productcat_2_R_4,topN=5)

# Construindo o modelo de An�lise de Cesta de Mercado (Market Basket Analysis) para
# categorias de produtos.
mba_product_cat <- apriori(order_productcat_2_R_4,parameter=list(support=0.01,confidence=0.10,minlen=2))

# Explorando o modelo de An�lise de Cesta de Mercado constru�do para categorias 
# de produtos.
mba_product_cat
summary(mba_product_cat)
inspect(mba_product_cat)
inspect(sort(mba_product_cat,by="confidence"))
inspect(sort(mba_product_cat,by="lift") [1:5])

### Visualiza��o dos resultados do modelo de An�lise de Cesta de Mercado para 
### Categorias de Produtos.

# Instalando e ativando pacotes de visualiza��o espec�fica para arules.
#install.packages("arulesViz")
require(arulesViz)
#library("arulesViz")

# Visualiza��o propriamente dita (BLOCO 1: B�SICA)
plot(mba_product_cat)
head(quality(mba_product_cat))
plot(mba_product_cat, measure=c("support", "lift"), shading="confidence")
plot(mba_product_cat, shading="order", control=list(main = "Two-key plot"))
interactplot <- plot(mba_product_cat, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

# Visualiza��o propriamente dita (BLOCO 2: MAIS SOFISTICADA)
plot(mba_product_cat, method="matrix", measure="lift")
plot(mba_product_cat, method="matrix3D", measure="lift")
plot(mba_product_cat, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(mba_product_cat, method="matrix", measure=c("lift", "confidence"))
plot(mba_product_cat, method="matrix", measure=c("lift", "confidence"),
     + control=list(reorder=TRUE))
plot(mba_product_cat, method="grouped")
plot(mba_product_cat, method="grouped", control=list(k=50))
interactplot_2 <- plot(mba_product_cat, method="grouped", interactive=TRUE)
plot(mba_product_cat, method="graph")
plot(mba_product_cat, method="graph", control=list(type="itemsets"))
saveAsGraph(head(sort(mba_product_cat, by="lift"),1000), file="mba_product_cat.graphml")
plot(mba_product_cat, method="paracoord")
