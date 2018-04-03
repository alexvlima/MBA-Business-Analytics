
#install.packages("RODBC", dependencies = TRUE)
library(RODBC)

#EXCEL

#ACCESS
conn <- odbcConnectAccess2007("./Big Data/dados/adventureworks.accdb")
sales <- sqlQuery(conn, "EXEC sqlSales", as.is=TRUE); #sqlSales e um SQL ja montado no access
odbcClose(conn)

#install.packages("dplyr")
library(dplyr)

#install.packages("nycflights13")
library(nycflights13)
flights[1:8,]

df <- data.frame(color = c("blue","black","blue", "blue", "black"),
                 value = 1:5)
df
filter(df, color=="blue")
filter(df, value%%2!=0)
filter(df, value %in% c(2,4))
select(df, color)
arrange(df,desc(color)) #Ordenar
mutate(df, double = 2 * value) #Acrescentar colunas
df %>% group_by(color) %>% summarise(total = sum(value))

df %>% filter(color=="blue")

x <- data.frame(name = c("John","Paul","George","Ringo","Stuart","John"),
            instrument = c("guitar","bass","guitar","drums","bass","drums"))

(filtrado <- x %>% filter(name=="John",instrument=="guitar"))
x %>% filter(name %in% c("John","Paul"))


y <- data.frame(nome = c("John","Paul","George","Ringo","Brian"),
                band = c("TRUE","TRUE","TRUE","TRUE","FALSE"))

inner_join(x,y,c("name"="nome"))

left_join(x,y,c("name"="nome"))

?left_join

flights

(niver <- filter(flights, month == 2, day == 28))

near(sqrt(2) ^ 2, 2)
near(sqrt(2) ^ 2, 3)
