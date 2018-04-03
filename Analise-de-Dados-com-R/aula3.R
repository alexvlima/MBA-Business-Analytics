library(tidyr)

View(table1) #dataset arrumado

# Funcao spread
View(table2) #dataset desarrumado...precisa dar um "spread"

table2a <- spread(data = table2, key = type, value = count)
View(table2a) #dataset arrumado

# Funcao separate
View(table3)
table3a <- separate(table3, rate, c("cases", "population"), 
                    sep = "/")
View(table3a)

# Funcao unite
View(table5)

table5a <- unite(table5, "year", c("century","year"), 
                 sep = "")
View(table5a)

# Funcao gather
View(table4a)
table4a2 <- gather(table4a, "1999","2000", 
                  key = "year", value = "cases")
View(table4a2)
View(table4b)
table4b2 <-  gather(table4b,"1999","2000", 
                    key = "year", value = "population")
View(table4b2)

table4_final <- left_join(table4a2, table4b2, 
          by = c("country"="country","year"="year"))
View(table4_final)
