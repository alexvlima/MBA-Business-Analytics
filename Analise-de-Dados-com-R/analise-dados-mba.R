
options(scipen = 14)

library(readxl)
library(MASS)
library(ggplot2)

base <- read_xlsx(path = "mba.xlsx",
                  sheet = "Sheet1")

nrow(base)
mean(base$salary)
median(base$salary)
hist(base$salary)

base2 <- subset(base, salary != 998 & salary != 999)
nrow(base2)

hist(base2$salary)
table(base2$salary)

base3 <- subset(base2, salary != 0)
nrow(base3)

hist(base3$salary)
boxplot(base3$salary)

mean(base3$salary) ; median(base3$salary)


# Analises utilizando o attach (guarda em memoria os dados)
attach(base3)
table(sex)
boxplot(salary ~ sex)
t.test(salary ~ sex) # pode utilizar oneway.test(salary ~ sex)
detach(base3)

rm(sex)

base4 <- subset(base3, salary != max(salary))
attach(base4)
boxplot(salary ~ sex)
t.test(salary ~ sex) # pode utilizar oneway.test(salary ~ sex)
detach(base4)

boxplot(salary ~ quarter, base3)
oneway.test(salary ~ quarter, base3)

boxplot(salary ~ quarter, base4)
oneway.test(salary ~ quarter, base4)

lm(salary ~ quarter, base3)
lm(salary ~ quarter, base4)

ggplot(base3, aes(quarter,salary)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(base4, aes(quarter,salary)) +
  geom_point() + 
  geom_smooth(method = "lm")

fill <- c("#4271AE","#FF0000","#1F3552","#FF6666")
line <- "#1F3552"

ggplot(base4, aes(x = as.factor(quarter), y = salary)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "Salary") +
  scale_x_discrete(name = "Quarter") +
  ggtitle("Boxplot do Salario segundo Quarter") +
  theme_bw()

modelo1 <- lm(salary ~ work_yrs, base4)
summary(modelo1)

modelo2 <- lm(salary ~ work_yrs + age + sex, base4)
summary(modelo2)
# Multicolinearidade entre as variaveis work_years e age

modelo3 <- lm(salary ~ work_yrs + sex, base4)
summary(modelo3)

modelo4 <- lm(salary ~ work_yrs + sex + quarter, base4)
summary(modelo4)

table(base4$frstlang)

# Regressao Linear com Stepwise
fit <- lm(salary ~ sex + age + gmat_tot + 
            quarter + work_yrs + frstlang, 
          data=base4)
step <- stepAIC(fit, direction="both")
step$anova # display results

modelo_final <- lm (salary ~ sex + age + quarter,
                    base4)

summary(modelo_final)
plot(modelo_final)


