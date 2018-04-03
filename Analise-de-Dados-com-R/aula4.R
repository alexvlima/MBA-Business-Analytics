
vetor <- 1:20
vetor
sd(vetor)

desvio <- function(entrada){
  media <- mean(entrada)
  diferenca <- entrada - media
  diferenca2 <- diferenca ^ 2
  n <- length(entrada)
  soma2 <- sum(diferenca2)
  varianca <- soma2/(n-1)
  resultado <- sqrt(varianca)
  return(resultado)
}

desvio(vetor)
sd(vetor)==desvio(vetor)

desvio2 <- function(entrada){
  n <- length(entrada)
  if (n <= 1) {
    resultado <- "Erro"
    return(resultado)
  }
  else {
    media <- mean(entrada)
    diff <- 0
    for (i in 1:n){
      diff <- diff + (entrada[i] - media) ^ 2
    }
    varianca <- diff/(n-1)
    desviopad <- sqrt(varianca)
    return(desviopad)
  }
}

desvio2(vetor)
desvio(vetor)==desvio2(vetor)
identical(desvio(vetor),desvio2(vetor)) # caso tenha problema com ponto flutuante
