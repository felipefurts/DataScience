########################################
# Teste 2         
# Nome(s): Felipe de Melo Furtado
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
  a <- tapply(df[[colsum]], df[[colgroup]], sum)
  tresult <- data.frame(a)
  names(tresult)[1] <- colsum
  names(tresult)[0] <- colgroup
  return(tresult)
}

## 2 - Binario para Decimal

binToDec <- function(vector){
  result <- 0
  j <- length(vector) - 1
  for (i in vector) {
    result <- result + (i * 2^j)
    j <- j - 1
  }
  return(result)
}

## 3 - Ocorrencia de palavras

wordCount <- function(word, text){
  text <- tolower(text)
  word <- tolower(word)
  sentences <-strsplit(text, split = " ", fixed = TRUE)[[1]]
  sentences <- sentences[grep(word, sentences, ignore.case = TRUE)]
  return(length(sentences))
}

## 4 - Ordenacao de panquecas

#giro

#ordenar
