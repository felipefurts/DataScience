########### Trabalho 2 - Módulo 2 ###########
## Individual [ X ]          Dupla [   ]    
## Aluno 1: Felipe de Melo Furtado
# 
## Aluno 2 (deixar em branco caso seja individual): 
# 

#### Atividade 1 ####

frogs <- read.csv("C:/Users/EMELFEL/OneDrive - Ericsson AB/Curso/Aprendizado de maquina � sup/Trabalho 2/frogs.csv", sep=",")

## Item 1
# Obtenção dos autovetores e autovalores
get_autovetores <- function(base) {
  data <- data.frame()
  data.pca <- data.frame()
  data <- base
  data.pca <- prcomp(data[1:22])
}

## Item 2
# Escolha do número de dimensões para redução
get_numero_dimensoes <- function(autovetores, x) {
  data <- autovetores
  divsdev<-(data$sdev^2)/sum(data$sdev^2)
  Esum <- cumsum(divsdev)
  k <- min(which((Esum>x)==TRUE))
  data$rotation[,1:k]
  print(k)
}

###################
#     #     K     #   
###################
# 90% #     8     # 
# 95% #     11    # 
# 99% #     17    #
###################


#### Atividade 2 ####

frogs <- read.csv("C:/Users/EMELFEL/OneDrive - Ericsson AB/Curso/Aprendizado de maquina � sup/Trabalho 2/frogs.csv", sep=",")

## Função 1
# Utilizando a catergoria Species
grafico_pca <- function(base){
  data <- base
  data.pca <- prcomp(data[1:22])
  plot(data.pca$x, col=data$Species)
}

## Função 2
# Utilizando a catergoria Species
grafico_tsne <- function(base){
  data <- base
  set.seed(20) # para reprodutibilidade
  tsne <- Rtsne(data[,1:22], perplexity = 30, dims=2, max_iter = 500, verbose = TRUE)
  plot(tsne$Y, col=data$Species, xlab="dimensao 1", ylab="dimensao 2", pch=16)
}

### Melhor projeção: A melhor proje��o � do TSNE
### Motivo: Graficamente fica f�cil identificar no TSNE os outliers e a distribui��o fica melhor visualmente