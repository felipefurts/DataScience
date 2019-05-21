########### Trabalho 1 - MÃ³dulo 2  ###########
## Individual [  ]          Dupla [ X ]    
## Aluno 1: Felipe de Melo Furtadi
	# 
## Aluno 2: Alexis Seiki Saito
	# 

#### Atividade 1 - base de dados: telecom-churn.csv
telecom_churn = read.table("C:/Curso/trabalho2/telecom-churn.csv", sep=",", header = TRUE, stringsAsFactors= FALSE)
## Item 1
# SeparaÃ§Ã£o dos dados numÃ©ricos
get_dados_numericos <- function(caminho) {
  retorno <- data.frame()
  retorno <- caminho[sapply(caminho,is.numeric)|sapply(caminho,is.integer)]
  retorno <- na.omit(retorno)
}

## Item 2
# InformaÃ§Ãµes de CaracterÃ­sticas
moda <- function(v) {
  uniqv <- unique(v)
  uniqv [ which.max( tabulate(match(v,uniqv)))]
}

print_info <- function(data_frame) {
  print("Média de cada coluna:")
  print(apply(data_frame,2,mean))
  print("Variância de cada coluna:")
  print(apply(data_frame,2,var))
  print("Momento de cada coluna:")
  print(apply(data_frame,2,moment))
  print("Assimetria de cada coluna:")
  print(apply(data_frame,2,skewness))
  print("Curtose de cada coluna:")
  print(apply(data_frame,2,kurtosis))
  print("Mínimo de cada coluna:")
  print(apply(data_frame,2,min))
  print("Máximo de cada coluna:")
  print(apply(data_frame,2,max))
  print("Mediana de cada coluna:")
  print(apply(data_frame,2,median))
  print("A Moda de cada coluna:")
  print(apply(data_frame,2,moda))
}

## Item 3
# RelaÃ§Ãµes entre CaracterÃ­sticas
print_relacao <- function(data_frame) {
  print("Correlação de cada coluna:")
  print(apply(data_frame,2,cor))
}



#### Atividade 2 - base de dados: bakery.csv

## Item 1
# AnÃ¡lise de FrequÃªncia
bakery <- read.transactions("C:/Users/edbaxsa/Documents/Data Science/bakery.csv", sep=",", format = "basket")

get_histo_top20 <- function(caminho) {
    itemFrequencyPlot (caminho, topN =20, type = "absolute")
}

## Item 2
regras <- apriori(bakery ,parameter = list ( supp =0.001 ,conf =0.8) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))

# MineraÃ§Ã£o de Regras
################################################################
#   # confianÃ§a # suporte #        regras de associaÃ§Ã£o        #    
################################################################
# 1 #  1        #0.001163242#  {Extra Salami or Feta,Juice}        => {Salad} # 
# 2 #  1        #0.001163242#  {Extra Salami or Feta,Sandwich}     => {Salad} # 
# 3 #  1        #0.001163242#  {Cake,Extra Salami or Feta}         => {Salad} # 
# 4 #  1        #0.002326483#  {Bread,Extra Salami or Feta}        => {Salad} # 
# 5 #  1        #0.001163242#  {Coffee,Extra Salami or Feta,Juice} => {Salad} # 
################################################################
# Segue as 5 regras de associação com o maior lift encontrado.
#     lhs                                    rhs     support     confidence lift     count
# [1] {Extra Salami or Feta,Juice}        => {Salad} 0.001163242 1          39.07576 3    
# [2] {Extra Salami or Feta,Sandwich}     => {Salad} 0.001163242 1          39.07576 3    
# [3] {Cake,Extra Salami or Feta}         => {Salad} 0.001163242 1          39.07576 3    
# [4] {Bread,Extra Salami or Feta}        => {Salad} 0.002326483 1          39.07576 6    
# [5] {Coffee,Extra Salami or Feta,Juice} => {Salad} 0.001163242 1          39.07576 3 
#
# Comentário: pelo resultado, todas as regras parecem ter bastante forte a relação entre {Extra Salami or Feta}
# e {Salad} apesar do resultado não apontar somente essa relação. 

regras <- apriori(bakery ,parameter = list ( supp =0.002 ,conf =0.1) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))


################################################################
#   # confianÃ§a # suporte #        regras de associaÃ§Ã£o        #    
################################################################
# 1 #  0.3750000#0.002326483#  {Bread,Salad}                => {Extra Salami or Feta} # 
# 2 #  0.3111111#0.005428461#  {Coffee,Salad}               => {Extra Salami or Feta} # 
# 3 #  1.0000000#0.002326483#  {Bread,Extra Salami or Feta} => {Salad}                # 
# 4 #  0.8000000#0.006203955#  {Extra Salami or Feta}       => {Salad}                # 
# 5 #  0.2424242#0.006203955#  {Salad}                      => {Extra Salami or Feta} # 
################################################################
# Segue as 5 regras de associação com o maior lift encontrado.
#     lhs                             rhs                    support     confidence lift     count
# [1] {Bread,Salad}                => {Extra Salami or Feta} 0.002326483 0.3750000  48.35625  6   
# [2] {Coffee,Salad}               => {Extra Salami or Feta} 0.005428461 0.3111111  40.11778 14   
# [3] {Bread,Extra Salami or Feta} => {Salad}                0.002326483 1.0000000  39.07576  6   
# [4] {Extra Salami or Feta}       => {Salad}                0.006203955 0.8000000  31.26061 16   
# [5] {Salad}                      => {Extra Salami or Feta} 0.006203955 0.2424242  31.26061 16  
# Comentário: a relação {Salad} <=> {Extra Salami or Feta} aparece em todos as regras, em especial
# as regras [4] e [5] faz a relação 1-1

regras <- apriori(bakery ,parameter = list ( supp =0.005 ,conf =0.3) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))
 
################################################################
#   # confianÃ§a # suporte #        regras de associaÃ§Ã£o        #    
################################################################
# 1 #  0.3111111#0.005428461#  {Coffee,Salad}                => {Extra Salami or Feta} # 
# 2 #  0.8000000#0.006203955#  {Extra Salami or Feta}        => {Salad}                # 
# 3 #  0.7777778#0.005428461#  {Coffee,Extra Salami or Feta} => {Salad}                # 
# 4 #  0.3191489#0.005816208#  {Coffee,Coke}                 => {Sandwich}             # 
# 5 #  0.3111111#0.005428461#  {Coffee,Salad}                => {Sandwich}             # 
################################################################
# Segue as 5 regras de associação com o maior lift encontrado.
#lhs                               rhs                       support     confidence lift      count
#[1] {Coffee,Salad}                => {Extra Salami or Feta} 0.005428461 0.3111111  40.117778 14   
#[2] {Extra Salami or Feta}        => {Salad}                0.006203955 0.8000000  31.260606 16   
#[3] {Coffee,Extra Salami or Feta} => {Salad}                0.005428461 0.7777778  30.392256 14   
#[4] {Coffee,Coke}                 => {Sandwich}             0.005816208 0.3191489   2.486662 15   
#[5] {Coffee,Salad}                => {Sandwich}             0.005428461 0.3111111   2.424035 14
# Comentário: as regras [1] e [3] tem relação parecida {Extra Salami or Feta} <=> {Salad} com item em
# comum {Coffe}.

regras <- apriori(bakery ,parameter = list ( supp =0.01 ,conf =0.5) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))

################################################################
#   # confianÃ§a # suporte #        regras de associaÃ§Ã£o        #    
################################################################
# 1 #  0.7202797#0.03993796#  {Toast}                => {Coffee} # 
# 2 #  0.6851852#0.01434665#  {Cake,Sandwich}        => {Coffee} # 
# 3 #  0.6818182#0.01744862#  {Salad}                => {Coffee} # 
# 4 #  0.6666667#0.01085692#  {Hot chocolate,Pastry} => {Coffee} # 
# 5 #  0.6279070#0.01046917#  {Sandwich,Soup}        => {Coffee} # 
################################################################
# Segue as 5 regras de associação com o maior lift encontrado.
#lhs                           rhs      support    confidence lift     count
#[1] {Toast}                => {Coffee} 0.03993796 0.7202797  1.324021 103  
#[2] {Cake,Sandwich}        => {Coffee} 0.01434665 0.6851852  1.259510  37  
#[3] {Salad}                => {Coffee} 0.01744862 0.6818182  1.253321  45  
#[4] {Hot chocolate,Pastry} => {Coffee} 0.01085692 0.6666667  1.225469  28  
#[5] {Sandwich,Soup}        => {Coffee} 0.01046917 0.6279070  1.154221  27  
#Comentário: [1] tem uma ocorrência muito maior (103) que os demais, regra [5] tem associação entre
# {Soup} => {Coffe} que parece fazer menos sentido, provelmente que por vir acompanhado por {Sandwich} 

regras <- apriori(bakery ,parameter = list ( supp =0.09 ,conf =0.1) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))

################################################################
#   # confianÃ§a # suporte #        regras de associaÃ§Ã£o        #    
################################################################
# 1 #  0.5592233#0.1116712#  {Cake}   => {Coffee}    # 
# 2 #  0.2052744#0.1116712#  {Coffee} => {Cake}      # 
# 3 #  0.1046917#0.1046917#  {}       => {Medialuna} # 
# 4 #  0.1043040#0.1043040#  {}       => {Juice}     # 
# 5 #  0.1136099#0.1136099#  {}       => {Cookies}   # 
################################################################
# Segue as 5 regras de associação com o maior lift encontrado.
#lhs             rhs         support   confidence lift     count
#[1] {Cake}   => {Coffee}    0.1116712 0.5592233  1.027966 288  
#[2] {Coffee} => {Cake}      0.1116712 0.2052744  1.027966 288  
#[3] {}       => {Medialuna} 0.1046917 0.1046917  1.000000 270  
#[4] {}       => {Juice}     0.1043040 0.1043040  1.000000 269  
#[5] {}       => {Cookies}   0.1136099 0.1136099  1.000000 293  
# Comentário: somente as regras [1] e [2] tem valores L não vazios e ocorrências similares o que 
# faz sentido.

## Item 3
# DeterminaÃ§Ã£o de DependÃªncia EstatÃ­stica
get_regras_interessantes <- function(caminho, minsupp, minconf) {
  
    transactions <- read.transactions(caminho, format="basket",sep=",")
    regras <- apriori(transactions, parameter=list(supp=minsupp,conf=minconf))
    convic <- interestMeasure(regras, "conviction", transactions)
    odds <- interestMeasure(regras, "oddsRatio", transactions)
    
    convic <- convic>=1.01&convic<=5
    odds <- odds >= 0
    lift <- regras@quality[["lift"]]>=1
    
    regras <- regras[convic&odds&lift]
}















