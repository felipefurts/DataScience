########### Trabalho 1 - Módulo 2  ###########
## Individual [  ]          Dupla [   ]    
## Aluno 1: 
	# 
## Aluno 2 (deixar em branco caso seja individual): 
	# 



#### Atividade 1 - base de dados: telecom-churn.csv
telecom_churn = read.table("C:/Curso/trabalho2/telecom-churn.csv", sep=",", header = TRUE, stringsAsFactors= FALSE)
## Item 1
# Separação dos dados numéricos
get_dados_numericos <- function(caminho) {
  retorno <- data.frame()
  retorno <- caminho[sapply(caminho,is.numeric)|sapply(caminho,is.integer)]
  retorno <- na.omit(retorno)
}

## Item 2
# Informações de Características
moda <- function(v) {
  uniqv <- unique(v)
  uniqv [ which.max( tabulate(match(v,uniqv)))]
}

print_info <- function(data_frame) {
  print("M�dia de cada coluna:")
  print(apply(data_frame,2,mean))
  print("Vari�ncia de cada coluna:")
  print(apply(data_frame,2,var))
  print("Momento de cada coluna:")
  print(apply(data_frame,2,moment))
  print("Assimetria de cada coluna:")
  print(apply(data_frame,2,skewness))
  print("Curtose de cada coluna:")
  print(apply(data_frame,2,kurtosis))
  print("M�nimo de cada coluna:")
  print(apply(data_frame,2,min))
  print("M�ximo de cada coluna:")
  print(apply(data_frame,2,max))
  print("Mediana de cada coluna:")
  print(apply(data_frame,2,median))
  print("A Moda de cada coluna:")
  print(apply(data_frame,2,moda))
}

## Item 3
# Relações entre Características
print_relacao <- function(data_frame) {
  print("Correla��o de cada coluna:")
  print(apply(data_frame,2,cor))
}



#### Atividade 2 - base de dados: bakery.csv

## Item 1
# Análise de Frequência
bakery <- read.transactions("C:/Curso/trabalho2/bakery.csv", sep=",", format = "basket")

get_histo_top20 <- function(caminho) {
    itemFrequencyPlot (caminho, topN =20, type = "absolute")
}

## Item 2
regras <- apriori(bakery ,parameter = list ( supp =0.001 ,conf =0.8) )
inspect(regras[1:5])
inspect(head(sort(regras, by="lift"), 5))

# Mineração de Regras
################################################################
#   # confiança # suporte #        regras de associação        #    
################################################################
# 1 #  1        #0.001163242#  {Extra Salami or Feta,Juice}        => {Salad} # 
# 2 #  1        #0.001163242#  {Extra Salami or Feta,Sandwich}     => {Salad} # 
# 3 #  1        #0.001163242#  {Cake,Extra Salami or Feta}         => {Salad} # 
# 4 #  1        #0.002326483#  {Bread,Extra Salami or Feta}        => {Salad} # 
# 5 #  1        #0.001163242#  {Coffee,Extra Salami or Feta,Juice} => {Salad} # 
################################################################
# Segue as 5 regras de associa��o com o maior lift encontrado.
#     lhs                                    rhs     support     confidence lift     count
# [1] {Extra Salami or Feta,Juice}        => {Salad} 0.001163242 1          39.07576 3    
# [2] {Extra Salami or Feta,Sandwich}     => {Salad} 0.001163242 1          39.07576 3    
# [3] {Cake,Extra Salami or Feta}         => {Salad} 0.001163242 1          39.07576 3    
# [4] {Bread,Extra Salami or Feta}        => {Salad} 0.002326483 1          39.07576 6    
# [5] {Coffee,Extra Salami or Feta,Juice} => {Salad} 0.001163242 1          39.07576 3  



## Item 3
# Determinação de Dependência Estatística
get_regras_interessantes <- function(caminho, minsupp, minconf) {
  regras <- apriori(caminho ,parameter = list(supp =minsupp ,conf =minconf))
  
}
















