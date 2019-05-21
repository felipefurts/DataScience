#Criando DF Cepagri e nomeando as colunas
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("C:/Users/EMELFEL/Downloads/cepagri.csv", header = FALSE , sep = ";", col.names = names)

#con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
#cepagri <- read.table(con, header = FALSE, fill = TRUE, sep = ";", col.names = names)

#convertendo as datas para padrão POSIXct
cepagri[ ,1] <- as.POSIXct(as.character(cepagri[,1]), format = '%d/%m/%Y-%H:%M')

#Convertendo coluna 2 de String para Numeric como as outras.
cepagri[ , 2] <- as.character(cepagri[ , 2])
cepagri[ , 2] <- as.numeric(cepagri[ , 2])

#Eliminando linhas com NAs
cepagri <- cepagri [!is.na(cepagri[ , 2]), ]
#Eliminando valor de medida imprecisa 99.9%
cepagri <- cepagri[cepagri[ , 5] != 99.9, ]

#Função para procurar dias consecutivos
#consecutive <- function(vector, k = 1) {
#  n <- length(vector)
#  result <- logical(n)
#  for (i in (1+k):n)
#    if (all(vector[(i-k):(i-1)] == vector[i]))
#      result[i] <- TRUE
#  for (i in 1:(n-k))
#    if (all(vector[(i+1):(i+k)] == vector[i]))
#      result[i] <- TRUE
#  return(result)
#}

#eliminando linhas com dados errados provocado por algum problema do sistema
#cepagri <- cepagri[!(consecutive(cepagri$temp, 6) &
#          consecutive(cepagri$vento, 6) &
#          consecutive(cepagri$umid, 6) &
#          consecutive(cepagri$sensa, 6)),]

#restringindo os dados entre 01/01/2015 até 31/12/2018
cepagri <- cepagri[cepagri[,1] >= "2015-01-01 00:00:00 -03",]
cepagri <- cepagri[cepagri[,1] <= "2018-12-31 23:59:59 -03",]

#preparando gráficos por ano
plot2015 <- cepagri[cepagri[,1] <= "2015-12-31 23:59:59 -03",]
plot2016 <- cepagri[((cepagri[,1] <= "2016-12-31 23:59:59 -03") & (cepagri[,1] >= "2016-01-01 00:00:00 -03")), ]
plot2017 <- cepagri[((cepagri[,1] <= "2017-12-31 23:59:59 -03") & (cepagri[,1] >= "2017-01-01 00:00:00 -03")), ]
plot2018 <- cepagri[((cepagri[,1] <= "2018-12-31 23:59:59 -03") & (cepagri[,1] >= "2018-01-01 00:00:00 -03")), ]

minMaxTemp <- function(x) {
  nomes <- c("ano", "mes", "min", "max", "media")
  ano <- year(as.POSIXlt(x$horario[1]))
  retorno <-data.frame() 
  for(i in 1:12) {
    retorno <- rbind(retorno, c(ano, i, min(x[month(as.POSIXlt(x$horario))==i, 2]),
                                max(x[month(as.POSIXlt(x$horario))==i, 2]), 
                                mean(x[month(as.POSIXlt(x$horario))==i, 2])))
  }
  names(retorno)[1:5] <- nomes
  return(retorno)
}

x2015 <- minMaxTemp(plot2015)
x2016 <- minMaxTemp(plot2016)
x2017 <- minMaxTemp(plot2017)
x2018 <- minMaxTemp(plot2018)

p <- ggplot() + 
  geom_line(data=x2015, aes(x=x2015$mes, y=x2015$max, colour = "2015")) + 
  geom_line(data=x2016, aes(x=x2016$mes, y=x2016$max, colour = "2016")) +
  geom_line(data=x2017, aes(x=x2017$mes, y=x2017$max, colour = "2017")) +
  geom_line(data=x2018, aes(x=x2018$mes, y=x2018$max, colour = "2018"))

p <- p + labs(colour = "Legenda:", title = "Temperatura mensal máxima por ano") + 
  theme(legend.background = element_rect(linetype = "solid")) + theme_minimal ()+
  ylab("Temperatura °C")  + xlab("Mês") + ylim(26,38) +  xlim(month.abb)

print(p)

minMaxPorSemana <- function(x, col_medida) {
  nomes <- c("ano", "semana", "min", "max", "media")
  ano <- year(as.POSIXlt(x$horario[1]))
  retorno <-data.frame() 
  for(i in 1:53) {
    retorno <- rbind(retorno, c(ano, i, min(x[week(as.POSIXlt(x$horario))==i, col_medida]), 
                                max(x[week(as.POSIXlt(x$horario))==i, col_medida]), 
                                mean(x[week(as.POSIXlt(x$horario))==i, col_medida])))
    
  }
  names(retorno)[1:5] <- nomes
  return(retorno)
}
