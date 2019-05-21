#######################################################################
###
### Trabalho final de Análise de Dados
### Curso de extensão em Ciência de Dados - UNICAMP
###
### Alunos: Alexis Saito / Felipe Furtado
### Período: Fevereiro de 2019
###
#######################################################################



## Carregamento dos dados do CEPAGRI
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv", header = FALSE , sep = ";", col.names = names)


## Preparação dos dados do CEPAGRI
cepagri[ ,1] <- as.POSIXct(as.character(cepagri[,1]), format = '%d/%m/%Y-%H:%M')
cepagri[ , 2] <- as.character(cepagri[ , 2])
cepagri[ , 2] <- as.numeric(cepagri[ , 2])
cepagri <- cepagri [!is.na(cepagri[ , 2]), ]
cepagri <- cepagri[cepagri[ , 5] != 99.9, ]

## Remoção de dados fora do período requerido pelo exercício
cepagri <- cepagri[cepagri[,1] >= "2015-01-01 00:00:00 -03",]
cepagri <- cepagri[cepagri[,1] <= "2018-12-31 23:59:59 -03",]

## Separação de dados por ano
dados2015 <- cepagri[cepagri[,1] <= "2015-12-31 23:59:59 -03",]
dados2016 <- cepagri[((cepagri[,1] <= "2016-12-31 23:59:59 -03") & (cepagri[,1] >= "2016-01-01 00:00:00 -03")), ]
dados2017 <- cepagri[((cepagri[,1] <= "2017-12-31 23:59:59 -03") & (cepagri[,1] >= "2017-01-01 00:00:00 -03")), ]
dados2018 <- cepagri[((cepagri[,1] <= "2018-12-31 23:59:59 -03") & (cepagri[,1] >= "2018-01-01 00:00:00 -03")), ]

########################################################################################
## minMaxMedida - função para calcular medida(Temp, vento, umidade, sensação) mímima, máxima e média por mês em 1 ano
########################################################################################
minMaxMedida <- function(x, col_medida) {
  nomes <- c("ano", "mes", "min", "max", "media")
  ano <- year(as.POSIXlt(x$horario[1]))
  retorno <-data.frame() 
  for(i in 1:12) {
    retorno <- rbind(retorno, c(ano, i, min(x[month(as.POSIXlt(x$horario))==i, col_medida]), 
                                max(x[month(as.POSIXlt(x$horario))==i, col_medida]), 
                                mean(x[month(as.POSIXlt(x$horario))==i, col_medida])))
    
  }
  names(retorno)[1:5] <- nomes
  return(retorno)
}
########################################################################################

########################################################################################
## incluiMes - função que inclui coluna de abrevição de mês em um data.frame 
########################################################################################
incluiMes <- function(x) {
  Mes <- factor(month.abb[as.numeric(month(as.POSIXlt(x$horario)))],
                levels = month.abb,
                ordered = TRUE)
  return(cbind(x,Mes))
}
########################################################################################


########################################################################################
## percentualMensalDeDiasSecos - função que calcula o percentual de dias secos (menores
##                               que 30% de umidade relativa) em cada mês ao longo de 1
##                               ano. O percentual é aproximado, ignorando os decimais
########################################################################################
percentualMensalDeDiasSecos <- function(df) {
  resultado <- NULL
  for (i in 1:12) {
    v <- (df[month(df$horario)==i,])
    v <- v[!is.na(v[ , 1]),]
    resultado[i] <- as.integer(100*(length(v[v$umid <= 30, 1])/length(v[, 1])))
  }
  return(resultado)
}


## Gera tabela de percentuais mensais de períodos em que a umidade ficou abaixo dos 30%.
percentualSeco <- data.frame(month.abb,
                             "2015" = percentualMensalDeDiasSecos(cepagri[year(cepagri$horario)==2015, ]),
                             "2016" = percentualMensalDeDiasSecos(cepagri[year(cepagri$horario)==2016, ]),
                             "2017" = percentualMensalDeDiasSecos(cepagri[year(cepagri$horario)==2017, ]),
                             "2018" = percentualMensalDeDiasSecos(cepagri[year(cepagri$horario)==2018, ]))

#### Visualização das tabelas de minimos e maximos para cada ano
View(minMaxMedida(dados2015, 4))
View(minMaxMedida(dados2016, 4))
View(minMaxMedida(dados2017, 4))
View(minMaxMedida(dados2018, 4))

#### Gera dados para ser usado no plot
umidade2015 <- incluiMes(dados2015)
umidade2016 <- incluiMes(dados2016)
umidade2017 <- incluiMes(dados2017)
umidade2018 <- incluiMes(dados2018)

ggplot(umidade2015, aes(y=umid, x=Mes, group=Mes)) +
  ggtitle("Variação de Umidade em Campinas (2015)") + ylab("Umidade") + xlab("Mês") +
  geom_boxplot()
ggplot(umidade2016, aes(y=umid, x=Mes, group=Mes)) +
  ggtitle("Variação de Umidade em Campinas (2016)") + ylab("Umidade") + xlab("Mês") +
  geom_boxplot()
ggplot(umidade2017, aes(y=umid, x=Mes, group=Mes)) +
  ggtitle("Variação de Umidade em Campinas (2017)") + ylab("Umidade") + xlab("Mês") +
  geom_boxplot()
ggplot(umidade2018, aes(y=umid, x=Mes, group=Mes)) +
  ggtitle("Variação de Umidade em Campinas (2018)") + ylab("Umidade") + xlab("Mês") +
  geom_boxplot()

########################################################################################
## Análise 2 - Comparação temperatura máxima entre 2015 - 2018
########################################################################################

#### dados de temperatura mínima, média e máxima entre 2015 - 2018
temp2015 <- minMaxMedida(dados2015, 2)
temp2016 <- minMaxMedida(dados2016, 2)
temp2017 <- minMaxMedida(dados2017, 2)
temp2018 <- minMaxMedida(dados2018, 2)

####Gráfico de Jan - dez da temperatura máxima mensal de cada ano
p <- ggplot() + 
  geom_line(data=temp2015, aes(x=temp2015$mes, y=temp2015$max, colour = "2015")) + 
  geom_line(data=temp2016, aes(x=temp2016$mes, y=temp2016$max, colour = "2016")) +
  geom_line(data=temp2017, aes(x=temp2017$mes, y=temp2017$max, colour = "2017")) +
  geom_line(data=temp2018, aes(x=temp2018$mes, y=temp2018$max, colour = "2018"))

####Formatação
p <- p + labs(colour = "Legenda:", title = "Temperatura mensal máxima por ano") + 
  theme(legend.background = element_rect(linetype = "solid")) + 
  ylab("Temperatura °C")  + xlab("Mês") + ylim(26,38) +  xlim(month.abb)

print(p)

########################################################################################
## Análise 3 - Relação entre como a velocidade média do vento impacta na temperatura média.
########################################################################################

########################################################################################
## minMaxSemana - função para calcular medida(Temp, vento, umidade, sensação) mímima, máxima e média por semana em 1 ano
########################################################################################

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

#### Calculando vento min, max, media por semana em 2018
vent2018 <- minMaxPorSemana(dados2018, 3)

#### Calculando temp min, max, media por semana em 2018
temp2018 <- minMaxPorSemana(dados2018, 2)

####Criando coluna de vento moderado/forte utilizando escala de BEAUFORT forte > 26 K/h
vent2018$vento <- ifelse(vent2018$media > 26, "Vento Forte", "Moderado")

####Adicionando temp media no DF vent2018
vent2018$temp <- temp2018$media

####Histograma com a média mensal de vento em 2018 pela temperatura
v <- ggplot(vent2018 , aes(x = temp)) + geom_histogram(aes(fill = vento), binwidth = 2.5, boundary = 0)

####Formatação
v <- v + labs(colour = "Legenda:", title = "Temperatura média ocorrência") + 
  theme(legend.background = element_rect(linetype = "solid")) + 
  ylab("Ocorrência")  + xlab("Temperatura")
print(v)
