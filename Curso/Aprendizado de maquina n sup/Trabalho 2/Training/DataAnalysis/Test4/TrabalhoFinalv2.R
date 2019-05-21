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
## minMaxUmidade - função para calcular umidade mímima, máxima e média por mês em 1 ano
########################################################################################
minMaxUmidade <- function(x) {
  nomes <- c("ano", "mes", "min", "max", "media")
  ano <- year(as.POSIXlt(x$horario[1]))
  retorno <-data.frame() 
  for(i in 1:12) {
    retorno <- rbind(retorno, c(ano, i, min(x[month(as.POSIXlt(x$horario))==i, 4]), 
                                max(x[month(as.POSIXlt(x$horario))==i, 4]), 
                                mean(x[month(as.POSIXlt(x$horario))==i, 4])))
    
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
##                               ano. O percentual é aproimado, ignorando os decimais
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
View(minMaxUmidade(dados2015))
View(minMaxUmidade(dados2016))
View(minMaxUmidade(dados2017))
View(minMaxUmidade(dados2018))

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

