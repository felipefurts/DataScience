######################################
#   Aprendizado Supervisionado I     #
#       Exerc�???cio OvA OpenSet        #
######################################

library(stats)
library(caret)



######################################
#               AUX                  #
######################################
# Função para plotar uma imagem a partir de um único feature vector
# imgArray = fv da imagem, com classe na V1 e os pixels em V2 ~ V785
# os pixels podem estar no intervalo [0.0,1.0] ou [0, 255]
plotImage = function(imgArray){
    # Transforma array em uma matrix 28x28 (ignorando a classe em V1)
    imgMatrix = matrix((imgArray[2:ncol(imgArray)]), nrow=28, ncol=28)
    
    # Transforma cada elemento em numeric
    im_numbers <- apply(imgMatrix, 2, as.numeric)
    
    # Girando a imagem apenas p/ o plot
    flippedImg = im_numbers[,28:1]
    
    image(1:28, 1:28, flippedImg, col=gray((0:255)/255), xlab="", ylab="")
    title(imgArray[1])
}




######################################
#           SETUP DADOS              #
######################################

# Todas as classes do Fashion-MNIST
# Iremos dividir em alguns conjuntos para simular um cenário open-set
classesNames <- c("T-shirt/top", "Trouser", "Pullover", 
                 "Dress", "Coat", "Sandal", "Shirt", 
                 "Sneaker", "Bag", "Ankle boot")

#Classes Conhecidas
#       "T-shirt/top", "Pullover", "Coat", "Shirt"
#Classes Conhecidas Desconhecidas da Validaçao
#       Trouser, Dress 
#Classes Desconhecidas do Teste
#       Sandal, Sneaker, Bag, Ankle boot


# Vamos ler os dados de treino que só contém elementos das classes conhecidas
kTrainData <- read.csv("known_trainData.csv", header=TRUE)

colnames(kTrainData) #label e pixels

summary(as.factor(kTrainData[,1])) 

plotImage(kTrainData[1,])       #Plot T-shirt/top
plotImage(kTrainData[4500,])    #Plot Pullover
plotImage(kTrainData[9000,])    #Plot Coat
plotImage(kTrainData[13500,])   #Plot Shirt



# Vamos aplicar o PCA, para eliminar features irrelevantes
pca <- prcomp(kTrainData[,2:ncol(kTrainData)]) #ignorando a primeira coluna por ser label

# Vamos ver qual a % da variância explicada 
std_dev <- pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

nComponents <- 300
sum(prop_varex[1:nComponents])


# Vamos projetar os dados de treino na base do PCA (pca já retorna isso diretamente)
pcaKTrainData <- as.data.frame(cbind(kTrainData$label, pca$x[,1:nComponents]))
colnames(pcaKTrainData)[1] <- "label"
colnames(pcaKTrainData)



######################################
#           TREINAMENTO OvA          #
######################################

classesNames
classIdx <- c(0, 2, 4, 6)  #labels das classes conhecidas
ovaModels <- list()        #lista para salvar cada um dos modelos OvA

for (id in 1:length(classIdx)) {
    # Setup das classes OvA 
    oneClass <- classIdx[id]                      
    allClasses <- classIdx[classIdx != oneClass]

    #renomeando labels p/ "One" e "All"
    oneTrainData <- pcaKTrainData[pcaKTrainData$label == oneClass, ]
    oneTrainData$label <- 1
    
    allTrainData <- pcaKTrainData[pcaKTrainData$label %in% allClasses, ]
    allTrainData$label <- 0
    
    #unindo os dois conjuntos
    trainData <- rbind(oneTrainData, allTrainData)
    trainData$label <- as.factor(trainData$label)
    
    #treinando RegLog
    regLog <- glm(formula = label ~., data=trainData, family=binomial(link="logit"))
    ovaModels[[id]] <- regLog
}



#########################################
#  Validação OvA em classes conhecidas  #
#########################################
kValData <- read.csv("known_valData.csv", header=TRUE)
summary(as.factor(kValData[,1]))


#Projetar dados na base do PCA
pcaKValData <- as.matrix(predict(pca, newdata=kValData[,2:ncol(kValData)]))
pcaKValData <- as.data.frame(cbind(kValData$label, pcaKValData[,1:nComponents]))
colnames(pcaKValData)[1] <- "label"



#Predict em cada um dos OvAs 
# predict(ovaModels[[1]], pcaKValData[1:3,], type="response")
valPred <- sapply(ovaModels, function(x) predict(x, pcaKValData, type="response"))
valPred[1:3,]



# Vamos definir vetores one-hot p/ cada classe
classVectors <- diag(4)
classVectors



#########################################
# Vamos calcular a distância Manhattan entre o vetor de probabilidades
# e cada um dos vetores one-hot das classes
# Iremos atribuir a classe do vetor que tiver a menor distância

# valPred[2,]
# classVectors[1,]
# dist(rbind(valPred[2,],classVectors[1,]), method="manhattan")
# 
# apply(classVectors, 1, function(x){
#                         dist(rbind(valPred[2,],x), method="manhattan")
#                         })
valDists <- c()
for (idx in 1:nrow(valPred)) {
    dist <- apply(classVectors, 1, 
                 function(x){ 
                     dist(rbind(valPred[idx,],x), method="manhattan")
                 })
    valDists <- rbind(valDists, dist)
}

# Vamos selecionar a classe com menor distância
valClass <- classIdx[apply(valDists, 1, which.min)]
table(valClass)

# Calculando a matriz de confusão
cm <- confusionMatrix(data=as.factor(valClass), reference=as.factor(kValData$label))
cm$table
cm$overall["Accuracy"]











####################################################
#  Open-Set - Calibrando os thresholds de decisão  #
####################################################

# Vamos ler as classes desconhecidas da validação
ukTrainData <- read.csv("unknown_trainData.csv", header=TRUE)

# os nossos modelos nunca viram essas classes no treinamento
summary(as.factor(ukTrainData[,1]))

# Vamos plotar algumas delas
plotImage(ukTrainData[1,])
plotImage(ukTrainData[7001,])


# Projetar estes dados no espaço do PCA
pcaUKTrainData <- as.matrix(predict(pca, newdata=ukTrainData[,2:ncol(ukTrainData)]))
pcaUKTrainData <- as.data.frame(cbind(ukTrainData$label, pcaUKTrainData[,1:nComponents]))
colnames(pcaUKTrainData)[1] <- "label"

# Usar os OvAs p/ obter os vetores de probabilidades
ukPred <- sapply(ovaModels, function(x) predict(x, pcaUKTrainData, type="response"))
ukPred[1,]

# Multiplicaremos todos valores de cada vetor
#esperamos que a distribuição de probabilidade das classes conhecidas
#será diferente da distribuição das classes desconhecidas
valProbMult <- apply(valPred, 1, prod)
ukProbMult <- apply(ukPred, 1, prod)


ukProbMult[1]



#Iremos procurar um limiar para poder separar exemplos de classes conhecidas
# de exemplos cujas classes nunca foram vistas no treino

# tList <- seq(0,1 , length=10)
# tList <- seq(0,0.01 , length=10)
tList <- seq(0,1e-04, length=10)

#Para cada limiar, iremos anotar qual seria a acurácia ao prever se 
#um elemento é ou não de uma classe de uma classe conhecida

#multProbs < threshold ----> Classe conhecida
#multProbs >= threhsold ---> Classe desconhecida

accK <- c()
accUk <- c()
for (idx in 1:length(tList)) {
    t <- tList[idx]
    
    accK[idx] <- sum(valProbMult < t) / length(valProbMult)
    accUk[idx] <- sum(ukProbMult >= t) / length(ukProbMult)
}

acc <- (accK + accUk) / 2.0

#Plot das acurácias
plot(accK, type="o", col="blue", xaxt="n", xlab="t", ylab="Acc", ylim = c(0.0,1.0), main="ACC x t")
axis(1, at=1:length(tList), labels=tList, cex.axis=0.5, las=2)

points(accUk, col="red", pch="*")
lines(accUk, col="red",lty=2)

points(acc, col="black", pch="*")
lines(acc, col="black",lty=3)


#Vamos escolher este threshold
openSetThreshold <- 2.2e-05







####################################################
#  Open-Set - Validando um conjunto de teste       #
####################################################
testData <- read.csv("testData.csv", header=TRUE)

#Este conjunto contém classes que não foram vistas 
#em nenhum momento pelos OvAs ou na escolha do threshold
summary(as.factor(testData[,1]))

plotImage(testData[testData$label==5,][1,])
plotImage(testData[testData$label==7,][1,])
plotImage(testData[testData$label==8,][1,])
plotImage(testData[testData$label==9,][1,])


#Label = -1 p/ classes desconhecidas
testData[testData$label %in% c(5,7,8,9), "label"] <- -1


# Projetar os dados de teste no espaço PCA
pcaTestData <- as.matrix(predict(pca, newdata=testData[,2:ncol(testData)]))
pcaTestData <- as.data.frame(cbind(testData$label, pcaTestData[,1:nComponents]))
colnames(pcaTestData)[1] <- "label"


# Obter probabilidades a partir dos OvAs
testPred <- sapply(ovaModels, function(x) predict(x, pcaTestData, type="response"))

# Multiplicar cada vetor de probabilidade
testProbMult <- apply(testPred, 1, prod)

# Calcularemos as distâncias manhattan para prever exemplos das classes conhecidas
testDists <- c()
for (idx in 1:nrow(testPred)) {
    dist <- apply(classVectors, 1, 
                 function(x){ 
                     dist(rbind(testPred[idx,],x), method="manhattan")
                 })
    
    testDists <- rbind(testDists, dist)
}


#Se a multProb for abaixo do threshold, é um exemplo de uma classe conhecida
# então prevemos a classe utilizando as distâncias Manhattan
#caso contrário, dizemos que é um exemplo de uma classe desconhecida
testClass <- c()
for (idx in 1:nrow(testData)) {
    testClass[idx] <- ifelse(testProbMult[idx] < openSetThreshold,
                            classIdx[which.min(testDists[idx,])],
                            -1)
}

# Calculando a matrix de confusão
cm <- confusionMatrix(data=as.factor(testClass), reference=as.factor(testData$label))
cm$table
cm$overall["Accuracy"]


