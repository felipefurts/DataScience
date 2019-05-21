#################################
# MDC - Machine Learning		#
# One Vs All    				#
#################################

set.seed(42)

# load dataset
data <- iris

# split the data from each class into train and val
setosaData <- data[data$Species == "setosa",]
idx <- sample(1:nrow(setosaData), 0.8*nrow(setosaData))
setosaTrainData <- setosaData[idx,]
setosaValData <- setosaData[-idx,]

versicolorData <- data[data$Species == "versicolor",]
idx <- sample(1:nrow(versicolorData), 0.8*nrow(versicolorData))
versicolorTrainData <- versicolorData[idx,]
versicolorValData <- versicolorData[-idx,]

virginicaData <- data[data$Species == "virginica",]
idx <- sample(1:nrow(virginicaData), 0.8*nrow(virginicaData))
virginicaTrainData <- virginicaData[idx,]
virginicaValData <- virginicaData[-idx,]

# join all validation data together
valData <- rbind(setosaValData, versicolorValData, virginicaValData)
featuresVal <- valData[,1:4]
labelVal <- valData[,"Species"]

# defining the formula
f <- as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")




############### Setosa vs Rest
positiveTrainData <- setosaTrainData
positiveTrainData$Species <- 1

negativeTrainData <- rbind(versicolorTrainData, virginicaTrainData)
negativeTrainData$Species <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)
nrow(trainData)


setosaRegLog <- glm(f, trainData, family=binomial(link="logit"))







############### Versicolor vs Rest
positiveTrainData <- versicolorTrainData
positiveTrainData$Species <- 1

negativeTrainData <- rbind(setosaTrainData, virginicaTrainData)
negativeTrainData$Species <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)

versicolorRegLog <- glm(f, trainData, family=binomial(link="logit"))





############### Virginica vs Rest
positiveTrainData <- virginicaTrainData
positiveTrainData$Species <- 1

negativeTrainData <- rbind(versicolorTrainData, setosaTrainData)
negativeTrainData$Species <- 0

trainData <- rbind(positiveTrainData, negativeTrainData)

virginicaRegLog <- glm(f, trainData, family=binomial(link="logit"))






###### Combining One vs All ######
# compute the prediction for each RegLog

## Setosa
predSetosa <- predict(setosaRegLog, featuresVal, type="response")
predSetosa[predSetosa < 0.5] <- -1
predSetosa[predSetosa >= 0.5] <- 1

## Versicolor
predVersicolor <- predict(versicolorRegLog, featuresVal, type="response")
predVersicolor[predVersicolor < 0.5] <- -1
predVersicolor[predVersicolor >= 0.5] <- 1

## Virginica
predVirginica <- predict(virginicaRegLog, featuresVal, type="response")
predVirginica[predVirginica < 0.5] <- -1
predVirginica[predVirginica >= 0.5] <- 1



#combining them
combinedPred <- data.frame(setosa=numeric(length(predSetosa)),
						versicolor=numeric(length(predSetosa)),
						virginica=numeric(length(predSetosa)))

combinedPred[,"setosa"] <-  predSetosa - predVersicolor - predVirginica
combinedPred[,"versicolor"] <-  predVersicolor - predSetosa - predVirginica
combinedPred[,"virginica"] <-  predVirginica - predVersicolor - predSetosa




#computing final prediction and confusion matrix
finalPred <- colnames(combinedPred)[apply(combinedPred, 1, which.max)]
CM <- as.matrix(table(Actual = labelVal, Predicted = finalPred))
CM
