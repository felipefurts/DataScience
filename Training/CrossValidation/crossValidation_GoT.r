source("GoT_aux.R") # auxiliary codes
set.seed(42)

#######################################
#    SETUP DATA                       #
#######################################
rawData <- read.csv("GoT_data.csv")

#Cleaning dataset 
cleanData <- cleanDataset(rawData)
summary(cleanData)


#Undersamlping Alive Class
aliveData <- cleanData[cleanData$deadOrAlive == "alive", ]
deadData <- cleanData[cleanData$deadOrAlive == "dead", ]
aliveIdx <- sample(1:nrow(aliveData), nrow(deadData), replace=FALSE)
data <- rbind(aliveData[aliveIdx,], deadData)

table(data$deadOrAlive)

# Setup Random Forest
targetAttribute <- "deadOrAlive"    #select the target variable
featureNames <- colnames(data[,2:(ncol(data)-1)])  #select the other features

percSampledFeatures <- 0.5  #Percentage of sampled features in each tree
percTrainingSamples <- 0.9  #Percentage of total data used in each tree

ntrees <- 10 #Number of trees






nrow(data)

# Cross-Validation SETUP
nFolds <- 10      # leave one out = nrow(data)
cv_splits <- createFolds(data$deadOrAlive, k = nFolds, returnTrain = TRUE) #generate the splits
summary(cv_splits)

accTrain <-  c()
accVal <-  c()

for (idx in 1:nFolds) {
    trainData <- data[cv_splits[[idx]], ]
    valData <- data[-cv_splits[[idx]], ]
    
    # table(trainData$deadOrAlive)
    # table(valData$deadOrAlive)
    
    rf <- rpartRF(trainData, targetAttribute, featureNames, 
                  percSampledFeatures, percTrainingSamples, ntrees)
    
    accTrain[idx] <- predictAndEvaluate(rf, trainData, ntrees)
    accVal[idx] <- predictAndEvaluate(rf, valData, ntrees)
}

# Plot acc for each fold
plot(accTrain, type="o", col="blue", xaxt="n", xlab="Fold", ylab="Acc", ylim=c(0.5, 1.0), main="Balanced ACC x Fold")
axis(1, at=1:nFolds, labels=1:nFolds, cex.axis=0.5, las=2)
points(accVal, col="red", pch="*")
lines(accVal, col="red",lty=1)

#Get Mean ACC among folds and STD
cvAccTrain <- mean(accTrain)
cvAccTrain
cvStdTrain <- sd(accTrain)
cvStdTrain

cvAccVal <- mean(accVal)
cvAccVal
cvStdVal <- sd(accVal)
cvStdVal

lines(rep(mean(accTrain), nFolds), col="blue", lty=2)
lines(rep(mean(accVal), nFolds), col="red", lty=2)











#######################################
#    Grid Search  - NTrees            #
#######################################
nFolds <- 5    
cv_splits <- createFolds(data$deadOrAlive, k = nFolds, returnTrain = TRUE)

cvTrainACC <- c()
cvValACC <- c()
nTrees <- c(5, 10, 25, 50)

for (treeIdx in 1:length(nTrees)) {
    accTrain <- c()
    accVal <- c()
    for (idx in 1:nFolds) {
        trainData <- data[cv_splits[[idx]], ]
        valData <- data[-cv_splits[[idx]], ]

        rf <- rpartRF(trainData, targetAttribute, featureNames, 
                      percSampledFeatures, percTrainingSamples, nTrees[treeIdx])
        
        accTrain[idx] <- predictAndEvaluate(rf, trainData, nTrees[treeIdx])
        accVal[idx] <- predictAndEvaluate(rf, valData, nTrees[treeIdx])
    }
    
    cvTrainACC[treeIdx] <- mean(accTrain)
    cvValACC[treeIdx] <- mean(accVal)
}

plot(cvTrainACC, type="o", col="blue", xaxt="n", xlab="nTrees", ylab="Acc", ylim=c(0.5, 1.0), main="Balanced ACC x nTrees")
axis(1, at=1:length(nTrees), labels=nTrees, cex.axis=0.5, las=2)
points(cvValACC, col="red", pch="*")
lines(cvValACC, col="red",lty=2)








#######################################
#    Grid Search  - NSampledFeatures  #
#######################################
nFolds <- 5    
cv_splits <- createFolds(data$deadOrAlive, k = nFolds, returnTrain = TRUE)

cvTrainACC <- c()
cvValACC <- c()

ntrees <- 25
percFeatures <- c(0.25, 0.5, 0.75, 0.9, 1.0)

for (sIdx in 1:length(percFeatures)) {
    accTrain <- c()
    accVal <- c()
    for (idx in 1:nFolds) {
        trainData <- data[cv_splits[[idx]], ]
        valData <- data[-cv_splits[[idx]], ]
        
        rf <- rpartRF(trainData, targetAttribute, featureNames, 
                      percFeatures[sIdx], percTrainingSamples, ntrees)
        
        accTrain[idx] <- predictAndEvaluate(rf, trainData, ntrees)
        accVal[idx] <- predictAndEvaluate(rf, valData, ntrees)
    }
    
    cvTrainACC[sIdx] <- mean(accTrain)
    cvValACC[sIdx] <- mean(accVal)
}

plot(cvTrainACC, type="o", col="blue", xaxt="n", xlab="% of sampled features", ylab="Acc", ylim=c(0.5, 1.0), main="Balanced ACC x sampled features")
axis(1, at=1:length(percFeatures), labels=percFeatures, cex.axis=0.5, las=2)
points(cvValACC, col="red", pch="*")
lines(cvValACC, col="red",lty=2)

