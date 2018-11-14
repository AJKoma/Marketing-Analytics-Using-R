#####          Homework 2          #####
#####   Xiaonan Hu      Liqi Zhu   #####
data.frame
########################################
# Part 1: Data Management 

## a. Load the dataset
setwd("C:/Users/Kiran/Desktop/2018 Spring/Marketing Analytics Using R/HW2")
DB <- read.csv("Homework 2 - MKT436R Data.csv")

## b. Initial a data frame for reshaping datadet
consumerID <- unique(DB$consumerID)
n <- length(consumerID)
DBS <- matrix(data = NA, nrow = n, ncol = 6)
DBS[,1] <- consumerID
DBS <- (DBS)
colnames(DBS) <- c('consumerID', 'rocky1', 'rocky2', 'rocky3', 'rocky4', 'rocky5')
DBS

## c. Reshape the dataset
### for loop
for (i in 1:nrow(DB)){
  r <- which(DBS$consumerID == DB$consumerID[i])
  c <- DB$rockyID[i] + 1
  DBS[r,c] <- DB$rating[i]
}
DBS
DBS[which(DBS$consumerID == "490432"), ]
### dcast function
library('reshape2')
DBS = dcast(DB,consumerID ~ rockyID, value.var = 'rating')
colnames(DBS) <- c('consumerID', 'rocky1', 'rocky2', 'rocky3', 'rocky4', 'rocky5')
DBS
DBS[which(DBS$consumerID == "490432"), ]

########################################
# Part 2: Data Exploration and Sampling Bias

## a. Compute the correlation matrix
cor(DBS[,2:6], use = "pairwise.complete.obs")

## b. Compute mean rating of each movie using all data
all_mean <- colMeans(DBS[,2:6], na.rm = TRUE)
all_mean

## c. Compute mean rating of each movie from consumers who rated rocky4
rocky4_subset <- DBS[!is.na(DBS$rocky4), ]
rocky4_subset
rocky4_subset_mean <- colMeans(rocky4_subset[, 2:6], na.rm = TRUE)
rbind(all_mean,rocky4_subset_mean)

## d. Load the complete dataset without NA
completeDB <- read.csv("Homework 2 - completeDB.csv")

########################################
# Part 3: Explanatory Models

## a. Generate different orders of interactions
firstInteractions <- model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4), completeDB)
secondInteractions <- model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^2, completeDB)
thirdInteractions <- model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^3, completeDB)
fourthInteractions <- model.matrix(~(-1+rocky1+rocky2+rocky3+rocky4)^4, completeDB)

## b. Run and store linear regressions for each of the former set
lm1 <- lm(completeDB$rocky5 ~ firstInteractions)
lm2 <- lm(completeDB$rocky5 ~ secondInteractions)
lm3 <- lm(completeDB$rocky5 ~ thirdInteractions)
lm4 <- lm(completeDB$rocky5 ~ fourthInteractions)

## c. Calculate AIC and BIC, store in a data frame
AIC <- AIC(lm1, lm2, lm3, lm4)
BIC <- BIC(lm1, lm2, lm3, lm4)
AICBIC <- cbind.data.frame(AIC, BIC$BIC)
colnames(AICBIC) <- c('df', 'AIC', 'BIC')
AICBIC

## d. Estimate lasso model using fourthInteractions, 
##   extract coefficients in the case of s=.05 and s=.5
library('glmnet')
lassoFit <- glmnet(fourthInteractions, completeDB$rocky5, alpha = 1)
COEF1 <- predict(lassoFit, s = .05, type = 'coefficients')
COEF2 <- predict(lassoFit, s = .5, type = 'coefficients')
COEF1; COEF2

## e. calculate an optimal penalty parameter using cross validation
lassoFitCV <- cv.glmnet(fourthInteractions, completeDB$rocky5, alpha = 1)

## f. Implement a ridge estimator using fourthInteractions
ridgeFit <- cv.glmnet(fourthInteractions, completeDB$rocky5, alpha = 0)

## g. Extract the coefficients from the lasso and the ridge regression
COEF_lassoFitCV <- predict(lassoFitCV, s = lassoFitCV$lambda.min, type = 'coefficients')
COEF_lassoFitCV
COEF_ridgeFit <- predict(ridgeFit, s = ridgeFit$lambda.min, type = 'coefficients')
COEF_ridgeFit

## Coefficients table
COEF_lm4 <- lm4$coefficients
COEF <- cbind(COEF_lm4, COEF_lassoFitCV, COEF_ridgeFit)
colnames(COEF) <- c('OLS', 'Lasso', 'Ridge')
COEF

########################################
# Part 4: Predictive Modelling

## a. K-Fold cross validation 
set.seed(1)
nFold <- 5
### Randomly assign each row in a fold
valNum <- floor(runif(nrow(completeDB))*nFold)+1

## b. Try different models
### i. Linear regressions
#### Create a matrix where we store lm-prediction error 
lmPerformance <- matrix(NA,nFold,10)
for(fold in 1:nFold){
  #### Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  #### Estimate models for this training data 
  LR1 <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky2:rocky4+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
            data = trainingData)
  LR2 <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky2:rocky3:rocky4, data = trainingData)
  LR3 <- lm(rocky5~ rocky2+rocky4+rocky1:rocky2+rocky1:rocky3+rocky2:rocky3+rocky1:rocky2:rocky3:rocky4, data = trainingData)
  LR4 <- lm(rocky5~ rocky2+rocky4+rocky1:rocky2+rocky1:rocky3+rocky1:rocky2:rocky3+rocky1:rocky3:rocky4+rocky2:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
            data = trainingData)
  LR5 <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky3+rocky1:rocky4+rocky2:rocky3+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4, 
            data = trainingData)
  LR6 <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4, 
            data = trainingData)
  LR7 <- lm(rocky5~ rocky1+rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky2:rocky3:rocky4, data = trainingData)
  LR8 <- lm(rocky5~ rocky2+rocky1:rocky2+rocky1:rocky3+rocky1:rocky4+rocky2:rocky3+rocky3:rocky4+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
            data = trainingData)
  LR9 <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky3+rocky1:rocky4+rocky2:rocky3+rocky1:rocky2:rocky4+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
            data = trainingData)
  LR10 <- lm(rocky5~ rocky1+rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
             data = trainingData)
  #### Calculate out of sample MSE for this validationData
  valid1 <- mean((validationData$rocky5 - predict(LR1, validationData))^2)^.5
  valid2 <- mean((validationData$rocky5 - predict(LR2, validationData))^2)^.5
  valid3 <- mean((validationData$rocky5 - predict(LR3, validationData))^2)^.5
  valid4 <- mean((validationData$rocky5 - predict(LR4, validationData))^2)^.5
  valid5 <- mean((validationData$rocky5 - predict(LR5, validationData))^2)^.5
  valid6 <- mean((validationData$rocky5 - predict(LR6, validationData))^2)^.5
  valid7 <- mean((validationData$rocky5 - predict(LR7, validationData))^2)^.5
  valid8 <- mean((validationData$rocky5 - predict(LR8, validationData))^2)^.5
  valid9 <- mean((validationData$rocky5 - predict(LR9, validationData))^2)^.5
  valid10 <- mean((validationData$rocky5 - predict(LR10, validationData))^2)^.5
  #### Store lm-models performance
  lmPerformance[fold,] <- c(valid1, valid2, valid3, valid4, valid5, valid6, valid7, valid8, valid9, valid10)
}
#### Check Average Model Performance
lm_mean_perform <- colMeans(lmPerformance)
#### The first one is best
which.min(lm_mean_perform)
min(lm_mean_perform)  #0.9593607


### ii. MARS
library(earth)
#### Create a matrix where we store MARS-prediction error 
MARSPerformance <- matrix(NA,nFold,10)
for(fold in 1:nFold){
  #### Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  #### Estimate models for this training data
  MARS1 <- earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData, degree = 1)
  MARS2 <- earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData, degree = 2)
  MARS3 <- earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData, degree = 3)
  MARS4 <- earth(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, degree = 1)
  MARS5 <- earth(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, degree = 2)
  MARS6 <- earth(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, degree = 3)
  MARS7 <- earth(rocky5 ~ rocky1 + rocky3 + rocky4, data = trainingData, degree = 1)
  MARS8 <- earth(rocky5 ~ rocky1 + rocky3 + rocky4, data = trainingData, degree = 3)
  MARS9 <- earth(rocky5 ~ rocky3 + rocky4, data = trainingData, degree = 1)
  MARS10 <- earth(rocky5 ~ rocky3 + rocky4, data = trainingData, degree = 3)
  #### Calculate out of sample MSE for this validationData
  valid1 <- mean((validationData$rocky5 - predict(MARS1, validationData))^2)^.5
  valid2 <- mean((validationData$rocky5 - predict(MARS2, validationData))^2)^.5
  valid3 <- mean((validationData$rocky5 - predict(MARS3, validationData))^2)^.5
  valid4 <- mean((validationData$rocky5 - predict(MARS4, validationData))^2)^.5
  valid5 <- mean((validationData$rocky5 - predict(MARS5, validationData))^2)^.5
  valid6 <- mean((validationData$rocky5 - predict(MARS6, validationData))^2)^.5
  valid7 <- mean((validationData$rocky5 - predict(MARS7, validationData))^2)^.5
  valid8 <- mean((validationData$rocky5 - predict(MARS8, validationData))^2)^.5
  valid9 <- mean((validationData$rocky5 - predict(MARS9, validationData))^2)^.5
  valid10 <- mean((validationData$rocky5 - predict(MARS10, validationData))^2)^.5
  #### Store MARS-models performance
  MARSPerformance[fold,] <- c(valid1, valid2, valid3, valid4, valid5, valid6, valid7, valid8, valid9, valid10)
}
#### Check Average Model Performance
MARS_mean_perform <- colMeans(MARSPerformance)
#### The third one is best
which.min(MARS_mean_perform)  
min(MARS_mean_perform)  #0.951864

### iii. Neural networks
library(nnet)
#### Create a matrix where we store Nnet-prediction error 
NnetPerformance <- matrix(NA,nFold,10)
for(fold in 1:nFold){
  #### Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  #### Estimate models for this training data
  set.seed(6)
  Nnet1 <- nnet(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 1, maxit = 10000, skip = TRUE)
  Nnet2 <- nnet(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 2, maxit = 10000, skip = TRUE)
  Nnet3 <- nnet(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 4, maxit = 10000, skip = TRUE)
  Nnet4 <- nnet(rocky5 ~ rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 5, maxit = 10000, skip = TRUE)
  Nnet5 <- nnet(rocky5 ~ rocky1 + rocky3 + rocky4, data = trainingData, linout = 1, size = 2, maxit = 10000, skip = TRUE)
  Nnet6 <- nnet(rocky5 ~ rocky1 + rocky3 + rocky4, data = trainingData, linout = 1, size = 5, maxit = 10000, skip = TRUE)
  Nnet7 <- nnet(rocky5 ~ rocky3 + rocky4, data = trainingData, linout = 1, size = 2, maxit = 10000, skip = TRUE)
  Nnet8 <- nnet(rocky5 ~ rocky3 + rocky4, data = trainingData, linout = 1, size = 5, maxit = 10000, skip = TRUE)
  Nnet9 <- nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 2, maxit = 10000, skip = TRUE)
  Nnet10 <- nnet(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = trainingData, linout = 1, size = 5, maxit = 10000, skip = TRUE)
  #### Calculate out of sample MSE for this validationData
  valid1 <- mean((validationData$rocky5 - predict(Nnet1, validationData))^2)^.5
  valid2 <- mean((validationData$rocky5 - predict(Nnet2, validationData))^2)^.5
  valid3 <- mean((validationData$rocky5 - predict(Nnet3, validationData))^2)^.5
  valid4 <- mean((validationData$rocky5 - predict(Nnet4, validationData))^2)^.5
  valid5 <- mean((validationData$rocky5 - predict(Nnet5, validationData))^2)^.5
  valid6 <- mean((validationData$rocky5 - predict(Nnet6, validationData))^2)^.5
  valid7 <- mean((validationData$rocky5 - predict(Nnet7, validationData))^2)^.5
  valid8 <- mean((validationData$rocky5 - predict(Nnet8, validationData))^2)^.5
  valid9 <- mean((validationData$rocky5 - predict(Nnet9, validationData))^2)^.5
  valid10 <- mean((validationData$rocky5 - predict(Nnet10, validationData))^2)^.5
  #### Store Nnet-models performance
  NnetPerformance[fold,] <- c(valid1, valid2, valid3, valid4, valid5, valid6, valid7, valid8, valid9, valid10)
}
#### Check Average Model Performance
Nnet_mean_perform <- colMeans(NnetPerformance)
#### The fourth one is best
which.min(Nnet_mean_perform) 
min(Nnet_mean_perform)  #0.9512391

### iv. K-Nearest Neighbour
library(class)
#### Create a matrix where we store knn-prediction error 
knnPerformance <- matrix(NA,nFold,10)
for(fold in 1:nFold){
  #### Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  cl <- trainingData$rocky5
  #### Estimate models for this training data
  knn1 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 5)
  knn2 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 6)
  knn3 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 7)
  knn4 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 8)
  knn5 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 9)
  knn6 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 10)
  knn7 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 11)
  knn8 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 12)
  knn9 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 13)
  knn10 <- knn(trainingData[,1:4], validationData[,1:4], cl, k = 14)
  #### Calculate out of sample MSE for this validationData
  valid1 = mean((validationData$rocky5 - c(knn1))^2)^.5
  valid2 = mean((validationData$rocky5 - c(knn2))^2)^.5
  valid3 = mean((validationData$rocky5 - c(knn3))^2)^.5
  valid4 = mean((validationData$rocky5 - c(knn4))^2)^.5
  valid5 = mean((validationData$rocky5 - c(knn5))^2)^.5
  valid6 = mean((validationData$rocky5 - c(knn6))^2)^.5
  valid7 = mean((validationData$rocky5 - c(knn7))^2)^.5
  valid8 = mean((validationData$rocky5 - c(knn8))^2)^.5
  valid9 = mean((validationData$rocky5 - c(knn9))^2)^.5
  valid10 = mean((validationData$rocky5 - c(knn10))^2)^.5
  #### Store knn-models performance
  knnPerformance[fold,] = c(valid1, valid2, valid3, valid4, valid5, valid6, valid7, valid8, valid9, valid10)
}
#### Check Average Model Performance
knn_mean_perform <- colMeans(knnPerformance)
#### The fourth one is best
which.min(knn_mean_perform)
min(knn_mean_perform) #1.091513

## c. Extract MSE results for best models
LMbestMSE <- lmPerformance[,which.min(lm_mean_perform)]
MARSbestMSE <- MARSPerformance[,which.min(MARS_mean_perform)]
NnetbestMSE <- NnetPerformance[,which.min(Nnet_mean_perform)]
knnbestMSE <- knnPerformance[,which.min(knn_mean_perform)]
###Combine all best MSE results
bestMSE <- cbind(LMbestMSE, MARSbestMSE, NnetbestMSE, knnbestMSE)
colMeans(bestMSE)

## d. Re-estimate best models using the entire dataset
TEST <- read.csv("Homework 2 - Test Set.csv")
LMbest <- lm(rocky5~ rocky2+rocky3+rocky1:rocky2+rocky1:rocky4+rocky2:rocky3+rocky1:rocky2:rocky4+rocky1:rocky3:rocky4+rocky1:rocky2:rocky3:rocky4,
             data = completeDB)
MARSbest <- earth(rocky5 ~ rocky1 + rocky2 + rocky3 + rocky4, data = completeDB, degree = 3)
Nnetbest <- nnet(rocky5 ~ rocky2 + rocky3 + rocky4, data = completeDB, linout = 1, size = 5, maxit = 10000, skip = TRUE)
knnbest <- knn(completeDB[,1:4], TEST, completeDB[,5], k = 8)

## e. Generate predictions using TEST set
### Initial a matrix to store predictions
bestPredictions <- matrix(NA,nrow(TEST),4)
bestPredictions[,1] <- predict(LMbest, TEST)
bestPredictions[,2] <- predict(MARSbest, TEST)
bestPredictions[,3] <- predict(Nnetbest, TEST)
bestPredictions[,4] <- knnbest
### Transfer matrix to data frame 
bestPredictions <- data.frame(bestPredictions)
### Define names of columns 
colnames(bestPredictions) <- c("Linear Regression", "MARS", "Neural Networks", "K-nearest Neighbour")
### Save the data frame as a .csv
write.csv(bestPredictions, file = "bestPredictionsTEAM.csv")

# Save my current workspace as a .Rdata
save.image(file = "hw2.RData")

##################################################################################################################################

# Appendix--Model selections

## Linear Regression Model Selection
### Define all independent variables and their interaction
allX <- colnames(fourthInteractions)
### Initial an empty list to store all possible combination of independent variables
Xcombn <- list()
### Paste combination with "+"
for (i in 1:length(allX)){
  Xcombn <- append(Xcombn,combn(allX,i,function(x) paste(x,collapse="+")))
}
### Define all formulas into a matrix
Allform <- as.matrix(paste("rocky5~",Xcombn))

### Initial a matrix to store MSE
LMMSE <- matrix(NA,nFold,length(Allform))
for(fold in 1:nFold){
  ## Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  ## Estimated all models and calculate MSE
  for (i in 1:length(Allform)){
    LM <- lm(as.formula(Allform[i]), data = trainingData)
    LMMSE[fold,i] <- mean((validationData$rocky5 - predict(LM, validationData))^2)^.5
  }
}
### Check Average Model Performance
AllLMperform <- colMeans(LMMSE)
### Selected the top 10 models
SelectedLM <- Allform[order(AllLMperform,decreasing = FALSE)[1:10]]
SelectedLM

### Define all formulas into a matrix
Alllogform <- as.matrix(paste("log(rocky5)~",Xcombn))

### Initial a matrix to store MSE
logLMMSE <- matrix(NA,nFold,length(Alllogform))
for(fold in 1:nFold){
  ## Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  ## Estimated all models and calculate MSE
  for (i in 1:length(Allform)){
    logLM <- lm(as.formula(Alllogform[i]), data = trainingData)
    logLMMSE[fold,i] <- mean((validationData$rocky5 - exp(predict(logLM, validationData)))^2)^.5
  }
}
### Check Average Model Performance
AlllogLMperform <- colMeans(logLMMSE)
### Selected the top 10 models
SelectedlogLM <- Alllogform[order(AlllogLMperform,decreasing = FALSE)[1:10]]


## MARS Model Selection
### Initial an empty list to store all possible combination of independent variables
firstcombn <- list()
### Paste combination with "+"
for (i in 1:4){
  firstcombn <- append(firstcombn,combn(c("rocky1", "rocky2", "rocky3", "rocky4"),i,function(x) paste(x,collapse="+")))
}
### Define all formulas into a matrix
Forms <- as.matrix(paste("rocky5~",firstcombn))

### Initial a matrix to store MSE
MARSMSE <- matrix(NA,nFold,length(firstcombn)*3)
for(fold in 1:nFold){
  ## Get the training and validation data for this fold
  trainingData <- subset(completeDB,valNum!=fold)
  validationData <- subset(completeDB,valNum==fold)
  ## Estimated all models and calculate MSE
  for (i in 1:length(firstcombn)){
    MARS1 <- earth(as.formula(Forms[i]), data = trainingData)
    MARS2 <- earth(as.formula(Forms[i]), data = trainingData, degree = 2)
    MARS3 <- earth(as.formula(Forms[i]), data = trainingData, degree = 3)
    MARSMSE[fold,(3*i-2)] <- mean((validationData$rocky5 - predict(MARS1, validationData))^2)^.5
    MARSMSE[fold,(3*i-1)] <- mean((validationData$rocky5 - predict(MARS2, validationData))^2)^.5
    MARSMSE[fold,(3*i)] <- mean((validationData$rocky5 - predict(MARS3, validationData))^2)^.5
  }
}
## Check Average Model Performance
AllMARSperform <- colMeans(MARSMSE)
## Selected the top 10 models
modelindex <- cbind(order(AllMARSperform,decreasing = FALSE)[1:10]%/%3,order(AllMARSperform,decreasing = FALSE)[1:10]%%3)
SelectedMARS <- matrix(NA,10,2)
colnames(SelectedMARS) <- c("Formula","degree")
SelectedMARS[,2] <- modelindex[,2]+1
for (i in 1:10){
  if (modelindex[i,2] == 0){
    SelectedMARS[i,1] <- Forms[modelindex[i,1]]
  }
  else{
    SelectedMARS[i,1] <- Forms[modelindex[i,1]+1]
  }
}
SelectedMARS