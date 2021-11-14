# Load Libraries


# Set seed
set.seed(42)

# Clear environment
rm(list = ls())

# Set working directory
setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC630/Project")

## LOAD DATAFRAMES
dfAn <- read.csv("dfAnalysis.csv")     # 1 1460 x 257 (with all dummies)
dfCorrSP <- read.csv("dfTrain1.csv")   # 2 1460 x  16 (correlate with SalePrice)
dfOrd <- read.csv("dfTrainC.csv")      # 3 1460 x  81 (ordinal variables)
dfFstat <- read.csv("featFstatistic.csv") # 4 1460 x 31 (top F stat features)
dfLBGM <- read.csv("featLightBGM.csv")    # 5 1460 x 31 (top LightBGM features)
dfLogReg <- read.csv("featLogisticRegression.csv") # 6 1460 x 31 (top logistic regression features)
dfMInf <- read.csv("featMutualInformation.csv")    # 7 1460 x 31 (top mutual information features)
dfOverall <- read.csv("featOverall.csv")  # 8 1460 x 31 (top overall features)
# dim(dfOverall)  # get dimensions of dataframe

# # install.packages("Hmisc")
# library(Hmisc)
# describe(dfAn)  # get descriptions of each variable

## LINEAR REGRESSION MODELING
lmAn = lm(formula = SalePrice~., data = dfAn)
summary(lmAn)      # 1 Adjusted R-squared: 0.9061

lmCorrSP = lm(formula = SalePrice~., data = dfCorrSP)
summary(lmCorrSP)  # 2 Adjusted R-squared: 0.7892

lmOrd = lm(formula = SalePrice~., data = dfOrd)
summary(lmOrd)     # 3 Adjusted R-squared: 0.9061

lmFstat = lm(formula = SalePrice~., data = dfFstat)
summary(lmFstat)   # 4 Adjusted R-squared: 0.8202

lmLBGM = lm(formula = SalePrice~., data = dfLBGM)
summary(lmLBGM)    # 5 Adjusted R-squared: 0.8137

lmLogReg = lm(formula = SalePrice~., data = dfLogReg)
summary(lmLogReg)  # 6 Adjusted R-squared: 0.7815

lmMInf = lm(formula = SalePrice~., data = dfMInf)
summary(lmMInf)    # 7 Adjusted R-squared: 0.3798

lmOverall = lm(formula = SalePrice~., data = dfOverall)
summary(lmOverall) # 8 Adjusted R-squared: 0.8257

## Run Model Comparisons
# https://www.scribbr.com/statistics/akaike-information-criterion/
# install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(lmAn, lmCorrSP, lmOrd, lmFstat,
               lmLBGM, lmLogReg, lmMInf, lmOverall)
model.names <- c('lmAn', 'lmCorrSP', 'lmOrd', 'lmFstat',
                 'lmLBGM', 'lmLogReg', 'lmMInf', 'lmOverall')
aictab(cand.set = models, modnames = model.names)

library(performance)  # https://cran.r-project.org/web/packages/performance/performance.pdf
compare_performance(lmAn, lmCorrSP, lmOrd, lmFstat, 
                    lmLBGM, lmLogReg, lmMInf, lmOverall) # I like this one

# Show Low/Med/High correlation between variables (Collinearity)
check_collinearity(lmOverall)  
if (require("see")) {
  x <- check_collinearity(lmOverall)
  plot(x)
}

# Visualization of multiple model checks
check_model(lmOverall)

# Measure Accuracy of Predictions from Model Fit
# performance_accuracy(lmAn, 
#                      method = c("cv", "boot"), 
#                      k = 5, 
#                      n = 1000, 
#                      verbose = TRUE)

performance_accuracy(lmCorrSP, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 87.91% Accuracy, SE: 5.03%

# performance_accuracy(lmOrd, 
#                      method = c("cv", "boot"), 
#                      k = 5, 
#                      n = 1000, 
#                      verbose = TRUE)

performance_accuracy(lmFstat, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 89.40% Accuracy, SE: 4.35%

performance_accuracy(lmLBGM, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 89.48% Accuracy, SE: 3.59%

performance_accuracy(lmLogReg, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 87.33% Accuracy, SE: 6.36%

performance_accuracy(lmMInf, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 59.44% Accuracy, SE: 7.26%

performance_accuracy(lmOverall, 
                     method = c("cv", "boot"), 
                     k = 5, 
                     n = 1000, 
                     verbose = TRUE)
# 90.32% Accuracy, SE: 3.56%

# # install.packages("mclust")
# library(mclust)
# check_multimodal(dfOverall)


# Detect outliers in dataframe
check_outliers(dfOverall, method = c("mahalanobis")) # wow, 247 outliers
check_outliers(dfCorrSP, method = c("mahalanobis")) # 130 outliers
check_outliers(dfCorrSP, method = c("mcd")) # 571 outliers
# install.packages('ICS')
library(ICS)
# install.packages('ICSOutlier')
library(ICSOutlier)
check_outliers(dfCorrSP, method = c("ics")) # 126 outliers
# install.packages('dbscan')
library(dbscan)
check_outliers(dfCorrSP, method = c("optics")) # 0 outliers
check_outliers(dfCorrSP, method = c("lof")) # 67 outliers
check_outliers(dfCorrSP, method = c("mahalanobis", "mcd", "ics", "lof")) # 110 outliers
check_outliers(dfCorrSP, method = "all") # 151 outliers


# # install.packages("lme4")
# library(lme4)
# lmerAn = lmer(formula = SalePrice~., data = dfAn) # Error: No random effects terms specified in formula
# summary(lmerAn)

# install.packages("BayesFactor")
library(BayesFactor)
rbfCorrSP = regressionBF(formula = SalePrice~., data = dfCorrSP) # took 8 minutes
# summary(rbfCorrSP)   
# see also lmBF(), generalTestBF()
head(rbfCorrSP)
# [1] OverallQual + YearBuilt + ExterQual + BsmtQual + X1stFlrSF + GrLivArea + KitchenQual + FireplaceQu + GarageCars               : 1.962229e+478 ±0%

# Hosmer-Lemeshow goodness-of-fit test
glmOverall = glm(formula = SalePrice~., data = dfOverall)
summary(glmOverall) 
performance_hosmer(glmOverall) # lm() won't work, needs glm()
# Model seems to fit well.


# DECISION TREE
library(rpart)
library(rpart.plot)

treeAn <- rpart(formula = SalePrice~., data = dfAn, minsplit = 20,
              minbucket = 7, xval = 10, maxdepth = 4, cp = 1e-05, 
              usesurrogate = 0, surrogatestyle = 0)

# print(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(treeAn, box.palette="RdBu", shadow.col="gray", nn=TRUE, type=3, 
           clip.right.labs=FALSE, fallen.leaves=FALSE)

tree <- rpart(formula = SalePrice~., data = dfOverall, minsplit = 20, 
              minbucket = 7, xval = 10, maxdepth = 20, cp = 1e-05, 
              usesurrogate = 0, surrogatestyle = 0)
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)

tree2 <- rpart(formula = SalePrice~.,
               data = dfCorrSP, minsplit = 20, minbucket = 7, xval = 10, 
               maxdepth = 5, cp = 1e-05, usesurrogate = 0, surrogatestyle = 0)
print(tree2)
# Visualize the decision tree with rpart.plot
rpart.plot(tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE, type=3, 
           clip.right.labs=FALSE, fallen.leaves=FALSE, tweak=1.2)


# https://www.datacamp.com/community/tutorials/decision-trees-R
# install.packages("ISLR")
library(ISLR)
if (!require(tree)) install.packages('tree') # installs package if not already installed
library(tree)

names(dfOverall)
# Look at distribution of target variable
hist(dfOverall$SalePrice)
hist(sqrt(dfOverall$SalePrice))
hist(log1p(dfOverall$SalePrice))
# n <- log1p(99)
# expm1(n)

summary(tree)
plot(tree)
text(tree, pretty = 0)
tree

if (!require(randomForest)) install.packages('randomForest') # installs package if not already installed
library(randomForest)

# nrow(dfOverall)/4
# ncol(dfOverall)
train.overall = sample(1:nrow(dfOverall), 1060)

rf.overall = randomForest(SalePrice~., data = dfOverall, subset = train)
rf.overall

oob.err = double(30)
test.err = double(30)

for (mtry in 1:30){
  fit = randomForest(SalePrice~., data = dfOverall, subset = train.overall, 
                     mtry = mtry, ntree = 350)
  oob.err[mtry] = fit$mse[10]
  pred = predict(fit, dfOverall[-train.overall,])
  test.err[mtry] = with(dfOverall[-train.overall,], mean( (SalePrice-pred)^2 ))
} # NOTE: takes about a minute

matplot(1:mtry, cbind(oob.err, test.err), pch = 23, col = c("red", "blue"), 
        type = "b", ylab = "Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))


## BOOSTING
if (!require(gbm)) install.packages('gbm')
library(gbm) # Gradient Boosting Model


boost.overall = gbm(SalePrice~., data = dfOverall[train.overall,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.overall)

plot(boost.overall,i="OverallQual")
plot(boost.overall,i="GrLivArea")
plot(boost.overall,i="X1stFlrSF")
plot(boost.overall,i="GarageArea")
plot(boost.overall,i="FireplaceQu")
plot(boost.overall,i="LotShape")
plot(boost.overall,i="Neighborhood_Timber")

n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.overall, newdata = dfOverall[-train.overall,], n.trees = n.trees)
dim(predmat)

boost.err = with(dfOverall[-train.overall,], apply( (predmat - SalePrice)^2, 2, mean) )
plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
abline(h = min(test.err), col = "red")

min(test.err)
min(boost.err)



## NEURAL NETWORK

library(nnet)

nnOverall <- nnet.formula(formula = SalePrice~., data = dfOverall, size = 10, 
                          linout = TRUE, rang = c(0.7), decay = 0.1, 
                          MaxNWts = 1000, maxit = 100)

# load library
require(neuralnet)

# fit neural network
nnOverall <- neuralnet(SalePrice~., data = dfOverall, hidden=3, act.fct = "tanh",# "logistic",
                       linear.output = TRUE, stepmax = 1e6) # does not converge
plot(nnOverall)

# create test set
test <- dfOverall[2:31]

Predict <- neuralnet::compute(nnOverall, test) # force use of compute() function from the correct package
Predict$net.result  # okay, it turns out compute is deprecated anyway. Use predict()

'NOTE: All of the results are the same. It turns out this is common when the'
'data being fed into the neural network are not normalized first. Moving on.'

# Predict2 <- predict(nnOverall, test)
# table(dfOverall$SalePrice, Predict2[,1])

# # fit neural network
# nnOverall <- neuralnet(SalePrice~., data = dfOverall, hidden=3, 
#                        linear.output = TRUE, stepmax = 1e6) # does not converge
# plot(nnOverall)
# 
# predict.Overall <- predict(nnOverall, dfOverall)
# predict.Overall

# # Split data
# train_idx <- sample(nrow(dfOverall), 2/3 * nrow(dfOverall))
# df_train <- dfOverall[train_idx,]
# df_test <- dfOverall[-train_idx,]
 
# nn <- neuralnet(SalePrice~., data = df_train, linear.output = TRUE, stepmax = 1e6)
# plot(nn)
# pred <- predict(nn, df_test)
# pred


## ENSEMBLE LEARNING
# Load package
library("SuperLearner")

# Split data
train_idx <- sample(nrow(dfOverall), 2/3 * nrow(dfOverall))
df_train <- dfOverall[train_idx,]
df_test <- dfOverall[-train_idx,]

# target variable
y <- df_train[,1]
ytest <- df_test[,1]

# predictor variables
x <- df_train[,2:21]
xtest <- df_test[,2:21]

# Set randomization seed
set.seed(42)


# Fit the ensemble model
if (!require(xgboost)) install.packages('xgboost')
library(xgboost)
library(polspline)
library(extraTrees)
library(biglasso)
library(party)
library(glmnet)
library(speedglm)
library(KernelKnn)
model <- SuperLearner(y,
                      x,
                      family=gaussian(),
                      SL.library=list("SL.bartMachine",
                                      "SL.gbm",
                                      # "SL.nnls",
                                      "SL.extraTrees",
                                      "SL.rpartPrune",
                                      "SL.stepAIC"))
model2 <- SuperLearner(y,
                      x,
                      family=gaussian(),
                      SL.library=list("SL.bayesglm", "SL.nnet", "SL.speedglm", 
                                      "SL.caret.rpart", "SL.glmnet",
                                      "SL.speedlm", "SL.randomForest", 
                                      "SL.biglasso", "SL.ranger", "SL.step.forward", 
                                      "SL.svm", "SL.earth", "SL.ipredbagg",
                                      "SL.polymars", "SL.step", "SL.xgboost", 
                                      "SL.loess", "SL.rpart", "SL.cforest", "SL.ksvm", 
                                      "SL.bartMachine", "SL.gbm",  
                                      "SL.extraTrees", "SL.rpartPrune", 
                                      "SL.stepAIC"))
# Return the model
model2

model3 <- SuperLearner(y,
                       x,
                       family=gaussian(),
                       SL.library=list("SL.speedlm", "SL.svm", "SL.gbm", 
                                       "SL.extraTrees"))

# Return the model
model3
model$times
# plot(cv.model)
warnings()

# Low coefficient models removed:
list("SL.bayesglm", "SL.nnet", "SL.speedglm", "SL.caret.rpart", "SL.glmnet", 
     "SL.randomForest", "SL.biglasso", "SL.ranger", "SL.step.forward", 
     "SL.earth", "SL.ipredbagg", "SL.polymars", "SL.step", "SL.rpart", 
     "SL.cforest", "SL.ksvm", "SL.stepAIC",  # removed first pass
     "SL.xgboost", "SL.bartMachine",         # removed second pass
     "SL.rpartPrune",                        # removed third pass
     "SL.loess"                              # removed due to high added risk
     )

# Second Pass:
list("SL.ranger", "SL.step.forward", "SL.svm", "SL.earth", "SL.glm", "SL.ipredbagg",
     "SL.lm", "SL.polymars", "SL.step", "SL.xgboost", "SL.loess") #lm "fit may be misleading"

# Third Pass:
list("SL.rpart", "SL.cforest", "SL.ksvm", "SL.bartMachine")

list("SL.bartMachine", "SL.gbm", "SL.nnls", "SL.extraTrees", "SL.rpartPrune", 
     "SL.stepAIC") # nnls highest coefficient but also high error


## Cross Validation
## Get V-fold cross-validated risk estimate
cv.model <- CV.SuperLearner(y, x, V=5,
                            SL.library = list("SL.gbm", 
                                              "SL.nnls", 
                                              "SL.extraTrees", 
                                              "SL.rpartPrune", 
                                              "SL.stepAIC"))
cv.model3 <- CV.SuperLearner(y, x, V=5, family = "gaussian"
                            SL.library = list("SL.speedlm", "SL.svm", "SL.gbm", 
                                              "SL.extraTrees"))
# Print out the summary statistics
summary(cv.model)
summary(cv.model3)

# Plot models used and their variation
plot(cv.model)
plot(cv.model3)


# ## Duh, can't use confusion matrix for continuous variable
# # Build a confusion matrix to review results:
# # Load in `caret`
# library(caret)
# 
# # Create the confusion matrix
# n <- as.matrix(ytest)
# cm <- confusionMatrix(predictions$pred, as.factor(n))
# # cm <- confusionMatrix(predictions, ytest)
# dim(predictions$pred)
# 
# dim(n)
# dim(xtest)
# length(ytest)
# n <- as.matrix(ytest)
# cm <- confusionMatrix(as.factor(predictions$pred), as.factor(n))
# levels(n)
# levels(as.factor(predictions$pred))
# levels(as.factor(n))
# length(levels(as.factor(n)))
# length(levels(as.factor(predictions$pred)))
# 
# n <- as.matrix(ytest)
# cm <- confusionMatrix(predictions$pred, n)
# cm <- confusionMatrix(as.factor(predictions$pred), as.factor(n))
# 
# dim(n)
# dim(predictions$pred)
# dim(as.factor(n))
# dim(as.factor(predictions$pred))


## Make Predictions with SuperLearner
predictions <- predict.SuperLearner(model3, newdata=xtest)

# Return ensemble predictions
head(predictions$pred)

# Return individual library predictions
head(predictions$library.predict)



# Predict back on the holdout dataset.
# onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
pred = predict(model3, xtest, onlySL = TRUE)

# Check the structure of this prediction object.
str(pred)

# Review the columns of $library.predict.
summary(pred$library.predict)

# Histogram of our predicted values.
library(ggplot2)
qplot(pred$pred[, 1]) + theme_minimal()

# Scatterplot of original values (0, 1) and predicted values.
# Ideally we would use jitter or slight transparency to deal with overlap.
qplot(ytest, pred$pred[, 1]) + theme_minimal()

# # NOPE, THIS IS FOR BINARY CLASSIFICATION ONLY
# # Review AUC - Area Under Curve
# pred_rocr = ROCR::prediction(pred$pred, ytest)
# auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
# auc














