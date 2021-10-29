# Load Libraries
library(nnet)
library(randomForest)
library(rpart)
library(earth)
library(performance)
library(see)
library(rpart.plot)
library(patchwork)

library(stats)
library(ggplot2)
library(class)
# install.packages('e1071')
library(e1071)
# install.packages('FNN')
library(FNN)
# install.packages('abind')
library(abind)
# library(randomForest)
# library(rpart)
library(cluster)
# install.packages('rattle')
library(rattle)

# Clear environment
rm(list = ls())

# getwd()
# setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC630/Project")


# head(subset(df, select = -c(SalePrice)))
# names(subset(dfc, select = -c(index)))
# names(df)

## LOAD DATAFRAMES
dfAn <- read.csv("dfAnalysis.csv")  # 1
dfCorr <- read.csv("dfCorr.csv")    # 2
dfTr1 <- read.csv("dfTrain1.csv")   # 3
dfTrC <- read.csv("dfTrainC.csv")   # 4
dfFstat <- read.csv("featFstatistic.csv") # 5
dfLBGM <- read.csv("featLightBGM.csv")    # 6
dfLogReg <- read.csv("featLogisticRegression.csv") # 7
dfMInf <- read.csv("featMutualInformation.csv")    # 8
dfOverall <- read.csv("featOverall.csv")  # 9

## LINEAR REGRESSION MODEL
lmAn = lm(formula = SalePrice~., data = dfAn)
summary(lmAn) # 1 Adjusted R-squared: 0.9061

lmCorr = lm(formula = SalePrice~., data = dfCorr)
summary(lmCorr) # 2 Adjusted R-squared: 0.9963

lmTr1 = lm(formula = SalePrice~., data = dfTr1)
summary(lmTr1) # 3 Adjusted R-squared: 0.7892

lmTrC = lm(formula = SalePrice~., data = dfTrC)
summary(lmTrC) # 4 Adjusted R-squared: 0.9061

lmFstat = lm(formula = SalePrice~., data = dfFstat)
summary(lmFstat) # 5 Adjusted R-squared: 0.8202

lmLBGM = lm(formula = SalePrice~., data = dfLBGM)
summary(lmLBGM) # 6 Adjusted R-squared: 0.8137

lmLogReg = lm(formula = SalePrice~., data = dfLogReg)
summary(lmLogReg) # 7 Adjusted R-squared: 0.7815

lmMInf = lm(formula = SalePrice~., data = dfMInf)
summary(lmMInf) # 8 Adjusted R-squared: 0.3798

lmOverall = lm(formula = SalePrice~., data = dfOverall)
summary(lmOverall) # 9 Adjusted R-squared: 0.8257

# Run model comparison
# # https://www.scribbr.com/statistics/akaike-information-criterion/
# # install.packages("AICcmodavg")
# library(AICcmodavg)
# models <- list(lmAn, lmCorr, lmTr1, lmTrC, lmFstat, 
#                lmLBGM, lmLogReg, lmMInf, lmOverall)
# model.names <- c('lmAn', 'lmCorr', 'lmTr1', 'lmTrC', 'lmFstat', 
#                  'lmLBGM', 'lmLogReg', 'lmMInf', 'lmOverall')
# aictab(cand.set = models, modnames = model.names)

# model_performance(lmAn)

# Compare performance of both models
compare_performance(lmAn, lmCorr, lmTr1, lmTrC, lmFstat, 
                    lmLBGM, lmLogReg, lmMInf, lmOverall) # I like this one
# test_performance(lmAn, lmCorr, lmTr1, lmTrC, lmFstat, 
#                  lmLBGM, lmLogReg, lmMInf, lmOverall)
# check_model(lmAn)

# model_performance(lmAn)[1]
# check_model(lmAn)

## NOTE: dfAn appears to be the same results as dfTrC?
dim(dfAn)
dim(dfTrC)


## NEURAL NETWORK
nnTr1 <- nnet.formula(formula = SalePrice~., data = dfTr1, size = 10, 
                       linout = TRUE, rang = c(0.7), decay = 0.1, 
                       MaxNWts = 1000, maxit = 100)

nnFstat <- nnet.formula(formula = SalePrice~., data = dfFstat, size = 10, 
                      linout = TRUE, rang = c(0.7), decay = 0.1, 
                      MaxNWts = 1000, maxit = 100)

nnLBGM <- nnet.formula(formula = SalePrice~., data = dfLBGM, size = 10, 
                      linout = TRUE, rang = c(0.7), decay = 0.1, 
                      MaxNWts = 1000, maxit = 100)

nnLogReg <- nnet.formula(formula = SalePrice~., data = dfLogReg, size = 10, 
                      linout = TRUE, rang = c(0.7), decay = 0.1, 
                      MaxNWts = 1000, maxit = 100)

nnMInf <- nnet.formula(formula = SalePrice~., data = dfMInf, size = 10, 
                      linout = TRUE, rang = c(0.7), decay = 0.1, 
                      MaxNWts = 1000, maxit = 100)

nnOverall <- nnet.formula(formula = SalePrice~., data = dfOverall, size = 10, 
                      linout = TRUE, rang = c(0.7), decay = 0.1, 
                      MaxNWts = 1000, maxit = 100)
# Okay, I don't know how to evaluate this...


## 