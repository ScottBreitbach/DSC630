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

getwd()
setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC630/Project")
df <- read.csv("dfTrain1.csv")
dfa <- read.csv("dfAnalysis.csv")
dfc <- read.csv("dfTrainC.csv")

head(df)

lm1 = lm(formula = SalePrice~., data = df)
# lm2 = lm(formula = SalePrice~., data = subset(df, select = -c(SalePrice)))
lm3 = lm(formula = SalePrice ~ index + OverallQual + YearBuilt + YearRemodAdd + 
           ExterQual + BsmtQual + TotalBsmtSF + X1stFlrSF + GrLivArea + FullBath +
           KitchenQual + TotRmsAbvGrd + FireplaceQu + GarageFinish + GarageCars + 
           GarageArea + SalePrice, data = df)


# head(df[, c('SalePrice')])
# head(df[c(1)])
# head(df)
# head(df[c('SalePrice')])
head(subset(df, select = -c(SalePrice)))

names(df)


