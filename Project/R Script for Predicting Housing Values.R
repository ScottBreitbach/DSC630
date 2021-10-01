library(forecast)
library(nnet)
library(randomForest)
library(rpart)

rm(list = ls())
setwd("C:/Users/NelsonR/Desktop/Predictive modelling")
df <- read.csv("dfTrain1.csv") 

# Neural Network
nn <-nnet.formula(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + GarageArea + BsmtQual_Ex + KitchenQual_Ex, data = df, size = 10, linout = TRUE, rang = c(0.7), decay = 0.1, MaxNWts = 1000, maxit = 100)

# Decision Tree
tree <- rpart(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + GarageArea + BsmtQual_Ex + KitchenQual_Ex, data = df, minsplit = 20, minbucket = 7,     xval = 10, maxdepth = 20, cp = 1e-05, usesurrogate = 0, surrogatestyle = 0)

# Linear Regression
reg <- lm(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + BsmtQual_Ex + KitchenQual_Ex, data = df)