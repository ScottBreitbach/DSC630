library(forecast)
library(nnet)
library(randomForest)
library(rpart)
library(earth)
library(performance)
library(see)
library(rpart.plot)

rm(list = ls())
setwd("C:/Users/NelsonR/Desktop/Predictive modelling")
df <- read.csv("dfTrain1.csv") 
dfa <- read.csv("dfAnalysis.csv") 

# Neural Network
nn <-nnet.formula(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + GarageArea + BsmtQual_Ex + KitchenQual_Ex, data = df, size = 10, linout = TRUE, rang = c(0.7), decay = 0.1, MaxNWts = 1000, maxit = 100)

# Decision Tree
tree <- rpart(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + GarageArea + BsmtQual_Ex + KitchenQual_Ex, data = df, minsplit = 20, minbucket = 7,     xval = 10, maxdepth = 20, cp = 1e-05, usesurrogate = 0, surrogatestyle = 0)
print(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)


# Linear Regression (ushing variables that are high correlation)
reg <- lm(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + BsmtQual_Ex + KitchenQual_Ex, data = df)

# Model Performance
print(reg)
r2(reg)
model_performance(reg)

#Visualization of model checks
check_model(reg)


# Linear Regression (second model, using only variables that are high confidence ***)
reg2 <- lm(formula = SalePrice ~ LotArea + OverallQual + OverallCond + YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + LandSlope_Mod + Neighborhood_StoneBr + Condition1_PosN + RoofMatl_ClyTile + ExterQual_Ex + BsmtQual_Ex + KitchenQual_Ex + GarageQual_Ex + GarageCond_Ex, data = dfa)
print(reg2)
r2(reg2)
model_performance(reg2)
check_model(reg2)


# Compare performance of both models
compare_performance(reg, reg2)
test_performance(reg, reg2)
check_model(reg)

# Spline Model
spline <- earth(formula = SalePrice ~ BsmtQual_Ex + GarageArea + GarageCars + GrLivArea + KitchenQual_Ex + OverallQual + TotalBsmtSF + YearBuilt + YearRemodAdd, data = df, glm = list(family = gaussian), minspan = 0)