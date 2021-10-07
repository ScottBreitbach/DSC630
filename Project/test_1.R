# install.packages("forecast")
library(forecast)
library(nnet)
# install.packages("randomForest")
library(randomForest)
library(rpart)
# install.packages("earth")
library(earth)
# install.packages("performance")
library(performance)
# install.packages("see")
library(see)
# install.packages("rpart.plot")
library(rpart.plot)

rm(list = ls())
# getwd()
setwd("C:/Users/micha/OneDrive/Documents/GitHub/DSC630")
df <- read.csv("Project/dfTrain1.csv")
dfa <- read.csv("Project/dfAnalysis.csv")
dfc <- read.csv("Project/dfTrainC.csv")

# Neural Network
nn <- nnet.formula(formula = SalePrice ~ OverallQual + YearBuilt + 
                     YearRemodAdd + TotalBsmtSF + GrLivArea + GarageCars + 
                     GarageArea + BsmtQual_Ex + KitchenQual_Ex, 
                   data = df, size = 10, linout = TRUE, rang = c(0.7), 
                   decay = 0.1, MaxNWts = 1000, maxit = 100)

# Decision Tree
tree <- rpart(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + 
                TotalBsmtSF + GrLivArea + GarageCars + GarageArea + 
                BsmtQual_Ex + KitchenQual_Ex, data = df, minsplit = 20, 
              minbucket = 7,     xval = 10, maxdepth = 4, cp = 1e-05, 
              usesurrogate = 0, surrogatestyle = 0)
# print(tree)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE, type=3, 
           clip.right.labs=FALSE, fallen.leaves=FALSE)

# Decision Tree
tree2 <- rpart(formula = SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
                 LotArea + Street + Alley + LotShape + LandContour + Utilities + 
                 LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                 BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + 
                 YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + Exterior2nd + 
                 MasVnrType + MasVnrArea + ExterQual + ExterCond + Foundation + 
                 BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + 
                 BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF + Heating + 
                 HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + 
                 LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + 
                 FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                 TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
                 GarageType + GarageYrBlt + GarageFinish + GarageCars + 
                 GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + 
                 OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + 
                 PoolArea + PoolQC + Fence + MiscFeature + MiscVal + MoSold + 
                 YrSold + SaleType + SaleCondition,
               data = dfc, minsplit = 20, minbucket = 7, xval = 10, 
               maxdepth = 5, cp = 1e-05, usesurrogate = 0, surrogatestyle = 0)
print(tree2)
# Visualize the decision tree with rpart.plot
rpart.plot(tree2, box.palette="RdBu", shadow.col="gray", nn=TRUE, type=3, 
           clip.right.labs=FALSE, fallen.leaves=FALSE, tweak=1.2)
