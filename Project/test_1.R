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
# install.packages('patchwork')
library(patchwork)

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


# Linear Regression (using variables that are high correlation)
reg <- lm(formula = SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + 
            TotalBsmtSF + GrLivArea + GarageCars + BsmtQual_Ex + KitchenQual_Ex, 
          data = df)

# Model Performance
print(reg)
r2(reg)
model_performance(reg)

#Visualization of model checks
check_model(reg)


# Linear Regression (second model, using only variables that are high confidence ***)
reg2 <- lm(formula = SalePrice ~ LotArea + OverallQual + OverallCond + YearBuilt + 
             MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + 
             X2ndFlrSF + LandSlope_Mod + Neighborhood_StoneBr + Condition1_PosN + 
             RoofMatl_ClyTile + ExterQual_Ex + BsmtQual_Ex + KitchenQual_Ex + 
             GarageQual_Ex + GarageCond_Ex, 
           data = dfa)
print(reg2)
r2(reg2)
model_performance(reg2)
check_model(reg2)


# Linear Regression (using all variables)
regAll <- lm(formula = SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
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
             data = dfc)

# Model Performance
print(regAll)
r2(regAll)
model_performance(regAll)

#Visualization of model checks
check_model(regAll)


# Linear Regression (Using variables that were effective in the decision tree)
regTree <- lm(formula = SalePrice ~ OverallQual + Neighborhood + X1stFlrSF + 
                GrLivArea + BsmtFinSF1 + TotalBsmtSF + Exterior2nd + GarageArea + 
                KitchenQual + SaleType, 
           data = dfc)
print(regTree)
r2(regTree)
model_performance(regTree)
check_model(regTree)


# Compare performance of both models
compare_performance(reg, reg2, regAll, regTree)
test_performance(reg, reg2, regAll, regTree)
check_model(reg)


# Spline Model
spline <- earth(formula = SalePrice ~ BsmtQual_Ex + GarageArea + GarageCars + 
                  GrLivArea + KitchenQual_Ex + OverallQual + TotalBsmtSF + 
                  YearBuilt + YearRemodAdd, 
                data = df, glm = list(family = gaussian), minspan = 0)

print(spline)


# Spline Model (all variables)
spline <- earth(formula = SalePrice ~ MSSubClass + MSZoning + LotFrontage + 
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
                data = dfc, glm = list(family = gaussian), minspan = 0)

print(spline)



summary(regAll)
# https://towardsdatascience.com/selecting-the-best-predictors-for-linear-regression-in-r-f385bf3d93e9
lm1 <- lm(dfc,formula=SalePrice ~.)
summary(lm1)

# install.packages('leaps')
library(leaps)

# Best_Subset <- regsubsets(SalePrice~.,
#                           data = dfc,
#                           nbest = 1,    # 1 best model for each number of predictors
#                           nvmax = NULL, # NULL for no limit on number of variables
#                           force.out = NULL,
#                           method = "exhaustive",
#                           really.big = TRUE) # because of large data set
# 
# summary_best_subset <- summary(regsubsets.out)
# as.data.frame(summary_best_subset$outmat)



# Number of Predictors: 
# See waht the package recommends for # predictors to use for our data set
numPred <- which.max(summary_best_subset$adjr2)

# What are the best predictors?
summary_best_subset$which[numPred,]

# Run the regression model with the best predictors
best.model <- lm(SalePrice ~ ,
                 data = dfc)
summary(best.model)



