# https://www.datacamp.com/community/tutorials/ensemble-r-machine-learning

if (!require(SuperLearner)) install.packages('SuperLearner')
library(SuperLearner)

# Get the `MASS` library
library(MASS)

# Train and test sets
train <- Pima.tr
test <- Pima.te

# Print out the first lines of `train`
head(train)

# Get a summary of `train`
summary(train)

# help(Pima.tr)

# Split out the predictors (X) and responses (y)
# Encode the factor for variable type to 0-1 encoding (subtract 1 to get 0/1):
y <- as.numeric(train[,8])-1
ytest <- as.numeric(test[,8])-1
# split out X
x <- data.frame(train[,1:7])
xtest <- data.frame(test[,1:7])

# Preview what models are available in the package
listWrappers()

if (!require(ranger)) install.packages('ranger')
library(ranger)

set.seed(42)

single.model <- SuperLearner(y,
                             x,
                             family=binomial(),
                             SL.library=list("SL.ranger"))

single.model # NOTE: risk factor here is less than 0.20, a good start


## Training an Ensemble with R: Kernel Support Vector Machines, Bayes GLM and Bagging

if (!require(arm)) install.packages('arm')
library(arm)
if (!require(kernlab)) install.packages('kernlab')
library(kernlab)
# if (!require(bayesglm)) install.packages('bayesglm')
# library(bayesglm)

# Fit the ensemble model
model <- SuperLearner(y,
                      x,
                      family=binomial(),
                      SL.library=list("SL.ranger",
                                      "SL.ksvm",
                                      "SL.ipredbagg",
                                      "SL.bayesglm"))

# Return the model
model

# Get V-fold cross-validated risk estimate
cv.model <- CV.SuperLearner(y,
                            x,
                            V=5,
                            SL.library=list("SL.ranger",
                                            "SL.ksvm",
                                            "SL.ipredbagg",
                                            "SL.bayesglm"))

# Print out the summary statistics
summary(cv.model)

plot(cv.model)



## Make Predictions with SuperLearner
predictions <- predict.SuperLearner(model, newdata=xtest)

# Return ensemble predictions
head(predictions$pred)
# Return individual library predictions
head(predictions$library.predict)

# Re-code probabilities (1/0):
# Load the package
library(dplyr)
# Recode probabilities
conv.preds <- ifelse(predictions$pred>=0.5,1,0)

# Build a confusion matrix to review results:
# Load in `caret`
library(caret)
# Create the confusion matrix
n <- as.matrix(ytest)
cm <- confusionMatrix(as.factor(conv.preds), as.factor(n))
# Return the confusion matrix
cm



## Tuning Hyperparameters

# Create learner functions
SL.ranger.tune <- function(...){
  SL.ranger(..., num.trees=1000, mtry=2)
}

SL.ipredbagg.tune <- function(...){
  SL.ipredbagg(..., nbagg=250)
}

# Pass learner functions along to cv formula
# Tune the model
cv.model.tune <- CV.SuperLearner(y,
                                 x,
                                 V=5,
                                 SL.library=list("SL.ranger",
                                                 "SL.ksvm",
                                                 "SL.ipredbagg","SL.bayesglm", 
                                                 "SL.ranger.tune",
                                                 "SL.ipredbagg.tune"))

# Get summary statistics
summary(cv.model.tune)

# Plot the tuned model
plot(cv.model.tune)

# Fit the new model with tuned parameters to see how they weigh
# Create the tuned model
model.tune <- SuperLearner(y,
                           x,
                           SL.library=list("SL.ranger",
                                           "SL.ksvm",
                                           "SL.ipredbagg",
                                           "SL.bayesglm",
                                           "SL.ranger.tune",
                                           "SL.ipredbagg.tune"))

# Return the tuned model
model.tune

# Predict on the test set
# Gather predictions for the tuned model
predictions.tune <- predict.SuperLearner(model.tune, newdata=xtest)

# Recode predictions
conv.preds.tune <- ifelse(predictions.tune$pred>=0.5,1,0)

# Create the confusion matrix
n <- as.matrix(ytest)
cm <- confusionMatrix(as.factor(conv.preds.tune), as.factor(n))
# Return the confusion matrix
cm



## a second method for tuning hyperparameters: create.Learner() to customize an existing SuperLearner
learner <- create.Learner("SL.ranger", params=list(num.trees=1000, mtry=2))
learner2 <- create.Learner("SL.ipredbagg", params=list(nbagg=250))

learner

# Create a second tuned model
cv.model.tune2 <- CV.SuperLearner(y,
                                  x,
                                  V=5,
                                  SL.library=list("SL.ranger",
                                                  "SL.ksvm",
                                                  "SL.ipredbagg",
                                                  "SL.bayesglm", 
                                                  learner$names,
                                                  learner2$names))

# Get summary statistics
summary(cv.model.tune2)

# Plot `cv.model.tune2`
plot(cv.model.tune2) # end result is the same as the first method

# Fit the new model with tuned parameters to see how they weigh
# Create the tuned model
model.tune2 <- SuperLearner(y,
                           x,
                           SL.library=list("SL.ranger",
                                           "SL.ksvm",
                                           "SL.ipredbagg",
                                           "SL.bayesglm",
                                           learner$names,
                                           learner2$names))

# Return the tuned model
model.tune2
model.tune
