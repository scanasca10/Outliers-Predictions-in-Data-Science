# How to deal with missing values in Data Science

install.packages("mlbench")
library("mlbench")
library("mice")

# initialize the data
data ("BostonHousing", package="mlbench")    
# load the data
original <- BostonHousing  # backup original data

# Let's insert random NA 
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA

# Let's check pattern or missing values in data
md.pattern(BostonHousing)

# 1. Deleting Observations
mod1 <- lm(medv ~ ptratio + rad, data = BostonHousing)
plot(mod1)

mod2 <- lm(medv ~ ptratio + rad, data = BostonHousing, na.action = na.omit)
plot(mod2)

# 2. Deliting variables
# Extreme case where the numbers of missing values is greater than the number 
# of real values in the data frame

index_ptratio <- which(is.na(BostonHousing$ptratio))
index_rad <- which(is.na(BostonHousing$rad))


# 3. Imputation
BostonHousing[index_ptratio,'ptratio'] <- median(BostonHousing$ptratio,
                                                              na.rm = T)
BostonHousing[index_rad,'rad'] <- mean(BostonHousing$rad,
                                                              na.rm = T)

# Accuracy is very important
install.packages("DMwR")
library(DMwR)

real_ptratio <- original$ptratio[index_ptratio]
fit_ptratio <- BostonHousing$ptratio[index_ptratio]
real_rad <- original$ptratio[index_rad]
fit_rad <- BostonHousing$ptratio[index_rad]

regr.eval(real_ptratio, fit_ptratio)
regr.eval(real_rad, fit_rad)

# Pretty bad prediction for 'ptratio' column

# Prediction based on kNN Imputation
# knnOutput <- knnImputation(BostonHousing[index_ptratio])

# perform knn imputation.
knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])
anyNA(knnOutput)

fit_ptratio2 <- knnOutput$ptratio[index_ptratio]
fit_rad2 <- knnOutput$rad[index_rad]

# Testing accuracy
regr.eval(real_ptratio, fit_ptratio2)
regr.eval(real_rad, fit_rad2)

# 4.2 rpart
# The limitation with DMwR::knnImputation is that it sometimes maynot be appropriate to use 
# when the missing value comes from a factor variable. The advantage with rpart is that you 
# just need one of the variables to be non NA in the predictor fields.

library(rpart)
mod_rad <- rpart(rad ~. -medv, data = BostonHousing[!index_rad,] ,
                   method = "anova", na.action = na.omit) # train without medv
mod_ptratio <- rpart(ptratio ~. -medv, data = BostonHousing[!index_ptratio,] ,
                   method = "anova", na.action = na.omit) # train without medv

fit_ptratio3 <- predict(mod_rad, BostonHousing[index_rad,])
fit_rad3 <- predict(mod_ptratio, BostonHousing[index_ptratio,])

# Testing accuracy
regr.eval(real_ptratio, fit_ptratio3)
regr.eval(real_rad, fit_rad3)

