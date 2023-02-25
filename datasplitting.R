#' Splitting based on outcome
# createDataPartition function. If y is a factor, random sampling occurs within each class and should preserve class distribution.
library(caret)
set.seed(3456)
train_index <- createDataPartition(iris$Species,
                                   p=.8,
                                   list = FALSE,
                                   times = 1)

head(train_index)

iris_train <- iris[,train_index]
iris_test <- iris[, -train_index]

# createResample can be used to make simple bootstrap
# createFolds can be used to generate balanced cross–validation groupings from a set of data


#' Splitting based on predictors
# maxDissim to create subsamples using maximum dissimilarity method.
# given 2 datasets A and B (B>>A), select m samples from B which are diverse (dissimilar) compared to A.
library(mlbench)
data("BostonHousing")

testing <- scale(BostonHousing[,c("age","nox")])
set.seed(5)

# random subset of 5 data points
startSet <- sample(1:dim(testing)[1],5)
samplePool <- testing[-startSet,]
start <- testing[startSet,]

newSamples <- maxDissim(start, samplePool,n=20)
head(newSamples)



#' Data splitting for timeseries
# createTimeSlices to create indices to move training and test sets in time.
# parsms for this function : 1.)initialWindow: the initial number of consecutive values in each training set sample.
# 2.) horizon: The number of consecutive values in test set sample
# 3.) fixedWindow: A logical: if FALSE, the training set always start at the first sample and the training set size will vary over data splits.


#' splitting with important groups
# groupKFold. 

set.seed(3527)
subjects <- sample(1:20, size = 80, replace = TRUE)
table(subjects)

folds <- groupKFold(subjects, k = 15)
