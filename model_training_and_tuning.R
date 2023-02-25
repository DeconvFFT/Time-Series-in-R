# for each parameter set
  # for each resampling iteration
    # holdout specific samples
    # preprocess data
    # fit model on remainder
    # predict holdout samples
  # calculate avg performance across holdouts 
# define optimal param set
# fit final model to all training data


library(mlbench)
data(Sonar)
str(Sonar[,1:10])

# createDatapartition to create stratified random sample of data into train and test sets
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p=.75, list = FALSE)

training <- Sonar[inTraining,]
testing <- Sonar[-inTraining,]

#' basic parameter tuning
# createDataPartiton uses simple bootstrapping methods

# using trainControl function
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           allowParallel = TRUE) # 10 fold cv repeated 10 times

set.seed(825)
gbmFit2 <- caret::train(Class ~ .,
                 data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit2
## gmb params toi tune : 1.) number of iterations (trees), 2.) complexity of tree (interaction.depth)
## 3.) learning rate : shrinkage, 4.) min tranining samples in a node to commence splitting (n.minobsinnode)

#' customizing tuning process
##' Preprocessing options
## train has paramter preprocess to specify preprocessing to be used. 
## additional options to preProcess can be passed via trainControl function.
## preprocessing steps will be applied to predict.train, extractPrediction, extractProbs.
## preprocessing steps not appliod if predictions use object$finalModel object.
## to handle imputations (handling missing data) either use kmeans, bagged trees(more accurate) or mean of predictor.
## if there are missing values in training set, PCA and ICA only considers complete samples.

##' Alternate tuning grids.
## tuneGrid param in train 
## search = "random" for random search on training data. Here tuneLength specifies no of combinations.
gbmGrid <- expand.grid(interaction.depth= c(1,5,9),
                       n.trees = (1:30)*50,
                       shrinkage = 0.1,
                       n.minobsinnode = 20)
gbmGrid

set.seed(825)
gmbFit2 <- caret::train(Class ~. ,
                        data = training,
                        method = "gbm",
                        trControl = fitControl,
                        verbose = FALSE,
                        tuneGrid = gbmGrid)

gmbFit2


##' Plotting resample profile
## plot estimates of performance vs tuning parameters
trellis.par.set(caretTheme())
plot(gbmFit2)

## plot for kappa
trellis.par.set(caretTheme())
plot(gbmFit2, metric="Kappa")


## max tree depth vs number of boosting iterations; colored by kappa
trellis.par.set(caretTheme())
plot(gbmFit2,
     metric= "Kappa",
     plotType = "level",
     scales = list(x= list(rot = 90)))

ggplot(gbmFit2)  


##' trainControl function
## method = resampling methods. (boot,cv,loocv,lgocv, repeatedcv, timeslice, none, oob(out of bag estimates
## can only be used by random forests, bagged trees, bagged earth, bagged flexible discriminant analysis,
## or conditional tree forest methods))
## Can't use gbm with oob method.
## number : number of folds in K fold cross validation
## repeats : number of resampling iterations.
## verboseIter : T/F for training logs
## returnData: T/F for saving data in a variable
## p: training percentage for leave out group validation
## for methods = "timeslice", params like initialWindow, horizon and fixedWindow are available.
## classProbs : bool to decide if class probabilities should be compared for held out samples during resample.
## index and indexOut : optional lists to specify indices to keep and hold out while training . If not provided, train will generate them.
## summaryFunction: a function for computed performance measures.
## selectionFunction: function to choose optimal tuning params and examples.
## PCAthresh, ICAcomp, K: options to pass the preprocess function when used.
## returnResamp: specifies how much of resampled performance measures to save. (all, final, none)
## allowParallel : if train should use parallel processing


##' Alternate performance metrics
## Default for regression: RMSE, R**2, MAE. Default for classification: Accuracy, Kappa
## Models with low % of samples in one class can be improved by using Kappa.
## for custom metric: user can use summaryFunction param in trainControl function. fun(data,lev,model)
## passing twoClassSummary as custom metric. It will compute ROC, specificity and sensitivity of a curve.

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- caret::train(Class ~ .,
                 data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 metric = "ROC")

gbmFit3

#' Choosing final model
# selectionFunction param in train can be used to select final model.
# 3 params here : best : best model, oneSE: model within one standard error of best, tolerance: least complex model with some tolerance % of best value.
# when selecting the best models, models are ordered by iteration and then by depth. 
# The package assumes that more iterations add complexity faster than adding depth.

#' Extracting predictions and class probabilities
# can specify type of prediction to be made using predict.
## options are : prob,posterior, response, probability, raw

predict(gbmFit3, newdata = head(testing))

predict(gbmFit3, newdata = head(testing), type = "prob")


#' Exploring and comparing resampling distributions
##' within model
## xyplot, stripplot to plot resampling statistics against tuning params(numeric).
## histogram and densityplot to look at distributions of tuning parameters.
trellis.par.set(caretTheme())
densityplot(gbmFit3, pch = "|")

# use resamples = all as control object to plot resampling results across multiple tuning parameters
set.seed(825)
svmFit <- caret::train(Class ~ .,
                data = training,
                method = "svmRadial",
                trControl = fitControl,
                preProc = c("center","scale"),
                tuneLength = 8,
                metric = "ROC")

svmFit

rdaFit <- caret::train(Class ~ .,
                    data = training,
                    method = "rda",
                    trControl = fitControl,
                    tuneLength = 4,
                    metric = "ROC")

rdaFit

# measuring performance difference between models
resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps
summary(resamps)


#visualizing resamples
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2,.2,.2,.4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1,0,0,.7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

trellis.par.set(theme1)
xyplot(resamps, what = "BlandAltman") # difference vs mean plot

splom(resamps) #scattered plot matrix. (Determine correlation btwn series of variables and arrang them in matrix)

diffvalues <- diff(resamps)
diffvalues

summary(diffvalues)

trellis.par.set(theme1)
bwplot(diffvalues, layout = c(3,1))

trellis.par.set(caretTheme())
dotplot(diffvalues)

#' fitting models without parameter tuning
fitControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(825)
gbmFit4 <- caret::train(Class ~ .,
                        data = training,
                        trControl = fitControl,
                        verbose = FALSE,
                        tunGrid = data.frame(interaction.depth = 4,
                                             n.trees = 100,
                                             shrinkage = .1,
                                             n.minobsinnode = 20),
                        metric = "ROC")
gbmFit4

predict(gbmFit4, newdata = head(testing))
predict(gbmFit4, newdata = head(testing), type = "prob")

