library(caret)
# getModelInfo to get specifics of a model
lda_model <- getModelInfo(model = "lda",regex = FALSE )[[1]]
names(lda_model)
 
#' SVM with laplacian kernels
# use package kernlab to access kernels other than linear, polynomial or radial.

#' Model components for SVM
# standrard cost parameter and kernel.
# sort element in train function: sorting params from most complex to list
# varImp: calculate variable importance metrics for model if any.

##' creating a new model (SVM with laplacian kernel)

lpSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL)

##' Parameters
## df containing parameters, class and label

prm <- data.frame(parameter = c("C","sigma"),
                  class = rep("numeric",2),# both numeric params
                  label = c("Cost","Sigma"))

lpSVM$parameters <- prm


##'  grid element
## Takes x,y, len, search. 
## len : tuneLength passed through train.
## search: grid or random.

svmGrid <- function(x,y, len = NULL, search = "grid"){
  library(kernlab)
  # this produces low, middle and high values for sigma
  sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
  
  # used namespace here with sigest because, caret wont load kernlab unless specified with the function to use.
  if(search == "grid"){
    out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                       C = 2^((1:len) -3))
  }
  else{
    # for random search, define ranges of parameters then
    # generate random values for them.
    # runif generates random derivatives
    rng <- extendrange(log(sigmas), f = .75)
    out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                      C = 2^runif(len, min =-5, max = 8))
    
  }
  out
}


lpSVM$grid <- svmGrid

##' Fit element
## ... to pass options from train class to fit class
## any preprocessing before call to train has already been applied to x
## :: operator to esure that function can be found
svmFit <- function(x,y,wts, param, lev, last, weights,classProbs, ...){
  kernlab::ksvm(
    x = as.matrix(x), # predictors need to be in a matrix hence as.matrix
    y = y,
    kernel = "rbfdot",
    kpar = list(sigma = param$sigma),
    C = param$C,
    prob.model = classProbs
  )
}

lpSVM$fit <- svmFit


##' predict element
## vector of predictions as output. 

svmPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  kernlab::predict(modelFit, newdata)
  
}

lpSVM$predict <- svmPred

##' prob element
svmProb <- function(modelFit, newdata, preProc = NULL,submodels = NULL){
  kernlab::predict(modelFit, newdata, type = "probabilities")
}

lpSVM$prob <- svmProb


##' sort element
## sorts tuning params from least to most complex
## sorting by C values

svmSort <- function(x) x[order(x$C),]

lpSVM$sort <- svmSort


##' levels element
## train ensures that classification models always predict factors with same levels.
## for s3 models, it is done automatically using obsLevel value. for s4 models, it uses levels info from levels slot in models list.
## only used in classification models using s4 methods
lpSVM$levels <- function(x) kernlab::lev(x)


library(mlbench)
data(Sonar)

library(caret)
set.seed(998)

inTraining <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)
training <- Sonar[inTraining, ]
testing <- Sonar[-inTraining, ]

fitControl <- trainControl(method = "repeatedcv",
                           # 10-fold cross validation
                           number = 10,
                           # repeat it 10 times
                           repeats = 10)

set.seed(825)
Laplacian <- train(Class ~ .,
                   data = training,
                   method = lpSVM,
                   preProc = c("center","scale"),
                   tuneLength = 8,
                   trControl = fitControl)

Laplacian

ggplot(Laplacian)+scale_x_log10()


#' Logit boost example
# using loop element to create custom loops for models to tune over. 
# instead of training n models, we can have a single model and derive predictions for n models using nIter( sequential tuning parameter).
loop <- data.frame(.nIter = 51)
loop

submodels <- list(data.frame(nIter = seq(11,41, by=10)))
submodels

fullGrid <- data.frame(nIter = seq(11,51,by=10))

loop <- fullGrid[which.max(fullGrid$nIter),,drop=FALSE]
loop

submodels <- fullGrid[-which.max(fullGrid$nIter),,drop=FALSE]

submodels <- list(submodels)
submodels

lbPredict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  
  # this model was fit with max value of nIter
  out <- caTools::predict.LogitBoost(modelFit, newdata, type = "class")
  
  # submodels is df with other values of nIter
  if(!is.null(submodels)){
    
    # save preditions in a list
    tmp<- out
    out <- vector(mode = "list", length = nrow(submodels)+1)
    out[[1]] <- tmp
    
    for (j in seq(along= submodels$nIter)) {
      out[[j+1]] <- caTools::predict.LogitBoost(
        modelFit, 
        newdata,
        nIter = submodels$nIter[j]
      )
    }
  }
  out
}

lbFuncs <- list(library = "caTools",
                loop = function(grid){
                  loop <- grid[which.max(grid$nIter),,drop = FALSE]
                  submodels <- grid[-which.max(grid$nIter),,drop = FALSE],
                  submodels <- list(submodels)
                  list(loop = loop, submodels = submodels)
                },
                
                type ="Classification",
                parameters = data.frame(parameter = "nIter",
                                        class = "numeric",
                                        label = "# Boosting Iterations"),
                
                grid <- function(x,y,len = NULL, search = "grid"){
                  out <- if(search == "grid")
                    data.frame(nIter = 1 + ((1:len)*10))
                  else
                    data.frame(nIter = sample(1:500, size = len))
                  
                  out
                },
                
                fit = function(x,y,wts, param, lev, last, weights, classProbs,...){
                  caTools::LogitBoost(as.matrix(x),y, nIter = param$nIter)
                },
                
                predict = )