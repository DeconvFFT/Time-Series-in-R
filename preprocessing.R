str(iris)
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)
featurePlot(x = iris[,1:4],
            y=iris$Species,
            plot="pairs",
            auto.key = list(columns=3))
featurePlot(x = iris[,1:4],
            y=iris$Species,
            plot="ellipse",
            auto.key = list(columns=3))

transparentTheme(trans = .9)
# overlayed density plots
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


#' creating dummy variables
library(earth)
data(etitanic)
# using model.matrix (base R function)
head(model.matrix(survived ~ ., data= etitanic))

# using dummyVars and predict to generate numeric values corresponding to each level
# in each factor
dummies <- dummyVars(survived ~ ., data =etitanic)
head(predict(dummies, newdata=etitanic))

#' zero and non zero variance predictors

# zero variance predictors are the ones which have only one unique value.
# This may crash models or make the fit unstable.
# Some predictors have a few unique values that occur with low frequency.
# this also makes data highly unstable.
# use frequency ratio (large for unbalanced, ~1 for balanced) and % unique values to find these near zero and zero variance predictors.
data(mdrr)
data.frame(table(mdrrDescr$nR11)) # column has near non zero predictors

nzv<- nearZeroVar(mdrrDescr, saveMetrics = TRUE)
nzv[nzv$nzv,][1:10,]

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv] # to remove nearzero values

#' identifying correlated predictors (finding/ removing corelated predictors)
# This is the logic findCorrelation function uses internally
descCorr <- cor(filteredDescr)
highCorr <- sum(abs(descCorr[upper.tri(descCorr)])>.999)


descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])

# removing descriptors with correlation >0.75
highlyCorrDescr <- findCorrelation(descrCor,cutoff = 0.75)
filteredDescr <- filteredDescr[,-highlyCorrDescr] # removing highly correlated descriptors

descCor2 <- cor(filteredDescr)
summary(descCor2[upper.tri(descCor2)])


#' Linear Dependencies

# findLinearCombos uses QR decompositon of matrix to enumerate sets of linear combinations if the exist.
# linear dependency : any combination of columns adding up to another column. 
# findLinearCombos checks and incrementally removes columns to check if dependency has been resolved.

ltfrDesign <- matrix(0, ncol=6, nrow=6)
ltfrDesign[,1] <- c(1,1,1,1,1,1)
ltfrDesign[,2] <- c(1,1,1,0,0,0)
ltfrDesign[,3] <- c(0,0,0,1,1,1)
ltfrDesign[,4] <- c(1,0,0,1,0,0)
ltfrDesign[,5] <- c(0,1,0,0,1,0)
ltfrDesign[,6] <- c(0,0,1,0,0,1)

combInfo <- findLinearCombos(ltfrDesign)
combInfo
ltfrDesign[,-combInfo$remove]

#' preprocess
# can be used for many predictors, including centering and scaling
# range option scales data between 0 and 1
set.seed(96)
inTrain <- sample(seq(along=mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center","scale"))

trainTransformed <- predict(preProcValues,training)
testTransformed <- predict(preProcValues,training)


#' Imputation
#  Scale data in training set.
# impute dataset using information in training data.
# KNN (the K closest neighbors are found in the training set and the value for the predictor is imputed using these values (e.g. using the mean)) and bagged trees are options for this. KNN is fast, bagged trees are more accurate.


#' transforming predictors
#  can transform predictors using PCA. method="pca" in preProcess transforms colnames as PC1,PC2,..
# pca creates a small subspace where new variables are independent of each other.
# using ICA (independent component analysis), finds new variables that are linear combination of original set such that the components are independent.

# spatial sign transformation projects data of a predictor into a unit circle of p dimensions.
# p is number of predictors.  i.e vector of data / norm of vector.
# predictors should be centered and scaled before applying this transformation

library(AppliedPredictiveModeling)
transparentTheme(trans= .4)

plotSubset <- data.frame(scale(mdrrDescr[,c("nC","X4v")]))
xyplot(nC ~ X4v,
       data= plotSubset,
       groups = mdrrClass,
       auto.key = list(columns=2))

# using spatial sign transformation 
transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(nC ~ X4v,
       data = transformed,
       groups = mdrrClass,
       auto.key = list(columns=2))

# boxcox transformation will be estimated on predictors if data>0
preProcValues2 <- preProcess(training, method="BoxCox")
trainBC <- predict(preProcValues2,training)
testBC <- predict(preProcValues2,test)

preProcValues2



#' pratical example of putting this all together

library(AppliedPredictiveModeling)
data("schedulingData")
str(schedulingData)

pp_hpc <- preProcess(schedulingData[,-8],
                     method = c("center","scale","YeoJohnson"))
pp_hpc

transformed <- predict(pp_hpc, schedulingData[,-8])
head(transformed)

mean(schedulingData$NumPending == 0) # sparse distribution in NumPending field

pp_no_nzv <- preProcess(schedulingData[,-8],
                        method = c("center","scale","YeoJohnson","nzv"))

pp_no_nzv

transformed <- predict(pp_no_nzv,schedulingData[,-8])


#' class distance calculations
# generate new predictors based on distance from class centroids
# for each level of factor variable, class centroid and covariance matrix is calculated. 
# for new samples, mahalanobis distance is calculated. This is helpful for non linear models when true decision boundary is actually linear.
# if there are more predictors in a class than samples, classDist function has pca and keep arguments that allows to use PCA within each class to avoid issues with singular covariance matrix.
# predict.Classdist 

centroids <- classDist(trainBC, trainMDRR)
distances <- predict(centroids,testBC)
distances <- as.data.frame(distances)


# plot distances
xyplot(dist.Active ~ dist.Inactive,
       data = distances,
       groups = testMDRR,
       auto.key = list(columns=2))
