# grid search (tuneGrid) is the default method of search.
# Alternative to this is gridSearch and racing, and random selection of tuning parameters
# which covers params to a lesser extent.
# random search is not always useful. Speed can sometimes cancel out other optimizations.

library(mlbench)
data("Sonar")

library(caret)

inTraining <- createDataPartition(Sonar$Class, p=.75, list = FALSE)
training <- Sonar[inTraining,]
testing <- Sonar[-inTraining,]

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random")

set.seed(825)

rdaFit <- train(Class ~ .,
                data = training, 
                method = "rda",
                metric = "ROC",
                tuneLength = 30,
                trControl = fitControl)

rdaFit

ggplot(rdaFit)+theme(legend.position = "top")
