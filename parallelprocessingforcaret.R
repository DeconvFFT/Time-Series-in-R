library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

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
stopCluster(cl)


gbmFit3

# parallel execution can achieve parallel speedups upto 4-5 workers. After that doesn't speedup much.
# more the n_workers, more space needed because more copies of data are formed( this can lead to OOM).
# if allowParellel = FALSE, always ignores parallel backend.
# When rfe or sbf calls train, if a parallel backend with P processors is being used, the combination of these functions will create P^2 processes.
