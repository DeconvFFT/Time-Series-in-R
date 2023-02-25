#' why?
# custom preprocessing
# call functions in order you like
# more options than preprocess functions
# subsampling or resampling columns for checking summaries or performance measures can be done

library(caret)
library(recipes)
library(dplyr)
library(QSARdata)

data(AquaticTox)
tox <- AquaticTox_moe2D
ncol(tox)

# Adding outcome variable to dataframe
tox$Activity <- AquaticTox_Outcome$Activity

# for this example, we add another variable manufacturability which is > is molecule 
# weight is less.

tox <- tox %>%
  select(-Molecule) %>%
  ## Suppose the easy of manufacturability is 
  ## related to the molecular weight of the compound
  mutate(manufacturability  = 1/moe2D_Weight) %>%
  mutate(manufacturability = manufacturability/sum(manufacturability))

# rmse using weights based on manufacturability column
model_stats <- function(data, lev = NULL, model = NULL) {
  
  stats <- defaultSummary(data, lev = lev, model = model)
  
  wt_rmse <- function (pred, obs, wts, na.rm = TRUE) 
    sqrt(weighted.mean((pred - obs)^2, wts, na.rm = na.rm))
  
  res <- wt_rmse(pred = data$pred,
                 obs = data$obs, 
                 wts = data$manufacturability)
  c(wRMSE = res, stats)
}

# creating a recipie to use model_stats function with train
tox_recipe <- recipe(Activity ~ ., data = tox) %>%
  add_role(manufacturability, new_role = "performance var")

tox_recipe

tox_recipe <- tox_recipe %>% step_nzv(all_predictors())
tox_recipe

# using PCA extraction for dimensionality reduction for the 56 highly correlated predictors
# this step will retain components needed to capture 95% of information in these 56 predictors
tox_recipe <- tox_recipe %>% 
  step_pca(contains("VSA"), prefix = "surf_area_",  threshold = .95) 

tox_recipe

# removing pairs with >90% correlation except surface area pairs
tox_recipe <- tox_recipe %>% 
  step_corr(all_predictors(), -starts_with("surf_area_"), threshold = .90)

tox_recipe

# center and scale all predictors available at the end of recipe
tox_recipe <- tox_recipe %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

tox_recipe

# using this recipe to fit SVM
tox_ctrl <- trainControl(method = "cv", summaryFunction = model_stats)
set.seed(888)
tox_svm <- train(tox_recipe, tox,
                 method = "svmRadial", 
                 metric = "wRMSE",
                 maximize = FALSE,
                 tuneLength = 10,
                 trControl = tox_ctrl)
tox_svm

rec <- recipe( ~ ., data = USArrests)
pca_trans <- rec %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric(), num_comp = 3)
pca_estimates <- prep(pca_trans, training = USArrests)
pca_data <- bake(pca_estimates, USArrests)

rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
plot(pca_data$PC1, pca_data$PC2,
     xlim = rng, ylim = rng)
