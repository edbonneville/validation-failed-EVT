##**************##
## Final script ##
##**************##


# (separate into scripts later)


# Load libraries and read-in processed data -------------------------------


# Load libraries (will check and install)
# (Remove unnecessary parts of tidyverse after..)
if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(
  "rms", # Validation and logistic regression modelling
  "foreign", # Reading-in data, editing labels
  "mice", # Imputation
  "glmnet", # LASSO
  "tidyverse", # Data manipulation/plotting
  "naniar"
)

dat <- readRDS("data/combined-data_processed.rds")

set.seed(123)

# MICE imputation ---------------------------------------------------------


# Check the missings 
dat %>% 
  filter(dataset == "train") %>% 
  naniar::gg_miss_var(show_pct = TRUE)

### Ask about the imputation of those vars not in LASSO models / those in train that are not in test
### I.e the univariable models in train
vars_excl <- c("acute_takeoff_angle", "ica_geq90", "origosten_geq50", "intracran_stenosis_geq50")

dat_temp <- dat %>% 
  select(!all_of(vars_excl)) 

# Set predictors for later
candidate_predictors <- colnames(dat_temp)[!(colnames(dat_temp) %in% c("dataset", "failure_femoral_approach"))]
names(candidate_predictors) <- candidate_predictors

# Make the weights for LASSO already
m <- 5 # number of imputations
dat_temp <- dat_temp %>% 
  naniar::add_prop_miss(all_of(candidate_predictors)) %>% # make own function
  mutate(wts = (1 - prop_miss_vars) / m) 

# Numerical summary of missings
dat_temp %>%
  group_by(dataset) %>% 
  miss_var_summary()

# Create predictor matrix mice
matpred <- matrix(
  data = 1, 
  nrow = ncol(dat_temp), 
  ncol = ncol(dat_temp),
  dimnames = list(names(dat_temp), names(dat_temp))
)

# Don't impute a var using itself, and exclude dataset from imp model
diag(matpred) <- 0 
matpred[, c("dataset", "wts", "prop_miss_vars")] <- 0

# Use parallel after
imps <- mice(
  data = dat_temp,
  m = 5,
  maxit = 10,
  predictorMatrix = matpred,
  ignore = (dat_temp[["dataset"]] == "test") # Impute test set using imp models from train
)

# Check convergence
plot(imps)

# Store imputed datasets
imp_dats <- mice::complete(imps, "long")
imp_dats_train <- imp_dats %>% filter(dataset == "train")
imp_dats_test <- imp_dats %>% filter(dataset == "test")



# Univariate odds ratios --------------------------------------------------


# Make them 
imp_list_train <- split(imp_dats_train, imp_dats_train[[".imp"]])

univar_OR_train <- purrr::map_dfr(.x = candidate_predictors, .f = ~ {
  
  form_univar <- as.formula(paste0("failure_femoral_approach ~ ", .x))
  
  # Start pooling
  lapply(imp_list_train, function(imp) {
    glm(form_univar, family = binomial(link = "logit"), data = imp)
  }) %>% 
    pool() %>% 
    summary(conf.int = 0.95, exponentiate = TRUE)
})



# Lasso modelling ---------------------------------------------------------


# Make model formula 
rhs <- paste(candidate_predictors, collapse = " + ")
lasso_form <- as.formula(paste0("failure_femoral_approach ~ ", rhs))
lasso_form

# Try CV on stacked dataset (compare with optimal lambda per dataset)
nfolds <- 10
ids <- unique(imp_dats_train[[".id"]])
fold_ids <- caret::createFolds(k = 10, y = ids, list = FALSE) # use own function later
imp_dats_train[["fold_id"]] <- fold_ids[imp_dats_train[[".id"]]]

# X should have intercept removed!!!!!!

X <- model.matrix(as.formula(paste0("~ + ", rhs)), data = imp_dats_train)
Y <- imp_dats_train[["failure_femoral_approach"]]
test_cv_stack <- cv.glmnet(
  x = X,
  y = Y,
  foldid = imp_dats_train[["fold_id"]],
  weights = imp_dats_train[["wts"]],
  family = "binomial"
)

plot(test_cv_stack)
test_cv_stack$lambda.min
test_cv_stack$lambda.1se

# Remmeber alpha=1 everywhere!!!
mod_stacked <- glmnet(x = X, y = Y, 
                      family = "binomial", weights = imp_dats_train[["wts"]], 
                      lambda = test_cv_stack$lambda.min, intercept = TRUE,
                      alpha = 1)

perf <- assess.glmnet(mod_stacked, newx = X, newy = Y,
              weights = imp_dats_train[["wts"]], 
              family = "binomial")

predict(mod_stacked, newx = X)
coefs_mod <- c(mod_stacked[["a0"]], mod_stacked[["beta"]][-1])
cbind(drop(X %*% coefs_mod), predict(mod_stacked, newx = X))



# Bootstrap validate stacked imp ------------------------------------------


imputations = imp_dats_train
formula = lasso_form
lambda_choice = "min"

validate_lasso_stackedImps <- function(imputations, # as returned by "long"
                                       # add id and imp indicator
                                       formula,
                                       wts,
                                       n_folds = 10,
                                       lambda_choice = c("min", "1se"),
                                       B = 5) {
  
  # Run model on full stacked data - first determine fold ids
  folds <- assign_crossval_folds(y = unique(imputations[[".id"]]), n_folds = n_folds)
  imputations[["fold_id"]] <- folds[match(imputations[[".id"]], folds[["y"]]), "fold"]
  
  # Prepare frame
  dat <- stats::model.frame(formula, data = imputations)
  X_orig <- stats::model.matrix(formula, data = dat)
  Y_orig <- stats::model.extract(dat, "response")
  
  # Collect common args across glmnet functions
  args <- list("x" = X_orig, "y" = Y_orig, "weights" = imputations[["wts"]], family = "binomial", "alpha" = 1)
  
  cv_apparent <- do.call(cv.glmnet, args = c(args, list("foldid" = imputations[["fold_id"]])))
  
  # Pick 'optimal' lambda
  lambda <- ifelse(lambda_choice == "min", cv_apparent[["lambda.min"]], cv_apparent[["lambda.1se"]])
  
  # Run apparent model, and check performanc
  mod_apparent <- do.call(glmnet, args = c(args, list("lambda" = lambda, "intercept" = TRUE)))
  
  performance_apparent <- assess.glmnet(
    mod_apparent, 
    newx = X_orig, 
    newy = Y_orig, 
    weights = imputations[["wts"]], 
    family = "binomial"
  )
  
  # Calibration
  lp <- predict(mod_apparent, newx = X_orig, type = "link")
  
  #val.prob.ci.2(logit = lp, y = as.numeric(Y) - 1, weights = imputations[["wts"]])
  glm(Y ~ offset(lp), family = "quasibinomial")#, weights = imputations[["wts"]])
  glm(Y ~ lp, family = "quasibinomial")#, #weights = imputations[["wts"]])
  # Run one bootstrap
  
} 




samps <- sample(ids, length(ids), replace = TRUE)
sampos <- by(imp_dats_train, imp_dats_train[[".imp"]], function(imp) imp[samps, ])
boot_samp <- do.call(rbind, sampos)


X <- model.matrix(as.formula(paste0("~ + ", rhs)), data = boot_samp)
Y <- boot_samp[["failure_femoral_approach"]]

ids <- unique(boot_samp[[".id"]])
fold_ids <- caret::createFolds(k = 10, y = ids, list = FALSE) # use own function later
boot_samp[["fold_id"]] <- fold_ids[which(ids == boot_samp$.id)]#fold_ids[boot_samp[[".id"]]]

boot_cv_stack <- cv.glmnet(
  x = X,
  y = Y,
  foldid = boot_samp[["fold_id"]],
  weights = boot_samp[["wts"]],
  family = "binomial"
)




# Validate for each imp separately
imp_lambdas <- purrr::map_dfr(.x = 1:m, .f = ~ {
  imp <- imp_dats_train %>% filter(.imp == .x)
  X <- model.matrix(as.formula(paste0("~ + ", rhs)), data = imp)
  cv_oneimp <- cv.glmnet(x = X, y = imp[["failure_femoral_approach"]], family = "binomial")
  cbind.data.frame("min_lambda" = cv_oneimp$lambda.min, "1se_lambda" = cv_oneimp$lambda.1se)
})

colMeans(imp_lambdas)



# Internal validation bootstrap -------------------------------------------

## 1. First fit single imp data (no weights needed here)
imp <- imp_dats_train %>% filter(.imp == 5)

# Set up arguments
outcome_name <- all.vars(update(lasso_form, . ~ 1))
mod_mat <- model.matrix(lasso_form, data = imp)
X_imp <- mod_mat[, !(colnames(mod_mat) %in% "(Intercept)")]
y_imp <- imp[[outcome_name]]
args_glmnet <- list("x" = X_imp, "y" = y_imp, "family" = "binomial", "alpha" = 1)

# Find lambda - add arg for min or not, and fit
cv_imp <- do.call(cv.glmnet, args_glmnet)
mod_imp <- do.call(glmnet, c(args_glmnet, list("lambda" = cv_imp[["lambda.min"]])))
apparent_res <- assess_performance(mod_imp, new_x = X_imp, new_y = y_imp)

# Plot calib - RECHECK BOOTSRAP RECALIBRATION RMS
# https://darrendahly.github.io/post/homr/
mod_lrm <- lrm(lasso_form, data = imp, x = TRUE, y = TRUE)
validate(mod_lrm, B = 100)
val.prob(y = as.numeric(y_imp) -1 , logit = predict(mod_imp, X_imp))
val.prob.ci.2(y = as.numeric(y_imp) -1 , logit = predict(mod_imp, X_imp))
lp <- predict(mod_imp, X_imp)
modo <- glm(y_imp ~ lp, family = "binomial") # THIS THE RECALIBRATED MODEL 
glm(y_imp ~ predict(modo), family = "binomial") # Omg 0 and 1, like in harell
calibrate(mod_lrm)

hist(lp)
hist(predict(modo))

# Same for bootstrap
B <- 100
boots <- purrr::map_dfr(.x = 1:B, .id = "boot_sample", .f = ~ {
  
  inds <- sample(nrow(imp), size = nrow(imp), replace = TRUE)
  mod_mat_boot <- model.matrix(lasso_form, data = boot_samp)
  X_boot <- X_imp[inds, ]
  y_boot <- y_imp[inds]
  args_glmnet_boot <- list("x" = X_boot, "y" = y_boot, "family" = "binomial", "alpha" = 1)
  
  # Find lambda - add arg for min or not, and fit
  cv_boot <- do.call(cv.glmnet, args_glmnet_boot)
  mod_boot <- do.call(glmnet, c(args_glmnet_boot, list("lambda" = cv_boot[["lambda.min"]])))
  boot_measures <- assess_performance(mod_boot, new_x = X_boot, new_y = y_boot)
  test_measures <- assess_performance(mod_boot, new_x = X_imp, new_y = y_imp)

  # Compute optimism
  boot_df <- cbind.data.frame(
    "measure" = test_measures[["measure"]],
    "boot_perf" = boot_measures[["value"]],
    "test_perf" = test_measures[["value"]],
    "optimism" = boot_measures[["value"]] - test_measures[["value"]]
  )
  
  return(boot_df)
})


boots %>% 
  group_by(measure) %>% 
  summarise("mean_optimism" = mean(optimism)) %>% 
  left_join(apparent_res) %>% 
  mutate("internal" = apparent - mean_optimism)


# Tables ------------------------------------------------------------------


# Kableextra to word here
