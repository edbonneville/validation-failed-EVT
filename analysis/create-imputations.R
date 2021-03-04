##*****************##
## Run imputations ##
##*****************##


# Set contrasts for ordered factors
options(contrasts = rep("contr.treatment", 2))
source("analysis/helper-functions.R")

# To avoid warnings - the cluster.seed() argument is what assures reproducibility
set.seed(2021)

# Read-in combined data
dat_combined <- readRDS("data/combined-data_processed.rds")

# Three rounds of imputation:

# 1. Imputation of development and validation separately for univariate assessments 
# (vascular characteristics not available at validation) - this could eventually be used for 
# validation (but not recommended)

# 2. Imputation of development + validation together using imputation models fitted on development data,
# and without outcome in validation. This is for actual performance assessment.

# 3. Imputation of development + validation using imputation models fitted on COMBINED data,
# this will be for the final combined model (to be used in practise)


# Imputation prep ---------------------------------------------------------


# Exclude variables what are not relevant to either imputation or pred model
vars_exclude <- c(
  "StudySubjectID",
  "M_premrs",
  "M_NIHSS_BL",
  "M_togroin",
  "Mc_pretici_short",
  "M_NIHSS_FU",
  "M_mrs_rev"
)

# We will remove "dataset" (indicator of develop/valid) from imputation model after
dat_to_impute <- dat_combined %>% select(-all_of(vars_exclude)) 

# We already compute weights based on candidate predictors (for stacked data)
m <- 15 # number of imputed datasets
iters <- 15 # number of imputation cycles

# Prepare parallel settings too
n_core <- 3
n_imp_core <- ceiling(m / n_core) # will do a few more imputations if m not divisible by n_core

candidate_predictors <- c(
  "M_age",
  "M_prev_ht",
  "AorticVariant",
  "ArchElongation",
  "AngleFollowingBifurcation",
  "InnCca_nr_90orLarger",
  "ICAE_NASCET_Degree"
)

# Get weights (check if also depends on auxiliary vars)
dat_to_impute[["wts"]] <- (1 - rowMeans(is.na(dat_to_impute[, candidate_predictors]))) / m

# Visualise missings beforehand
naniar::gg_miss_var(dat_to_impute, facet = dataset, show_pct = TRUE)


# Imputation round 1A: development data -----------------------------------


# Seperate datasets
dat_develop <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "develop"))
dat_valid <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "valid"))

# Run in parallel - number of imputations is n.core * n.imp.core
imps_dev <- mice::parlmice(
  data = dat_develop,
  n.imp.core = n_imp_core,
  cluster.seed = 0202,
  method = set_mice_methods(dat_develop),
  predictorMatrix = create_mice_predmatrix(
    dat = dat_develop, 
    exclude_imp_models = "wts"
  ),
  n.core = n_core,
  maxit = iters,
  cl.type = "PSOCK"
)

# Save
saveRDS(imps_dev, file = "data/imps_dev.rds")


# Imputation round 1B: validation data ------------------------------------


# Get names of unmeasured vars in valid (i.e. percent missing = 100)
vascular_vars_develop <- names(which(colMeans(is.na(dat_valid)) == 1))


# Run in parallel - number of imputations is n.core * n.imp.core
imps_valid <- mice::parlmice(
  data = dat_valid,
  n.imp.core = n_imp_core,
  cluster.seed = 0203,
  method = set_mice_methods(
    dat_valid, 
    skip_impute = c(vascular_vars_develop)
  ),
  predictorMatrix = create_mice_predmatrix(
    dat = dat_valid, 
    exclude_imp_models = c("wts", vascular_vars_develop)
  ),
  n.core = n_core,
  maxit = iters,
  cl.type = "PSOCK"
)

# Save
saveRDS(imps_valid, file = "data/imps_valid.rds")


# Imputation round 2: for model assessment --------------------------------


# Artificially set outcome to NA - save "true" outcome
dat_assess <- dat_to_impute %>% 
  mutate(
    outcome_true = Mc_FailedFemoralApproach,
    Mc_FailedFemoralApproach = ifelse(dataset == "valid", NA_character_, as.character(Mc_FailedFemoralApproach)),
    Mc_FailedFemoralApproach = factor(Mc_FailedFemoralApproach, levels = c("success", "failure"))
  )

# Set predictor matrix (Mc_FailedFemoralApproach gets thrown out due to collinear with outcome true)
predmat_assess <- create_mice_predmatrix(
  dat = dat_assess, 
  exclude_imp_models = c("wts", "dataset", "outcome_true", vascular_vars_develop)
)

predmat_assess[, "Mc_FailedFemoralApproach"] <- 1
diag(predmat_assess) <- 0 


# Run imputations - about 3.5 minutes  (for 15 imps, 15 iters, all across 3 cors)
imps_assess <- mice::parlmice(
  data = dat_assess,
  n.imp.core = n_imp_core,
  cluster.seed = 0204,
  predictorMatrix = predmat_assess,
  method = set_mice_methods(
    dat = dat_assess, 
    skip_impute = vascular_vars_develop,
    method_adjust = c("Mc_FailedFemoralApproach" = "logreg")
  ),
  n.core = n_core,
  maxit = iters,
  cl.type = "PSOCK",
  ignore = (dat_assess[["dataset"]] == "valid") # Important line
)

# Save
saveRDS(imps_assess, file = "data/imps_assess.rds")


# Imputation round 3: combined datasets -----------------------------------


# Impute combined (without imputing missing vascular characteristics in validation)
combined_to_impute <- dat_to_impute %>% select(-all_of(vascular_vars_develop))

# Impute
imps_combined <- mice::parlmice(
  data = combined_to_impute,
  n.imp.core = n_imp_core,
  cluster.seed = 0205,
  predictorMatrix = create_mice_predmatrix(
    dat = combined_to_impute, 
    exclude_imp_models = c("wts", "dataset")
  ),
  method = set_mice_methods(dat = combined_to_impute),
  n.core = n_core,
  maxit = iters,
  cl.type = "PSOCK"
)

# Save
saveRDS(imps_combined, file = "data/imps_combined.rds")
