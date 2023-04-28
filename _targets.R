# Global pipeline set-up --------------------------------------------------


# Workhorse packages
library("targets")
library("tarchetypes")
library("future")
library("future.callr")

# All packages used by the projects 
project_pkgs <- c(
  "rms", # Validation and logistic regression modelling
  "foreign", # Reading-in data, editing labels
  "mice", # Imputation
  "glmnet", # LASSO
  "future", # For running bootstraps in parallel
  "furrr", # For running bootstraps in parallel
  "tidyverse", # Data manipulation/plotting
  "tableone", # For getting data of table one
  "splines"
)

tar_option_set(packages = project_pkgs, error = "continue")

# See https://github.com/ropensci/targets/discussions/359
# Try: plan(list(tweak(callr, workers = 1), tweak(callr, workers = 3))) ?
plan(callr)


# Analysis pipeline -------------------------------------------------------


# Set contrasts globally
options(contrasts = rep("contr.treatment", 2))

# Source support functions
source("data-raw/prepare_raw_data.R")
source("R/imputation-helpers.R")
source("R/model-validation-helpers.R")

# Pipeline 
list(

  # -- Part 1: Data preparation
  tar_target(
    develop_n887_raw,
    foreign::read.spss(
      file = "data-raw/RegistryPart1_N887.sav", 
      to.data.frame = TRUE, 
      use.value.labels = TRUE
    )
  ),
  tar_target(
    valid_n1111_raw,
    foreign::read.spss(
      file = "data-raw/RegistryPart2_N1111.sav", 
      to.data.frame = TRUE, 
      use.value.labels = TRUE
    )
  ),
  tar_target(dat_combined, prepare_raw_data(develop_n887_raw, valid_n1111_raw)),
  
  # -- Part 2: Imputations
  
  # We pick the number of imputations and specify our candidate predictors
  # such that we can directly add the weights for the stacked ridge
  tar_target(validation_settings, list("B" = 200L, "n_folds" = 10L)), 
  tar_target(imp_settings, list("m" = 50L, "n_cycles" = 20L)),
  tar_target(
    candidate_predictors, c(
      "M_age",
      "M_prev_ht",
      "AorticVariant",
      "ArchElongation",
      "AngleFollowingBifurcation",
      "InnCca_nr_90orLarger",
      "ICAE_NASCET_Degree"
    )
  ),
  tar_target(
    dat_to_impute, 
    add_stacked_weights(dat_combined, candidate_predictors, imp_settings)
  ),
  
  # All rounds of imputations here:
  tar_target(imps_assess, run_imputations(dat_to_impute, imp_settings, type = "model")),
  tar_target(imps_combined, run_imputations(dat_to_impute, imp_settings, type = "combined")),
  
  # Combine all imputed datasets in one big df
  tar_target(
    imps_all,
    bind_imps(list("imps_assess" = imps_assess, "imps_combined" = imps_combined)),
    format = "fst"
  ),

  # Model validation..
  tar_target(
    model_formula,
    reformulate(termlabels = candidate_predictors, response = "Mc_FailedFemoralApproach")
  ),
  # Internal validation development set
  tar_target(
    validation_dev_lambdamin,
    validate_stackedImps(
      imputations = imps_all %>% filter(imps_label == "imps_assess" & dataset == "develop"),
      formula = model_formula,
      wts = "wts",
      n_folds = validation_settings$n_folds,
      lambda_choice = "min", # "1se" results too extreme, avoid
      B = validation_settings$B
    )
  ),
  tar_target(
    validation_comb_lambdamin,
    validate_stackedImps(
      imputations = imps_all %>% filter(imps_label == "imps_combined"),
      formula = model_formula,
      wts = "wts",
      n_folds = validation_settings$n_folds,
      lambda_choice = "min",
      B = validation_settings$B
    )
  ),
  tarchetypes::tar_render(analysis_summary, path = "analysis/article-results.Rmd")
)
