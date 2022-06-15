# Global pipeline set-up --------------------------------------------------

# See following for setting up final pipline:
# https://github.com/epiforecasts/evaluate-delta-for-forecasting/blob/4d8449fbedba71690ac6f1320a8438bdc10a4f44/_targets.R
# - Also use capsule for renv

# Workhorse packages
library("targets")
library("tarchetypes")

# All packages used by the projects - this is not good for renv (add instead to _packages.R files)
project_pkgs <- c(
  "rms", # Validation and logistic regression modelling
  "foreign", # Reading-in data, editing labels
  "mice", # Imputation
  "glmnet", # LASSO
  "future", # For running bootstraps in parallel
  "furrr", # For running bootstraps in parallel
  "tidyverse", # Data manipulation/plotting
  "tableone", # For getting data of table one
  "WeightedROC", # Calculating AUC on stacked data
  "parallel"
)

tar_option_set(packages = project_pkgs, error = "continue")
# Uncomment if running scripts interactively:
# sapply(project_pkgs, function(pkg) require(pkg, character.only = TRUE)); rm(project_pkgs)


# Analysis pipeline -------------------------------------------------------


# Source support functions
source("data-raw/prepare_raw_data.R")
source("R/imputation-helpers.R")

# Pipeline (parts are in separate files):
targets_list <- list(

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
  
  # -- Part 2: imputations
  
  # We pick the number of imputations and specify our candidate predictors
  # such that we can directly add the weights for the stacked LASOO
  tar_target(imputation_settings, list("m" = 3L, "n_cycles" = 2L, "n_cores" = 1L)),
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
    add_stacked_weights(dat_combined, candidate_predictors, imputation_settings)
  ),
  
  # (For univariable assessments: run imputation on develop and validation separately)
  #tar_target(imps_dev, run_imputations(dat_to_impute, imputation_settings, type = "develop"))
  tar_target(
    dat_develop, subset(x = dat_to_impute, select = -dataset, subset = (dataset == "develop"))
  ),
  tar_target(
    imps_dev,
    {
      
      cl<-makeCluster(3L, type = "PSOCK")
      #::detectCores()
      dat_dev <- dat_develop
      
      base <- 2
      clusterExport(cl, varlist = c("base", ls(parent.frame())), 
                    envir = environment())
      parLapply(cl,  2:4, 
                function(exponent)dat_dev$M_age[1])
      
     # stopCluster(cl)
      
      #list(
      #  ls(parent.frame()),
      #  ls(environment())
      #)
      
      
      #assign("dat_develop", dat_develop, envir = .GlobalEnv)
      #assign("dat_develop", dat_develop, envir = .GlobalEnv)
      #assign("imputation_settings", imputation_settings, envir = .GlobalEnv)
      #source("R/imputation-helpers.R", local = .GlobalEnv)
      #assign("create_mice_predmatrix", create_mice_predmatrix, envir = .GlobalEnv)
      #assign("dat_develop", dat_develop, envir = environment())
      #assign("imputation_settings", imputation_settings, envir = environment())
      #assign("set_mice_methods", set_mice_methods, envir = environment())
      #meths <<- set_mice_methods(dat_develop)
      #predmat <<- create_mice_predmatrix(dat = dat_develop, exclude_imp_models = "wts")
      
      # assign("dat_develop", dat_develop, envir = parent.frame())

    }, 
  )
  # tar_target(
  #   imps_dev, 
  #   {
  #     
  #     mice::parlmice(
  #       data = dat_develop,
  #       n.imp.core = ceiling(imputation_settings$m / imputation_settings$n_cores),
  #       cluster.seed = tar_seed(),
  #       method = set_mice_methods(dat_develop),
  #       predictorMatrix = create_mice_predmatrix(dat = dat_develop, exclude_imp_models = "wts"),
  #       n.core = imputation_settings$n_cores,
  #       maxit = imputation_settings$n_cycles,
  #       cl.type = "PSOCK"
  #     )
  #   }, 
  #   deployment = "main"
  # )
  
  
  
  #tarchetypes::tar_render(analysis_summary, path = "analysis/2020-09_analysis-summary.Rmd")
  #tarchetypes::tar_render([and rmd with raw data visualisations, also after data prep..
  #.. interactive with plotly?])
  # Or with shiny??
)

# Source targets
#("R/NMA-preDLI-models.R")
#source("R/NMA-postDLI-models.R")

#targets_list <- c(targets_list, preDLI_targets, postDLI_targets)
targets_list


    
