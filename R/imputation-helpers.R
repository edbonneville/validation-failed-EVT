# Add weights to combined data 
add_stacked_weights <- function(dat_combined,
                                candidate_predictors,
                                imputation_settings) {
  
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
  
  dat_to_impute[["wts"]] <- (1 - rowMeans(is.na(dat_to_impute[, candidate_predictors]))) / 
    imputation_settings$m
  
  return(dat_to_impute)
}

run_imputations <- function(dat_to_impute,
                            imputation_settings,
                            type = c("develop", "valid", "model", "combined")) {
  
  # Read-in valid and develop separately
  dat_develop <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "develop"))
  dat_valid <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "valid"))
  
  # Find (vascular) variables only present in validation set
  vascular_vars_develop <- names(which(colMeans(is.na(dat_valid)) == 1))
  
  # Easiest to do are imputation for separate develop, valid and combine
  if (type != "model") {
    
    imps <- switch(
      type,
      develop = {
        mice::parlmice(
          data = dat_develop,
          n.imp.core = ceiling(imputation_settings$m / imputation_settings$n_cores),
          cluster.seed = tar_seed(),
          method = set_mice_methods(dat_develop),
          predictorMatrix = create_mice_predmatrix(dat = dat_develop, exclude_imp_models = "wts"),
          n.core = imputation_settings$n_cores,
          maxit = imputation_settings$n_cycles,
          cl.type = "PSOCK"
        )
      },
      valid = {
        mice::parlmice(
          data = dat_valid,
          n.imp.core = ceiling(imputation_settings$m / imputation_settings$n_cores),
          cluster.seed = tar_seed(),
          method = set_mice_methods(dat_valid, skip_impute = c(vascular_vars_develop)),
          predictorMatrix = create_mice_predmatrix(
            dat = dat_valid, 
            exclude_imp_models = c("wts", vascular_vars_develop)
          ),
          n.core = imputation_settings$n_cores,
          maxit = imputation_settings$n_cycles,
          cl.type = "PSOCK"
        )
      },
      combined = {
        combined_to_impute <- dat_to_impute %>% select(-all_of(vascular_vars_develop))
        mice::parlmice(
          data = combined_to_impute,
          n.imp.core = ceiling(imputation_settings$m / imputation_settings$n_cores),
          cluster.seed = tar_seed(),
          predictorMatrix = create_mice_predmatrix(
            dat = combined_to_impute, 
            exclude_imp_models = c("wts", "dataset")
          ),
          method = set_mice_methods(dat = combined_to_impute),
          n.core = imputation_settings$n_cores,
          maxit = imputation_settings$n_cycles,
          cl.type = "PSOCK"
        )
      }
    )
    
    # This is the external validation part
  } else { 
    
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
    
    imps <- mice::parlmice(
      data = dat_assess,
      n.imp.core = ceiling(imputation_settings$m / imputation_settings$n_cores),
      cluster.seed = tar_seed(),
      predictorMatrix = predmat_assess,
      method = set_mice_methods(
        dat = dat_assess, 
        skip_impute = vascular_vars_develop,
        method_adjust = c("Mc_FailedFemoralApproach" = "logreg")
      ),
      n.core = imputation_settings$n_cores,
      maxit = imputation_settings$n_cycles,
      cl.type = "PSOCK",
      ignore = (dat_assess[["dataset"]] == "valid") # Important line
    )
  }
  
  return(imps)
}

create_mice_predmatrix <- function(dat, exclude_imp_models = NULL) {
  
  # Make basic matrix
  matpred <- mice::mice(dat, m = 1, maxit = 0)[["predictorMatrix"]]
  
  # Exclude from imputation model
  matpred[, exclude_imp_models] <- 0
  
  return(matpred)
}

# method adjust is names list i.e. c("ICAIAtherosclerosis" = "norm")
set_mice_methods <- function(dat, skip_impute = NULL, method_adjust = NULL) {
  
  method_vec <- mice::mice(dat, m = 1, maxit = 0)[["method"]]
  method_vec[skip_impute] <- ""
  
  # Edit remaining methods
  if (!is.null(method_adjust)) {
    method_vec <- replace(x = method_vec, list = names(method_adjust), values = method_adjust)
  } 
  
  return(method_vec)
}



