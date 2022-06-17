#' Add weights to combined (validation + development) data 
#' 
#' See Wan et al. (2015), or notation in section 2.2 from
#' Thao et al. (2019). The weights used in the LASSO procedure are equal to
#' 1/m * (number of non-missing variable for subject i / p), where m is the
#' number of imputed datasets, and p the number of candidate predictors.
#' 
#' In the function we also exclude variables not relevant to either
#' imputation or analysis model.
#'  
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
  
  # Add weights, see ref: 
  dat_to_impute[["wts"]] <- (1 - rowMeans(is.na(dat_to_impute[, candidate_predictors]))) / 
    imputation_settings$m
  
  return(dat_to_impute)
}

#' Worker function for multiple imputation
#' 
#' Four rounds of imputation are performed:
#' - Once for development set alone (for univariable analyses)
#' - Once for validation set alone (for univariable analyses)
#' - Once as part of model validation, imputing both development and 
#' validation data using imputation models fitted only on development set. This
#' procedure is described in Hoogland et al. (2020)
#' - Once on combined cohort
#' 
run_imputations <- function(dat_to_impute,
                            imputation_settings,
                            type = c("develop", "valid", "model", "combined")) {
  
  # For ordered covariates to be safe (not sure if Targets passes this on)
  options(contrasts = rep("contr.treatment", 2))
  
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
        mice::mice(
          data = dat_develop,
          m = imputation_settings$m,
          method = set_mice_methods(dat_develop),
          predictorMatrix = create_mice_predmatrix(dat = dat_develop, exclude_imp_models = "wts"),
          maxit = imputation_settings$n_cycles,
          printFlag = FALSE
        )
      },
      valid = {
        mice::mice(
          data = dat_valid,
          m = imputation_settings$m,
          method = set_mice_methods(dat_valid, skip_impute = c(vascular_vars_develop)),
          predictorMatrix = create_mice_predmatrix(
            dat = dat_valid, 
            exclude_imp_models = c("wts", vascular_vars_develop)
          ),
          maxit = imputation_settings$n_cycles,
          printFlag = FALSE
        )
      },
      combined = {
        combined_to_impute <- dat_to_impute %>% select(-all_of(vascular_vars_develop))
        mice::mice(
          data = combined_to_impute,
          m = imputation_settings$m,
          predictorMatrix = create_mice_predmatrix(
            dat = combined_to_impute, 
            exclude_imp_models = c("wts", "dataset")
          ),
          method = set_mice_methods(dat = combined_to_impute),
          printFlag = FALSE,
          maxit = imputation_settings$n_cycles
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
    
    imps <- mice::mice(
      data = dat_assess,
      m = imputation_settings$m,
      predictorMatrix = predmat_assess,
      method = set_mice_methods(
        dat = dat_assess, 
        skip_impute = vascular_vars_develop,
        method_adjust = c("Mc_FailedFemoralApproach" = "logreg")
      ),
      maxit = imputation_settings$n_cycles,
      printFlag = FALSE,
      ignore = (dat_assess[["dataset"]] == "valid") # Important line
    )
  }
  
  return(imps)
}

#' Version of `mice::make.predictorMatrix()` with additional argument
#' allowing to exclude variables from all imputation models.
#' 
create_mice_predmatrix <- function(dat, exclude_imp_models = NULL) {
  
  # Make basic matrix
  matpred <- mice::mice(dat, m = 1, maxit = 0)[["predictorMatrix"]]
  
  # Exclude from imputation model
  matpred[, exclude_imp_models] <- 0
  
  return(matpred)
}

#' Extended version of `mice::make.method()`
#' 
#' The `method_adjust` argument is a nameed list 
#' i.e. c("ICAIAtherosclerosis" = "norm")
#' 
set_mice_methods <- function(dat, skip_impute = NULL, method_adjust = NULL) {
  
  method_vec <- mice::mice(dat, m = 1, maxit = 0)[["method"]]
  method_vec[skip_impute] <- ""
  
  # Edit remaining methods
  if (!is.null(method_adjust)) {
    method_vec <- replace(x = method_vec, list = names(method_adjust), values = method_adjust)
  } 
  
  return(method_vec)
}

#' Bind all the imputation rounds in long format
#' 
bind_imps <- function(imps_list) {
  
  # Read-in and transform variables
  imps_all <- imps_list %>% 
    map(.f = ~ mice::complete(.x, action = "long")) %>% 
    bind_rows(.id = "imps_label") %>% 
    mutate(
      ICAE_NASCET_Degree = factor(
        x = ifelse(ICAE_NASCET_Degree < 99, "leq_99", "geq_99"), 
        levels = c("leq_99", "geq_99")
      ),
      InnCca_nr_90orLarger = fct_collapse(
        .f = InnCca_nr_90orLarger, 
        "0" = "0", 
        "geq_1" = c("1", "2", "3")
      ),
      ICA_nr_90orLarger = fct_collapse(
        .f = ICA_nr_90orLarger, 
        "0" = "0", 
        "geq_1" = c("1", "2", "3")
      ),
      ICAIAtherosclerosis = fct_collapse(
        .f = ICAIAtherosclerosis, 
        "under50%" = c("No", "Yes, calcified spots", "Yes, <50% stenosis"),
        "geq50%" = "Yes, >=50% stenosis"
      ),
      AngleAaInn_OR_AaCca = factor(
        x = ifelse(AngleAaInn_OR_AaCca > 45, "above45", "leq45"),
        levels = c("above45", "leq45")
      ),
      M_age = M_age / 10 # Change to decades
    )  
  
  # Change back to true outcome for assess imps 
  # (where validation outcome was implicitly imputed)
  imps_all <- imps_all %>% 
    mutate(
      Mc_FailedFemoralApproach = ifelse(
        imps_label == "imps_assess", 
        as.character(outcome_true), 
        as.character(Mc_FailedFemoralApproach)
      ),
      Mc_FailedFemoralApproach = factor(
        x = Mc_FailedFemoralApproach, 
        levels = c("success", "failure")
      )
    )
  
  return(imps_all)
}

