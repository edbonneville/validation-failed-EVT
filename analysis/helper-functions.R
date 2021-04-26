assign_crossval_folds <- function(y, n_folds) {
  n <- length(y)
  inds <- sample(x = rep(1:n_folds, ceiling(n / n_folds)), size = n)
  mat <- cbind.data.frame("y" = y, "fold" = inds)
  return(mat)
}

# Returns list of bootstrap datasets
group_resample <- function(dat, 
                           id_var, 
                           B = 50, 
                           boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids <- unique(dat[[id_var]])

  # Create bootstrap datasets
  boot_dats <- replicate(
    n = B,
    simplify = FALSE,
    expr = purrr::map_dfr(
      .x = sample(x = ids, replace = TRUE), 
      .f = ~ dat[which(dat[[id_var]] == .x), ], 
      .id = boot_id_name
    )
  )
 
  return(boot_dats)
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

imps_univariate_ORs <- function(outcome,
                                predictors,
                                imps,
                                keep_intercepts = FALSE) {
  
  # Check if from mice() or not
  #imp_dats <- mice::complete(imps, action = "all")
  
  imp_dats <- imps
  
  # Run ORs - maybe later exclude intercept
  univariate_analyses <- purrr::map_dfr(.x = predictors, .f = ~ {
    
    form_univar <- stats::reformulate(response = outcome, termlabels = .x) 
    
    # Start pooling
    mods_imp_dats <- lapply(
      X = imp_dats, 
      FUN = function(imp) {
        # Check first if predictor not totally missing (and not an interaction)
        if (all(is.na(imp[[.x]])) & !grepl(x = .x, pattern = "\\*")) 
          form_univar <- update(form_univar, . ~ 1)
        glm(form_univar, family = binomial(link = "logit"), data = imp)
      } 
    )  

    summary(mice::pool(mods_imp_dats), conf.int = 0.95, exponentiate = TRUE)
  })
  
  # Remove intercepts for ease of reading
  if (!keep_intercepts) {
    univariate_analyses <- univariate_analyses[univariate_analyses[["term"]] != "(Intercept)", ]
  }
  
  return(univariate_analyses)
}

calibration_intercept_slope <- function(y, lp, wts = NULL) {
  
  # Fit models to linear predictors 
  mod_lp <- rms::lrm.fit(y = y, x = drop(lp), weights = wts)
  mod_offset <- rms::lrm.fit(y = y, offset = drop(lp), weights = wts)
  result <- c(
    "intercept" = coef(mod_offset)[["Intercept"]], 
    "slope" = coef(mod_lp)[["x[1]"]]
  )
  return(result)
}


assess_performance <- function(glmnet_model, # Already with a picked lambda
                               new_x,
                               new_y,
                               wts = NULL) {
  
  # Compute linear predictor
  lp <- drop(glmnet::predict.glmnet(glmnet_model, newx = new_x))
  
  # Get performance measures from within glmnet (incl. AUC)
  general_performance <- glmnet::assess.glmnet(
    object = glmnet_model, 
    newx = new_x, 
    newy = new_y, 
    family = "binomial",
    weights = wts
  )
  
  # Assess calibration
  calibration <- calibration_intercept_slope(y = new_y, lp = lp, wts = wts)
  
  # Bind results
  measures <- c(unlist(general_performance), calibration)
  measures_df <- cbind.data.frame("measure" = names(measures), "value" = measures)
  rownames(measures_df) <- NULL
  return(measures_df)
}



# LASSO -------------------------------------------------------------------


# Add alpha = 1 to args..
run_lasso_glmnet <- function(x,
                             y,
                             foldid = NULL,
                             family = "binomial",
                             lambda_choice = c("min", "1se"),
                             wts = NULL,
                             ...) {
  
  # Get lambda choice
  lambda_choice <- match.arg(lambda_choice)
  
  # Run cross-validation
  cross_val <- glmnet::cv.glmnet(
    x = x,
    y = y,
    weights = wts,
    foldid = foldid,
    family = family,
    ...
  )
  
  # Keep lambda
  lambda <- ifelse(lambda_choice == "min", cross_val[["lambda.min"]], cross_val[["lambda.1se"]])
  
  # Run actual model
  mod <- glmnet::glmnet(
    x = x,
    y = y,
    weights = wts,
    family = family,
    lambda = lambda,
    intercept = TRUE
  )
  
  return(mod)
}


validate_lasso_stackedImps <- function(imputations, # as returned by "long"
                                       formula,
                                       wts, # var_name with weights
                                       n_folds = 10,
                                       lambda_choice = c("min", "1se"),
                                       B = 10) {
  
  # match arg lambda
  lambda_choice <- match.arg(lambda_choice)
  
  # Extract model matrix and response variable names
  response_var <- all.vars(stats::update(formula, . ~ 1))
  X_orig <- subset(x = stats::model.matrix(formula, data = imputations), select = -`(Intercept)`)
  y_orig <- imputations[[response_var]]
  wts_orig <- imputations[[wts]]
  
  # First apparent performance
  folds <- assign_crossval_folds(y = unique(imputations[[".id"]]), n_folds = n_folds)
  mod_orig <- run_lasso_glmnet(
    x = X_orig,
    y = y_orig,
    wts = wts_orig,
    foldid = folds[match(imputations[[".id"]], folds[["y"]]), "fold"],
    lambda_choice = lambda_choice
  )
  
  apparent_perform <- assess_performance(
    glmnet_model = mod_orig,
    new_x = X_orig, 
    new_y = y_orig,
    wts = wts_orig
  )
  
  # Bootstrap model frame to avoid calling model.matrix too many times
  prepped_dat <- cbind.data.frame(y_orig, ".id" = imputations[[".id"]], X_orig, wts_orig)
  colnames(prepped_dat)[which(colnames(prepped_dat) == c("y_orig", "wts_orig"))] <- c(response_var, wts)
  
  # Create bootstrap samples
  boot_dats <- group_resample(
    dat = prepped_dat,
    id_var = ".id",
    B = B, 
    boot_id_name = ".id_boot"
  )
  
  # Begin bootstraps (can do in parallel)
  bootstrap_valids <- furrr::future_map_dfr(
    .x = boot_dats,
    .id = "boot_num",
    .options = furrr_options(seed = TRUE),
    .f = ~ {
      
      # Same business
      y_boot <- .x[[response_var]]
      wts_boot <- .x[[wts]]
      folds_boot <- assign_crossval_folds(y = unique(.x[[".id_boot"]]), n_folds = n_folds)
      X_boot <- as.matrix(.x[, setdiff(colnames(.x), c(".id_boot", ".id", response_var, wts))])
      
      mod_boot <- run_lasso_glmnet(
        x = X_boot,
        y = y_boot,
        wts = wts_boot,
        foldid = folds_boot[match(.x[[".id_boot"]], folds_boot[["y"]]), "fold"],
        lambda_choice = lambda_choice
      )
      
      training_perform <- assess_performance(
        glmnet_model = mod_boot,
        new_x = X_boot, 
        new_y = y_boot,
        wts = wts_boot
      )
      
      # Test performance
      test_perform <- assess_performance(
        glmnet_model = mod_boot,
        new_x = X_orig, 
        new_y = y_orig,
        wts = wts_orig
      )
      
      # Performance
      res_boot <- cbind.data.frame(
        "measure" = training_perform[["measure"]],
        "apparent" = apparent_perform[["value"]],
        "training" = training_perform[["value"]],
        "test" = test_perform[["value"]],
        "optimism" = training_perform[["value"]] - test_perform[["value"]]
      )
      
      # Compute corrected measure
      res_boot[["corrected"]] = res_boot[["apparent"]] - res_boot[["optimism"]]
      return(res_boot)
    }
  )  
  
  # Summarise
  results <- bootstrap_valids %>%
    group_by(measure) %>% 
    dplyr::summarise(
      apparent = mean(apparent),
      training = mean(training),
      test = mean(test),
      optimism = mean(optimism),
      lower_corrected = quantile(x = corrected, probs = 0.025),
      upper_corrected = quantile(x = corrected, probs = 0.975),
      corrected = mean(corrected)
    ) %>% 
    mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
  
  # Return both model and internal validation procedure
  list_res <- list("model_fit" = mod_orig, "validation_summary" = results)
  return(list_res)
}


# To get CI 
boot_weighted_auc <- function(lp, y, weights = 1, B = 10) {
  
  # Prepare predictions to bootstrap
  auc_dat <- cbind.data.frame("lp" = drop(lp), "label" = drop(y), "wt" = drop(weights))
  
  # Get point estimate
  tpfp <- WeightedROC::WeightedROC(
    guess = auc_dat$lp, 
    label = auc_dat$label, 
    weight = auc_dat$wt
  )
  
  auc_est <- WeightedROC::WeightedAUC(tpfp)
  
  # Begin bootstraps
  aucs_boot <- replicate(
    n = B,
    expr = {
      inds <- sample(nrow(auc_dat), replace = TRUE)
      boot_dat <- auc_dat[inds, ]
      tpfp_boot <- WeightedROC::WeightedROC(
        guess = boot_dat$lp, 
        label = boot_dat$label, 
        weight = boot_dat$wt
      )
      WeightedROC::WeightedAUC(tpfp_boot)
    }
  )
  
  res <- c("auc" = auc_est, quantile(aucs_boot, probs = c(0.025, 0.975)))
  return(res)
}
