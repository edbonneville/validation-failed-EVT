assign_crossval_folds <- function(y, n_folds) {
  n <- length(y)
  inds <- sample(x = rep(1:n_folds, ceiling(n / n_folds)), size = n)
  mat <- cbind.data.frame("y" = y, "fold" = inds)
  return(mat)
}

#' Calculate calibration intercept and slope
#' 
#' As per van Calster
#' 
#' @param y response
#' @param lp linear predictor
#' @param wts optional weights
calibration_intercept_slope <- function(y, lp, wts = NULL) {
  
  # Check if any fractional weights
  family <- ifelse(any((wts %% 1) != 0), "quasibinomial", "binomial")
  
  # Fit models to linear predictors
  mod_lp <- stats::glm(y ~ lp, weights = wts, family = family)
  mod_offset <- stats::glm(y ~ offset(lp), weights = wts, family = family)
  result <- c("intercept" = coef(mod_offset)[["(Intercept)"]], "slope" = coef(mod_lp)[["lp"]])
  return(result)
}

assess_performance <- function(glmnet_model, # Already with a picked lambda
                               new_x,
                               new_y,
                               wts = NULL) {
  
  # Compute linear predictor
  lp <- glmnet::predict.glmnet(glmnet_model, newx = new_x)
  
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
