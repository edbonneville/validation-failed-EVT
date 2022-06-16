# Testing new analyses
tar_load(
  c(
    dat_to_impute,
    imps_all,
    candidate_predictors
  )
)

source("data-raw/prepare_raw_data.R")
source("R/imputation-helpers.R")
source("R/model-validation-helpers.R")

# Development imputation
imps_develop <- imps_all %>% 
  filter(imps_label == "imps_assess" & dataset == "develop")

# General model formula - keep response character
form <- reformulate(termlabels = candidate_predictors, response = "Mc_FailedFemoralApproach")
response_var <- all.vars(stats::update(form, . ~ 1))
form


fit_validation_develop <- validate_lasso_stackedImps(
  imputations = imps_develop,
  formula = form,
  wts = "wts",
  n_folds = 10,
  lambda_choice = "min",
  B = 5
)
fit_validation_develop$validation_summary


# (figure out nested futures for bootstrap here..)


formula <- form
lambda_choice <- "min"
imputations <- imps_develop
wts <- "wts"
n_folds <- 10

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

lp <- drop(glmnet::predict.glmnet(mod_orig, newx = X_orig))

# Pooled calibration intercepts and slope
imps_develop$lp <- lp

# New calibrations and intercept slopes 
split(imps_develop, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ lp, family = binomial, data = .x)) |> 
  pool() |> 
  summary()

split(imps_develop, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ offset(lp), family = binomial, data = .x)) |> 
  pool() |> 
  summary()

calibration_intercept_slope(y = imps_develop$Mc_FailedFemoralApproach, lp = lp)

split(imps_develop, ~ .imp) |> 
  map(.f = ~ pROC::auc(formula = Mc_FailedFemoralApproach ~ lp, data = .x)) |> 
  unlist() |> 
  mean()

glmnet::assess.glmnet(
  object = mod_orig, 
  newx = X_orig, 
  newy = imps_develop$Mc_FailedFemoralApproach, 
  family = "binomial"#,
  #weights = wts_orig
)$auc

apparent_perform <- assess_performance(
 glmnet_model = mod_orig,
 new_x = X_orig,
 new_y = y_orig,
 wts = wts_orig
)




# New part ----------------------------------------------------------------


tar_load(
  c(
    dat_to_impute,
    imps_all,
    candidate_predictors, 
    fit_validation_develop,
    model_formula
  )
)

imps_develop <- imps_all %>% filter(imps_label == "imps_assess" & dataset == "develop")
response_var <- all.vars(stats::update(model_formula, . ~ 1))

# Model to externally validate
mod_develop <- fit_validation_develop$model_fit

# Get coefficients (table 4) - this is LASSO coefficients on development data
coef(mod_develop)
mod_develop$lambda # selected penalt

# Make calibration plot
X_dev <- subset(x = stats::model.matrix(model_formula, data = imps_develop), select = -`(Intercept)`)
y_dev <- imps_develop[[response_var]]
lp_dev <- drop(predict(mod_develop, newx = X_dev))

df_calplot_develop <- cbind.data.frame(lp_dev, imps_develop)

# Try first without accounting for uncertainty
library(splines)
mod_rcs_nopool <- lrm(
  Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
  data = df_calplot_develop, 
  x = TRUE # after check diff with weights..
)
L <- predict(mod_rcs_nopool, se.fit = TRUE) 
CIs <- plogis(with(L, linear.predictors + 1.96*cbind(-se.fit,se.fit)))
plot(plogis(lp_dev), plogis(L$linear.predictors), type = "p", xlim = c(0, 0.8),
     ylim = c(0, 1))
abline(c(0, 1))
points(plogis(lp_dev), CIs[, 1])
points(plogis(lp_dev), CIs[, 2])

# Second: with weights
mod_rcs_nopool_wts <- lrm(
  Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
  data = df_calplot_develop, 
  x = TRUE, # after check diff with weights..
  weights = df_calplot_develop$wts
)
L <- predict(mod_rcs_nopool_wts, se.fit = TRUE) 
CIs <- plogis(with(L, linear.predictors + 1.96*cbind(-se.fit,se.fit)))
plot(plogis(lp_dev), plogis(L$linear.predictors), type = "p", xlim = c(0, 0.8),
     ylim = c(0, 1))
abline(c(0, 1))
points(plogis(lp_dev), CIs[, 1])
points(plogis(lp_dev), CIs[, 2])


# Try now with pooling
library(splines)
split(df_calplot_develop, ~ .imp) |> 
  map(
    .f = ~ glm(
      Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
      data = .x, 
      family = binomial() # after check diff with weights..
    )
  ) |> 
  pool() |> 
  summary()

mod_rcs_nopool_wts


mod_rcs_nopool <- lrm(
  Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
  data = df_calplot_develop, 
  x = TRUE # after check diff with weights..
)
L <- predict(mod_rcs_nopool, se.fit = TRUE) 
CIs <- plogis(with(L, linear.predictors + 1.96*cbind(-se.fit,se.fit)))
plot(plogis(lp_dev), plogis(L$linear.predictors), type = "p", xlim = c(0, 0.8),
     ylim = c(0, 1))
abline(c(0, 1))
points(plogis(lp_dev), CIs[, 1])
points(plogis(lp_dev), CIs[, 2])





# Knots = 4 ---------------------------------------------------------------

# Get sequence of predict
knots <- 4
pred_probs <- plogis(df_calplot_develop$lp_dev)
probs_grid <- seq(min(pred_probs), max(pred_probs), by = 0.01)
lp_grid <- qlogis(probs_grid)

# What do we do? plot all calib plots in one figure, with flexible on entire stacked
# ... as dark
mod_cal_stacked <- glm(
  formula = Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
  data = df_calplot_develop, 
  family = binomial()
)

predicted_stacked <- predict(mod_cal_stacked, newdata = data.frame("lp_dev" = lp_grid), type = "response")

split(df_calplot_develop, ~ .imp) %>%
  map(
    .f = ~ {
      mod_cal <- glm(
        formula = Mc_FailedFemoralApproach ~ ns(lp_dev, 4), 
        data = .x, 
        family = binomial()
      )
      data.frame(
        "predicted" = probs_grid,
        "observed" = predict(mod_cal, newdata = data.frame("lp_dev" = lp_grid), type = "response"),
        row.names = NULL
      )
    }
  ) %>%
  bind_rows(.id = ".imp") %>%
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, col = "red", size = 1) +
  geom_line(aes(group = .imp), alpha = 0.5) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank()) + 
  geom_line(
    data = cbind.data.frame(
      "predicted" = probs_grid,
      "observed" = predict(mod_cal_stacked, newdata = data.frame("lp_dev" = lp_grid), type = "response")
    ),
    size = 2
  )



# Lets check out external validation -------------------------------------



imps_valid <- imps_all %>% 
  filter(imps_label == "imps_assess" & dataset == "valid")

X_valid <- subset(x = stats::model.matrix(form, data = imps_valid), select = -`(Intercept)`)
y_valid <- imps_valid[[response_var]]
wts_valid <- imps_valid[["wts"]]

# Compute linear predictor
lp_valid <- predict(mod_develop, newx = X_valid)


