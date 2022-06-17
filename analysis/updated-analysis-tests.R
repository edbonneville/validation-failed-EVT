# You actual knob, please FUCKING keep the next line!!!
# (or always add explicit contrasts to model matrix)
options(contrasts = rep("contr.treatment", 2))


source("data-raw/prepare_raw_data.R")
source("R/imputation-helpers.R")
source("R/model-validation-helpers.R")

# Minimal packages for now
library(targets)
library(glmnet)
library(tidyverse)

targets::tar_load(
  c(
    dat_to_impute,
    imps_all,
    candidate_predictors, 
    fit_validation_dev,
    #fit_validation_develop,
    model_formula,
    analysis_settings
  )
)


#
set.seed(targets::tar_meta(fit_validation_dev, seed)$seed)
test <- validate_lasso_stackedImps(
  imputations = dplyr::filter(imps_all, imps_label == "imps_assess" & dataset == "develop"),
  formula = model_formula,
  wts = "wts",
  n_folds = analysis_settings$n_folds,
  lambda_choice = "min", #"1se", # crazy results with 1se
  B = 2 #analysis_settings$B
)

all.equal(test$model_fit, fit_validation_dev$model_fit)
cbind(coef(test$model_fit), glmnet::coef.glmnet(fit_validation_dev$model_fit))

# 
dat <- dplyr::filter(imps_all, imps_label == "imps_assess" & dataset == "develop")
head(
  drop(as.numeric(coef(fit_validation_dev$model_fit)) %*% t(stats::model.matrix(model_formula, data = dat)))
)




# Model
dat <- dplyr::filter(imps_all, imps_label == "imps_assess" & dataset == "develop")
model <- fit_validation_dev$model_fit 
coef(model)
coef(glm(model_formula, data = dat, family = binomial()))
X_new <- subset(x = stats::model.matrix(model_formula, data = dat), select = -`(Intercept)`)
head(
  predict.glmnet(model, newx = X_new, type = "link")
)
head(
drop(as.numeric(coef(model)) %*% t(stats::model.matrix(model_formula, data = dat)))
)

#-- for actual code:
mod_orig <- fit_validation_dev$model_fit
imputations <- dplyr::filter(imps_all, imps_label == "imps_assess" & dataset == "develop")
formula <- model_formula
wts <- "wts"
#--


# Need to figure out mystery of cal slope/intercept apparent --------------

imps_develop <- imps_all %>% filter(imps_label == "imps_assess" & dataset == "develop")
response_var <- all.vars(stats::update(model_formula, . ~ 1))
X_orig <- subset(x = stats::model.matrix(model_formula, data = imps_develop), select = -`(Intercept)`)
y_orig <- imps_develop[[response_var]]

head(drop(glmnet::predict.glmnet(mod_orig, newx = X_orig)))


assess_performance(fit_validation_dev$model_fit, X_orig, y_orig)

lp2 <- drop(predict.glmnet(fit_validation_dev$model_fit, newx = X_orig))
intercept <- coef(fit_validation_dev$model_fit)[1, ]

#glm(y_orig ~ offset(lp), family = binomial())
glm(y_orig ~ offset(I(lp2 + intercept)), family = binomial())


# New part ----------------------------------------------------------------




imps_develop <- imps_all %>% filter(imps_label == "imps_assess" & dataset == "develop")
response_var <- all.vars(stats::update(model_formula, . ~ 1))

# Model to externally validate
mod_develop <- fit_validation_dev$model_fit

fit_validation_dev$validation_summary

fit_validation_dev$validation_df |> 
  filter(measure %in% c("auc", "intercept", "slope")) |> 
  ggplot(aes(x = corrected)) +
  geom_histogram() +
  facet_wrap(~ measure)

# Get coefficients (table 4) - this is LASSO coefficients on development data
coef(mod_develop)
mod_develop$lambda # selected penalt

# Make calibration plot
X_dev <- subset(x = stats::model.matrix(model_formula, data = imps_develop), select = -`(Intercept)`)
y_dev <- imps_develop[[response_var]]
lp_dev <- drop(predict(mod_develop, newx = X_dev))

glm(y_dev ~ offset(lp_dev), family = binomial())


df_calplot_develop <- cbind.data.frame(lp_dev, imps_develop)

# Check auc on stacked is more/less same auc on individuals
split(df_calplot_develop, ~ .imp) %>%
  map(.f = ~ {
    auc_obj <- pROC::auc(Mc_FailedFemoralApproach ~ lp_dev, data = .x)
    cbind.data.frame("auc" = as.numeric(auc_obj), "var_auc" = pROC::var(auc_obj))
  }) %>%
  bind_rows(.id = ".imp") %>%
  summarise(auc = mean(auc))

as.numeric(pROC::auc(Mc_FailedFemoralApproach ~ lp_dev, data = df_calplot_develop))

assess_performance(
  mod_orig, X_dev, df_calplot_develop$Mc_FailedFemoralApproach
)


# Knots = 4 ---------------------------------------------------------------

# Get sequence of predict
knots <- 4
pred_probs <- plogis(df_calplot_develop$lp_dev)
probs_grid <- seq(min(pred_probs), max(pred_probs), by = 0.01)
lp_grid <- qlogis(probs_grid)

# What do we do? plot all calib plots in one figure, with flexible on entire stacked
# ... as dark
mod_cal_stacked <- glm(
  formula = Mc_FailedFemoralApproach ~ ns(lp_dev, knots), 
  data = df_calplot_develop, 
  family = binomial()
)

predicted_stacked <- predict(mod_cal_stacked, 
                             newdata = data.frame("lp_dev" = lp_grid), type = "response")

split(df_calplot_develop, ~ .imp) %>%
  map(
    .f = ~ {
      mod_cal <- glm(
        formula = Mc_FailedFemoralApproach ~ ns(lp_dev, knots), 
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
  geom_line(aes(group = .imp), alpha = 0.8, col = "lightgray", size = 1) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major = element_blank()) + 
  geom_line(
    data = cbind.data.frame(
      "predicted" = probs_grid,
      "observed" = predict(mod_cal_stacked, newdata = data.frame("lp_dev" = lp_grid), type = "response")
    ),
    size = 2
  ) +
  geom_abline(intercept = 0, slope = 1, col = "red", size = 1, linetype = "dashed") 
  

# And cal intercept + slope
split(df_calplot_develop, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ lp_dev, family = binomial, data = .x)) |> 
  pool() |> 
  summary()

split(df_calplot_develop, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ offset(lp_dev), family = binomial, data = .x)) |> 
  pool() |> 
  summary()

glm(Mc_FailedFemoralApproach ~ offset(lp_dev), family = binomial(), data = df_calplot_develop)

assess_performance(
  mod_develop,
  X_dev,
  df_calplot_develop$Mc_FailedFemoralApproach
)


# Lets check out external validation -------------------------------------



imps_valid <- imps_all %>% 
  filter(imps_label == "imps_assess" & dataset == "valid")

X_valid <- subset(x = stats::model.matrix(model_formula, data = imps_valid), select = -`(Intercept)`)
y_valid <- imps_valid[[response_var]]

# Compute linear predictor
lp_valid <- drop(predict(mod_develop, newx = X_valid))
df_calplot_valid <- cbind.data.frame(lp_valid, imps_valid)

# Pool AUC
auc_df <- split(df_calplot_valid, ~ .imp) %>%
  map(.f = ~ {
    auc_obj <- pROC::auc(Mc_FailedFemoralApproach ~ lp_valid, data = .x)
    cbind.data.frame("auc" = as.numeric(auc_obj), "var_auc" = pROC::var(auc_obj))
  }) %>%
  bind_rows(.id = ".imp") 

psfmi::pool_auc(auc_df$auc, sqrt(auc_df$var_auc), nimp = max(as.numeric(auc_df$.imp)))
#as.numeric(pROC::auc(Mc_FailedFemoralApproach ~ lp_valid, data = df_calplot_valid))

# Now go calibration plot


# Get sequence of predict
knots <- 4
pred_probs <- plogis(df_calplot_valid$lp_valid)
probs_grid <- seq(min(pred_probs), max(pred_probs), by = 0.01)
lp_grid <- qlogis(probs_grid)

# What do we do? plot all calib plots in one figure, with flexible on entire stacked
# ... as dark
mod_cal_stacked <- glm(
  formula = Mc_FailedFemoralApproach ~ ns(lp_valid, knots), 
  data = df_calplot_valid, 
  family = binomial()
)

predicted_stacked <- predict(mod_cal_stacked, newdata = data.frame("lp_valid" = lp_grid), type = "response")

split(df_calplot_valid, ~ .imp) %>%
  map(
    .f = ~ {
      mod_cal <- glm(
        formula = Mc_FailedFemoralApproach ~ ns(lp_valid, knots), 
        data = .x, 
        family = binomial()
      )
      data.frame(
        "predicted" = probs_grid,
        "observed" = predict(mod_cal, newdata = data.frame("lp_valid" = lp_grid), type = "response"),
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
      "observed" = predict(mod_cal_stacked, newdata = data.frame("lp_valid" = lp_grid), type = "response")
    ),
    size = 2
  ) +
  lims(x = c(0, 0.8), y = c(0, 0.8))

# And cal intercept + slope
split(df_calplot_valid, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ lp_valid, family = binomial, data = .x)) |> 
  pool() |> 
  summary()

split(df_calplot_valid, ~ .imp) |> 
  map(.f = ~ glm(Mc_FailedFemoralApproach ~ offset(lp_valid), family = binomial, data = .x)) |> 
  pool() |> 
  summary()


# Check n imps

## 11 minutes for 20 imps and niter = 20
howManyImputations::how_many_imputations(
  with(
    tar_read(imps_assess),
    glm(Mc_FailedFemoralApproach ~ M_age + M_prev_ht + AorticVariant + 
          ArchElongation + AngleFollowingBifurcation + InnCca_nr_90orLarger + 
          ICAE_NASCET_Degree, family = binomial())
  ) |> 
    pool()
)

plot(imps_assess,layout = c(2, 8))
