##**************************##
## Post-imputation analyses ##
##**************************##


# Set contrasts for ordered factors
options(contrasts = rep("contr.treatment", 2))
source("analysis/helper-functions.R")

# To reproduce all bootstraps
set.seed(1202)

candidate_predictors <- c(
  "M_age",
  "M_prev_ht",
  "AorticVariant",
  "ArchElongation",
  "AngleFollowingBifurcation",
  "InnCca_nr_90orLarger",
  "ICAE_NASCET_Degree"
)

# General model formula - keep response character
form <- reformulate(termlabels = candidate_predictors, response = "Mc_FailedFemoralApproach")
response_var <- all.vars(stats::update(form, . ~ 1))


# Prepare variables  ------------------------------------------------------


# Prepare to read-in imputations
imps_files <- list.files(path = "data/", pattern = "^imps", full.names = TRUE)
names(imps_files) <- gsub(x = imps_files, pattern = "^data/|\\.rds$", replacement = "")

# Read all in together to transform variables at once (Julia's code)
imps_all <- imps_files %>% 
  map(.f = ~ mice::complete(readRDS(.x), action = "long")) %>% 
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


# Univariate analyses -----------------------------------------------------


# Do this for all and make table
univariate_analyses <- imps_all %>% 
  
  # Only use separately imputed develop, valid and combined cohorts
  filter(imps_label != "imps_assess") %>% 
  
  # Split each of three cohorts, and run analyses on each
  split(f = .$imps_label) %>% 
  map(.f = ~ {
    imps_univariate_ORs(
      outcome = "Mc_FailedFemoralApproach", 
      imps = split(.x, .x$.imp),
      predictors = c(
        candidate_predictors, 
        "ICA_nr_90orLarger",
        "OrigoStenosis50percent",
        "ICAIAtherosclerosis",
        "AngleAaInn_OR_AaCca"
      )
    )
  }) %>% 
  bind_rows(.id = "cohort") 

# Make the table
univariate_table <- univariate_analyses %>% 
  select(cohort, term, estimate, `2.5 %`, `97.5 %`) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>% 
  mutate(
    OR = paste0(estimate, " [", `2.5 %`, "-", `97.5 %`, "]"),
    cohort = factor(
      cohort, 
      levels = c("imps_dev", "imps_valid", "imps_combined"),
      labels = c("develop_cohort", "validation_cohort", "combined_cohort")
    )
  ) %>% 
  
  # Spread and set column order
  pivot_wider(names_from = cohort, values_from = OR, id_cols = term) %>% 
  select(term, develop_cohort, validation_cohort, combined_cohort)

univariate_table


# Cohort interactions (Should probably be ommitted from manuscript)
dat_interactions <- imps_all %>% 
  filter(imps_label %in% c("imps_dev", "imps_valid"))

interactions_table <- imps_univariate_ORs(
  outcome = "Mc_FailedFemoralApproach",
  imps = split(x = dat_interactions, f = dat_interactions$.imp),
  predictors = paste0(candidate_predictors, " * imps_label") # imp_label is surrogate for cohort
) %>% 
  
  # Keep only interaction terms - keep  p-values
  filter(grepl(x = term, pattern = "\\:")) %>% 
  select(term, p.value)

interactions_table



# Model development + validation (separate cohorts) -----------------------


# Development imputation
imps_develop <- imps_all %>% 
  filter(imps_label == "imps_assess" & dataset == "develop")

# For parallel
#future::plan(future::sequential)
future::plan(future::multisession, workers = 3)

# Fit and internal validation
fit_validation_develop <- validate_lasso_stackedImps(
  imputations = imps_develop,
  formula = form, wts = "wts", 
  n_folds = 10,
  lambda_choice = "min",
  B = 5
)

# Save this
saveRDS(fit_validation_develop, file = "data/validation_develop.rds")

# Internal validation summary
fit_validation_develop$validation_summary

# Model to externally validate
mod_develop <- fit_validation_develop$model_fit

# Get coefficients (table 4)
coef(mod_develop)

# Make calibration plot
X_dev <- subset(x = stats::model.matrix(form, data = imps_develop), select = -`(Intercept)`)
y_dev <- imps_develop[[response_var]]


val.prob.ci.2(
  y = as.numeric(y_dev) - 1,
  logit = predict(mod_develop, newx = X_dev), 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_develop, newx = X_dev, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  dostats = FALSE, 
  xlim = c(0, 0.8), 
  legendloc = c(0, 0.95)
)


# External validation --


# Prepare validation data
imps_valid <- imps_all %>% 
  filter(imps_label == "imps_assess" & dataset == "valid")

X_valid <- subset(x = stats::model.matrix(form, data = imps_valid), select = -`(Intercept)`)
y_valid <- imps_valid[[response_var]]
wts_valid <- imps_valid[["wts"]]

# Compute linear predictor
lp_valid <- predict(mod_develop, newx = X_valid)

# Assess external performance (with glmnet function)
performance_valid <- glmnet::assess.glmnet(
  object = mod_develop,
  newx = X_valid,
  newy = y_valid,
  weights = wts_valid, 
  family = "binomial"
)

performance_valid$auc

# AUC curves
tpfp <- WeightedROC::WeightedROC(lp_valid, label = y_valid, weight = wts_valid)
WeightedROC::WeightedAUC(tpfp)# same as glmnet
tpfp %>%
  ggplot() + 
  geom_path(aes(FPR, TPR)) + 
  coord_equal()

# Get CI for external AUC by bootstrap
boot_weighted_auc(lp_valid, y_valid, wts_valid, B = 200)

# Assess external calibration
calibration_intercept_slope(y_valid, lp_valid, wts = wts_valid)

# If we want confidence intervals
mod_lp <- rms::lrm.fit(y = y_valid, x = drop(lp_valid), weights = wts_valid)
mod_offset <- rms::lrm.fit(y = y_valid, offset = drop(lp_valid), weights = wts_valid)

# Slope with CI
cbind(coef(mod_lp), confint(mod_lp))[-1, ]

# Intercept with CI
cbind(coef(mod_offset), confint(mod_offset))


# RCS val.prob
val.prob.ci.2(
  y = as.numeric(y_valid) - 1,
  logit = lp_valid, 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_develop, newx = X_valid, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  dostats = FALSE, 
  xlim = c(0, 0.8), 
  legendloc = c(0, 0.95)
)


# Combined cohort development + interval valid ----------------------------


# Prepare combined data
imps_combined <- imps_all %>% 
  filter(imps_label == "imps_combined")

fit_validation_combined <- validate_lasso_stackedImps(
  imputations = imps_combined,
  formula = form, 
  wts = "wts", 
  n_folds = 10,
  lambda_choice = "min",
  B = 5
)

# Save this
saveRDS(fit_validation_combined, file = "data/validation_combined.rds")

# Summary measures
fit_validation_combined$validation_summary

# Calibration plot
X_comb <- subset(x = stats::model.matrix(form, data = imps_combined), select = -`(Intercept)`)
y_comb <- imps_combined[[response_var]]
mod_comb <- fit_validation_combined$model_fit
coef(mod_comb)

# Compute linear predictor
lp_combined <- predict(mod_comb, newx = X_comb)

# RCS val.prob
val.prob.ci.2(
  y = as.numeric(y_comb) - 1,
  logit = lp_combined, 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_comb, newx = X_comb, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  dostats = FALSE, 
  xlim = c(0, 0.525), 
  legendloc = c(0, 0.95)
)


# Try nomogram
coefs_combined <- drop(coef(mod_comb))

# Keep selected 
mod_lrm <- lrm(form, data = imps_combined %>% select(all_of(c(response_var, candidate_predictors))))
mod_lrm$coefficients <- coefs_combined

coef(mod_lrm)

dd <- datadist(imps_combined %>% select(all_of(c(response_var, candidate_predictors)))); 
options(datadist = "dd")
par(mar=c(2,2,1,1))
nom <- nomogram(
  fit = mod_lrm, 
  fun = plogis, 
  maxscale = 10, 
  funlabel = "Predicted probability", 
  lp = FALSE, 
  fun.at = c(.001,.01,.05, seq(.1,.9,by=.1), .95, .99, .999)
)

plot(nom)
print(nom)



# Extra test with weighted loess calibration ------------------------------

loess_resp <- as.numeric(imps_combined[[response_var]]) - 1
pred_probs <- predict(mod_comb, newx = X_comb, type = "response")

mod_loess <- loess(
  loess_resp ~ drop(pred_probs),
  weights = imps_combined[["wts"]],
  control = loess.control(surface = "direct")
)

cbind.data.frame(
  "predicted" = seq(0, 1, by = 0.01),
  "observed" = predict(mod_loess, seq(0, 1, by = 0.01))
) %>% 
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, col = "black") +
  geom_line() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))


# cbind.data.frame(
#   "predicted" = drop(pred_probs),
#   "observed" = predict(mod_loess, drop(pred_probs))
# ) %>% 
#   ggplot(aes(predicted, observed)) +
#   geom_abline(intercept = 0, slope = 1, col = "black") +
#   geom_line() +
#   coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

