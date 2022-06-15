##**************************##
## Post-imputation analyses ##
##**************************##


# Run from start to finish - I have commented out code that should
# not be re-run.

# Set contrasts for ordered factors
options(contrasts = rep("contr.treatment", 2))

# Read-in custom functions for validation
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
form

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
#future::plan(future::multisession, workers = 3)

# Fit and internal validation - takes 10 minutes approx with 3 cores
# fit_validation_develop <- validate_lasso_stackedImps(
#   imputations = imps_develop,
#   formula = form, 
#   wts = "wts", 
#   n_folds = 10,
#   lambda_choice = "min",
#   B = 200 
# )

# Save this
#saveRDS(fit_validation_develop, file = "data/validation_develop.rds")

# Read in internal validation summary
fit_validation_develop <- readRDS("data/validation_develop.rds")

# Internal validation summary
fit_validation_develop$validation_summary

# Selecting only necessary elements
fit_validation_develop$validation_summary %>% 
  filter(measure %in% c("auc", "intercept", "slope")) %>% 
  group_by(measure) %>% 
  summarise(value = paste0(corrected, " [", lower_corrected, ";", upper_corrected, "]"))

# Model to externally validate
mod_develop <- fit_validation_develop$model_fit

# Get coefficients (table 4) - this is LASSO coefficients on development data
coef(mod_develop)
mod_develop$lambda # selected penalt

# Make calibration plot
X_dev <- subset(x = stats::model.matrix(form, data = imps_develop), select = -`(Intercept)`)
y_dev <- imps_develop[[response_var]]


dd <- datadist(imps_develop %>% select(all_of(c(response_var, candidate_predictors)))); 
options(datadist = "dd")

# CI probably too narrow?
CalibrationCurves::val.prob.ci.2(
  y = as.numeric(y_dev) - 1,
  logit = predict(mod_develop, newx = X_dev), 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_develop, newx = X_dev, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  dostats = FALSE,
  nr.knots = 4,
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
  coord_equal() +
  geom_abline(intercept = 0, slope = 1)

# Get CI for external AUC by bootstrap
# Probably set a seed here again
boot_weighted_auc(lp_valid, y_valid, wts_valid, B = 200)


# Try here with pROC
auc_df_valid <- cbind.data.frame(
  ".imp" = imps_valid$.imp,
  "y_valid" = as.numeric(imps_valid[[response_var]]) - 1,
  "preds_lp" = drop(predict(mod_develop, newx = X_valid))
)

# 

test <- by(data = auc_df_valid, INDICES = auc_df_valid$.imp, FUN = function(df) {
  obj <- pROC::auc(y_valid ~ preds_lp, data = df)
  cbind("auc" = obj, "se" = pROC::var(obj))
}, simplify = FALSE)
test
hi <- do.call(rbind.data.frame, test)
psfmi::pool_auc(hi$auc, hi$se, nimp = 15)

# Assess external calibration
calibration_intercept_slope(y_valid, lp_valid, wts = wts_valid)

# If we want confidence intervals
mod_lp <- rms::lrm.fit(y = y_valid, x = drop(lp_valid), weights = wts_valid)
mod_offset <- rms::lrm.fit(y = y_valid, offset = drop(lp_valid), weights = wts_valid)

# Slope with CI
cbind(coef(mod_lp), confint(mod_lp))[-1, ]

# Intercept with CI
cbind(coef(mod_offset), confint(mod_offset))


dd <- datadist(imps_valid %>% select(all_of(c(response_var, candidate_predictors)))); 
options(datadist = "dd")

# RCS val.prob
CalibrationCurves::val.prob.ci.2(
  y = as.numeric(y_valid) - 1,
  logit = lp_valid, 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_develop, newx = X_valid, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  nr.knots = 4,
  dostats = FALSE, 
  xlim = c(0, 0.8), 
  legendloc = c(0, 0.95)
)


# Combined cohort development + interval valid ----------------------------


# Prepare combined data
imps_combined <- imps_all %>% 
  filter(imps_label == "imps_combined")

# Takes 20-35 mins on 3 cores
# fit_validation_combined <- validate_lasso_stackedImps(
#   imputations = imps_combined,
#   formula = form, 
#   wts = "wts", 
#   n_folds = 10,
#   lambda_choice = "min",
#   B = 200
# )

# Save this
#saveRDS(fit_validation_combined, file = "data/validation_combined.rds")
fit_validation_combined <- readRDS("data/validation_combined.rds")

# Summary measures
fit_validation_combined$validation_summary

# Keep relevant parts
fit_validation_combined$validation_summary %>% 
  filter(measure %in% c("auc", "intercept", "slope")) %>% 
  group_by(measure) %>% 
  summarise(value = paste0(corrected, " [", lower_corrected, ";", upper_corrected, "]"))

# Calibration plot
X_comb <- subset(x = stats::model.matrix(form, data = imps_combined), select = -`(Intercept)`)
y_comb <- imps_combined[[response_var]]
mod_comb <- fit_validation_combined$model_fit
coef(mod_comb)

# Compute linear predictor
lp_combined <- predict(mod_comb, newx = X_comb)


dd <- datadist(imps_combined %>% select(all_of(c(response_var, candidate_predictors)))); 
options(datadist = "dd")

# Figure - and save
png(
  filename = "analysis/figures/calibration-plot-combined.png",
  width = 8,
  height = 5,
  units = "in",
  res = 300
)
val.prob.ci.2(
  y = as.numeric(y_comb) - 1,
  logit = lp_combined, 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_comb, newx = X_comb, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  dostats = FALSE, 
  nr.knots = 4,
  xlim = c(0, 0.6), # change to 0.6 
  legendloc = c(0, 0.95)
)
dev.off()


# Nomogram ----------------------------------------------------------------


# Try nomogram
coefs_combined <- drop(coef(mod_comb))

# Make data for nomogram labels
dat_nomogram <- imps_combined %>% 
  select(all_of(c(response_var, candidate_predictors))) %>% 
  mutate(
    InnCca_nr_90orLarger = factor(
      InnCca_nr_90orLarger,
      levels = levels(InnCca_nr_90orLarger),
      labels = c("0", ">=1")
    ),
    ICAE_NASCET_Degree = factor(
      ICAE_NASCET_Degree,
      levels = levels(ICAE_NASCET_Degree),
      labels = c("<99", ">=99")
    )
  )

# Set variable labels
Hmisc::label(dat_nomogram$M_age) <- "Age (decades)"
Hmisc::label(dat_nomogram$M_prev_ht) <- "Hypertension"
Hmisc::label(dat_nomogram$AorticVariant) <- "Aortic variant"
Hmisc::label(dat_nomogram$ArchElongation) <- "Arch Elongation"
Hmisc::label(dat_nomogram$AngleFollowingBifurcation) <- "Angle post-bifurcation"
Hmisc::label(dat_nomogram$InnCca_nr_90orLarger) <- "CCA angles >=90 degr."
Hmisc::label(dat_nomogram$ICAE_NASCET_Degree) <- "ICA stenosis"

dd <- datadist(dat_nomogram)
options(datadist = "dd")

# Edit some labels
mod_lrm <- lrm(form, data = dat_nomogram)
mod_lrm$coefficients <- setNames(unname(coefs_combined), names(mod_lrm$coefficients))


par(mar = c(2,2,1,1))
nom <- nomogram(
  fit = mod_lrm, 
  fun = plogis, 
  maxscale = 10, 
  funlabel = "Predicted probability", 
  lp = FALSE, 
  fun.at = c(.01, .05, seq(.1, 1, by = .1))
)

# Plot and save nomogram
png(
  filename = "analysis/figures/nomogram-combined.png",
  width = 8,
  height = 11,
  units = "in",
  res = 300
)
plot(nom)
dev.off()

# Of all patients in dataset, max probability of failure is 55%
max(plogis(lp_combined)) 

# Distribution of predicted risks (across all patients in all imputed datasets)
hist(
  x = plogis(lp_combined),
  breaks = 50,
  main = "Distribution of predicted failure risk in combined cohort"
)

# End parallel plan
#future::plan(future::sequential)


# END (Skip all below here)


# Visualising risk for elder patients -------------------------------------


# In 20s
dat_age_grps <- cbind.data.frame(
  imps_combined, 
  "pred" = plogis(drop(lp_combined))
) %>% 
  mutate(
    Mc_FailedFemoralApproach = as.numeric(Mc_FailedFemoralApproach) - 1,
    M_age_grp = cut_width(M_age * 10, width = 20, boundary = 18)
  )

dat_age_grps %>% 
  filter(M_age_grp != "(98,118]") %>% 
  ggplot(aes(x = pred)) +
  geom_histogram(bins = 20, fill = "lightblue", col = "black") +
  facet_wrap(~ M_age_grp, scales = "free") +
  theme_minimal() +
  labs(x = "Predicted risk of failure")
  


# Decision curves test ----------------------------------------------------


library(rmda)

dec_curv <- decision_curve(
  Mc_FailedFemoralApproach ~ pred,
  data = dat_age_grps, 
  policy = "opt-out", 
  fitted.risk = TRUE,
  bootstraps = 10
)

plot_decision_curve(
  dec_curv,
  legend.position = "bottomright",
  standardize = FALSE,
  curve.names = c("Model"),
  confidence.intervals = FALSE
)



# Make patient-specific predictions ---------------------------------------

# 
coef(mod_comb)

# Format for glmnet predicting - need a matrix
new_patient <- head(X_comb, 1)
new_patient
new_patient[1, "M_age"] <- 8 # 80 years old
new_patient[1, "M_prev_htYes"] <- 0 # no ht
new_patient[1, "AorticVariantType B"] <- 0 # Set to zero since if C, otherwise 1
new_patient[1, "AorticVariantType C"] <- 1 # Aortic variant C
new_patient[1, "ArchElongationII"] <- 0 
new_patient[1, "ArchElongationIII"] <- 1 # Arch elongation III
new_patient[1, "AngleFollowingBifurcationYes, angle >=90 degrees"] <- 1 # Angle >=90 
new_patient[1, "InnCca_nr_90orLargergeq_1"] <- 1 # number of CCA angles >= 1
new_patient[1, "ICAE_NASCET_Degreegeq_99"] <- 1 # ICA stenosis >= 99

# Given you are 80 years, if you have all other risk factors chance of failure is 58%
predict(mod_comb, newx = new_patient, type = "response")

# Only way you can get a higher risk now is by being older
# Let's see the probabilities for people 80-100, given all other risk factors are there
ages <- seq(8, 10, by = 0.1) # decades
ages
ages_probs <- sapply(ages, function(age) {
  new_pat <- new_patient
  new_pat[1, "M_age"] <- age
  predict(mod_comb, newx = new_pat, type = "response")
})
plot(ages, ages_probs, col = "blue")


# Example subgroup analysis (Over 80 years) -------------------------------


ind_sub <- which(imps_combined$M_age >= 8)
X_comb_sub <- X_comb[ind_sub, ]
y_comb_sub <- y_comb[ind_sub]

folds <- assign_crossval_folds(y = unique(imps_combined[ind_sub, ]$.id), n_folds = 10)
mod_comb_sub <- run_lasso_glmnet(
  x = X_comb_sub,
  y = y_comb_sub,
  wts = imps_combined[ind_sub, ]$wts,
  foldid = folds[match(imps_combined[ind_sub, ]$.id, folds[["y"]]), "fold"],
  lambda_choice = "min"
)

coef(mod_comb_sub)
coef(mod_comb)
predict(mod_comb_sub, newx = new_patient, type = "response")

