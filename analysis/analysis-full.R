##***********************##
## Descriptives (tables) ##
##***********************##


# Set contrasts for ordered factors
options(contrasts = rep("contr.treatment", 2))
source("R/helper-functions.R")
set.seed(1984)
#... source other things too

# Read-in
dat_combined <- readRDS("data/combined-data_processed.rds")

# Make df here with var labels..

# Exclude vars not in table one
vars_exclude_table <- c("sidescored", "AngleCcaIca", "ICAEAtherosclerosis", "StudySubjectID")

# Gives you everything for tables 1 and 2 (warnings due to variables not in validation, ignore!)
table_one <- dat_combined[, !(colnames(dat_combined) %in% vars_exclude_table)] %>% 
  
  # Here we dichotomise purely for the table, does not affect original data
  mutate(
    ICA_nr_90orLarger_geq1 = factor(ifelse(ICA_nr_90orLarger >= 1, 1, 0)),
    ICA_nr_90orLarger_geq2 = factor(ifelse(ICA_nr_90orLarger >= 2, 1, 0)),
    ICAE_NASCET_99 = factor(ifelse(ICAE_NASCET_Degree < 99, 0,1)),
    InnCca_90orLarger_geq1 = factor(ifelse(InnCca_nr_90orLarger >= 1, 1, 0)),
    InnCca_90orLarger_geq2 = factor(ifelse(InnCca_nr_90orLarger >= 2, 1, 0)),
    ICAI_stenosis50 = factor(ifelse(ICAIAtherosclerosis == "Yes, >=50% stenosis", 1, 0)),
    AngleAaInn_OR_AaCca_dich45 = factor(ifelse(AngleAaInn_OR_AaCca > 45, 0, 1))
  ) %>% 
  CreateTableOne(data = ., strata = "dataset")

# Summaries of all continuous variables
print(table_one$ContTable, nonnormal = TRUE)

# Summaries of all categorical variables
print(table_one$CatTable)


# Imputation prep ---------------------------------------------------------


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
dat_to_impute <- dat_combined[, !(colnames(dat_combined) %in% vars_exclude)]

# We already compute weights based on candidate predictors (for stacked data)
m <- 15 # number of imputed datasets
candidate_predictors <- c(
  "M_age",
  "M_prev_ht",
  "AorticVariant",
  "ArchElongation",
  "AngleFollowingBifurcation",
  "InnCca_nr_90orLarger",
  "ICAE_NASCET_Degree"
)

# Get weights (check if also depends on auxiliary vars)
dat_to_impute[["wts"]] <- (1 - rowMeans(is.na(dat_to_impute[, candidate_predictors]))) / m

# Now three rounds of imputation:
# 1. Imputation of development and validation separately for univariate assessments 
# (vascular characteristics not available at validation) - this could eventually be used for 
# validation (but not recommended)
# 2. Imputation of development + validation using imputation models fitted on development data,
# and without outcome in validation. This is for performance assessment
# 3. Imputation of development + validation using imputation models fitted on COMBINED data,
# this will be for the final combined model (to be used in practise)
# (Possible 4.) Could impute validation set entirely

# Visualise missings beforehand
naniar::gg_miss_var(dat_to_impute, facet = dataset, show_pct = TRUE)


# Imputation round one ----------------------------------------------------


dat_develop <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "develop"))
dat_valid <- subset(x = dat_to_impute, select = -dataset, subset = (dataset == "valid"))

# Run in parallel - number of imputations is n.core * n.imp.core
imps_dev <- mice::parlmice(
  data = dat_develop,
  n.imp.core = 1,
  cluster.seed = 2020,
  predictorMatrix = create_mice_predmatrix(dat = dat_develop, exclude_imp_models = "wts"),
  n.core = 3,
  maxit = 5,
  cl.type = "PSOCK"
)

# Add function to transform imp datasets (pre-pooling) using Julia's script
# - age to decades
# - dichotomising rest...

imps_univariate_ORs(
  outcome = "Mc_FailedFemoralApproach", 
  imps = imps_dev,
  predictors = c(
    candidate_predictors,
    "OrigoStenosis50percent"
  )
)

# Same thing for validation


# Imputations round 2 -----------------------------------------------------


# Get names of unmeasured vars in valid (i.e. percent missing = 100)
vascular_vars_develop <- names(which(colMeans(is.na(dat_valid)) == 1))

# Artificially set outcome to NA - save "true" outcome
dat_to_impute <- dat_to_impute %>% 
  mutate(
    outcome_true = Mc_FailedFemoralApproach,
    Mc_FailedFemoralApproach = ifelse(dataset == "valid", NA_character_, as.character(Mc_FailedFemoralApproach)),
    Mc_FailedFemoralApproach = factor(Mc_FailedFemoralApproach, levels = c("success", "failure"))
  )

# Set predictor matrix (Mc_FailedFemoralApproach gets thrown out due to collinear with outcome true)
predmat_assess <- create_mice_predmatrix(
  dat = dat_to_impute, 
  exclude_imp_models = c("wts", "dataset", "outcome_true", vascular_vars_develop)
)

predmat_assess[, "Mc_FailedFemoralApproach"] <- 1
diag(predmat_assess) <- 0 


# Run imputations - about 3.5 minutes  (for 15 imps, 15 iters, all across 3 cors)
imps_assess <- mice::parlmice(
  data = dat_to_impute,
  n.imp.core = 5,
  cluster.seed = 2022,
  predictorMatrix = predmat_assess,
  method = set_mice_methods(
    dat = dat_to_impute, 
    skip_impute = vascular_vars_develop,
    method_adjust = c("Mc_FailedFemoralApproach" = "logreg")
  ),
  n.core = 3,
  maxit = 15,
  cl.type = "PSOCK",
  ignore = (dat_to_impute[["dataset"]] == "valid")
)

plot(imps_assess)

# Temporary save before final run
saveRDS(imps_assess, file = "data/imps_validation.rds")


# Validation --------------------------------------------------------------

imps_assess <- read_rds("data/imps_validation.rds")

# Prepare data (re-write of Julia's part)
imps_stack <- mice::complete(imps_assess, action = "long") %>% 
  mutate(
    ICAE_NASCET_Degree = factor(ifelse(ICAE_NASCET_Degree < 99, "leq_99", "geq_99"), levels = c("leq_99", "geq_99")),
    InnCca_nr_90orLarger = fct_collapse(.f = InnCca_nr_90orLarger,"0" = "0", "geq_1" = c("1", "2", "3")),
    Mc_FailedFemoralApproach = outcome_true, #  Set back to true values for validation
    M_age = M_age / 10 # Change to decades
  ) %>% 
  select(
    all_of(c("dataset", ".id", ".imp", "wts", "Mc_FailedFemoralApproach", candidate_predictors))
  ) 
  

imps_stack_develop <- subset(imps_stack, subset = (dataset == "develop"), select = -dataset)
imps_stack_valid <- subset(imps_stack, subset = (dataset == "valid"), select = -dataset)


# Try bootstrap of whole stack
form <- reformulate(termlabels = candidate_predictors, response = "Mc_FailedFemoralApproach")

modo <- lrm(form, data = imps_stack_develop,
            x= TRUE, y = TRUE)
#val.prob.ci.2(y = as.numeric(y_orig) -1, logit = lp, smooth = "rcs")


# Internal validation (stacked) -------------------------------------------

plan(future::multisession, workers = 3)

# Maybe return CIs after?
test <- validate_lasso_stackedImps(
  imputations = imps_stack_develop,
  formula = form,
  wts = "wts", 
  n_folds = 10,
  lambda_choice = "min", 
  B = 15 # 200 later
)

test

plan(future::sequential)


# Externally validate 
response_var <- all.vars(stats::update(form, . ~ 1))
X_orig <- subset(x = stats::model.matrix(form, data = imps_stack_develop), select = -`(Intercept)`)
y_orig <- imps_stack_develop[[response_var]]
wts_orig <- imps_stack_develop[["wts"]]

# First apparent performance
folds <- assign_crossval_folds(y = unique(imps_stack_develop[[".id"]]), n_folds = 10)
mod_orig <- run_lasso_glmnet(
  x = X_orig,
  y = y_orig,
  wts = wts_orig,
  foldid = folds[match(imps_stack_develop[[".id"]], folds[["y"]]), "fold"],
  lambda_choice = "min"
)

apparent_perform <- assess_performance(
  glmnet_model = mod_orig,
  new_x = X_orig, 
  new_y = y_orig,
  wts = wts_orig
)

# Internal calibration plot
val.prob.ci.2(
  y = as.numeric(y_orig) - 1,
  logit = predict(mod_orig, X_orig),
  smooth = "rcs", 
  dostats = FALSE
)

val.prob.ci.2(
  y = as.numeric(y_orig) - 1,
  logit = predict(mod_orig, X_orig), 
  smooth = "rcs",
  cuts = quantile(predict(mod_orig, X_orig, type = "response"), 
                  probs = seq(0, 1, length.out = 10)),
  dostats = FALSE
)


# External valid ----------------------------------------------------------

# Calibration external
X_valid <- subset(x = stats::model.matrix(form, data = imps_stack_valid), select = -`(Intercept)`)
y_valid <- imps_stack_valid[[response_var]]
wts_valid <- imps_stack_valid[["wts"]]


valid_perform <- assess_performance(
  glmnet_model = mod_orig,
  new_x = X_valid, 
  new_y = y_valid,
  wts = wts_valid
)

glm(y_valid ~ predict(mod_orig, X_valid), family = "binomial")

val.prob.ci.2(
  y = as.numeric(y_valid) - 1,
  logit = predict(mod_orig, X_valid), #-1.0744 + 0.5453 * predict(mod_orig, X_valid),
  smooth = "rcs", 
  cuts = quantile(predict(mod_orig, X_valid, type = "response"), 
                  probs = seq(0, 1, length.out = 10)),
  dostats = FALSE
)

library(psfmi)

# https://www.nature.com/articles/s41398-020-01172-y.pdf - good reporting
imps_stack_develop %>% 
  mutate(
    y = as.numeric(Mc_FailedFemoralApproach) - 1,
    preds = predict(mod_orig, X_orig, type = "response")
  ) %>% 
  mutate(group = ntile(x = preds, n = 10)) %>% 
  group_by(group) %>% 
  summarise(
    observed = mean(y),
    predicted = mean(preds)
  ) %>% 
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, col = "black") +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

dat_loess <- imps_stack_develop %>% 
  mutate(
    y = as.numeric(Mc_FailedFemoralApproach) - 1,
    preds = predict(mod_orig, X_orig, type = "response")
  ) %>% 
  mutate(group = ntile(x = preds, n = 20)) %>% 
  group_by(group) %>% 
  summarise(
    observed = mean(y),
    predicted = mean(preds)
  )
  
mod_loess <- loess(observed ~ predicted, data = dat_loess, 
                   control = loess.control(surface = "direct"))

predict(mod_loess, newdata =  data.frame("predicted" = c(dat_loess$predicted, seq(0.3, 1, by = 0.01))))

cbind.data.frame(
  "predicted" = c(dat_loess$predicted, seq(0.3, 1, by = 0.01)),
  "observed" = predict(mod_loess, newdata =  data.frame("predicted" = c(dat_loess$predicted, seq(0.3, 1, by = 0.01))))
) %>% 
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, col = "black") +
  geom_line() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))


cbind.data.frame(
  "predicted" = seq(0, 1, by = 0.1),
  "observed" = predict(mod_loess, newdata =  data.frame("predicted" = seq(0, 1, by = 0.1)))
) %>% 
  ggplot(aes(predicted, observed)) +
  geom_abline(intercept = 0, slope = 1, col = "black") +
  geom_line() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))


# See for reporting 
#https://www.bmj.com/content/bmj/366/bmj.l4293.full.pdf

# Imputations round 3 -----------------------------------------------------


# Everything together..




# Validation of model on combined dataset ---------------------------------



#Internally validate
# Recalibrate??


