##**************************##
## Post-imputation analyses ##
##**************************##

# Set contrasts for ordered factors
options(contrasts = rep("contr.treatment", 2))
source("R/helper-functions.R")
set.seed(2022)


candidate_predictors <- c(
  "M_age",
  "M_prev_ht",
  "AorticVariant",
  "ArchElongation",
  "AngleFollowingBifurcation",
  "InnCca_nr_90orLarger",
  "ICAE_NASCET_Degree"
)


# Univariate analyses -----------------------------------------------------


# Read-in imputations
imps_dev <- readRDS("data/imps_dev.rds")
imps_valid <- readRDS("data/imps_valid.rds")


# Edit vars - make split in function later
imps_univariate_ORs(
  outcome = "Mc_FailedFemoralApproach", 
  imps = imps_dev,
  predictors = candidate_predictors
)

imps_univariate_ORs(
  outcome = "Mc_FailedFemoralApproach", 
  imps = imps_valid,
  predictors = candidate_predictors
)



# Rest here.. -------------------------------------------------------------



