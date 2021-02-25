##******************##
## Process raw data ##
##******************##


if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(
  "rms", # Validation and logistic regression modelling
  "foreign", # Reading-in data, editing labels
  "mice", # Imputation
  "glmnet", # LASSO
  "tidyverse" # Data manipulation/plotting
)


# Read-in raw data --------------------------------------------------------


develop_raw <- foreign::read.spss(
  file = "data-raw/RegistryPart1_N887.sav", 
  to.data.frame = TRUE, 
  use.value.labels = TRUE
)

valid_raw <- foreign::read.spss(
  file = "data-raw/RegistryPart2_N1111.sav", 
  to.data.frame = TRUE, 
  use.value.labels = TRUE
)



# Prepare development data ------------------------------------------------

develop <- develop_raw %>% 
  
  # Take care of variable labels
  mutate(
    Mc_FailedFemoralApproach = fct_recode(
      .f = Mc_FailedFemoralApproach, 
      "success" = "Successful Femoral Approach",
      "failure" = "Failed Femoral Approach" 
    ),
    Mc_pretici_short = fct_recode(
      .f = Mc_pretici_short,
      "2B, 2C or 3" = "2B - 3"
    )
  )
  
  

# Prepare validation data -------------------------------------------------


valid <- valid_raw %>% 
  
  # Remove vars in 'dropped' (bottom of excel)
  select(-c("InnCca_90orLarger", "ICAE_NASCET_99", "pretici_c", "occlsegment_c_short")) %>%  
  
  # Remove Incaa with 4 categories (only five pats have value of 4), keep 3 cat version
  select(-InnCca_nr_90orLarger) %>% 
  rename("InnCca_nr_90orLarger" = InnCca_nr_90orLarger_col) %>% 
  
  # Edit factor levels 
  mutate(
    Mc_FailedFemoralApproach = fct_recode(
      .f = Mc_FailedFemoralApproach, 
      "success" = "Successful",
      "failure" = "Failed" 
    ),
    AngleFollowingBifurcation = fct_recode(
      .f = AngleFollowingBifurcation,
      "No angle >= 90 degrees" = "no",
      "Yes, angle >=90 degrees" = "yes"
    )
  )


# Check which are not in both
setdiff(names(develop), names(valid))


combined <- bind_rows(develop, valid, .id = "dataset") %>% 
  mutate(
    dataset = factor(x = dataset, levels = c("1", "2"), labels = c("develop", "valid")),
    M_premrs = ifelse(M_premrs >= 1, 1, 0), # for table one
    M_mrs_rev = ifelse(M_mrs_rev >= 3, 1, 0)
  ) 
  

# To-do POST imputation:
#- age to decades




# Prepare training data ---------------------------------------------------


# Select variables to keep
subset_vars_train <- c(
  "Mc_FailedFemoralApproach", 
  "M_age", 
  "M_prev_ht", 
  "ArchElongation",
  "AngleAaInn_OR_AaCca_dich45", 
  "AorticVariant", 
  "InnCca_90orLarger",
  "ICA_90orLarger", 
  "AngleFollowingBifurcation", 
  "OrigoStenosis50percent",
  "ICAE_NASCET_Degree", # Recode to 99 after
  "ICAI_stenosis50"
)

train <- train_raw %>% 
  select(all_of(subset_vars_train)) %>% 
  
  # Rename variable names
  rename(
    failure_femoral_approach = Mc_FailedFemoralApproach,
    age = M_age,
    hypertension = M_prev_ht,
    arch_elongation = ArchElongation,
    acute_takeoff_angle = AngleAaInn_OR_AaCca_dich45,
    aortic_variant = AorticVariant,
    cca_geq90 = InnCca_90orLarger, 
    ica_geq90 = ICA_90orLarger,
    angle_post_bifurcation = AngleFollowingBifurcation,
    origosten_geq50 = OrigoStenosis50percent,
    ica_stenosis = ICAE_NASCET_Degree,
    intracran_stenosis_geq50 = ICAI_stenosis50
  ) %>% 
  
  # Prepare variables for combining with test set
  mutate(
    ica_stenosis_geq99 = ifelse(ica_stenosis >= 99, 1, 0),
    age = age / 10, # in decades
    aortic_variant = fct_collapse(aortic_variant, "Type A" = c("Type A", "Type D")),
    cca_geq90 = as.numeric(levels(cca_geq90))[cca_geq90] - 1,
    failure_femoral_approach = factor(failure_femoral_approach, levels = c(0, 1), labels = c("Successful", "Failed")),
    angle_post_bifurcation = fct_recode(angle_post_bifurcation, "no" = "No angle >= 90 degrees", "yes" = "Yes, angle >=90 degrees")
  )


# Prepare test data -------------------------------------------------------


subset_vars_test <- c(
  "Failed_TFA_NEW", 
  "age", 
  "prev_ht", 
  "ArchElongation", 
  "AorticVariant",
  "InnCca_90orLarger", 
  "AngleFollowingBifurcation", 
  "ICAE_NASCET_99"
)

test <- test_raw %>% 
  select(all_of(subset_vars_test)) %>% 
  rename(
    failure_femoral_approach = Failed_TFA_NEW,
    hypertension = prev_ht,
    arch_elongation = ArchElongation,
    aortic_variant = AorticVariant,
    cca_geq90 = InnCca_90orLarger,
    angle_post_bifurcation = AngleFollowingBifurcation,
    ica_stenosis_geq99 = ICAE_NASCET_99
  ) %>% 
  mutate(age = age / 10) # in decades



# Bind test and train -----------------------------------------------------


# Try with this for now
combined <- bind_rows(train, test, .id = "dataset") %>% 
  mutate(dataset = factor(dataset, levels = c(1, 2), labels = c("train", "test"))) %>% 
  select(-ica_stenosis)

# Save it
saveRDS(combined, file = "data/combined-data_processed.rds")
