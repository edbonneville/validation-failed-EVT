##******************##
## Process raw data ##
##******************##


# Load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(
  "rms", # Validation and logistic regression modelling
  "foreign", # Reading-in data, editing labels
  "mice", # Imputation
  "glmnet", # LASSO
  "future", # For running bootstraps in parallel
  "furrr", # For running bootstraps in parallel
  "tidyverse", # Data manipulation/plotting
  "tableone" # For getting data of table one
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
  select(-InnCca_90orLarger, -ICAE_NASCET_99, -pretici_c, -occlsegment_c_short) %>%  
  
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

# Check which from develop are not in valid
setdiff(names(develop), names(valid))


# Combine datasets and save -----------------------------------------------


# Combine datasets
dat_combined <- bind_rows(develop, valid, .id = "dataset") %>% 
  mutate(
    dataset = factor(x = dataset, levels = c("1", "2"), labels = c("develop", "valid")),
    M_premrs = ifelse(M_premrs >= 1, 1, 0), # for table one
    M_mrs_rev = ifelse(M_mrs_rev >= 3, 1, 0)
  )  %>% 
  
  # Set ordered covariates (makes more sense than numeric to impute)
  mutate(
    ICAIAtherosclerosis = as.ordered(ICAIAtherosclerosis),
    M_collaterals = as.ordered(M_collaterals),
    InnCca_nr_90orLarger = as.ordered(InnCca_nr_90orLarger),
    ICA_nr_90orLarger = as.ordered(ICA_nr_90orLarger)
  ) %>% 
  
  # Collapse sparse categories
  mutate(
    ICA_nr_90orLarger = fct_collapse(
      .f = ICA_nr_90orLarger,
      "3" = c("3", "4", "5"),
      "2" = "2",
      "1" = "1"
    )
  )

# Keep variable labels
var_labs <- attr(develop, "variable.labels")[names(dat_combined)]
attr(dat_combined, "variable.labels") <- var_labs

# Save it
saveRDS(dat_combined, file = "data/combined-data_processed.rds")

# To-do POST imputation:
#- age to decades


# Descriptives ------------------------------------------------------------


# Exclude vars not in table one
vars_exclude_table <- c("sidescored", "AngleCcaIca", "ICAEAtherosclerosis", "StudySubjectID")

# Gives you everything for tables 1 and 2 (warnings due to variables not in validation, ignore!)
table_one <- dat_combined %>%  
  select(-all_of(vars_exclude_table)) %>% 
  
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
