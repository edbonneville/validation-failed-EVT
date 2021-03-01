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
  "future",
  "furrr",
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

# Check which from develop are not in valid
setdiff(names(develop), names(valid))


# Combine datasets and save -----------------------------------------------


# Combine datasets
combined <- bind_rows(develop, valid, .id = "dataset") %>% 
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
var_labs <- attr(develop, "variable.labels")[names(combined)]
attr(combined, "variable.labels") <- var_labs

# Save it
saveRDS(combined, file = "data/combined-data_processed.rds")

# To-do POST imputation:
#- age to decades


