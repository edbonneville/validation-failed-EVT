#' Combines development and validation cohort
#' 
#' Does this to harmonize variable codings,
#' and prepare factor variables for imputation
#' (specifying ordered or not)
#' 
prepare_raw_data <- function(develop_raw, valid_raw) {
  
  # Prepare development data
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
  
  # Prep validation data
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
  
  # Combine datasets
  dat_combined <- bind_rows(develop, valid, .id = "dataset") %>% 
    mutate(
      dataset = factor(x = dataset, levels = c("1", "2"), labels = c("develop", "valid")),
      M_premrs = ifelse(M_premrs >= 1, 1, 0), # For table one
      M_mrs_rev = ifelse(M_mrs_rev >= 3, 1, 0)
    )  %>% 
    
    # Set ordered covariates (makes more sense than numeric to impute)
    mutate(
      ICAIAtherosclerosis = as.ordered(ICAIAtherosclerosis),
      M_collaterals = as.ordered(M_collaterals),
      InnCca_nr_90orLarger = as.ordered(InnCca_nr_90orLarger),
      ICA_nr_90orLarger = as.ordered(ICA_nr_90orLarger),
      M_posttici_c = as.ordered(M_posttici_c)
    ) %>% 
    
    # Collapse sparse categories
    mutate(
      ICA_nr_90orLarger = fct_collapse(
        .f = ICA_nr_90orLarger,
        "3" = c("3", "4", "5"),
        "2" = "2",
        "1" = "1"
      ),
      StudySubjectID = factor(StudySubjectID),
      M_premrs = factor(M_premrs),
      M_mrs_rev = factor(M_mrs_rev)
    )
  
  # Keep variable labels
  var_labs <- attr(develop, "variable.labels")[names(dat_combined)]
  attr(dat_combined, "variable.labels") <- var_labs
  
  return(dat_combined)
}