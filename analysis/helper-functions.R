





# LASSO -------------------------------------------------------------------




# To get CI 
boot_weighted_auc <- function(lp, y, weights = 1, B = 10) {
  
  # Prepare predictions to bootstrap
  # This is exactly the same as bootsrapping validation set and
  # predicting in each
  auc_dat <- cbind.data.frame("lp" = drop(lp), "label" = drop(y), "wt" = drop(weights))
  
  # Get point estimate
  tpfp <- WeightedROC::WeightedROC(
    guess = auc_dat$lp, 
    label = auc_dat$label, 
    weight = auc_dat$wt
  )
  
  auc_est <- WeightedROC::WeightedAUC(tpfp)
  
  # Begin bootstraps
  aucs_boot <- replicate(
    n = B,
    expr = {
      inds <- sample(nrow(auc_dat), replace = TRUE)
      boot_dat <- auc_dat[inds, ]
      tpfp_boot <- WeightedROC::WeightedROC(
        guess = boot_dat$lp, 
        label = boot_dat$label, 
        weight = boot_dat$wt
      )
      WeightedROC::WeightedAUC(tpfp_boot)
    }
  )
  
  res <- c("auc" = auc_est, quantile(aucs_boot, probs = c(0.025, 0.975)))
  return(res)
}
