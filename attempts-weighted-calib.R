# To-do:
# - Check prop of complete cases
library(JointAI)
?plot_all
JointAI::plot_all(dat_combined, nrow = 3, ncol = 4)
JointAI::plot_all(dat_combined |> filter(dataset == "develop"), nrow = 3, ncol = 4)
JointAI::plot_all(dat_combined |> filter(dataset == "valid"), nrow = 3, ncol = 4)



imps_dev <- mice::parlmice(
  data = dat_develop,
  n.imp.core = 3,
  cluster.seed = 64545,
  method = set_mice_methods(dat_develop),
  predictorMatrix = create_mice_predmatrix(
    dat = dat_develop, 
    exclude_imp_models = "wts"
  ),
  n.core = 3,
  maxit = 20,
  cl.type = "PSOCK"
)

plot(imps_dev)
densityplot(imps_dev)
propplot(imps_dev)


# Validate validation imps
imps_valid <- mice::parlmice(
  data = dat_valid,
  n.imp.core = 4,
  cluster.seed = 468435,
  method = set_mice_methods(
    dat_valid, 
    skip_impute = c(vascular_vars_develop)
  ),
  predictorMatrix = create_mice_predmatrix(
    dat = dat_valid, 
    exclude_imp_models = c("wts", vascular_vars_develop)
  ),
  n.core = 3,
  maxit = 30,
  cl.type = "PSOCK"
)

plot(imps_valid, layout = c(2, 8))






imps_assess <- mice::parlmice(
  data = dat_assess,
  n.imp.core = 7,
  cluster.seed = 749864684,
  predictorMatrix = predmat_assess,
  method = set_mice_methods(
    dat = dat_assess, 
    skip_impute = vascular_vars_develop,
    method_adjust = c("Mc_FailedFemoralApproach" = "logreg")
  ),
  n.core = 3,
  maxit = 10,
  cl.type = "PSOCK",
  ignore = (dat_assess[["dataset"]] == "valid") # Important line
)

plot(imps_assess, layout = c(2, 8))
densityplot(imps_assess)
propplot(imps_assess)

howManyImputations::how_many_imputations(
  with(
    imps_assess,
    glm(Mc_FailedFemoralApproach ~ M_age + M_prev_ht + AorticVariant + 
          ArchElongation + AngleFollowingBifurcation + InnCca_nr_90orLarger + 
          ICAE_NASCET_Degree, family = binomial())
  ) |> 
    pool()
)



CalibrationCurves::val.prob.ci.2(
  y = as.numeric(y_dev) - 1,
  logit = predict(mod_develop, newx = X_dev), 
  smooth = "rcs",
  cuts = quantile(
    predict(mod_develop, newx = X_dev, type = "response"), 
    probs = seq(0, 1, by = 0.1) # plot deciles of risk
  ),
  group = imps_develop$.imp,
  weights = imps_develop$wts,
  dostats = FALSE,
  nr.knots = 4,
  xlim = c(0, 0.8), 
  legendloc = c(0, 0.95)
)


tpfp <- WeightedROC::WeightedROC(plogis(lp_valid), label = y_valid, weight = wts_valid)
WeightedROC::WeightedAUC(tpfp)



mod_lp <- rms::lrm.fit(
  y = as.numeric(y_valid) - 1, x = drop(lp_valid), weights = wts_valid
)


mod_lp2 <-  rms::lrm.fit(
  y = as.numeric(y_valid) - 1, 
  x = ns(drop(lp_valid), 3), 
  weights = wts_valid
)

df_test <- cbind.data.frame("y_valid" = as.numeric(y_valid) - 1,
                            "lp_valid" = drop(lp_valid),
                            "wts_valid" = wts_valid)
mod_lp3 <- lrm(y_valid ~ ns(lp_valid, 3), data = df_test, x = TRUE, weights = wts_valid)
L <- predict(mod_lp3, se.fit=TRUE) 
CIs <- plogis(with(L, linear.predictors + 1.96*cbind(-se.fit,se.fit)))
plot(plogis(lp_valid), plogis(L$linear.predictors), type = "p", xlim = c(0, 0.8),
     ylim = c(0, 1))
abline(c(0, 1))
points(plogis(lp_valid), CIs[, 1])
points(plogis(lp_valid), CIs[, 2])
