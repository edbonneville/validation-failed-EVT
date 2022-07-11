calplot_MI <- function(imps_long,
                       impdat_ind = ".imp",
                       model,
                       model_formula,
                       height_hist = 0.25,
                       startpos_hist = -0.25,
                       knots = 4,
                       n_bins = 50,
                       xlim = c(0, 1),
                       ylim = c(0, 1)) {
  
  response_var <- all.vars(stats::update(model_formula, . ~ 1))
  X <- subset(x = stats::model.matrix(model_formula, data = imps_long), select = -`(Intercept)`)
  y <- imps_long[[response_var]]
  lp <- drop(predict(model, newx = X, type = "link")) # linear predictor
  
  # Make df for calibration
  df_calplot <- cbind.data.frame(lp, imps_long)
  
  # Get limits of predicted probabilities across all imputed datasets
  pred_probs <- plogis(df_calplot$lp) 
  probs_grid <- seq(min(pred_probs), max(pred_probs), by = 0.005)
  lp_grid <- qlogis(probs_grid)
  
  # Plot deciles predicted risks
  risk_deciles <- quantile(pred_probs, probs = seq(0, 1, by = 0.1))
  risk_groups <- cut(pred_probs, breaks = risk_deciles, include.lowest = T)
  
  # Scale the wts
  scaled_wts <- imps_long$wts / sum(imps_long$wts)
  
  # https://stats.stackexchange.com/questions/220156/how-to-combine-pool-binomial-confidence-intervals-after-multiple-imputation 
  #https://github.com/mwheymans/miceafter/blob/main/R/pool_prop_wilson.R
  
  # (Also see https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Standard_error_of_a_proportion_estimation_when_using_weighted_data)
  # This is wrong!!
  risk_pts <- cbind.data.frame(
    "predicted" = tapply(pred_probs, risk_groups, function(x) mean(x, na.rm = TRUE)),
    "observed" = tapply(as.numeric(y) - 1L, risk_groups, function(x) mean(x, na.rm = TRUE)),
    "sum_wts" = tapply(imps_long$wts, risk_groups, function(x) sum(x^2)), #sum((x / sum(x))^2)),
    "n" = as.numeric(table(risk_groups))
  )
  risk_pts$se <- with(risk_pts, sqrt(observed * (1 - observed) / n))
  risk_pts$lower <- with(risk_pts, observed - qnorm(0.975) * se)
  risk_pts$upper <- with(risk_pts, observed + qnorm(0.975) * se)
  
  # The "average" smooth calibration is simply the one fitted on stacked data
  smooth_form <- reformulate(response = response_var, termlabels = paste0("splines::ns(lp, ", knots, ")"))
  mod_cal_stacked <- glm(formula = smooth_form, data = df_calplot, family = binomial(),#)#,
                         weights = df_calplot$wts)
  stacked_df <- data.frame(
    "predicted" = probs_grid,
    "observed" = predict(mod_cal_stacked, newdata = data.frame("lp" = lp_grid), type = "response") - startpos_hist
  )
  
  # Now we want to assess calibration in each imputed dataset
  ls_impdats <- split(x = df_calplot, f = imps_long[[impdat_ind]])
  smooths_impdats <- lapply(ls_impdats, function(impdat) {
    mod_cal <- glm(formula = smooth_form, data = impdat, family = binomial())
    data.frame(
      "predicted" = probs_grid,
      "observed" = predict(mod_cal, newdata = data.frame("lp" = lp_grid), type = "response") - startpos_hist,
      row.names = NULL
    )
  })
  
  smooths_df <- bind_rows(smooths_impdats, .id = impdat_ind)
  
  # Start plot
  p <- ggplot() +
    geom_line(
      data = smooths_df,
      aes(x = predicted, y = observed, group = .data[[impdat_ind]]),
      alpha = 0.8, 
      col = "lightgray",
      size = 1
    ) +
    geom_line(
      data = stacked_df,
      aes(x = predicted, y = observed),
      size = 2
    ) +
    geom_abline(
      intercept = 0 - startpos_hist,
      slope = 1, col = "red", size = 1, linetype = "dashed"
    ) +
    geom_histogram(
      data = data.frame("predicted" = plogis(df_calplot$lp)),
      aes(x = predicted, y = stat(height_hist * count / max(count))),
      bins = n_bins
    ) +
    scale_y_continuous(
      labels = function(y) y + startpos_hist, 
      breaks = c(0, 0.25, 0.5, 0.75, 1) - startpos_hist
    ) +
    coord_cartesian(
      expand = 0, 
      xlim = xlim,
      ylim = c(ylim[1] - startpos_hist, ylim[2] - startpos_hist),
    )
  
  p +
    geom_point(
      data = risk_pts,
      aes(predicted, observed - startpos_hist),
      size = 2,
      col = "blue", 
      shape = 2
    ) 
  # p +
  #   geom_linerange(
  #     data = risk_pts,
  #     aes(predicted, observed - startpos_hist,
  #         ymin = lower - startpos_hist, ymax = upper - startpos_hist),
  #     size = 1
  #   )
  
  #. Plot also deciles/quintiles of predicted/observed as in val.prob.ci2?
  
  #return(p)
}

# See https://stackoverflow.com/questions/35324892/ggplot2-setting-geom-bar-baseline-to-1-instead-of-zero

