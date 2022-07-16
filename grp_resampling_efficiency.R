library(targets)
library(tidyverse)

tar_load(imps_all)

imps_combined <- imps_all %>% 
  filter(imps_label == "imps_combined")



# All functions -----------------------------------------------------------


# Original
group_resample <- function(dat, 
                           id_var, 
                           B = 50, 
                           boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids <- unique(dat[[id_var]])
  
  # Create bootstrap datasets
  boot_dats <- replicate(
    n = B,
    simplify = FALSE,
    expr = purrr::map_dfr(
      .x = sample(x = ids, replace = TRUE), 
      .f = ~ dat[which(dat[[id_var]] == .x), ], 
      .id = boot_id_name
    )
  )
  
  return(boot_dats)
}

# For now all one bootstrap sample
group_resample2 <- function(dat, 
                            id_var, 
                            B = 50, 
                            boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids_long <- dat[[id_var]]
  ids_unique <- unique(ids_long)
  
  # Get indicators
  inds_long <- lapply(
    X = sample(x = ids_unique, replace = TRUE), 
    function(x) which(ids_long == x)
  )
  
  dat[unlist(inds_long), ]
}

group_resample3 <- function(dat, 
                            id_var, 
                            B = 50, 
                            boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids_long <- dat[[id_var]]
  ids_unique <- unique(ids_long)
  
  # Sample, and prepare slices
  samps <- sample(x = ids_unique, replace = TRUE)
  inds_bigdat <- which(ids_long %in% samps)
  slices_ls <- split(inds_bigdat, ids_long[inds_bigdat])
  tbl <- table(samps)
  slices_repped <- rep(slices_ls, times = tbl[match(names(slices_ls), names(tbl))])
  
  dat[unlist(slices_repped), ]
}

group_resample3_ext <- function(dat, 
                                id_var, 
                                B = 50, 
                                boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids_long <- dat[[id_var]]
  ids_unique <- unique(ids_long)
  
  # Sample, and prepare slices
  boot_dats <- lapply(
    seq_len(B), function(b) {
      cat(b)
      samps <- sample(x = ids_unique, replace = TRUE)
      inds_bigdat <- which(ids_long %in% samps)
      slices_ls <- split(inds_bigdat, ids_long[inds_bigdat])
      tbl <- table(samps)
      slices_repped <- rep(slices_ls, times = tbl[match(names(slices_ls), names(tbl))])
      dat[unlist(slices_repped), ]
    }
  )
  
  return(boot_dats)
}

library(data.table)
group_resample_dt <- function(dat, 
                              id_var, 
                              B = 50, 
                              boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids_long <- dat[[id_var]]

  # Sample, and prepare slices
  samps <- sample(x = unique(ids_long), replace = TRUE)
  inds_bigdat <- which(ids_long %in% samps)
  df <- data.table("ind" = inds_bigdat, "ids" = ids_long[inds_bigdat])
  tbl <- table(samps)
  df[, "reps" := tbl[match(ids, names(tbl))]]
  slices_repped <- df[, .(inds_boot = rep(ind, reps))][["inds_boot"]]
  dplyr::slice(dat, slices_repped)
}

group_resample_dt2 <- function(dat, 
                               id_var, 
                               B = 50, 
                               boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  ids_long <- dat[[id_var]]
  
  # Sample, and prepare slices
  samps <- sample(x = unique(ids_long), replace = TRUE)
  inds_bigdat <- which(ids_long %in% samps)
  df <- data.table("ind" = inds_bigdat, "ids" = ids_long[inds_bigdat])
  df[, reps := sum(samps == ids), by = ids]
  slices_repped <- df[, .(inds_boot = rep(ind, reps))][["inds_boot"]]
  dplyr::slice(dat, slices_repped)
}


group_resample4 <- function(dat, 
                            id_var, 
                            B = 50, 
                            boot_id_name = ".id_boot") {
  
  # Gather unique ids and resample
  setDT(dat)
  ids_long <- dat[[id_var]]
  
  # Sample, and prepare slices (not as.numeric if making more general)
  boot_dats <- lapply(seq_len(B), function(b) {
    #cat(b)
    samps <- sample(x = unique(ids_long), replace = TRUE)
    inds_bigdat <- which(ids_long %in% samps)
    ids_bigdat <- ids_long[inds_bigdat]
    tbl <- tabulate(samps, nbins = length(samps))
    dat[rep(inds_bigdat, times = tbl[ids_bigdat])]
  })
  
  return(boot_dats)
}


testo <- group_resample4(
  dat = imps_combined,
  id_var = ".id",
  B = 200
)

res <- microbenchmark::microbenchmark(
  group_resample4(
    dat = imps_combined,
    id_var = ".id",
    B = 20
  ),
  times = 50L
)
boxplot(res)
ggplot2::autoplot(res)


#
microbenchmark::microbenchmark(
  "orig" = {
    group_resample(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "lapply" = {
    group_resample2(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "slices_rep" = {
    group_resample3(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "slices_rep_dt" = {
    group_resample_dt(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "slices_rep_dt2" = {
    group_resample_dt2(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  times = 5L
)


# The beasts
microbenchmark::microbenchmark(
  "slices_rep_dt" = {
    group_resample_dt(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "slices_rep_dt2" = {
    group_resample_dt2(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  "tabul" = {
    group_resample4(
      dat = imps_combined,
      id_var = ".id",
      B = 1 ,
      boot_id_name = ".id_boot"
    )
  },
  times = 30L
)



test <- group_resample3_ext(
  dat = imps_combined,
  id_var = ".id",
  B = 200,
  boot_id_name = ".id_boot"
)

table(test$.id) /50L


