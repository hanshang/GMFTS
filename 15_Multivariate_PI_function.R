###########################################################
# constructing multivariate pointwise prediction intervals
###########################################################

library(demography)
library(ftsa)

# data_series: specific data series
# series: total, female, male
# fh: forecast horizon
# nboot: number of bootstrap replication
# alpha: nominal coverage probability
# transformation: logit or log

#########################
# female and male series
#########################

# Define a function for nonparametric bootstrap of gender-specific series

find_enlarge_val_mfts <- function(data_series, pcamethod = c("static", "dynamic"), fh = 1, nboot = 1000, alpha = 0.8)
{
  smooth_dat = smooth.demogdata(data_series)
  
  # select ncomp_comb based on 95% of total variation; use all available data
  n = length(names(smooth_dat$rate))
  # if the data contains female, male and total, then we consider only female and male
  if(n == 3)
  {
    n_pop = 2
  } else {
    n_pop = n
  }
  
  rowmeans_object = sd_object = decenter_object = list()
  for(ik in 1:n_pop)
  {
    # compute mean and standard deviation functions
    rowmeans_object[[ik]] = rowMeans(log(smooth_dat$rate[[ik]]), na.rm=TRUE)
    sd_object[[ik]] = apply(log(smooth_dat$rate[[ik]]), 1, sd, na.rm=TRUE)
    
    # de-center functional data
    decenter_object[[ik]] = t(scale(t(log(smooth_dat$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object = do.call(rbind, decenter_object)
  
  # produce forecasts according two different dimension reduction methods
  
  if (pcamethod == "static")
  {
    ncomp_comb  = min(head(which(cumsum(ftsm(fts(1:nrow(comb_object), comb_object), order = 20)$varprop) >= 0.95), 1), 11)
    
    if( ncomp_comb > 11 || ncomp_comb == 0)
    {
      warning("ncomp_comb is out of 11")
    }
    
    # calculate gender specific in-sample forecast curves
    
    fore_curve_female = fore_curve_male = matrix(NA, length(data_series$age), length(data_series$year) - ncomp_comb - fh +1)
    for(ij in 1:(length(data_series$year) - ncomp_comb - fh + 1))
    {
      
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        
        dat_one_step = as.data.frame(log(smooth_dat$rate[[ik]][, 1:(ncomp_comb + ij - 1)]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # one-step-ahead at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object_one_step), comb_object_one_step), order = ncomp_comb), h = fh)
      fore_res  = fore_ftsm$mean$y * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
      # fill in gender specific in-sample forecast curves
      fore_curve_female[,ij] = fore_res[1:101,fh]
      fore_curve_male[,ij]   = fore_res[102:202,fh]
    }
  }
  
  
  if (pcamethod == "dynamic")
  {
    data_dum = comb_object
    
    C_0 = long_run_covariance_estimation(data_dum, H = 3, C0 = 3)
    eigen_decomp = eigen(C_0)
    ncomp_comb = head(which(cumsum(eigen_decomp$values)/sum(eigen_decomp$values) >= 0.95),1)
    
    if(ncomp_comb == 1)
    {
      ncomp_comb = 2
    }
    
    dynamic_basis = as.matrix(eigen_decomp$vectors[,1:ncomp_comb])
    dynamic_scores = t(dynamic_basis) %*% data_dum
    
    fore_curve_female = fore_curve_male = matrix(NA, length(data_series$age), length(data_series$year) - ncomp_comb - fh)
    for(ij in 1:(length(data_series$year) - ncomp_comb - fh))
    {
      
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        
        dat_one_step = as.data.frame(log(smooth_dat$rate[[ik]][, 1:(ncomp_comb + ij)]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # one-step-ahead forecasts at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      scores_fit = scores_fore = list()
      fore_ftsm_dyn = matrix(NA, nrow = nrow(comb_object_one_step), ncol = fh)
      
      for(ik in 1:ncomp_comb)
      {
        scores_fit[[ik]] = auto.arima(dynamic_scores[ik,])
        scores_fore[[ik]] = forecast(scores_fit[[ik]], h = fh)$mean
      }
      
      for(ih in 1:fh)
      {
        fore_ftsm_dyn[,ih] = dynamic_basis %*% unlist(lapply(scores_fore,`[[`,ih))
      }
      
      fore_res = fore_ftsm_dyn * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
 
      # fill in gender specific in-sample forecast curves
      fore_curve_female[,ij] = fore_res[1:101,fh]
      fore_curve_male[,ij]   = fore_res[102:202,fh]
    }
   }


  # holdout data samples
  
  true_dat = extract.years(data_series, data_series$year[(ncomp_comb + fh + 1):length(data_series$year)])
  true_dat_smooth = extract.years(smooth_dat, data_series$year[(ncomp_comb + fh + 1):length(data_series$year)])
  
  # female sample
  holdout_val_male = log(true_dat$rate$male)
  holdout_val_male_index = which(!is.finite(holdout_val_male))
  if(length(holdout_val_male_index) > 0)
  {
    holdout_val_male = replace(holdout_val_male, holdout_val_male_index, log(true_dat_smooth$rate$male)[holdout_val_male_index])
  }
  
  # male sample
  holdout_val_female = log(true_dat$rate$female)
  holdout_val_female_index = which(!is.finite(holdout_val_female))
  if(length(holdout_val_female_index) > 0)
  {
    holdout_val_female = replace(holdout_val_female, holdout_val_female_index, log(true_dat_smooth$rate$female)[holdout_val_female_index])
  }
  
  # Warnings if infinite values detected
  if(length(which(!is.finite(holdout_val_female))) > 0)
  {
    warning("female holdout data have Inf")
  }
  
  if(length(which(!is.finite(holdout_val_male))) > 0)
  {
    warning("male holdout data have Inf")
  }
  
  # gender specific errors 
  
  err_female = holdout_val_female - fore_curve_female
  err_male   = holdout_val_male - fore_curve_male
  
  # bootstrap error function
  err_boot_fore_female = err_boot_fore_male = matrix(0, nrow(err_female), nboot)
  for(ij in 1:nboot)
  {
    err_boot_fore_female[,ij] = err_female[, sample(1:ncol(err_female), 1, replace = TRUE)]
    err_boot_fore_male[,ij]   = err_male[, sample(1:ncol(err_male), 1, replace = TRUE)]
  }
  
  # constructing PI
  
  fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp_comb), h = fh)
  fore_res  = fore_ftsm$mean$y * do.call(c,sd_object) + do.call(c, rowmeans_object)
  
  fore_mfts_dat_female = as.matrix(fore_res[1:101,])
  fore_mfts_dat_male   = as.matrix(fore_res[102:202,])
  
  boot_PI_female = err_boot_fore_female + matrix(rep(fore_mfts_dat_female, nboot), nrow = length(data_series$age), ncol = nboot)
  boot_PI_male   = err_boot_fore_male + matrix(rep(fore_mfts_dat_male, nboot), nrow = length(data_series$age), ncol = nboot)
  
  boot_PI_lb_ub_female = apply(boot_PI_female, 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  boot_PI_lb_ub_male   = apply(boot_PI_male, 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  
  return(list(boot_PI_lb_ub_female = boot_PI_lb_ub_female, boot_sample_female = boot_PI_female,
              boot_PI_lb_ub_male   = boot_PI_lb_ub_male,   boot_sample_male   = boot_PI_male))
}    

################################
# Prefecture level total series
################################

# Define a function for nonparametric bootstrap of prefecture total series

PI_total_state_mfts <- function(dat, pcamethod = c("static", "dynamic"), dat_unsmooth, fh = 1, nboot = 1000, alpha = 0.8)
{  
  # select ncomp_comb based on 95% of total variation; use all available data
  n_pop = length(names(dat$rate))
  rowmeans_object = sd_object = decenter_object = list()
  for(ik in 1:n_pop)
  {
    # compute mean and standard deviation functions
    rowmeans_object[[ik]] = rowMeans(log(dat$rate[[ik]]), na.rm=TRUE)
    sd_object[[ik]] = apply(log(dat$rate[[ik]]), 1, sd, na.rm=TRUE)
    
    # de-center functional data
    decenter_object[[ik]] = t(scale(t(log(dat$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object = do.call(rbind, decenter_object)
  
  if (pcamethod == "static")
  {
    ncomp_comb = min(head(which(cumsum(ftsm(fts(1:nrow(comb_object), comb_object), order = 20)$varprop) >= 0.95), 1), 11)
    
    if(ncomp_comb > 11 || ncomp_comb == 0)
    {
      warning("ncomp_comb is out of 11")
    }
    
    # calculate in-sample forecast curves
    fore_curve_state_total = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 47))
    
    for(ij in 1:(length(dat$year) - ncomp_comb - fh + 1))
    {
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        dat_one_step = as.data.frame(log(dat$rate[[ik]][, 1:(ncomp_comb + ij - 1 )]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # one-step-ahead at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object_one_step), comb_object_one_step), order = ncomp_comb), h = fh)
      fore_res  = fore_ftsm$mean$y * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
      
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:47)
      {
        fore_curve_state_total[,ij,iwk] = fore_res[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }
  
  if (pcamethod == "dynamic")
  {
    data_dum = comb_object
    
    C_0 = long_run_covariance_estimation(data_dum, H = 3, C0 = 3)
    eigen_decomp = eigen(C_0)
    ncomp_comb = head(which(cumsum(eigen_decomp$values)/sum(eigen_decomp$values) >= 0.95),1)
    
    if(ncomp_comb == 1)
    {
      ncomp_comb = 2
    }
    
    dynamic_basis = as.matrix(eigen_decomp$vectors[,1:ncomp_comb])
    dynamic_scores = t(dynamic_basis) %*% data_dum
    
    # calculate in-sample forecast curves
    fore_curve_state_total = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 47))
    
    for(ij in 1:(length(dat$year) - ncomp_comb - fh + 1))
    {
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        dat_one_step = as.data.frame(log(dat$rate[[ik]][, 1:(ncomp_comb + ij - 1 )]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # one-step-ahead at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      scores_fit = scores_fore = list()
      fore_ftsm_dyn = matrix(NA, nrow = nrow(comb_object_one_step), ncol = fh)
      
      for(ik in 1:ncomp_comb)
      {
        scores_fit[[ik]] = auto.arima(dynamic_scores[ik,])
        scores_fore[[ik]] = forecast(scores_fit[[ik]], h = fh)$mean
      }
      
      for(ih in 1:fh)
      {
        fore_ftsm_dyn[,ih] = dynamic_basis %*% unlist(lapply(scores_fore,`[[`,ih))
      }
      
      fore_res = fore_ftsm_dyn * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
      
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:47)
      {
        fore_curve_state_total[,ij,iwk] = fore_res[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }
  
  
  # holdout data samples
  
  true_dat = extract.years(dat_unsmooth, dat_unsmooth$year[(ncomp_comb + fh):length(dat_unsmooth$year)])
  true_dat_smooth = extract.years(dat, dat$year[(ncomp_comb + fh):length(dat$year)])
  
  holdout_val = holdout_smooth = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 47))
  for(iwk in 1:47)
  {
    holdout_val[,,iwk] = log(true_dat$rate[[iwk]])
    holdout_smooth[,,iwk] = log(true_dat_smooth$rate[[iwk]])
  }
  holdout_val_index = which(!is.finite(holdout_val))
 
  
  if(length(holdout_val_index) > 0)
  {
    holdout_val = replace(holdout_val, holdout_val_index, holdout_smooth[holdout_val_index])
  }
  
  # Warnings if infinite values detected
  if(length(which(!is.finite(holdout_val))) > 0)
  {
    warning("some holdout data have Inf")
  }
  
  # state level total series errors
  err_state_total = holdout_val - fore_curve_state_total
  
  # bootstrap error function
  err_boot_state_total = array(NA, dim = c(nrow(err_state_total), nboot, 47))
  for(iwk in 1:47)
  {
    for(ij in 1:nboot)
    {
      err_boot_state_total[,ij,iwk] = err_state_total[, sample(1:ncol(err_state_total), 1, replace = TRUE), iwk]
    }
  }
  
  # constructing PI
  fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp_comb), h = fh)
  fore_res  = fore_ftsm$mean$y * do.call(c,sd_object) + do.call(c, rowmeans_object)
  fore_mfts_state_total = array(NA, dim = c(length(dat$age), 1, 47))
  for(iwk in 1:47)
  {
    fore_mfts_state_total[,,iwk] = t(fore_res[(101*(iwk-1)+1):(101*iwk),fh])
  }
  
  boot_PI_state_total = array(NA, dim = c(length(dat$age), nboot, 47))
  for(iwk in 1:47)
  {
    boot_PI_state_total[,,iwk] = err_boot_state_total[,,iwk] + matrix(rep(fore_mfts_state_total[,,iwk], nboot), nrow = length(dat$age), ncol = nboot)
  }
  
  boot_PI_lb_ub_state_total = array(NA, dim = c(2, 101, 47))
  for(okw in 1:47)
  {
    boot_PI_lb_ub_state_total = apply(boot_PI_state_total[,,iwk], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }
  
  return(list(boot_PI_lb_ub_state_total = boot_PI_lb_ub_state_total, boot_sample_state_total = boot_PI_state_total))

}

###########################
# Region level total series
###########################

# Define a function for nonparametric bootstrap of region total series

PI_total_region_mfts <- function(dat,  pcamethod = c("static", "dynamic"), dat_unsmooth, fh = 1, nboot = 1000, alpha = 0.8)
{
  # select ncomp_comb based on 95% of total variation; use all available data
  n_pop = length(names(dat$rate))
  rowmeans_object = sd_object = decenter_object = list()
  for(ik in 1:n_pop)
  {
    # compute mean and standard deviation functions
    rowmeans_object[[ik]] = rowMeans(log(dat$rate[[ik]]), na.rm=TRUE)
    sd_object[[ik]] = apply(log(dat$rate[[ik]]), 1, sd, na.rm=TRUE)
    
    # de-center functional data
    decenter_object[[ik]] = t(scale(t(log(dat$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object = do.call(rbind, decenter_object)
  
  if (pcamethod == "static")
  {
    ncomp_comb = min(head(which(cumsum(ftsm(fts(1:nrow(comb_object), comb_object), order = 20)$varprop) >= 0.95), 1), 11)
    
    if(ncomp_comb> 11 || ncomp_comb == 0)
    {
      warning("ncomp_comb is out of 11")
    }
    
    # calculate in-sample forecast curves
    fore_curve_region_total = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 8))
    
    for(ij in 1:(length(dat$year) - ncomp_comb - fh + 1))
    {
      
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        dat_one_step = as.data.frame(log(dat$rate[[ik]][, 1:(ncomp_comb + ij - 1)]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # h-step-ahead at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object_one_step), comb_object_one_step), order = ncomp_comb), h = fh)
      fore_res  = fore_ftsm$mean$y * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
      
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:8)
      {
        fore_curve_region_total[,ij,iwk] = fore_res[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }
  
  if (pcamethod == "dynamic")
  {
    data_dum = comb_object
    
    C_0 = long_run_covariance_estimation(data_dum, H = 3, C0 = 3)
    eigen_decomp = eigen(C_0)
    ncomp_comb = head(which(cumsum(eigen_decomp$values)/sum(eigen_decomp$values) >= 0.95),1)
    
    if(ncomp_comb == 1)
    {
      ncomp_comb = 2
    }
    
    dynamic_basis = as.matrix(eigen_decomp$vectors[,1:ncomp_comb])
    dynamic_scores = t(dynamic_basis) %*% data_dum
    
    # calculate in-sample forecast curves
    fore_curve_region_total = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 8))
    for(ij in 1:(length(dat$year) - ncomp_comb - fh + 1))
    {
      rowmeans_object_one_step = sd_object_one_step = decenter_object_one_step = list()
      for(ik in 1:n_pop)
      {
        # compute mean and standard deviation functions
        dat_one_step = as.data.frame(log(dat$rate[[ik]][, 1:(ncomp_comb + ij - 1)]))
        rowmeans_object_one_step[[ik]] = rowMeans(dat_one_step, na.rm=TRUE)
        sd_object_one_step[[ik]] = apply(dat_one_step, 1, sd, na.rm=TRUE)
        
        # de-center functional data
        decenter_object_one_step[[ik]] = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      }
      
      # h-step-ahead at each time
      comb_object_one_step = do.call(rbind, decenter_object_one_step)
      
      scores_fit = scores_fore = list()
      fore_ftsm_dyn = matrix(NA, nrow = nrow(comb_object_one_step), ncol = fh)
      
      for(ik in 1:ncomp_comb)
      {
        scores_fit[[ik]] = auto.arima(dynamic_scores[ik,])
        scores_fore[[ik]] = forecast(scores_fit[[ik]], h = fh)$mean
      }
      
      for(ih in 1:fh)
      {
        fore_ftsm_dyn[,ih] = dynamic_basis %*% unlist(lapply(scores_fore,`[[`,ih))
      }
      
      fore_res = fore_ftsm_dyn * do.call(c,sd_object_one_step) + do.call(c, rowmeans_object_one_step)
      
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:8)
      {
        fore_curve_region_total[,ij,iwk] = fore_res[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }

  # holdout data samples
  
  true_dat = extract.years(dat_unsmooth, dat_unsmooth$year[(ncomp_comb + fh):length(dat_unsmooth$year)])
  true_dat_smooth = extract.years(dat, dat$year[(ncomp_comb + fh):length(dat$year)])
  
  holdout_val = holdout_smooth = array(NA, dim = c(length(dat$age), length(dat$year) - ncomp_comb - fh + 1, 8))
  for(iwk in 1:8)
  {
    holdout_val[,,iwk] = log(true_dat$rate[[iwk]])
    holdout_smooth[,,iwk] = log(true_dat_smooth$rate[[iwk]])
  }
  holdout_val_index = which(!is.finite(holdout_val))
 
  if(length(holdout_val_index) > 0)
  {
    holdout_val = replace(holdout_val, holdout_val_index, holdout_smooth[holdout_val_index])
  }
  
  # Warnings if infinite values detected
  if(length(which(!is.finite(holdout_val))) > 0)
  {
    warning("some holdout data have Inf")
  }
  
  # region level total series errors
  err_region_total = holdout_val - fore_curve_region_total
  
  
  # bootstrap error function
  err_boot_region_total = array(NA, dim = c(nrow(err_region_total), nboot, 8))
  for(iwk in 1:8)
  {
    for(ij in 1:nboot)
    {
      err_boot_region_total[,ij,iwk] = err_region_total[, sample(1:ncol(err_region_total), 1, replace = TRUE), iwk]
    }
  }
  
  # constructing PI
  
  fore_ftsm = forecast(ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp_comb), h = fh)
  fore_res  = fore_ftsm$mean$y * do.call(c,sd_object) + do.call(c, rowmeans_object)
  fore_mfts_region_total = array(NA, dim = c(length(dat$age), 1, 8))
  for(iwk in 1:8)
  {
    fore_mfts_region_total[,,iwk] = t(fore_res[(101*(iwk-1)+1):(101*iwk),fh])
  }
  
  boot_PI_region_total = array(NA, dim = c(length(dat$age), nboot, 8))
  for(iwk in 1:8)
  {
    boot_PI_region_total[,,iwk] = err_boot_region_total[,,iwk] + matrix(rep(fore_mfts_region_total[,,iwk], nboot), nrow = length(dat$age), ncol = nboot)
  }
   
  boot_PI_lb_ub_region_total = array(NA, dim = c(2, 101, 8))
  for(okw in 1:8)
  {
    boot_PI_lb_ub_region_total = apply(boot_PI_region_total[,,iwk], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }
  
  return(list(boot_PI_lb_ub_region_total = boot_PI_lb_ub_region_total, boot_sample_region_total = boot_PI_region_total))
}

