######################################################
# Construct univariate pointwise prediction intervals
######################################################

library(demography)
library(ftsa)

# data_series: specific data series
# series: total, female, male
# fh: forecast horizon
# nboot: number of bootstrap replication
# alpha: nominal coverage probability
# transformation: logit or log

# Define a function for nonparametric bootstrap

find_enlarge_val <- function(data_series, pcamethod = c("static", "dynamic"), series, fh = 1, nboot = 1000, alpha = 0.8, 
                             transformation = c("logit", "log"))
{
  transformation  = match.arg(transformation)
  smooth_dat = smooth.demogdata(data_series)
  
  if (pcamethod == "static")
  {
    # select ncomp based on 95% of total variation
    
    if(transformation == "logit")
    {
      if(series == "female")
      {
        pca_dat = ftsm(fts(0:100, log((smooth_dat$rate$female)/(1-smooth_dat$rate$female))), order = 10)
      }
      if(series == "male")
      {
        pca_dat = ftsm(fts(0:100, log((smooth_dat$rate$male)/(1-smooth_dat$rate$male))), order = 10)
      }
      if(series == "total")
      {
        pca_dat = ftsm(fts(0:100, log((smooth_dat$rate$total)/(1-smooth_dat$rate$total))), order = 10)
      }
    }  
    if(transformation == "log")
    {
      pca_dat = fdm(smooth_dat, series = series, order = 10)
    }
    ncomp = head(which(round(cumsum(pca_dat$varprop), 3) >= 0.95),1)
    if(class(ncomp) != "integer")
    {
      warning("ncomp is out of 10")
    }
    
    # calculate in-sample forecast curves
    
    fore_curve = matrix(NA, length(data_series$age), length(data_series$year) - ncomp - fh + 1)
    for(ij in 1:(length(data_series$year) - ncomp - fh + 1))
    {
      fore_coef = matrix(NA, ncomp, 1)
      for(ik in 2:(ncomp + 1))
      {
        fore_coef[ik - 1,1] = forecast(auto.arima(pca_dat$coeff[1:(ij + ncomp - 1), ik]), h = fh)$mean[fh]
      }      
      fore_curve[,ij] = pca_dat$basis[,1] + pca_dat$basis[,2:(ncomp + 1)] %*% fore_coef
    }
    
    # holdout data samples
    
    true_dat = extract.years(data_series, data_series$year[(ncomp + fh):length(data_series$year)])
    true_dat_smooth = extract.years(smooth_dat, data_series$year[(ncomp + fh):length(data_series$year)])
    if(series == "total")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$total)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$total)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$total/(1 - true_dat$rate$total))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$total/(1 - true_dat_smooth$rate$total))[holdout_val_index])
        }
      }
    }
    if(series == "male")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$male)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$male)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$male/(1 - true_dat$rate$male))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$male/(1 - true_dat_smooth$rate$male))[holdout_val_index])
        }
      }
    }
    if(series == "female")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$female)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$female)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$female/(1 - true_dat$rate$female))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$female/(1 - true_dat_smooth$rate$female))[holdout_val_index])
        }
      }
    }
    if(length(which(!is.finite(!holdout_val))) > 0)
    {
      warning("some holdout data have Inf")
    }
    
    err = holdout_val - fore_curve
    
    # bootstrap error function
    err_boot_fore = matrix(NA, nrow(err), nboot)
    for(ij in 1:nboot)
    {
      err_boot_fore[,ij] = err[, sample(1:ncol(err), 1, replace = TRUE)]
    }
    
    # constructing PI
    if(transformation == "log")
    {
      if(series == "female")
      {
        fore_pca_dat = log(forecast(pca_dat, h = fh)$rate$female)[,fh]
      }
      if(series == "male")
      {
        fore_pca_dat = log(forecast(pca_dat, h = fh)$rate$male)[,fh]
      }
      if(series == "total")
      {
        fore_pca_dat = log(forecast(pca_dat, h = fh)$rate$total)[,fh]
      }
    }
    if(transformation == "logit")
    {
      forecast_rate = exp(forecast.ftsm(pca_dat, h = fh, method = "arima")$mean$y)
      fore_pca_dat = log(forecast_rate/(1 - forecast_rate))[,fh]
    }    
    boot_PI = err_boot_fore + matrix(rep(fore_pca_dat, nboot), nrow = length(data_series$age), ncol = nboot)
    boot_PI_lb_ub = apply(boot_PI, 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }
  
  ###########
  
  if (pcamethod == "dynamic")
  {
    # select ncomp based on 95% of total variation
    
    if(transformation == "logit")
    {
      if(series == "female")
      {
        data_dum = log((smooth_dat$rate$female)/(1-smooth_dat$rate$female))
      }
      if(series == "male")
      {
        data_dum = log((smooth_dat$rate$male)/(1-smooth_dat$rate$male))
        
      }
      if(series == "total")
      {
        data_dum = log((smooth_dat$rate$total)/(1-smooth_dat$rate$total))
      }
    }
    
    if(transformation == "log")
    {
      if(series == "female")
      {
        series_ind = 1
      }
      if(series == "male")
      {
        series_ind = 2
      }
      if(series == "total")
      {
        series_ind = 3
      }

      # compute mean and standard deviation functions
      rowmeans_object = rowMeans(log(smooth_dat$rate[[series_ind]]), na.rm = TRUE)
      sd_object = apply(log(smooth_dat$rate[[series_ind]]), 1, sd, na.rm=TRUE)
      
      # de-center functional data
      data_dum = t(scale(t(log(smooth_dat$rate[[series_ind]])), center = TRUE, scale = TRUE))
    }
    
    C_0 = long_run_covariance_estimation(data_dum, H = 3, C0 = 3)
    eigen_decomp = eigen(C_0)
    ncomp = max(head(which(cumsum(eigen_decomp$values)/sum(eigen_decomp$values) >= 0.95),1), 2)
    
    dynamic_basis = as.matrix(eigen_decomp$vectors[,1:ncomp])
    dynamic_scores = t(dynamic_basis) %*% data_dum
    
    # calculate in-sample forecast curves
    
    fore_curve = matrix(NA, length(data_series$age), length(data_series$year) - ncomp - fh + 1)

    for(ij in 1:(length(data_series$year) - ncomp - fh + 1))
    {
      # compute mean and standard deviation functions
      
      dat_one_step = as.data.frame(log(smooth_dat$rate[[series_ind]][, 1:(ncomp + ij - 1)]))
      rowmeans_object_one_step = rowMeans(dat_one_step, na.rm=TRUE)
      sd_object_one_step = apply(dat_one_step, 1, sd, na.rm=TRUE)
      
      # de-center functional data
      decenter_object_one_step = t(scale(t(dat_one_step), center = TRUE, scale = TRUE))
      
      # one-step-ahead forecasts at each time
      comb_object_one_step = decenter_object_one_step
      
      scores_fit = scores_fore = list()
      fore_ftsm_dyn = matrix(NA, nrow = nrow(comb_object_one_step), ncol = fh)
      
      for(ik in 1:ncomp)
      {
        scores_fit[[ik]] = auto.arima(dynamic_scores[ik,])
        scores_fore[[ik]] = forecast(scores_fit[[ik]], h = fh)$mean
      }
      
      for(ih in 1:fh)
      {
        fore_ftsm_dyn[,ih] = dynamic_basis %*% unlist(lapply(scores_fore,`[[`,ih))
      }
      
      fore_curve[,ij] = as.matrix(fore_ftsm_dyn * sd_object_one_step + rowmeans_object_one_step)[,fh]
    }
    
    # holdout data samples
    
    true_dat = extract.years(data_series, data_series$year[(ncomp + fh):length(data_series$year)])
    true_dat_smooth = extract.years(smooth_dat, data_series$year[(ncomp + fh):length(data_series$year)])
    if(series == "total")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$total)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$total)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$total/(1 - true_dat$rate$total))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$total/(1 - true_dat_smooth$rate$total))[holdout_val_index])
        }
      }
    }
    if(series == "male")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$male)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$male)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$male/(1 - true_dat$rate$male))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$male/(1 - true_dat_smooth$rate$male))[holdout_val_index])
        }
      }
    }
    if(series == "female")
    {
      if(transformation == "log")
      {
        holdout_val = log(true_dat$rate$female)
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$female)[holdout_val_index])
        }
      }
      if(transformation == "logit")
      {
        holdout_val = log(true_dat$rate$female/(1 - true_dat$rate$female))
        holdout_val_index = which(!is.finite(holdout_val))
        if(length(holdout_val_index) > 0)
        {
          holdout_val = replace(holdout_val, holdout_val_index, log(true_dat_smooth$rate$female/(1 - true_dat_smooth$rate$female))[holdout_val_index])
        }
      }
    }
    if(length(which(!is.finite(!holdout_val))) > 0)
    {
      warning("some holdout data have Inf")
    }
    
    err = holdout_val - fore_curve
    
    # bootstrap error function
    err_boot_fore = matrix(NA, nrow(err), nboot)
    for(ij in 1:nboot)
    {
      err_boot_fore[,ij] = err[, sample(1:ncol(err), 1, replace = TRUE)]
    }
    
    # constructing PI
    if(transformation == "log")
    {
      fore_ftsm = forecast(ftsm(fts(1:nrow(data_dum), data_dum), order = ncomp), h = fh)
      fore_res = fore_ftsm$mean$y * sd_object + rowmeans_object
      fore_pca_dat = as.matrix(fore_res)
    }
    
    if(transformation == "logit")
    {
      forecast_rate = exp(orecast(ftsm(fts(1:nrow(data_dum), data_dum), order = ncomp), h = fh)$mean$y)
      fore_pca_dat = log(forecast_rate/(1 - forecast_rate))[,fh]
    }
    
    boot_PI = err_boot_fore + matrix(rep(fore_pca_dat, nboot), nrow = length(data_series$age), ncol = nboot)
    boot_PI_lb_ub = apply(boot_PI, 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }
  
  return(list(boot_PI_lb_ub = boot_PI_lb_ub, boot_sample = boot_PI))
}    

  