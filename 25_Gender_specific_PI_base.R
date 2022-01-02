#########################################
# Female and Male gender-specific series
#########################################

library(demography)
library(ftsa)

# Define variables for gender specific series

## Prefecture smooth series
state_female_smooth = state_male_smooth = state_female_pop_smooth = state_male_pop_smooth = matrix(NA, 101*42, 47)

for(iw in 2:48)
{
  state_female_smooth[,iw-1] = as.numeric(get(state_smooth[iw])$rate$female)
  state_female_pop_smooth[,iw-1] = as.numeric(get(state_smooth[iw])$pop$female)
  
  state_male_smooth[,iw-1] = as.numeric(get(state_smooth[iw])$rate$male)
  state_male_pop_smooth[,iw-1] = as.numeric(get(state_smooth[iw])$pop$male)
}

state_female_smooth_na_ind = which(is.na(state_female_smooth))

state_male_smooth_na_ind = which(is.na(state_male_smooth))

sum(is.na(state_male_smooth))
sum(is.na(state_female_smooth))

state_female_comb_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_smooth)
state_female_comb_pop_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_pop_smooth)
colnames(state_female_comb_smooth) = colnames(state_female_comb_pop_smooth) = c("Year", "Age", state_smooth[2:48])

state_male_comb_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_smooth)
state_male_comb_pop_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_pop_smooth)
colnames(state_male_comb_smooth) = colnames(state_male_comb_pop_smooth) = c("Year", "Age", state_smooth[2:48])

write.table(state_female_comb_smooth, file = "state_female_comb_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(state_female_comb_pop_smooth, file = "state_female_comb_pop_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

write.table(state_male_comb_smooth, file = "state_male_comb_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(state_male_comb_pop_smooth, file = "state_male_comb_pop_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

state_demog_female_smooth = read.demogdata("state_female_comb_smooth.txt", "state_female_comb_pop_smooth.txt", 
                                                 type = "mortality", label="state_female_comb_smooth", skip = 0)
state_demog_male_smooth = read.demogdata("state_male_comb_smooth.txt", "state_male_comb_pop_smooth.txt", 
                                               type = "mortality", label="state_male_comb_smooth", skip = 0)

## Region smooth series
region_female_smooth = region_male_smooth = region_female_pop_smooth = region_male_pop_smooth = matrix(NA, 101*42, 8)
for(iw in  1:8)
{
  region_female_smooth[,iw] = as.numeric(get(region_smooth[iw])$rate$female)
  region_female_pop_smooth[,iw] = as.numeric(get(region_smooth[iw])$pop$female)
  
  region_male_smooth[,iw] = as.numeric(get(region_smooth[iw])$rate$male)
  region_male_pop_smooth[,iw] = as.numeric(get(region_smooth[iw])$pop$male)
}

region_female_smooth_na_ind = which(is.na(region_female_smooth))
region_male_smooth_na_ind = which(is.na(region_male_smooth))

sum(is.na(region_male_smooth))
sum(is.na(region_female_smooth))

region_female_comb_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_smooth)
region_female_comb_pop_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_pop_smooth)
colnames(region_female_comb_smooth) = colnames(region_female_comb_pop_smooth) = c("Year", "Age", region_smooth[1:8])

region_male_comb_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_smooth)
region_male_comb_pop_smooth = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_pop_smooth)
colnames(region_male_comb_smooth) = colnames(region_male_comb_pop_smooth) = c("Year", "Age", region_smooth[1:8])

write.table(region_female_comb_smooth, file = "region_female_comb_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(region_female_comb_pop_smooth, file = "region_female_comb_pop_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

write.table(region_male_comb_smooth, file = "region_male_comb_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(region_male_comb_pop_smooth, file = "region_male_comb_pop_smooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

region_demog_female_smooth = read.demogdata("region_female_comb_smooth.txt", "region_female_comb_pop_smooth.txt", 
                                                  type = "mortality", label="region_female_comb_smooth", skip = 0)
region_demog_male_smooth = read.demogdata("region_male_comb_smooth.txt", "region_male_comb_pop_smooth.txt", 
                                                type = "mortality", label="region_male_comb_smooth", skip = 0)

## Prefecture raw series
state_female_raw = state_male_raw = state_female_pop_raw = state_male_pop_raw = matrix(NA, 101*42, 47)
for(iw in 2:48)
{
  state_female_raw[,iw-1] = as.numeric(get(state[iw])$rate$female)
  state_female_pop_raw[,iw-1] = as.numeric(get(state[iw])$pop$female)
  
  state_male_raw[,iw-1] = as.numeric(get(state[iw])$rate$male)
  state_male_pop_raw[,iw-1] = as.numeric(get(state[iw])$pop$male)
}

state_female_raw_na_ind = which(is.na(state_female_raw))

state_male_raw_na_ind = which(is.na(state_male_raw))
state_male_raw =  replace(state_male_raw, state_male_raw_na_ind, state_male_smooth[state_male_raw_na_ind])

sum(is.na(state_male_raw))
sum(is.na(state_female_raw))

state_female_comb_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_raw)
state_female_comb_pop_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_pop_raw)
colnames(state_female_comb_raw) = colnames(state_female_comb_pop_raw) = c("Year", "Age", state[2:48])

state_male_comb_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_raw)
state_male_comb_pop_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_pop_raw)
colnames(state_male_comb_raw) = colnames(state_male_comb_pop_raw) = c("Year", "Age", state[2:48])

write.table(state_female_comb_raw, file = "state_female_comb_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(state_female_comb_pop_raw, file = "state_female_comb_pop_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

write.table(state_male_comb_raw, file = "state_male_comb_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(state_male_comb_pop_raw, file = "state_male_comb_pop_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

state_demog_female_raw = read.demogdata("state_female_comb_raw.txt", "state_female_comb_pop_raw.txt", 
                                             type = "mortality", label="state_female_comb_raw", skip = 0)
state_demog_male_raw = read.demogdata("state_male_comb_raw.txt", "state_male_comb_pop_raw.txt", 
                                                 type = "mortality", label="state_male_comb_raw", skip = 0)

## Region raw series
region_female_raw = region_male_raw = region_female_pop_raw = region_male_pop_raw = matrix(NA, 101*42, 8)
for(iw in  1:8)
{
  region_female_raw[,iw] = as.numeric(get(region[iw])$rate$female)
  region_female_pop_raw[,iw] = as.numeric(get(region[iw])$pop$female)
  
  region_male_raw[,iw] = as.numeric(get(region[iw])$rate$male)
  region_male_pop_raw[,iw] = as.numeric(get(region[iw])$pop$male)
}

region_female_raw_na_ind = which(is.na(region_female_raw))
region_female_raw =  replace(region_female_raw, region_female_raw_na_ind, region_female_smooth[region_female_raw_na_ind])

region_male_raw_na_ind = which(is.na(region_male_raw))
region_male_raw =  replace(region_male_raw, region_male_raw_na_ind, region_male_smooth[region_male_raw_na_ind])

sum(is.na(region_male_raw))
sum(is.na(region_female_raw))

region_female_comb_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_raw)
region_female_comb_pop_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_pop_raw)
colnames(region_female_comb_raw) = colnames(region_female_comb_pop_raw) = c("Year", "Age", region[1:8])

region_male_comb_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_raw)
region_male_comb_pop_raw = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_pop_raw)
colnames(region_male_comb_raw) = colnames(region_male_comb_pop_raw) = c("Year", "Age", region[1:8])

write.table(region_female_comb_raw, file = "region_female_comb_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(region_female_comb_pop_raw, file = "region_female_comb_pop_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

write.table(region_male_comb_raw, file = "region_male_comb_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(region_male_comb_pop_raw, file = "region_male_comb_pop_raw.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

region_demog_female_raw = read.demogdata("region_female_comb_raw.txt", "region_female_comb_pop_raw.txt", 
                                                 type = "mortality", label="region_female_comb_raw", skip = 0)
region_demog_male_raw = read.demogdata("region_male_comb_raw.txt", "region_male_comb_pop_raw.txt", 
                                               type = "mortality", label="region_male_comb_raw", skip = 0)

###########################################################
# constructing multivariate pointwise prediction intervals
###########################################################

### prefecture level

state_PI_fh_gender <- function(fh, state_comb_raw, state_comb, pcamethod = c("static", "dynamic"), nboot = 1000)
{
  PI_boot_mfts_total = array(NA, dim = c(101, nboot, (16-fh), 47))
  
  for(ij in 1:(16-fh))
  {
    dum_pointwise = PI_total_state_mfts(dat = extract.years(state_comb, 1975:(2001+ij)), dat_unsmooth = extract.years(state_comb_raw, 1975:(2001+ij)), pcamethod = pcamethod, fh = fh)
    PI_boot_mfts_total[,,ij,]  = dum_pointwise$boot_sample_state_total
  }
  return(list(PI_boot_mfts_total = PI_boot_mfts_total))
}

interval_score_state_gender <- function(PI_val, data_series, data_series_raw, fh, alpha = 0.8)
{
  test_val_total = extract.years(data_series_raw, (2001+fh):2016)$rate
  
  # transform back to the original scale
  
  boot_sample_total = exp(PI_val[[fh]]$PI_boot_mfts_total) # an array of dim = c(101, 1000, (16-fh), 8)
  boot_index_total  = which(boot_sample_total > 1)
  boot_index_below_total = which(boot_sample_total < 0)
  if(length(boot_index_total) > 0)
  {
    boot_sample_v1_total = replace(boot_sample_total, boot_index_total, 1)
  } else
  {
    boot_sample_v1_total = boot_sample_total
  }
  if(length(boot_index_below_total) > 0)
  {
    boot_sample_v2_total = replace(boot_sample_v1_total, boot_index_below_total, 0)
  } else
  {
    boot_sample_v2_total = boot_sample_v1_total
  }
  
  # lower and upper bounds  
  dummy_total = PI_lb_val_total = PI_ub_val_total = lb_ind_total = ub_ind_total = score_total = list()
  
  if(fh != 15)
  {
    for(ik in 1:47)
    {
      dummy_total[[ik]] = apply(boot_sample_v2_total[,,,ik], c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_total[[ik]]  = dummy_total[[ik]][1,,]
      PI_ub_val_total[[ik]]  = dummy_total[[ik]][2,,]
      lb_ind_total[[ik]]  = ifelse(test_val_total[[ik]] < PI_lb_val_total[[ik]], 1, 0)
      ub_ind_total[[ik]]  = ifelse(test_val_total[[ik]] > PI_ub_val_total[[ik]], 1, 0)
      score_total[[ik]]  = mean((PI_ub_val_total[[ik]] - PI_lb_val_total[[ik]]) + 2/(1 - alpha) * (PI_lb_val_total[[ik]] - test_val_total[[ik]]) * lb_ind_total[[ik]] + 2/(1 - alpha) * (test_val_total[[ik]] - PI_ub_val_total[[ik]]) * ub_ind_total[[ik]])
    }
    
  } else 
    
  {
    for(ik in 1:47)
    {
      dummy_total[[ik]] = apply(boot_sample_v2_total[,,,ik], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_total[[ik]]  = dummy_total[[ik]][1,]
      PI_ub_val_total[[ik]]  = dummy_total[[ik]][2,]
      lb_ind_total[[ik]]  = ifelse(test_val_total[[ik]] < PI_lb_val_total[[ik]], 1, 0)
      ub_ind_total[[ik]]  = ifelse(test_val_total[[ik]] > PI_ub_val_total[[ik]], 1, 0)
      score_total[[ik]]  = mean((PI_ub_val_total[[ik]] - PI_lb_val_total[[ik]]) + 2/(1 - alpha) * (PI_lb_val_total[[ik]] - test_val_total[[ik]]) * lb_ind_total[[ik]] + 2/(1 - alpha) * (test_val_total[[ik]] - PI_ub_val_total[[ik]]) * ub_ind_total[[ik]])
    }
  }
  
  return(as.matrix(score_total, nrow = 47))
}


### region level 

region_PI_fh_gender <- function(fh, region_comb_raw, region_comb, pcamethod = c("static", "dynamic"), nboot = 1000)
{
  PI_boot_mfts_total = array(NA, dim = c(101, nboot, (16-fh), 8))
  
  for(ij in 1:(16-fh))
  {
    dum_pointwise = PI_total_region_mfts(dat = extract.years(region_comb, 1975:(2001+ij)), dat_unsmooth = extract.years(region_comb_raw, 1975:(2001+ij)), pcamethod = pcamethod, fh = fh)
    PI_boot_mfts_total[,,ij,]  = dum_pointwise$boot_sample_region_total
  }
  return(list(PI_boot_mfts_total = PI_boot_mfts_total))
}


interval_score_region_gender <- function(PI_val, data_series, data_series_raw, fh, alpha = 0.8)
{
  test_val_total = extract.years(data_series_raw, (2001+fh):2016)$rate
  
  # transform back to the original scale
  
  boot_sample_total = exp(PI_val[[fh]]$PI_boot_mfts_total) # an array of dim = c(101, 1000, (16-fh), 8)
  boot_index_total  = which(boot_sample_total > 1)
  boot_index_below_total = which(boot_sample_total < 0)
  if(length(boot_index_total) > 0)
  {
    boot_sample_v1_total = replace(boot_sample_total, boot_index_total, 1)
  } else
  {
    boot_sample_v1_total = boot_sample_total
  }
  if(length(boot_index_below_total) > 0)
  {
    boot_sample_v2_total = replace(boot_sample_v1_total, boot_index_below_total, 0)
  } else
  {
    boot_sample_v2_total = boot_sample_v1_total
  }
  
  # lower and upper bounds  
  dummy_total = PI_lb_val_total = PI_ub_val_total = lb_ind_total = ub_ind_total = score_total = list()
  
  if(fh != 15)
  {
    for(ik in 1:8)
    {
      dummy_total[[ik]] = apply(boot_sample_v2_total[,,,ik], c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_total[[ik]]  = dummy_total[[ik]][1,,]
      PI_ub_val_total[[ik]]  = dummy_total[[ik]][2,,]
      lb_ind_total[[ik]]  = ifelse(test_val_total[[ik]] < PI_lb_val_total[[ik]], 1, 0)
      ub_ind_total[[ik]]  = ifelse(test_val_total[[ik]] > PI_ub_val_total[[ik]], 1, 0)
      score_total[[ik]]  = mean((PI_ub_val_total[[ik]] - PI_lb_val_total[[ik]]) + 2/(1 - alpha) * (PI_lb_val_total[[ik]] - test_val_total[[ik]]) * lb_ind_total[[ik]] + 2/(1 - alpha) * (test_val_total[[ik]] - PI_ub_val_total[[ik]]) * ub_ind_total[[ik]])
    }
    
  } else 
    
  {
    for(ik in 1:8)
    {
      dummy_total[[ik]] = apply(boot_sample_v2_total[,,,ik], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_total[[ik]]  = dummy_total[[ik]][1,]
      PI_ub_val_total[[ik]]  = dummy_total[[ik]][2,]
      lb_ind_total[[ik]]  = ifelse(test_val_total[[ik]] < PI_lb_val_total[[ik]], 1, 0)
      ub_ind_total[[ik]]  = ifelse(test_val_total[[ik]] > PI_ub_val_total[[ik]], 1, 0)
      score_total[[ik]]  = mean((PI_ub_val_total[[ik]] - PI_lb_val_total[[ik]]) + 2/(1 - alpha) * (PI_lb_val_total[[ik]] - test_val_total[[ik]]) * lb_ind_total[[ik]] + 2/(1 - alpha) * (test_val_total[[ik]] - PI_ub_val_total[[ik]]) * ub_ind_total[[ik]])
    }
  }
  
  return(as.matrix(score_total, nrow = 8))
}

# compute interval scores for a particular forecast horizon 

cl <- makeCluster(8) 
registerDoParallel(cl)

## state series
state_total_fh_PI_gender_female = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% state_PI_fh_gender(pcamethod = "static", state_comb = state_comb_demogdata_female, state_comb_raw = state_comb_demogdata_female_raw, fh = ik)
state_total_fh_PI_gender_male = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% state_PI_fh_gender(pcamethod = "static", state_comb = state_comb_demogdata_male, state_comb_raw = state_comb_demogdata_male_raw, fh = ik)


interval_score_state_gender_female = matrix(NA, ncol = 15, nrow = 47)
for(ij in 1:15)
{
  interval_score_state_gender_female[,ij] = as.numeric(interval_score_state_gender(PI_val = state_total_fh_PI_gender_female, fh = ij, data_series_raw = state_comb_demogdata_female_raw))
  print(ij)
}

interval_score_state_gender_male = matrix(NA, ncol = 15, nrow = 47)
for(ij in 1:15)
{
  interval_score_state_gender_male[,ij] = as.numeric(interval_score_state_gender(PI_val = state_total_fh_PI_gender_male, fh = ij, data_series_raw = state_comb_demogdata_male_raw))
  print(ij)
}


for(ik in 2:48)
{
  assign(noquote(paste("interval_score_gender_female_", state[ik], "_mfts", sep = "")), interval_score_state_gender_female[ik-1,])
  assign(noquote(paste("interval_score_gender_male_", state[ik], "_mfts", sep = "")), interval_score_state_gender_male[ik-1,])
}


## region series

region_total_fh_PI_gender_female = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% region_PI_fh_gender(pcamethod = "static", region_comb = region_comb_demogdata_female, region_comb_raw = region_comb_demogdata_female_raw, fh = ik)
region_total_fh_PI_gender_male = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% region_PI_fh_gender(pcamethod = "static", region_comb = region_comb_demogdata_male, region_comb_raw = region_comb_demogdata_male_raw, fh = ik)


interval_score_region_gender_female = matrix(NA, ncol = 15, nrow = 8)
for(ij in 1:15)
{
  interval_score_region_gender_female[,ij] = as.numeric(interval_score_region_gender(PI_val = region_total_fh_PI_gender_female, fh = ij, data_series_raw = region_comb_demogdata_female_raw))
  print(ij)
}

interval_score_region_gender_male = matrix(NA, ncol = 15, nrow = 8)
for(ij in 1:15)
{
  interval_score_region_gender_male[,ij] = as.numeric(interval_score_region_gender(PI_val = region_total_fh_PI_gender_male, fh = ij, data_series_raw = region_comb_demogdata_male_raw))
  print(ij)
}


stopCluster(cl)
rm(cl)

## separate into vectors

for(ik in 1:8)
{
  assign(noquote(paste("interval_score_R", ik, "_gender_female", sep = "")), interval_score_region_gender_female[ik,])
  assign(noquote(paste("interval_score_R", ik, "_gender_male", sep = "")), interval_score_region_gender_male[ik,])
}

interval_score_gender_Level_3_mfts = rowMeans(cbind(interval_score_R1_gender_female, interval_score_R2_gender_female, interval_score_R3_gender_female, interval_score_R4_gender_female,
                                                    interval_score_R5_gender_female, interval_score_R6_gender_female, interval_score_R7_gender_female, interval_score_R8_gender_female,
                                                    interval_score_R1_gender_male, interval_score_R2_gender_male, interval_score_R3_gender_male, interval_score_R4_gender_male,
                                                    interval_score_R5_gender_male, interval_score_R6_gender_male, interval_score_R7_gender_male, interval_score_R8_gender_male))


##############################################
# Reconciliation of gender-specific forecasts
##############################################


find_enlarge_gender <- function(data_comb_female, data_comb_male, data_comb_female_raw, data_comb_male_raw, pcamethod = c("static", "dynamic"), fh = 1, nboot = 1000, alpha = 0.8)
{
  if(length(names(data_comb_female$rate)) != length(names(data_comb_male$rate)))
  {
    warning("female series and male series come from different levels.")
  }
  
  # select ncomp_comb based on 95% of total variation; use all available data
   n_pop_female = length(names(data_comb_female$rate))
   n_pop_male = length(names(data_comb_male$rate))

  # female
  rowmeans_object_female = sd_object_female = decenter_object_female = rowmeans_object_male = sd_object_male = decenter_object_male = list()
  for(ik in 1:n_pop_female)
  {
    # compute mean and standard deviation functions
    rowmeans_object_female[[ik]] = rowMeans(log(data_comb_female$rate[[ik]]), na.rm=TRUE)
    sd_object_female[[ik]] = apply(log(data_comb_female$rate[[ik]]), 1, sd, na.rm=TRUE)

    # de-center functional data
    decenter_object_female[[ik]] = t(scale(t(log(data_comb_female$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object_female = do.call(rbind, decenter_object_female)

  # male
  for(ik in 1:n_pop_male)
  {
    # compute mean and standard deviation functions
    rowmeans_object_male[[ik]] = rowMeans(log(data_comb_male$rate[[ik]]), na.rm=TRUE)
    sd_object_male[[ik]] = apply(log(data_comb_male$rate[[ik]]), 1, sd, na.rm=TRUE)

    # de-center functional data
    decenter_object_male[[ik]] = t(scale(t(log(data_comb_male$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object_male = do.call(rbind, decenter_object_male)

  # produce forecasts according two different dimension reduction methods

  if (pcamethod == "static")
  {
    ncomp_comb_female  = min(head(which(cumsum(ftsm(fts(1:nrow(comb_object_female), comb_object_female), order = 20)$varprop) >= 0.95), 1), 11)
    ncomp_comb_male  = min(head(which(cumsum(ftsm(fts(1:nrow(comb_object_male), comb_object_male), order = 20)$varprop) >= 0.95), 1), 11)

    # calculate gender specific in-sample forecast curves

    fore_curve_female  = array(NA, dim = c(length(data_comb_female$age), length(data_comb_female$year) - ncomp_comb_female - fh + 1, n_pop_female))
    fore_curve_male  = array(NA, dim = c(length(data_comb_male$age), length(data_comb_male$year) - ncomp_comb_male - fh + 1, n_pop_male))

    # female
    for(ij in 1:(length(data_comb_female$year) - ncomp_comb_female - fh + 1))
    {

      rowmeans_object_one_step_female = sd_object_one_step_female = decenter_object_one_step_female = list()
      for(ik in 1:n_pop_female)
      {
        # compute mean and standard deviation functions

        dat_one_step_female = as.data.frame(log(data_comb_female$rate[[ik]][, 1:(ncomp_comb_female + ij - 1)]))
        rowmeans_object_one_step_female[[ik]] = rowMeans(dat_one_step_female, na.rm=TRUE)
        sd_object_one_step_female[[ik]] = apply(dat_one_step_female, 1, sd, na.rm=TRUE)

        # de-center functional data
        decenter_object_one_step_female[[ik]] = t(scale(t(dat_one_step_female), center = TRUE, scale = TRUE))
      }

      # one-step-ahead at each time
      comb_object_one_step_female = do.call(rbind, decenter_object_one_step_female)

      fore_ftsm_female = forecast(ftsm(fts(1:nrow(comb_object_one_step_female), comb_object_one_step_female), order = ncomp_comb_female), h = fh)
      fore_res_female  = fore_ftsm_female$mean$y * do.call(c,sd_object_one_step_female) + do.call(c, rowmeans_object_one_step_female)
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:n_pop_female)
      {
        fore_curve_female[,,iwk] = t(fore_res_female[(101*(iwk-1)+1):(101*iwk),fh])
      }
    }

    # male
    for(ij in 1:(length(data_comb_male$year) - ncomp_comb_male - fh + 1))
    {

      rowmeans_object_one_step_male = sd_object_one_step_male = decenter_object_one_step_male = list()
      for(ik in 1:n_pop_male)
      {
        # compute mean and standard deviation functions

        dat_one_step_male = as.data.frame(log(data_comb_male$rate[[ik]][, 1:(ncomp_comb_male + ij - 1)]))
        rowmeans_object_one_step_male[[ik]] = rowMeans(dat_one_step_male, na.rm=TRUE)
        sd_object_one_step_male[[ik]] = apply(dat_one_step_male, 1, sd, na.rm=TRUE)

        # de-center functional data
        decenter_object_one_step_male[[ik]] = t(scale(t(dat_one_step_male), center = TRUE, scale = TRUE))
      }

      # one-step-ahead at each time
      comb_object_one_step_male = do.call(rbind, decenter_object_one_step_male)

      fore_ftsm_male = forecast(ftsm(fts(1:nrow(comb_object_one_step_male), comb_object_one_step_male), order = ncomp_comb_male), h = fh)
      fore_res_male  = fore_ftsm_male$mean$y * do.call(c,sd_object_one_step_male) + do.call(c, rowmeans_object_one_step_male)
      # fill in gender specific in-sample forecast curves
      for(iwk in 1:n_pop_male)
      {
        fore_curve_male[,ij,iwk] = fore_res_male[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }


  if (pcamethod == "dynamic")
  {
    data_dum_female = comb_object_female
    data_dum_male = comb_object_male

    C_0_female = long_run_covariance_estimation(data_dum_female, H = 3, C0 = 3)
    eigen_decomp_female = eigen(C_0_female)
    ncomp_comb_female = head(which(cumsum(eigen_decomp_female$values)/sum(eigen_decomp_female$values) >= 0.95),1)

    dynamic_basis_female = as.matrix(eigen_decomp_female$vectors[,1:ncomp_comb_female], drop = FALSE)
    dynamic_scores_female = t(dynamic_basis_female) %*% data_dum_female

    C_0_male = long_run_covariance_estimation(data_dum_male, H = 3, C0 = 3)
    eigen_decomp_male = eigen(C_0_male)
    ncomp_comb_male = head(which(cumsum(eigen_decomp_male$values)/sum(eigen_decomp_male$values) >= 0.95),1)

    dynamic_basis_male = as.matrix(eigen_decomp_male$vectors[,1:ncomp_comb_male], drop = FALSE)
    dynamic_scores_male = t(dynamic_basis_male) %*% data_dum_male


    fore_curve_female =  array(NA, dim = c(length(data_comb_female$age), length(data_comb_female$year) - ncomp_comb_female - fh + 1, n_pop_female))
    fore_curve_male =  array(NA, dim = c(length(data_comb_male$age), length(data_comb_male$year) - ncomp_comb_male - fh + 1, n_pop_male))

    for(ij in 1:(length(data_comb_female$year) - ncomp_comb_female - fh + 1))
    {

      rowmeans_object_one_step_female = sd_object_one_step_female = decenter_object_one_step_female = list()
      for(ik in 1:n_pop_female)
      {
        # compute mean and standard deviation functions

        dat_one_step_female = as.data.frame(log(data_comb_female$rate[[ik]][, 1:(ncomp_comb_female + ij - 1)]))
        rowmeans_object_one_step_female[[ik]] = rowMeans(dat_one_step_female, na.rm=TRUE)
        sd_object_one_step_female[[ik]] = apply(dat_one_step_female, 1, sd, na.rm=TRUE)

        # de-center functional data
        decenter_object_one_step_female[[ik]] = t(scale(t(dat_one_step_female), center = TRUE, scale = TRUE))
      }

      # one-step-ahead forecasts at each time
      comb_object_one_step_female = do.call(rbind, decenter_object_one_step_female)

      scores_fit_female = scores_fore_female = list()
      fore_ftsm_dyn_female = matrix(NA, nrow = nrow(comb_object_one_step_female), ncol = fh)

      for(ik in 1:ncomp_comb_female)
      {
        scores_fit_female[[ik]] = auto.arima(dynamic_scores_female[ik,])
        scores_fore_female[[ik]] = forecast(scores_fit_female[[ik]], h = fh)$mean
      }

      for(ih in 1:fh)
      {
        fore_ftsm_dyn_female[,ih] = dynamic_basis_female %*% unlist(lapply(scores_fore_female,`[[`,ih))
      }

      fore_res_female = fore_ftsm_dyn_female * do.call(c,sd_object_one_step_female) + do.call(c, rowmeans_object_one_step_female)

      # fill in gender specific in-sample forecast curves
      for(iwk in 1:n_pop_female)
      {
        fore_curve_female[,,iwk] = t(fore_res_female[(101*(iwk-1)+1):(101*iwk),fh])
      }
    }

    for(ij in 1:(length(data_comb_male$year) - ncomp_comb_male - fh + 1))
    {

      rowmeans_object_one_step_male = sd_object_one_step_male = decenter_object_one_step_male = list()
      for(ik in 1:n_pop_male)
      {
        # compute mean and standard deviation functions

        dat_one_step_male = as.data.frame(log(data_comb_male$rate[[ik]][, 1:(ncomp_comb_male + ij - 1)]))
        rowmeans_object_one_step_male[[ik]] = rowMeans(dat_one_step_male, na.rm=TRUE)
        sd_object_one_step_male[[ik]] = apply(dat_one_step_male, 1, sd, na.rm=TRUE)

        # de-center functional data
        decenter_object_one_step_male[[ik]] = t(scale(t(dat_one_step_male), center = TRUE, scale = TRUE))
      }

      # one-step-ahead forecasts at each time
      comb_object_one_step_male = do.call(rbind, decenter_object_one_step_male)

      scores_fit_male = scores_fore_male = list()
      fore_ftsm_dyn_male = matrix(NA, nrow = nrow(comb_object_one_step_male), ncol = fh)

      for(ik in 1:ncomp_comb_male)
      {
        scores_fit_male[[ik]] = auto.arima(dynamic_scores_male[ik,])
        scores_fore_male[[ik]] = forecast(scores_fit_male[[ik]], h = fh)$mean
      }

      for(ih in 1:fh)
      {
        fore_ftsm_dyn_male[,ih] = dynamic_basis_male %*% unlist(lapply(scores_fore_male,`[[`,ih))
      }

      fore_res_male = fore_ftsm_dyn_male * do.call(c,sd_object_one_step_male) + do.call(c, rowmeans_object_one_step_male)

      # fill in gender specific in-sample forecast curves
      for(iwk in 1:n_pop_male)
      {
        fore_curve_male[,ij,iwk] = fore_res_male[(101*(iwk-1)+1):(101*iwk),fh]
      }
    }
  }
  
  
  # holdout data samples
  
  true_dat_female = extract.years(data_comb_female_raw, data_comb_female_raw$year[(ncomp_comb_female + fh):length(data_comb_female$year)])
  true_dat_smooth_female = extract.years(data_comb_female, data_comb_female$year[(ncomp_comb_female + fh):length(data_comb_female$year)])
  
  true_dat_male = extract.years(data_comb_male_raw, data_comb_male_raw$year[(ncomp_comb_male + fh):length(data_comb_male$year)])
  true_dat_smooth_male = extract.years(data_comb_male, data_comb_male$year[(ncomp_comb_male + fh):length(data_comb_male$year)])
  
  # female sample
  
  holdout_val_female = holdout_smooth_female = array(NA, dim = c(length(data_comb_female$age), length(data_comb_female$year) - ncomp_comb_female - fh + 1, n_pop_female))
  for(iwk in 1:n_pop_female)
  {
    holdout_val_female[,,iwk] = log(true_dat_female$rate[[iwk]])
    holdout_smooth_female[,,iwk] = log(true_dat_smooth_female$rate[[iwk]])
  }
  holdout_val_index_female = which(!is.finite(holdout_val_female))
  
  
  if(length(holdout_val_index_female) > 0)
  {
    holdout_val_female_v2 = replace(holdout_val_female, holdout_val_index_female, holdout_smooth_female[holdout_val_index_female])
  }
  
  # Warnings if infinite values detected
  if(length(which(!is.finite(holdout_val_female_v2))) > 0)
  {
    warning("some holdout data have Inf")
  }
  
  # male sample
  
  holdout_val_male = holdout_smooth_male = array(NA, dim = c(length(data_comb_male$age), length(data_comb_male$year) - ncomp_comb_male - fh + 1, n_pop_male))
  for(iwk in 1:n_pop_male)
  {
    holdout_val_male[,,iwk] = log(as.numeric(true_dat_male$rate[[iwk]]))
    holdout_smooth_male[,,iwk] = log(as.numeric(true_dat_smooth_male$rate[[iwk]]))
  }
  holdout_val_index_male = which(!is.finite(holdout_val_male))
  
  
  if(length(holdout_val_index_male) > 0)
  {
    holdout_val_male_v2 = replace(holdout_val_male, holdout_val_index_male, holdout_smooth_male[holdout_val_index_male])
  }
  
  # Warnings if infinite values detected
  if(length(which(!is.finite(holdout_val_male_v2))) > 0)
  {
    warning("some holdout data have Inf")
  }

  
  # gender specific errors 
  
  err_female = holdout_val_female_v2 - fore_curve_female
  err_male   = holdout_val_male_v2 - fore_curve_male
  
  # bootstrap error function
  err_boot_fore_female =  array(NA, dim = c(nrow(err_female), nboot, n_pop_female))
  err_boot_fore_male =  array(NA, dim = c(nrow(err_male), nboot, n_pop_male))
  
  for(iwk in 1:n_pop_female)
  {
    for(ij in 1:nboot)
    {
      err_boot_fore_female[,ij,iwk] = err_female[, sample(1:ncol(err_female), 1, replace = TRUE), iwk]
    }
  }
  
  for(iwk in 1:n_pop_male)
  {
    for(ij in 1:nboot)
    {
      err_boot_fore_male[,ij,iwk] = err_male[, sample(1:ncol(err_male), 1, replace = TRUE), iwk]
    }
  }

  # constructing PI
  
  # female
  fore_ftsm_female = forecast(ftsm(fts(1:nrow(comb_object_female), comb_object_female), order = ncomp_comb_female), h = fh)
  fore_res_female  = fore_ftsm_female$mean$y * do.call(c, sd_object_female) + do.call(c, rowmeans_object_female)
  
  fore_mfts_gender_female = array(NA, dim = c(length(data_comb_female$age), 1, n_pop_female))
  for(iwk in 1:n_pop_female)
  {
    fore_mfts_gender_female[,,iwk] = t(fore_res_female[(101*(iwk-1)+1):(101*iwk),fh])
  }
  
  boot_PI_gender_female = array(NA, dim = c(length(data_comb_female$age), nboot, n_pop_female))
  for(iwk in 1:n_pop_female)
  {
    boot_PI_gender_female[,,iwk] = err_boot_fore_female[,,iwk] + matrix(rep(fore_mfts_gender_female[,,iwk], nboot), nrow = length(data_comb_female$age), ncol = nboot)
  }
  
  boot_PI_lb_ub_gender_female = array(NA, dim = c(2, length(data_comb_female$age), n_pop_female))
  for(iwk in 1:n_pop_female)
  {
    boot_PI_lb_ub_gender_female[,,iwk] = apply(boot_PI_gender_female[,,iwk], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }

  # male
  fore_mfts_gender_male = array(NA, dim = c(length(data_comb_male$age), 1, n_pop_male))
  for(iwk in 1:n_pop_male)
  {
    fore_mfts_gender_male[,,iwk] = t(fore_res_male[(101*(iwk-1)+1):(101*iwk),fh])
  }
  
  boot_PI_gender_male = array(NA, dim = c(length(data_comb_male$age), nboot, n_pop_male))
  for(iwk in 1:n_pop_male)
  {
    boot_PI_gender_male[,,iwk] = err_boot_fore_male[,,iwk] + matrix(rep(fore_mfts_gender_male[,,iwk], nboot), nrow = length(data_comb_male$age), ncol = nboot)
  }
  
  boot_PI_lb_ub_gender_male = array(NA, dim = c(2, length(data_comb_male$age), n_pop_male))
  for(iwk in 1:n_pop_male)
  {
    boot_PI_lb_ub_gender_male[,,iwk] = apply(boot_PI_gender_male[,,iwk], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2))
  }
  
  
  return(list(boot_PI_lb_ub_gender_female = boot_PI_lb_ub_gender_female, boot_sample_gender_female = boot_PI_gender_female,
              boot_PI_lb_ub_gender_male   = boot_PI_lb_ub_gender_male,   boot_sample_gender_male   = boot_PI_gender_male))
}    

################################################################################################
# Calculate prediction intervals for the multivariate forecasting method without reconciliation
################################################################################################

# Compute gender-specific prediction intervals

PI_fh_gender <- function(comb_female, comb_male, comb_female_raw, comb_male_raw, pcamethod = c("static", "dynamic"), fh, nboot = 1000)
{
  n_pop_female = length(names(comb_female$rate))
  n_pop_male = length(names(comb_male$rate))
  
  PI_boot_mfts_gender_female  = array(NA, dim = c(length(comb_female$age), nboot, (16-fh), n_pop_female))
  PI_boot_mfts_gender_male    = array(NA, dim = c(length(comb_male$age), nboot, (16-fh), n_pop_male))
  for(ij in 1:(16-fh))
  {
    dum_pointwise = find_enlarge_gender(data_comb_female = extract.years(comb_female, 1975:(2001+ij)), data_comb_male = extract.years(comb_male, 1975:(2001+ij)), 
                                        data_comb_female_raw = extract.years(comb_female_raw, 1975:(2001+ij)), data_comb_male_raw = extract.years(comb_male_raw, 1975:(2001+ij)), pcamethod = pcamethod, fh = fh, nboot = nboot)
    PI_boot_mfts_gender_female[,,ij,]  = dum_pointwise$boot_sample_gender_female
    PI_boot_mfts_gender_male[,,ij,]    = dum_pointwise$boot_sample_gender_male
  }
  return(list(PI_boot_mfts_gender_female = PI_boot_mfts_gender_female, PI_boot_mfts_gender_male = PI_boot_mfts_gender_male))
}

# compute (gender specific) interval scores  for a particular forecast horizon 

interval_score_gender_mfts <- function(PI_val,data_comb_female_raw, data_comb_male_raw, fh, alpha = 0.8)
{
  n_pop_female = length(names(data_comb_female_raw$rate))
  n_pop_male = length(names(data_comb_male_raw$rate))
  
  test_val_female = extract.years(data_comb_female_raw, (2001+fh):2016)$rate
  test_val_male = extract.years(data_comb_male_raw, (2001+fh):2016)$rate
  
  # transform back to the original scale
  
  # female series
  boot_sample_female = exp(PI_val[[fh]]$PI_boot_mfts_gender_female)
  boot_index_female = which(boot_sample_female > 1)
  boot_index_below_female = which(boot_sample_female < 0)
  if(length(boot_index_female) > 0)
  {
    boot_sample_v1_female = replace(boot_sample_female, boot_index_female, 1)
  } else 
  {
    boot_sample_v1_female = boot_sample_female
  }
  if(length(boot_index_below_female) > 0)
  {
    boot_sample_v2_female = replace(boot_sample_v1_female, boot_index_below_female, 0)
  } else
  {
    boot_sample_v2_female = boot_sample_v1_female
  }
  
  # male series
  boot_sample_male = exp(PI_val[[fh]]$PI_boot_mfts_gender_male)
  boot_index_male = which(boot_sample_male > 1)
  boot_index_below_male = which(boot_sample_male < 0)
  if(length(boot_index_male) > 0)
  {
    boot_sample_v1_male = replace(boot_sample_male, boot_index_male, 1)
  } else
  {
    boot_sample_v1_male = boot_sample_male
  }
  if(length(boot_index_below_male) > 0)
  {
    boot_sample_v2_male = replace(boot_sample_v1_male, boot_index_below_male, 0)
  } else
  {
    boot_sample_v2_male = boot_sample_v1_male
  }  
  
  # lower and upper bounds  
  # # female
  
  dummy_gender_female = PI_lb_val_gender_female = PI_ub_val_gender_female = lb_ind_gender_female = ub_ind_gender_female = score_gender_female = list()
  
  if(fh != 15)
  {
    for(ik in 1:n_pop_female)
    {
      dummy_gender_female[[ik]] = apply(boot_sample_v2_female[,,,ik], c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_gender_female[[ik]]  = dummy_gender_female[[ik]][1,,]
      PI_ub_val_gender_female[[ik]]  = dummy_gender_female[[ik]][2,,]
      lb_ind_gender_female[[ik]]  = ifelse(test_val_female[[ik]] < PI_lb_val_gender_female[[ik]], 1, 0)
      ub_ind_gender_female[[ik]]  = ifelse(test_val_female[[ik]] > PI_ub_val_gender_female[[ik]], 1, 0)
      score_gender_female[[ik]]  = mean((PI_ub_val_gender_female[[ik]] - PI_lb_val_gender_female[[ik]]) + 2/(1 - alpha) * (PI_lb_val_gender_female[[ik]] - test_val_female[[ik]]) * lb_ind_gender_female[[ik]] 
                                      + 2/(1 - alpha) * (test_val_female[[ik]] - PI_ub_val_gender_female[[ik]]) * ub_ind_gender_female[[ik]])
    }
    
  } else 
    
  {
    for(ik in 1:n_pop_female)
    {
      dummy_gender_female[[ik]] = apply(boot_sample_v2_female[,,,ik], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_gender_female[[ik]]  = dummy_gender_female[[ik]][1,]
      PI_ub_val_gender_female[[ik]]  = dummy_gender_female[[ik]][2,]
      lb_ind_gender_female[[ik]]  = ifelse(test_val_female[[ik]] < PI_lb_val_gender_female[[ik]], 1, 0)
      ub_ind_gender_female[[ik]]  = ifelse(test_val_female[[ik]] > PI_ub_val_gender_female[[ik]], 1, 0)
      score_gender_female[[ik]]  = mean((PI_ub_val_gender_female[[ik]] - PI_lb_val_gender_female[[ik]]) + 2/(1 - alpha) * (PI_lb_val_gender_female[[ik]] - test_val_female[[ik]]) * lb_ind_gender_female[[ik]] 
                                      + 2/(1 - alpha) * (test_val_female[[ik]] - PI_ub_val_gender_female[[ik]]) * ub_ind_gender_female[[ik]])
    }
  }

  
  # male
  
  dummy_gender_male = PI_lb_val_gender_male = PI_ub_val_gender_male = lb_ind_gender_male = ub_ind_gender_male = score_gender_male = list()
  
  if(fh != 15)
  {
    for(ik in 1:n_pop_male)
    {
      dummy_gender_male[[ik]] = apply(boot_sample_v2_male[,,,ik], c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_gender_male[[ik]]  = dummy_gender_male[[ik]][1,,]
      PI_ub_val_gender_male[[ik]]  = dummy_gender_male[[ik]][2,,]
      lb_ind_gender_male[[ik]]  = ifelse(test_val_male[[ik]] < PI_lb_val_gender_male[[ik]], 1, 0)
      ub_ind_gender_male[[ik]]  = ifelse(test_val_male[[ik]] > PI_ub_val_gender_male[[ik]], 1, 0)
      score_gender_male[[ik]]  = mean((PI_ub_val_gender_male[[ik]] - PI_lb_val_gender_male[[ik]]) + 2/(1 - alpha) * (PI_lb_val_gender_male[[ik]] - test_val_male[[ik]]) * lb_ind_gender_male[[ik]] + 2/(1 - alpha) * (test_val_male[[ik]] - PI_ub_val_gender_male[[ik]]) * ub_ind_gender_male[[ik]])
    }
    
  } else 
    
  {
    for(ik in 1:n_pop_male)
    {
      dummy_gender_male[[ik]] = apply(boot_sample_v2_male[,,,ik], 1, quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
      PI_lb_val_gender_male[[ik]]  = dummy_gender_male[[ik]][1,]
      PI_ub_val_gender_male[[ik]]  = dummy_gender_male[[ik]][2,]
      lb_ind_gender_male[[ik]]  = ifelse(test_val_male[[ik]] < PI_lb_val_gender_male[[ik]], 1, 0)
      ub_ind_gender_male[[ik]]  = ifelse(test_val_male[[ik]] > PI_ub_val_gender_male[[ik]], 1, 0)
      score_gender_male[[ik]]  = mean((PI_ub_val_gender_male[[ik]] - PI_lb_val_gender_male[[ik]]) + 2/(1 - alpha) * (PI_lb_val_gender_male[[ik]] - test_val_male[[ik]]) * lb_ind_gender_male[[ik]] + 2/(1 - alpha) * (test_val_male[[ik]] - PI_ub_val_gender_male[[ik]]) * ub_ind_gender_male[[ik]])
    }
  }
  
  return(list(score_gender_female = as.matrix(score_gender_female, nrow = n_pop_female), score_gender_male = as.matrix(score_gender_male, nrow = n_pop_male)))
  
}


# Calulating prefecture level gender_specific interval score

## state gender-specific series

library(doParallel)
cl <- makeCluster(5) 
registerDoParallel(cl)

gender_state_fh_PI_mfts_1 = foreach(ik = 1:4, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "dynamic", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_1, file = "gender_state_fh_PI_mfts_1.RData")
rm(gender_state_fh_PI_mfts_1)

gender_state_fh_PI_mfts_2 = foreach(ik = 5:9, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "dynamic", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_2, file = "gender_state_fh_PI_mfts_2.RData")
rm(gender_state_fh_PI_mfts_2)

gender_state_fh_PI_mfts_3 = foreach(ik = 10:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "dynamic", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_3, file = "gender_state_fh_PI_mfts_3.RData")
rm(gender_state_fh_PI_mfts_3)

stopCluster(cl)
rm(cl)

gender_fh_PI_mfts = do.call(c, list(gender_state_fh_PI_mfts_1, gender_state_fh_PI_mfts_2, gender_state_fh_PI_mfts_3))
save(gender_fh_PI_mfts, file = "gender_fh_PI_mfts.RData")

###########################################################
# Forecasting with static FPCA method (A benchmark method)
###########################################################

gender_state_fh_PI_mfts_static_1to4 = foreach(ik = 1:4, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "static", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_static_1to4, file = "gender_female_fh_PI_mfts_static_1to4.RData")

gender_state_fh_PI_mfts_static_5to8 = foreach(ik = 5:8, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "static", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_static_5to8, file = "gender_female_fh_PI_mfts_static_5to8.RData")

gender_state_fh_PI_mfts_static_9to12 = foreach(ik = 9:12, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "static", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_static_9to12, file = "gender_female_fh_PI_mfts_static_9to12.RData")

gender_state_fh_PI_mfts_static_13to15 = foreach(ik = 13:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = state_demog_female_smooth, comb_male = state_demog_male_smooth, comb_female_raw = state_demog_female_raw, comb_male_raw = state_demog_male_raw, pcamethod = "static", fh = ik, nboot = 1000)
save(gender_state_fh_PI_mfts_static_13to15, file = "gender_female_fh_PI_mfts_static_13to15.RData")

gender_state_fh_PI_mfts_static = do.call(c, list(gender_state_fh_PI_mfts_static_1to4, gender_state_fh_PI_mfts_static_5to8,
                                           gender_state_fh_PI_mfts_static_9to12, gender_state_fh_PI_mfts_static_13to15))
save(gender_state_fh_PI_mfts_static, file = "gender_state_fh_PI_mfts_static.RData")

stopCluster(cl)
rm(cl)


# compute interval scores for all forecast horizons

interval_score_gender_female_state_mfts = interval_score_gender_male_state_mfts = matrix(NA, ncol = 15, nrow = 47)
for(ij in 1:15)
{
  dum_interval = interval_score_gender_mfts(PI_val = gender_fh_PI_mfts, 
         data_comb_female_raw =  state_demog_female_raw, data_comb_male_raw = state_demog_male_raw, fh = ij, alpha = 0.8)
  interval_score_gender_female_state_mfts[,ij] = as.numeric(dum_interval$score_gender_female)
  interval_score_gender_male_state_mfts[,ij] = as.numeric(dum_interval$score_gender_male)
  print(ij)
}


for(ik in 2:48)
{
  assign(noquote(paste("interval_score_gender_female_", state[ik], "_mfts", sep = "")), interval_score_gender_female_state_mfts[ik-1,])
  assign(noquote(paste("interval_score_gender_male_", state[ik], "_mfts", sep = "")), interval_score_gender_male_state_mfts[ik-1,])
}


for(iw in 2:48)
{
  assign(noquote(paste(state[iw], "_gender_female_fh_PI_mfts", sep = "")), list())
  assign(noquote(paste(state[iw], "_gender_male_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  Hokkaido_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,1]
  Aomori_gender_female_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,2]
  Iwate_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,3]
  Miyagi_gender_female_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,4]
  Akita_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,5]
  Yamagata_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,6]
  Fukushima_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,7]
  Ibaraki_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,8]
  Tochigi_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,9]
  Gunma_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,10]
  Saitama_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,11]
  Chiba_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,12]
  Tokyo_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,13]
  Kanagawa_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,14]
  Niigata_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,15]
  Toyama_gender_female_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,16]
  Ishikawa_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,17]
  Fukui_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,18]
  Yamanashi_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,19]
  Nagano_gender_female_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,20]
  Gifu_gender_female_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,21]
  Shizuoka_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,22]
  Aichi_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,23]
  Mie_gender_female_fh_PI_mfts[[ik]]        = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,24]
  Shiga_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,25]
  Kyoto_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,26]
  Osaka_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,27]
  Hyogo_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,28]
  Nara_gender_female_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,29]
  Wakayama_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,30]
  Tottori_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,31]
  Shimane_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,32]
  Okayama_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,33]
  Hiroshima_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,34]
  Yamaguchi_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,35]
  Tokushima_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,36]
  Kagawa_gender_female_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,37]
  Ehime_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,38]
  Kochi_gender_female_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,39]
  Fukuoka_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,40]
  Saga_gender_female_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,41]
  Nagasaki_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,42]
  Kumamoto_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,43]
  Oita_gender_female_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,44]
  Miyazaki_gender_female_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,45]
  Kagoshima_gender_female_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,46]
  Okinawa_gender_female_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,47]

  Hokkaido_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,1]
  Aomori_gender_male_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,2]
  Iwate_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,3]
  Miyagi_gender_male_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,4]
  Akita_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,5]
  Yamagata_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,6]
  Fukushima_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,7]
  Ibaraki_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,8]
  Tochigi_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,9]
  Gunma_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,10]
  Saitama_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,11]
  Chiba_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,12]
  Tokyo_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,13]
  Kanagawa_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,14]
  Niigata_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,15]
  Toyama_gender_male_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,16]
  Ishikawa_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,17]
  Fukui_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,18]
  Yamanashi_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,19]
  Nagano_gender_male_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,20]
  Gifu_gender_male_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,21]
  Shizuoka_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,22]
  Aichi_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,23]
  Mie_gender_male_fh_PI_mfts[[ik]]        = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,24]
  Shiga_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,25]
  Kyoto_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,26]
  Osaka_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,27]
  Hyogo_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,28]
  Nara_gender_male_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,29]
  Wakayama_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,30]
  Tottori_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,31]
  Shimane_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,32]
  Okayama_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,33]
  Hiroshima_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,34]
  Yamaguchi_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,35]
  Tokushima_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,36]
  Kagawa_gender_male_fh_PI_mfts[[ik]]     = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,37]
  Ehime_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,38]
  Kochi_gender_male_fh_PI_mfts[[ik]]      = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,39]
  Fukuoka_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,40]
  Saga_gender_male_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,41]
  Nagasaki_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,42]
  Kumamoto_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,43]
  Oita_gender_male_fh_PI_mfts[[ik]]       = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,44]
  Miyazaki_gender_male_fh_PI_mfts[[ik]]   = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,45]
  Kagoshima_gender_male_fh_PI_mfts[[ik]]  = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,46]
  Okinawa_gender_male_fh_PI_mfts[[ik]]    = gender_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,47]
}

Japan_gender_female_fh_PI_mfts = Japan_female_fh_PI
Japan_gender_male_fh_PI_mfts   = Japan_male_fh_PI


## regional gender-specific series

library(doParallel)
cl <- makeCluster(8) 
registerDoParallel(cl)

### static ####

gender_region_fh_PI_mfts_static = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = region_demog_female_smooth, comb_male = region_demog_male_smooth, comb_female_raw = region_demog_female_raw, comb_male_raw = region_demog_male_raw, pcamethod = "static", fh = ik, nboot = 1000)
save(gender_region_fh_PI_mfts_static, file = "gender_region_fh_PI_mfts_static.RData")

### dynamic ###

gender_region_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_gender(comb_female = region_demog_female_smooth, comb_male = region_demog_male_smooth, comb_female_raw = region_demog_female_raw, comb_male_raw = region_demog_male_raw, pcamethod = "dynamic", fh = ik, nboot = 1000)
save(gender_region_fh_PI_mfts, file = "gender_region_fh_PI_mfts.RData")


stopCluster(cl)
rm(cl)

# compute interval scores for all forecast horizons

interval_score_gender_female_region_mfts = interval_score_gender_male_region_mfts = matrix(NA, ncol = 15, nrow = 8)
for(ij in 1:15)
{
  dum_interval = interval_score_gender_mfts(PI_val = gender_region_fh_PI_mfts, 
                                            data_comb_female_raw =  region_demog_female_raw, data_comb_male_raw = region_demog_male_raw, fh = ij, alpha = 0.8)
  interval_score_gender_female_region_mfts[,ij] = as.numeric(dum_interval$score_gender_female)
  interval_score_gender_male_region_mfts[,ij] = as.numeric(dum_interval$score_gender_male)
  print(ij)
}


# separate into vectors

for(ik in 1:8)
{
  assign(noquote(paste("interval_score_gender_female_R", ik, "_mfts", sep = "")), interval_score_gender_female_region_mfts[ik,])
  assign(noquote(paste("interval_score_gender_male_R", ik, "_mfts", sep = "")), interval_score_gender_male_region_mfts[ik,])
}

# separate into female_fh_PI_mfts and 

for(iw in 1:8)
{
  assign(noquote(paste("R", iw, "_gender_female_fh_PI_mfts", sep = "")), list())
  assign(noquote(paste("R", iw, "_gender_male_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  R1_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,1]
  R2_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,2]
  R3_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,3]
  R4_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,4]
  R5_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,5]
  R6_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,6]
  R7_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,7]
  R8_gender_female_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_female[,,,8]
  
  R1_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,1]
  R2_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,2]
  R3_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,3]
  R4_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,4]
  R5_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,5]
  R6_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,6]
  R7_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,7]
  R8_gender_male_fh_PI_mfts[[ik]]  = gender_region_fh_PI_mfts[[ik]]$PI_boot_mfts_gender_male[,,,8]
}


for (i in 2:48)
{
  save(list = paste0(state[i], "_gender_female", "_fh_PI_mfts"), file =  paste(state[i], "gender_female_fh_PI_mfts.RData", sep = "_"))
  save(list = paste0(state[i], "_gender_male", "_fh_PI_mfts"), file =  paste(state[i], "gender_male_fh_PI_mfts.RData", sep = "_"))
}

for(i in 2:48)
{
  rm(list = c(paste(state[i], "gender_female", "fh_PI_mfts", sep = "_"), paste(state[i], "gender_male", "fh_PI_mfts", sep = "_")))
}

for(i in 1:8)
{
  rm(list = c(paste("R",i, "_gender_female_fh_PI_mfts", sep = ""), paste("R",i, "_gender_male_fh_PI_mfts", sep = "")))
}

################################################################
# Calculate interval score at different levels of the hierarchy
################################################################

# Japan (total)

interval_score_gender_Level_0_mfts = interval_score_Japan_total

# Total gender-specific

interval_score_gender_Level_1_mfts = rowMeans(cbind(interval_score_Japan_female_mfts, interval_score_Japan_male_mfts))


# Japan region (total)

interval_score_gender_Level_2_mfts = rowMeans(cbind(interval_score_R1_total_mfts, interval_score_R2_total_mfts, interval_score_R3_total_mfts, interval_score_R4_total_mfts,
                                             interval_score_R5_total_mfts, interval_score_R6_total_mfts, interval_score_R7_total_mfts, interval_score_R8_total_mfts))


# Region gender-specific

  interval_score_gender_Level_3_mfts = rowMeans(cbind(interval_score_gender_female_R1_mfts, interval_score_gender_female_R2_mfts, interval_score_gender_female_R3_mfts, interval_score_gender_female_R4_mfts,
                                                    interval_score_gender_female_R5_mfts, interval_score_gender_female_R6_mfts, interval_score_gender_female_R7_mfts, interval_score_gender_female_R8_mfts,
                                                    interval_score_gender_male_R1_mfts, interval_score_gender_male_R2_mfts, interval_score_gender_male_R3_mfts, interval_score_gender_male_R4_mfts,
                                                    interval_score_gender_male_R5_mfts, interval_score_gender_male_R6_mfts, interval_score_gender_male_R7_mfts, interval_score_gender_male_R8_mfts))

# Japan prefecture (total)

interval_score_gender_Level_4_mfts = rowMeans(cbind(interval_score_Hokkaido_total_mfts, interval_score_Aomori_total_mfts,
                                             interval_score_Iwate_total_mfts, interval_score_Miyagi_total_mfts,
                                             interval_score_Akita_total_mfts, interval_score_Yamagata_total_mfts,
                                             interval_score_Fukushima_total_mfts, interval_score_Ibaraki_total_mfts,
                                             interval_score_Tochigi_total_mfts, interval_score_Gunma_total_mfts,
                                             interval_score_Saitama_total_mfts, interval_score_Chiba_total_mfts,
                                             interval_score_Tokyo_total_mfts, interval_score_Kanagawa_total_mfts,
                                             interval_score_Niigata_total_mfts, interval_score_Toyama_total_mfts,
                                             interval_score_Ishikawa_total_mfts, interval_score_Fukui_total_mfts,
                                             interval_score_Yamanashi_total_mfts, interval_score_Nagano_total_mfts,
                                             interval_score_Gifu_total_mfts, interval_score_Shizuoka_total_mfts,
                                             interval_score_Aichi_total_mfts, interval_score_Mie_total_mfts,
                                             interval_score_Shiga_total_mfts, interval_score_Kyoto_total_mfts,
                                             interval_score_Osaka_total_mfts, interval_score_Hyogo_total_mfts,
                                             interval_score_Nara_total_mfts, interval_score_Wakayama_total_mfts,
                                             interval_score_Tottori_total_mfts, interval_score_Shimane_total_mfts,
                                             interval_score_Okayama_total_mfts, interval_score_Hiroshima_total_mfts,
                                             interval_score_Yamaguchi_total_mfts, interval_score_Tokushima_total_mfts,
                                             interval_score_Kagawa_total_mfts, interval_score_Ehime_total_mfts,
                                             interval_score_Kochi_total_mfts, interval_score_Fukuoka_total_mfts,
                                             interval_score_Saga_total_mfts, interval_score_Nagasaki_total_mfts,
                                             interval_score_Kumamoto_total_mfts, interval_score_Oita_total_mfts,
                                             interval_score_Miyazaki_total_mfts, interval_score_Kagoshima_total_mfts,
                                             interval_score_Okinawa_total_mfts))


# State gender-specific

interval_score_gender_Level_5_mfts = rowMeans(cbind(interval_score_gender_female_Hokkaido_mfts, interval_score_gender_female_Aomori_mfts,
                                             interval_score_gender_female_Iwate_mfts, interval_score_gender_female_Miyagi_mfts,
                                             interval_score_gender_female_Akita_mfts, interval_score_gender_female_Yamagata_mfts,
                                             interval_score_gender_female_Fukushima_mfts, interval_score_gender_female_Ibaraki_mfts,
                                             interval_score_gender_female_Tochigi_mfts, interval_score_gender_female_Gunma_mfts,
                                             interval_score_gender_female_Saitama_mfts, interval_score_gender_female_Chiba_mfts,
                                             interval_score_gender_female_Tokyo_mfts, interval_score_gender_female_Kanagawa_mfts,
                                             interval_score_gender_female_Niigata_mfts, interval_score_gender_female_Toyama_mfts,
                                             interval_score_gender_female_Ishikawa_mfts, interval_score_gender_female_Fukui_mfts,
                                             interval_score_gender_female_Yamanashi_mfts, interval_score_gender_female_Nagano_mfts,
                                             interval_score_gender_female_Gifu_mfts, interval_score_gender_female_Shizuoka_mfts,
                                             interval_score_gender_female_Aichi_mfts, interval_score_gender_female_Mie_mfts,
                                             interval_score_gender_female_Shiga_mfts, interval_score_gender_female_Kyoto_mfts,
                                             interval_score_gender_female_Osaka_mfts, interval_score_gender_female_Hyogo_mfts,
                                             interval_score_gender_female_Nara_mfts, interval_score_gender_female_Wakayama_mfts,
                                             interval_score_gender_female_Tottori_mfts, interval_score_gender_female_Shimane_mfts,
                                             interval_score_gender_female_Okayama_mfts, interval_score_gender_female_Hiroshima_mfts,
                                             interval_score_gender_female_Yamaguchi_mfts, interval_score_gender_female_Tokushima_mfts,
                                             interval_score_gender_female_Kagawa_mfts, interval_score_gender_female_Ehime_mfts,
                                             interval_score_gender_female_Kochi_mfts, interval_score_gender_female_Fukuoka_mfts,
                                             interval_score_gender_female_Saga_mfts, interval_score_gender_female_Nagasaki_mfts,
                                             interval_score_gender_female_Kumamoto_mfts, interval_score_gender_female_Oita_mfts,
                                             interval_score_gender_female_Miyazaki_mfts, interval_score_gender_female_Kagoshima_mfts,
                                             interval_score_gender_female_Okinawa_mfts,
                                             
                                             interval_score_gender_male_Hokkaido_mfts, interval_score_gender_male_Aomori_mfts,
                                             interval_score_gender_male_Iwate_mfts, interval_score_gender_male_Miyagi_mfts,
                                             interval_score_gender_male_Akita_mfts, interval_score_gender_male_Yamagata_mfts,
                                             interval_score_gender_male_Fukushima_mfts, interval_score_gender_male_Ibaraki_mfts,
                                             interval_score_gender_male_Tochigi_mfts, interval_score_gender_male_Gunma_mfts,
                                             interval_score_gender_male_Saitama_mfts, interval_score_gender_male_Chiba_mfts,
                                             interval_score_gender_male_Tokyo_mfts, interval_score_gender_male_Kanagawa_mfts,
                                             interval_score_gender_male_Niigata_mfts, interval_score_gender_male_Toyama_mfts,
                                             interval_score_gender_male_Ishikawa_mfts, interval_score_gender_male_Fukui_mfts,
                                             interval_score_gender_male_Yamanashi_mfts, interval_score_gender_male_Nagano_mfts,
                                             interval_score_gender_male_Gifu_mfts, interval_score_gender_male_Shizuoka_mfts,
                                             interval_score_gender_male_Aichi_mfts, interval_score_gender_male_Mie_mfts,
                                             interval_score_gender_male_Shiga_mfts, interval_score_gender_male_Kyoto_mfts,
                                             interval_score_gender_male_Osaka_mfts, interval_score_gender_male_Hyogo_mfts,
                                             interval_score_gender_male_Nara_mfts, interval_score_gender_male_Wakayama_mfts,
                                             interval_score_gender_male_Tottori_mfts, interval_score_gender_male_Shimane_mfts,
                                             interval_score_gender_male_Okayama_mfts, interval_score_gender_male_Hiroshima_mfts,
                                             interval_score_gender_male_Yamaguchi_mfts, interval_score_gender_male_Tokushima_mfts,
                                             interval_score_gender_male_Kagawa_mfts, interval_score_gender_male_Ehime_mfts,
                                             interval_score_gender_male_Kochi_mfts, interval_score_gender_male_Fukuoka_mfts,
                                             interval_score_gender_male_Saga_mfts, interval_score_gender_male_Nagasaki_mfts,
                                             interval_score_gender_male_Kumamoto_mfts, interval_score_gender_male_Oita_mfts,
                                             interval_score_gender_male_Miyazaki_mfts, interval_score_gender_male_Kagoshima_mfts,
                                             interval_score_gender_male_Okinawa_mfts))


# Averaged interval scores at each level of the hierarchy

interval_score_gender_all_mfts = cbind(interval_score_gender_Level_0_mfts, interval_score_gender_Level_1_mfts, 
                                       interval_score_gender_Level_2_mfts, interval_score_gender_Level_3_mfts,
                                       interval_score_gender_Level_4_mfts, interval_score_gender_Level_5_mfts)
interval_score_gender_all_stats_mfts = rbind(interval_score_gender_all_mfts, colMeans(interval_score_gender_all_mfts), apply(interval_score_gender_all_mfts, 2, median))
colnames(interval_score_gender_all_stats_mfts) = colnames(interval_score_gender_all_mfts) = c("Level 0", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
rownames(interval_score_gender_all_stats_mfts) = c(1:15,"Mean","Median")
rownames(interval_score_gender_all_mfts) = 1:15






















