################################################################################################
# Calculate prediction intervals for the multivariate forecasting method without reconciliation
################################################################################################

library(demography)
library(ftsa)

# data_series: specific data series
# series: total, female, male
# fh: forecast horizon
# nboot: number of bootstrap replications

# Define a function to find pointwise prediction intervals by bootstrap

PI_fh_mfts <- function(data_series, pcamethod = c("static", "dynamic"), fh, nboot = 1000)
{
  PI_boot_mfts_female  = array(NA, dim = c(length(data_series$age), nboot, (16-fh)))
  PI_boot_mfts_male    = array(NA, dim = c(length(data_series$age), nboot, (16-fh)))
  for(ij in 1:(16-fh))
  {
    dum_pointwise = find_enlarge_val_mfts(data_series = extract.years(data_series, 1975:(2001+ij)),  pcamethod = pcamethod, fh = fh)
    PI_boot_mfts_female[,,ij]  = dum_pointwise$boot_sample_female
    PI_boot_mfts_male[,,ij]    = dum_pointwise$boot_sample_male
  }
  return(list(PI_boot_mfts_female = PI_boot_mfts_female, PI_boot_mfts_male = PI_boot_mfts_male))
}

############################################################
# compute interval scores for a particular forecast horizon 
############################################################

# Define a function to calculate interval scores for the bootstrapped PIs

## PI_val: one- to 15-step-ahead prediction intervals
## alpha: nominal coverage probability

interval_score_mfts <- function(PI_val, data_series, fh, alpha = 0.8)
{
  test_val_female = extract.years(data_series, (2001+fh):2016)$rate$female
  test_val_male   = extract.years(data_series, (2001+fh):2016)$rate$male
  

  # transform back to the original scale
  
  # female series
  boot_sample_female = exp(PI_val[[fh]]$PI_boot_mfts_female)
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
  boot_sample_male = exp(PI_val[[fh]]$PI_boot_mfts_male)
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
  # female series
  
  dummy_female = apply(boot_sample_female, c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
  PI_lb_val_female = dummy_female[1,,]
  PI_ub_val_female  = dummy_female[2,,]
  
  lb_ind_female  = ifelse(test_val_female < PI_lb_val_female, 1, 0)
  ub_ind_female  = ifelse(test_val_female > PI_ub_val_female, 1, 0)
  score_female  = mean((PI_ub_val_female - PI_lb_val_female) + 2/(1 - alpha) * (PI_lb_val_female - test_val_female) * lb_ind_female + 
                  2/(1 - alpha) * (test_val_female - PI_ub_val_female) * ub_ind_female)
  
  # male series
  
  dummy_male = apply(boot_sample_male, c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
  PI_lb_val_male = dummy_male[1,,]
  PI_ub_val_male  = dummy_male[2,,]
  
  lb_ind_male  = ifelse(test_val_male < PI_lb_val_male, 1, 0)
  ub_ind_male  = ifelse(test_val_male > PI_ub_val_male, 1, 0)
  score_male  = mean((PI_ub_val_male - PI_lb_val_male) + 2/(1 - alpha) * (PI_lb_val_male - test_val_male) * lb_ind_male + 
                  2/(1 - alpha) * (test_val_male - PI_ub_val_male) * ub_ind_male)
  
  return(list(score_female = score_female, score_male = score_male))
}


##########################################################
# Japan total sereis with multivariate forecasting method
##########################################################

# pointwise prediction interval for all 15 forecast horizons

library(doParallel)

cl <- makeCluster(15) 
registerDoParallel(cl)

Japan_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Japan, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Japan_female_mfts = interval_score_Japan_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Japan_female_mfts[ij] = interval_score_mfts(PI_val = Japan_fh_PI_mfts, 
                                                   data_series = Japan, fh = ij)$score_female
  
  interval_score_Japan_male_mfts[ij] = interval_score_mfts(PI_val = Japan_fh_PI_mfts, 
                                                 data_series = Japan, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

##################################
# Prefecture level interval score
##################################

###########
# Hokkaido
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Hokkaido_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Hokkaido, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hokkaido_female_mfts = interval_score_Hokkaido_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Hokkaido_female_mfts[ij] = interval_score_mfts(PI_val = Hokkaido_fh_PI_mfts, 
                                                   data_series = Hokkaido, fh = ij)$score_female
  
  interval_score_Hokkaido_male_mfts[ij] = interval_score_mfts(PI_val = Hokkaido_fh_PI_mfts, 
                                                 data_series = Hokkaido, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

#########
# Aomori
#########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Aomori_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Aomori, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Aomori_female_mfts = interval_score_Aomori_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Aomori_female_mfts[ij] = interval_score_mfts(PI_val = Aomori_fh_PI_mfts, 
                                                   data_series = Aomori, fh = ij)$score_female
  
  interval_score_Aomori_male_mfts[ij] = interval_score_mfts(PI_val = Aomori_fh_PI_mfts, 
                                                 data_series = Aomori, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Iwate
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Iwate_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Iwate, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Iwate_female_mfts = interval_score_Iwate_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Iwate_female_mfts[ij] = interval_score_mfts(PI_val = Iwate_fh_PI_mfts, 
                                                   data_series = Iwate, fh = ij)$score_female
  
  interval_score_Iwate_male_mfts[ij] = interval_score_mfts(PI_val = Iwate_fh_PI_mfts, 
                                                 data_series = Iwate, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

#########
# Miyagi
#########


# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Miyagi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Miyagi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Miyagi_female_mfts = interval_score_Miyagi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Miyagi_female_mfts[ij] = interval_score_mfts(PI_val = Miyagi_fh_PI_mfts, 
                                                   data_series = Miyagi, fh = ij)$score_female
  
  interval_score_Miyagi_male_mfts[ij] = interval_score_mfts(PI_val = Miyagi_fh_PI_mfts, 
                                                 data_series = Miyagi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Akita
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Akita_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Akita, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Akita_female_mfts = interval_score_Akita_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Akita_female_mfts[ij] = interval_score_mfts(PI_val = Akita_fh_PI_mfts, 
                                                   data_series = Akita, fh = ij)$score_female
  
  interval_score_Akita_male_mfts[ij] = interval_score_mfts(PI_val = Akita_fh_PI_mfts, 
                                                 data_series = Akita, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
#Yamagata
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Yamagata_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Yamagata, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamagata_female_mfts = interval_score_Yamagata_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Yamagata_female_mfts[ij] = interval_score_mfts(PI_val = Yamagata_fh_PI_mfts, 
                                                   data_series = Yamagata, fh = ij)$score_female
  
  interval_score_Yamagata_male_mfts[ij] = interval_score_mfts(PI_val = Yamagata_fh_PI_mfts, 
                                                 data_series = Yamagata, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Fukushima
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Fukushima_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Fukushima, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukushima_female_mfts = interval_score_Fukushima_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Fukushima_female_mfts[ij] = interval_score_mfts(PI_val = Fukushima_fh_PI_mfts, 
                                                                data_series = Fukushima, fh = ij)$score_female
  
  interval_score_Fukushima_male_mfts[ij] = interval_score_mfts(PI_val = Fukushima_fh_PI_mfts, 
                                                              data_series = Fukushima, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Ibaraki
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Ibaraki_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Ibaraki, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ibaraki_female_mfts = interval_score_Ibaraki_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Ibaraki_female_mfts[ij] = interval_score_mfts(PI_val = Ibaraki_fh_PI_mfts, 
                                                                data_series = Ibaraki, fh = ij)$score_female
  
  interval_score_Ibaraki_male_mfts[ij] = interval_score_mfts(PI_val = Ibaraki_fh_PI_mfts, 
                                                              data_series = Ibaraki, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

##########
# Tochigi
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Tochigi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Tochigi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tochigi_female_mfts = interval_score_Tochigi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Tochigi_female_mfts[ij] = interval_score_mfts(PI_val = Tochigi_fh_PI_mfts, 
                                                                data_series = Tochigi, fh = ij)$score_female
  
  interval_score_Tochigi_male_mfts[ij] = interval_score_mfts(PI_val = Tochigi_fh_PI_mfts, 
                                                              data_series = Tochigi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Gunma
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Gunma_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Gunma, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Gunma_female_mfts = interval_score_Gunma_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Gunma_female_mfts[ij] = interval_score_mfts(PI_val = Gunma_fh_PI_mfts, 
                                                                data_series = Gunma, fh = ij)$score_female
  
  interval_score_Gunma_male_mfts[ij] = interval_score_mfts(PI_val = Gunma_fh_PI_mfts, 
                                                              data_series = Gunma, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

##########
# Saitama
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Saitama_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Saitama, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Saitama_female_mfts = interval_score_Saitama_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Saitama_female_mfts[ij] = interval_score_mfts(PI_val = Saitama_fh_PI_mfts, 
                                                                data_series = Saitama, fh = ij)$score_female
  
  interval_score_Saitama_male_mfts[ij] = interval_score_mfts(PI_val = Saitama_fh_PI_mfts, 
                                                              data_series = Saitama, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Chiba
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Chiba_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Chiba, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Chiba_female_mfts = interval_score_Chiba_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Chiba_female_mfts[ij] = interval_score_mfts(PI_val = Chiba_fh_PI_mfts, 
                                                                data_series = Chiba, fh = ij)$score_female
  
  interval_score_Chiba_male_mfts[ij] = interval_score_mfts(PI_val = Chiba_fh_PI_mfts, 
                                                              data_series = Chiba, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Tokyo
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Tokyo_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Tokyo, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tokyo_female_mfts = interval_score_Tokyo_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Tokyo_female_mfts[ij] = interval_score_mfts(PI_val = Tokyo_fh_PI_mfts, 
                                                                data_series = Tokyo, fh = ij)$score_female
  
  interval_score_Tokyo_male_mfts[ij] = interval_score_mfts(PI_val = Tokyo_fh_PI_mfts, 
                                                              data_series = Tokyo, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Kanagawa
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kanagawa_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kanagawa, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kanagawa_female_mfts = interval_score_Kanagawa_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kanagawa_female_mfts[ij] = interval_score_mfts(PI_val = Kanagawa_fh_PI_mfts, 
                                                                data_series = Kanagawa, fh = ij)$score_female
  
  interval_score_Kanagawa_male_mfts[ij] = interval_score_mfts(PI_val = Kanagawa_fh_PI_mfts, 
                                                              data_series = Kanagawa, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Niigata
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Niigata_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Niigata, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Niigata_female_mfts = interval_score_Niigata_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Niigata_female_mfts[ij] = interval_score_mfts(PI_val = Niigata_fh_PI_mfts, 
                                                                data_series = Niigata, fh = ij)$score_female
  
  interval_score_Niigata_male_mfts[ij] = interval_score_mfts(PI_val = Niigata_fh_PI_mfts, 
                                                              data_series = Niigata, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#########
# Toyama
#########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Toyama_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Toyama, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Toyama_female_mfts = interval_score_Toyama_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Toyama_female_mfts[ij] = interval_score_mfts(PI_val = Toyama_fh_PI_mfts, 
                                                                data_series = Toyama, fh = ij)$score_female
  
  interval_score_Toyama_male_mfts[ij] = interval_score_mfts(PI_val = Toyama_fh_PI_mfts, 
                                                              data_series = Toyama, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Ishikawa
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Ishikawa_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Ishikawa, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ishikawa_female_mfts = interval_score_Ishikawa_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Ishikawa_female_mfts[ij] = interval_score_mfts(PI_val = Ishikawa_fh_PI_mfts, 
                                                                data_series = Ishikawa, fh = ij)$score_female
  
  interval_score_Ishikawa_male_mfts[ij] = interval_score_mfts(PI_val = Ishikawa_fh_PI_mfts, 
                                                              data_series = Ishikawa, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Fukui
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Fukui_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Fukui, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukui_female_mfts = interval_score_Fukui_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Fukui_female_mfts[ij] = interval_score_mfts(PI_val = Fukui_fh_PI_mfts, 
                                                                data_series = Fukui, fh = ij)$score_female
  
  interval_score_Fukui_male_mfts[ij] = interval_score_mfts(PI_val = Fukui_fh_PI_mfts, 
                                                              data_series = Fukui, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Yamanashi
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Yamanashi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Yamanashi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamanashi_female_mfts = interval_score_Yamanashi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Yamanashi_female_mfts[ij] = interval_score_mfts(PI_val = Yamanashi_fh_PI_mfts, 
                                                                data_series = Yamanashi, fh = ij)$score_female
  
  interval_score_Yamanashi_male_mfts[ij] = interval_score_mfts(PI_val = Yamanashi_fh_PI_mfts, 
                                                              data_series = Yamanashi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#########
# Nagano
#########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Nagano_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Nagano, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nagano_female_mfts = interval_score_Nagano_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Nagano_female_mfts[ij] = interval_score_mfts(PI_val = Nagano_fh_PI_mfts, 
                                                                data_series = Nagano, fh = ij)$score_female
  
  interval_score_Nagano_male_mfts[ij] = interval_score_mfts(PI_val = Nagano_fh_PI_mfts, 
                                                              data_series = Nagano, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#######
# Gifu
#######

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Gifu_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Gifu, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Gifu_female_mfts = interval_score_Gifu_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Gifu_female_mfts[ij] = interval_score_mfts(PI_val = Gifu_fh_PI_mfts, 
                                                                data_series = Gifu, fh = ij)$score_female
  
  interval_score_Gifu_male_mfts[ij] = interval_score_mfts(PI_val = Gifu_fh_PI_mfts, 
                                                              data_series = Gifu, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Shizuoka
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Shizuoka_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Shizuoka, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shizuoka_female_mfts = interval_score_Shizuoka_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Shizuoka_female_mfts[ij] = interval_score_mfts(PI_val = Shizuoka_fh_PI_mfts, 
                                                                data_series = Shizuoka, fh = ij)$score_female
  
  interval_score_Shizuoka_male_mfts[ij] = interval_score_mfts(PI_val = Shizuoka_fh_PI_mfts, 
                                                              data_series = Shizuoka, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Aichi
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Aichi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Aichi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Aichi_female_mfts = interval_score_Aichi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Aichi_female_mfts[ij] = interval_score_mfts(PI_val = Aichi_fh_PI_mfts, 
                                                                data_series = Aichi, fh = ij)$score_female
  
  interval_score_Aichi_male_mfts[ij] = interval_score_mfts(PI_val = Aichi_fh_PI_mfts, 
                                                              data_series = Aichi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


######
# Mie
######

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Mie_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Mie, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Mie_female_mfts = interval_score_Mie_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Mie_female_mfts[ij] = interval_score_mfts(PI_val = Mie_fh_PI_mfts, 
                                                                data_series = Mie, fh = ij)$score_female
  
  interval_score_Mie_male_mfts[ij] = interval_score_mfts(PI_val = Mie_fh_PI_mfts, 
                                                              data_series = Mie, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Shiga
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Shiga_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Shiga, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shiga_female_mfts = interval_score_Shiga_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Shiga_female_mfts[ij] = interval_score_mfts(PI_val = Shiga_fh_PI_mfts, 
                                                                data_series = Shiga, fh = ij)$score_female
  
  interval_score_Shiga_male_mfts[ij] = interval_score_mfts(PI_val = Shiga_fh_PI_mfts, 
                                                              data_series = Shiga, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Kyoto
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kyoto_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kyoto, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kyoto_female_mfts = interval_score_Kyoto_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kyoto_female_mfts[ij] = interval_score_mfts(PI_val = Kyoto_fh_PI_mfts, 
                                                                data_series = Kyoto, fh = ij)$score_female
  
  interval_score_Kyoto_male_mfts[ij] = interval_score_mfts(PI_val = Kyoto_fh_PI_mfts, 
                                                              data_series = Kyoto, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Osaka
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Osaka_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Osaka, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Osaka_female_mfts = interval_score_Osaka_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Osaka_female_mfts[ij] = interval_score_mfts(PI_val = Osaka_fh_PI_mfts, 
                                                                data_series = Osaka, fh = ij)$score_female
  
  interval_score_Osaka_male_mfts[ij] = interval_score_mfts(PI_val = Osaka_fh_PI_mfts, 
                                                              data_series = Osaka, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Hyogo
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Hyogo_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Hyogo, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hyogo_female_mfts = interval_score_Hyogo_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Hyogo_female_mfts[ij] = interval_score_mfts(PI_val = Hyogo_fh_PI_mfts, 
                                                                data_series = Hyogo, fh = ij)$score_female
  
  interval_score_Hyogo_male_mfts[ij] = interval_score_mfts(PI_val = Hyogo_fh_PI_mfts, 
                                                              data_series = Hyogo, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#######
# Nara
#######

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Nara_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Nara, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nara_female_mfts = interval_score_Nara_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Nara_female_mfts[ij] = interval_score_mfts(PI_val = Nara_fh_PI_mfts, 
                                                                data_series = Nara, fh = ij)$score_female
  
  interval_score_Nara_male_mfts[ij] = interval_score_mfts(PI_val = Nara_fh_PI_mfts, 
                                                              data_series = Nara, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Wakayama
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Wakayama_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Wakayama, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Wakayama_female_mfts = interval_score_Wakayama_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Wakayama_female_mfts[ij] = interval_score_mfts(PI_val = Wakayama_fh_PI_mfts, 
                                                                data_series = Wakayama, fh = ij)$score_female
  
  interval_score_Wakayama_male_mfts[ij] = interval_score_mfts(PI_val = Wakayama_fh_PI_mfts, 
                                                              data_series = Wakayama, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Tottori
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Tottori_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Tottori, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tottori_female_mfts = interval_score_Tottori_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Tottori_female_mfts[ij] = interval_score_mfts(PI_val = Tottori_fh_PI_mfts, 
                                                                data_series = Tottori, fh = ij)$score_female
  
  interval_score_Tottori_male_mfts[ij] = interval_score_mfts(PI_val = Tottori_fh_PI_mfts, 
                                                              data_series = Tottori, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Shimane
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Shimane_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Shimane, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shimane_female_mfts = interval_score_Shimane_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Shimane_female_mfts[ij] = interval_score_mfts(PI_val = Shimane_fh_PI_mfts, 
                                                                data_series = Shimane, fh = ij)$score_female
  
  interval_score_Shimane_male_mfts[ij] = interval_score_mfts(PI_val = Shimane_fh_PI_mfts, 
                                                              data_series = Shimane, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Okayama
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Okayama_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Okayama, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Okayama_female_mfts = interval_score_Okayama_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Okayama_female_mfts[ij] = interval_score_mfts(PI_val = Okayama_fh_PI_mfts, 
                                                                data_series = Okayama, fh = ij)$score_female
  
  interval_score_Okayama_male_mfts[ij] = interval_score_mfts(PI_val = Okayama_fh_PI_mfts, 
                                                              data_series = Okayama, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Hiroshima
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Hiroshima_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Hiroshima, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hiroshima_female_mfts = interval_score_Hiroshima_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Hiroshima_female_mfts[ij] = interval_score_mfts(PI_val = Hiroshima_fh_PI_mfts, 
                                                                data_series = Hiroshima, fh = ij)$score_female
  
  interval_score_Hiroshima_male_mfts[ij] = interval_score_mfts(PI_val = Hiroshima_fh_PI_mfts, 
                                                              data_series = Hiroshima, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Yamaguchi
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Yamaguchi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Yamaguchi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamaguchi_female_mfts = interval_score_Yamaguchi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Yamaguchi_female_mfts[ij] = interval_score_mfts(PI_val = Yamaguchi_fh_PI_mfts, 
                                                                data_series = Yamaguchi, fh = ij)$score_female
  
  interval_score_Yamaguchi_male_mfts[ij] = interval_score_mfts(PI_val = Yamaguchi_fh_PI_mfts, 
                                                              data_series = Yamaguchi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Tokushima
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Tokushima_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Tokushima, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tokushima_female_mfts = interval_score_Tokushima_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Tokushima_female_mfts[ij] = interval_score_mfts(PI_val = Tokushima_fh_PI_mfts, 
                                                                data_series = Tokushima, fh = ij)$score_female
  
  interval_score_Tokushima_male_mfts[ij] = interval_score_mfts(PI_val = Tokushima_fh_PI_mfts, 
                                                              data_series = Tokushima, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#########
# Kagawa
#########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kagawa_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kagawa, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kagawa_female_mfts = interval_score_Kagawa_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kagawa_female_mfts[ij] = interval_score_mfts(PI_val = Kagawa_fh_PI_mfts, 
                                                                data_series = Kagawa, fh = ij)$score_female
  
  interval_score_Kagawa_male_mfts[ij] = interval_score_mfts(PI_val = Kagawa_fh_PI_mfts, 
                                                              data_series = Kagawa, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Ehime
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Ehime_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Ehime, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ehime_female_mfts = interval_score_Ehime_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Ehime_female_mfts[ij] = interval_score_mfts(PI_val = Ehime_fh_PI_mfts, 
                                                                data_series = Ehime, fh = ij)$score_female
  
  interval_score_Ehime_male_mfts[ij] = interval_score_mfts(PI_val = Ehime_fh_PI_mfts, 
                                                              data_series = Ehime, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


########
# Kochi
########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kochi_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kochi, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kochi_female_mfts = interval_score_Kochi_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kochi_female_mfts[ij] = interval_score_mfts(PI_val = Kochi_fh_PI_mfts, 
                                                                data_series = Kochi, fh = ij)$score_female
  
  interval_score_Kochi_male_mfts[ij] = interval_score_mfts(PI_val = Kochi_fh_PI_mfts, 
                                                              data_series = Kochi, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Fukuoka
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Fukuoka_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Fukuoka, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukuoka_female_mfts = interval_score_Fukuoka_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Fukuoka_female_mfts[ij] = interval_score_mfts(PI_val = Fukuoka_fh_PI_mfts, 
                                                                data_series = Fukuoka, fh = ij)$score_female
  
  interval_score_Fukuoka_male_mfts[ij] = interval_score_mfts(PI_val = Fukuoka_fh_PI_mfts, 
                                                              data_series = Fukuoka, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#######
# Saga
#######

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Saga_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Saga, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Saga_female_mfts = interval_score_Saga_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Saga_female_mfts[ij] = interval_score_mfts(PI_val = Saga_fh_PI_mfts, 
                                                                data_series = Saga, fh = ij)$score_female
  
  interval_score_Saga_male_mfts[ij] = interval_score_mfts(PI_val = Saga_fh_PI_mfts, 
                                                              data_series = Saga, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Nagasaki
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Nagasaki_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Nagasaki, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nagasaki_female_mfts = interval_score_Nagasaki_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Nagasaki_female_mfts[ij] = interval_score_mfts(PI_val = Nagasaki_fh_PI_mfts, 
                                                                data_series = Nagasaki, fh = ij)$score_female
  
  interval_score_Nagasaki_male_mfts[ij] = interval_score_mfts(PI_val = Nagasaki_fh_PI_mfts, 
                                                              data_series = Nagasaki, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Kumamoto
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kumamoto_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kumamoto, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kumamoto_female_mfts = interval_score_Kumamoto_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kumamoto_female_mfts[ij] = interval_score_mfts(PI_val = Kumamoto_fh_PI_mfts, 
                                                                data_series = Kumamoto, fh = ij)$score_female
  
  interval_score_Kumamoto_male_mfts[ij] = interval_score_mfts(PI_val = Kumamoto_fh_PI_mfts, 
                                                              data_series = Kumamoto, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#######
# Oita
#######

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Oita_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Oita, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Oita_female_mfts = interval_score_Oita_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Oita_female_mfts[ij] = interval_score_mfts(PI_val = Oita_fh_PI_mfts, 
                                                                data_series = Oita, fh = ij)$score_female
  
  interval_score_Oita_male_mfts[ij] = interval_score_mfts(PI_val = Oita_fh_PI_mfts, 
                                                              data_series = Oita, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


###########
# Miyazaki
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Miyazaki_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Miyazaki, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Miyazaki_female_mfts = interval_score_Miyazaki_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Miyazaki_female_mfts[ij] = interval_score_mfts(PI_val = Miyazaki_fh_PI_mfts, 
                                                                data_series = Miyazaki, fh = ij)$score_female
  
  interval_score_Miyazaki_male_mfts[ij] = interval_score_mfts(PI_val = Miyazaki_fh_PI_mfts, 
                                                              data_series = Miyazaki, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


############
# Kagoshima
############

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Kagoshima_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Kagoshima, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kagoshima_female_mfts = interval_score_Kagoshima_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Kagoshima_female_mfts[ij] = interval_score_mfts(PI_val = Kagoshima_fh_PI_mfts, 
                                                                data_series = Kagoshima, fh = ij)$score_female
  
  interval_score_Kagoshima_male_mfts[ij] = interval_score_mfts(PI_val = Kagoshima_fh_PI_mfts, 
                                                              data_series = Kagoshima, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


##########
# Okinawa
##########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Okinawa_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = Okinawa, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_Okinawa_female_mfts = interval_score_Okinawa_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_Okinawa_female_mfts[ij] = interval_score_mfts(PI_val = Okinawa_fh_PI_mfts, 
                                                                data_series = Okinawa, fh = ij)$score_female
  
  interval_score_Okinawa_male_mfts[ij] = interval_score_mfts(PI_val = Okinawa_fh_PI_mfts, 
                                                              data_series = Okinawa, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

# separate into female_fh_PI_mfts and male_fh_PI_mfts

for(iw in 2:48)
{
  assign(noquote(paste(state[iw], "_female_fh_PI_mfts", sep = "")), list())
  assign(noquote(paste(state[iw], "_male_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  Hokkaido_female_fh_PI_mfts[[ik]]   = Hokkaido_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Aomori_female_fh_PI_mfts[[ik]]     = Aomori_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Iwate_female_fh_PI_mfts[[ik]]      = Iwate_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Miyagi_female_fh_PI_mfts[[ik]]     = Miyagi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Akita_female_fh_PI_mfts[[ik]]      = Akita_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Yamagata_female_fh_PI_mfts[[ik]]   = Yamagata_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Fukushima_female_fh_PI_mfts[[ik]]  = Fukushima_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Ibaraki_female_fh_PI_mfts[[ik]]    = Ibaraki_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Tochigi_female_fh_PI_mfts[[ik]]    = Tochigi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Gunma_female_fh_PI_mfts[[ik]]      = Gunma_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Saitama_female_fh_PI_mfts[[ik]]    = Saitama_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Chiba_female_fh_PI_mfts[[ik]]      = Chiba_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Tokyo_female_fh_PI_mfts[[ik]]      = Tokyo_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kanagawa_female_fh_PI_mfts[[ik]]   = Kanagawa_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Niigata_female_fh_PI_mfts[[ik]]    = Niigata_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Toyama_female_fh_PI_mfts[[ik]]     = Toyama_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Ishikawa_female_fh_PI_mfts[[ik]]   = Ishikawa_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Fukui_female_fh_PI_mfts[[ik]]      = Fukui_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Yamanashi_female_fh_PI_mfts[[ik]]  = Yamanashi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Nagano_female_fh_PI_mfts[[ik]]     = Nagano_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Gifu_female_fh_PI_mfts[[ik]]       = Gifu_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Shizuoka_female_fh_PI_mfts[[ik]]   = Shizuoka_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Aichi_female_fh_PI_mfts[[ik]]      = Aichi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Mie_female_fh_PI_mfts[[ik]]        = Mie_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Shiga_female_fh_PI_mfts[[ik]]      = Shiga_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kyoto_female_fh_PI_mfts[[ik]]      = Kyoto_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Osaka_female_fh_PI_mfts[[ik]]      = Osaka_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Hyogo_female_fh_PI_mfts[[ik]]      = Hyogo_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Nara_female_fh_PI_mfts[[ik]]       = Nara_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Wakayama_female_fh_PI_mfts[[ik]]   = Wakayama_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Tottori_female_fh_PI_mfts[[ik]]    = Tottori_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Shimane_female_fh_PI_mfts[[ik]]    = Shimane_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Okayama_female_fh_PI_mfts[[ik]]    = Okayama_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Hiroshima_female_fh_PI_mfts[[ik]]  = Hiroshima_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Yamaguchi_female_fh_PI_mfts[[ik]]  = Yamaguchi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Tokushima_female_fh_PI_mfts[[ik]]  = Tokushima_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kagawa_female_fh_PI_mfts[[ik]]     = Kagawa_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Ehime_female_fh_PI_mfts[[ik]]      = Ehime_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kochi_female_fh_PI_mfts[[ik]]      = Kochi_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Fukuoka_female_fh_PI_mfts[[ik]]    = Fukuoka_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Saga_female_fh_PI_mfts[[ik]]       = Saga_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Nagasaki_female_fh_PI_mfts[[ik]]   = Nagasaki_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kumamoto_female_fh_PI_mfts[[ik]]   = Kumamoto_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Oita_female_fh_PI_mfts[[ik]]       = Oita_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Miyazaki_female_fh_PI_mfts[[ik]]   = Miyazaki_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Kagoshima_female_fh_PI_mfts[[ik]]  = Kagoshima_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  Okinawa_female_fh_PI_mfts[[ik]]    = Okinawa_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  
  Hokkaido_male_fh_PI_mfts[[ik]]   = Hokkaido_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Aomori_male_fh_PI_mfts[[ik]]     = Aomori_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Iwate_male_fh_PI_mfts[[ik]]      = Iwate_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Miyagi_male_fh_PI_mfts[[ik]]     = Miyagi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Akita_male_fh_PI_mfts[[ik]]      = Akita_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Yamagata_male_fh_PI_mfts[[ik]]   = Yamagata_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Fukushima_male_fh_PI_mfts[[ik]]  = Fukushima_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Ibaraki_male_fh_PI_mfts[[ik]]    = Ibaraki_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Tochigi_male_fh_PI_mfts[[ik]]    = Tochigi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Gunma_male_fh_PI_mfts[[ik]]      = Gunma_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Saitama_male_fh_PI_mfts[[ik]]    = Saitama_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Chiba_male_fh_PI_mfts[[ik]]      = Chiba_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Tokyo_male_fh_PI_mfts[[ik]]      = Tokyo_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kanagawa_male_fh_PI_mfts[[ik]]   = Kanagawa_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Niigata_male_fh_PI_mfts[[ik]]    = Niigata_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Toyama_male_fh_PI_mfts[[ik]]     = Toyama_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Ishikawa_male_fh_PI_mfts[[ik]]   = Ishikawa_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Fukui_male_fh_PI_mfts[[ik]]      = Fukui_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Yamanashi_male_fh_PI_mfts[[ik]]  = Yamanashi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Nagano_male_fh_PI_mfts[[ik]]     = Nagano_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Gifu_male_fh_PI_mfts[[ik]]       = Gifu_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Shizuoka_male_fh_PI_mfts[[ik]]   = Shizuoka_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Aichi_male_fh_PI_mfts[[ik]]      = Aichi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Mie_male_fh_PI_mfts[[ik]]        = Mie_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Shiga_male_fh_PI_mfts[[ik]]      = Shiga_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kyoto_male_fh_PI_mfts[[ik]]      = Kyoto_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Osaka_male_fh_PI_mfts[[ik]]      = Osaka_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Hyogo_male_fh_PI_mfts[[ik]]      = Hyogo_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Nara_male_fh_PI_mfts[[ik]]       = Nara_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Wakayama_male_fh_PI_mfts[[ik]]   = Wakayama_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Tottori_male_fh_PI_mfts[[ik]]    = Tottori_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Shimane_male_fh_PI_mfts[[ik]]    = Shimane_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Okayama_male_fh_PI_mfts[[ik]]    = Okayama_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Hiroshima_male_fh_PI_mfts[[ik]]  = Hiroshima_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Yamaguchi_male_fh_PI_mfts[[ik]]  = Yamaguchi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Tokushima_male_fh_PI_mfts[[ik]]  = Tokushima_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kagawa_male_fh_PI_mfts[[ik]]     = Kagawa_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Ehime_male_fh_PI_mfts[[ik]]      = Ehime_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kochi_male_fh_PI_mfts[[ik]]      = Kochi_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Fukuoka_male_fh_PI_mfts[[ik]]    = Fukuoka_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Saga_male_fh_PI_mfts[[ik]]       = Saga_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Nagasaki_male_fh_PI_mfts[[ik]]   = Nagasaki_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kumamoto_male_fh_PI_mfts[[ik]]   = Kumamoto_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Oita_male_fh_PI_mfts[[ik]]       = Oita_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Miyazaki_male_fh_PI_mfts[[ik]]   = Miyazaki_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Kagoshima_male_fh_PI_mfts[[ik]]  = Kagoshima_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  Okinawa_male_fh_PI_mfts[[ik]]    = Okinawa_fh_PI_mfts[[ik]]$PI_boot_mfts_male
}


###############################
# Region level interval scores
###############################

#####
# R1
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R1_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R1, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R1_female_mfts = interval_score_R1_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R1_female_mfts[ij] = interval_score_mfts(PI_val = R1_fh_PI_mfts, 
                                                                data_series = mfts_R1, fh = ij)$score_female
  
  interval_score_R1_male_mfts[ij] = interval_score_mfts(PI_val = R1_fh_PI_mfts, 
                                                              data_series = mfts_R1, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

#####
# R2
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R2_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R2, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R2_female_mfts = interval_score_R2_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R2_female_mfts[ij] = interval_score_mfts(PI_val = R2_fh_PI_mfts, 
                                                          data_series = mfts_R2, fh = ij)$score_female
  
  interval_score_R2_male_mfts[ij] = interval_score_mfts(PI_val = R2_fh_PI_mfts, 
                                                        data_series = mfts_R2, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R3
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R3_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R3, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R3_female_mfts = interval_score_R3_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R3_female_mfts[ij] = interval_score_mfts(PI_val = R3_fh_PI_mfts, 
                                                          data_series = mfts_R3, fh = ij)$score_female
  
  interval_score_R3_male_mfts[ij] = interval_score_mfts(PI_val = R3_fh_PI_mfts, 
                                                        data_series = mfts_R3, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R4
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R4_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R4, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R4_female_mfts = interval_score_R4_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R4_female_mfts[ij] = interval_score_mfts(PI_val = R4_fh_PI_mfts, 
                                                          data_series = mfts_R4, fh = ij)$score_female
  
  interval_score_R4_male_mfts[ij] = interval_score_mfts(PI_val = R4_fh_PI_mfts, 
                                                        data_series = mfts_R4, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R5
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R5_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R5, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R5_female_mfts = interval_score_R5_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R5_female_mfts[ij] = interval_score_mfts(PI_val = R5_fh_PI_mfts, 
                                                          data_series = mfts_R5, fh = ij)$score_female
  
  interval_score_R5_male_mfts[ij] = interval_score_mfts(PI_val = R5_fh_PI_mfts, 
                                                        data_series = mfts_R5, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R6
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R6_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R6, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R6_female_mfts = interval_score_R6_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R6_female_mfts[ij] = interval_score_mfts(PI_val = R6_fh_PI_mfts, 
                                                          data_series = mfts_R6, fh = ij)$score_female
  
  interval_score_R6_male_mfts[ij] = interval_score_mfts(PI_val = R6_fh_PI_mfts, 
                                                        data_series = mfts_R6, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R7
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R7_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R7, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R7_female_mfts = interval_score_R7_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R7_female_mfts[ij] = interval_score_mfts(PI_val = R7_fh_PI_mfts, 
                                                          data_series = mfts_R7, fh = ij)$score_female
  
  interval_score_R7_male_mfts[ij] = interval_score_mfts(PI_val = R7_fh_PI_mfts, 
                                                        data_series = mfts_R7, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)


#####
# R8
#####

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

R8_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh_mfts(data_series = mfts_R8, pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_R8_female_mfts = interval_score_R8_male_mfts = rep(0,15)
for(ij in 1:15)
{
  interval_score_R8_female_mfts[ij] = interval_score_mfts(PI_val = R8_fh_PI_mfts, 
                                                          data_series = mfts_R8, fh = ij)$score_female
  
  interval_score_R8_male_mfts[ij] = interval_score_mfts(PI_val = R8_fh_PI_mfts, 
                                                        data_series = mfts_R8, fh = ij)$score_male
  
  print(ij)
}

stopCluster(cl)
rm(cl)

# separate into female_fh_PI_mfts and male_fh_PI_mfts

for(iw in 1:8)
{
  assign(noquote(paste("R", iw, "_female_fh_PI_mfts", sep = "")), list())
  assign(noquote(paste("R", iw, "_male_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  R1_female_fh_PI_mfts[[ik]]  = R1_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R2_female_fh_PI_mfts[[ik]]  = R2_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R3_female_fh_PI_mfts[[ik]]  = R3_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R4_female_fh_PI_mfts[[ik]]  = R4_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R5_female_fh_PI_mfts[[ik]]  = R5_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R6_female_fh_PI_mfts[[ik]]  = R6_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R7_female_fh_PI_mfts[[ik]]  = R7_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  R8_female_fh_PI_mfts[[ik]]  = R8_fh_PI_mfts[[ik]]$PI_boot_mfts_female
  
  R1_male_fh_PI_mfts[[ik]]    = R1_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R2_male_fh_PI_mfts[[ik]]    = R2_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R3_male_fh_PI_mfts[[ik]]    = R3_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R4_male_fh_PI_mfts[[ik]]    = R4_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R5_male_fh_PI_mfts[[ik]]    = R5_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R6_male_fh_PI_mfts[[ik]]    = R6_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R7_male_fh_PI_mfts[[ik]]    = R7_fh_PI_mfts[[ik]]$PI_boot_mfts_male
  R8_male_fh_PI_mfts[[ik]]    = R8_fh_PI_mfts[[ik]]$PI_boot_mfts_male
}

#########################################################################
# compute total series interval scores for a particular forecast horizon 
#########################################################################

##############################################################
# Prefecture level total series prediction intervals function
##############################################################

total_comb = total_comb_pop =  matrix(NA, 101*42, 47)
for(iw in 2:48)
{
  total_comb[,iw-1] = as.numeric(get(state[iw])$rate$total)
  total_comb_pop[,iw-1] = as.numeric(get(state[iw])$pop$total)
}
total_comb_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_comb)
total_comb_pop_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_comb_pop)
colnames(total_comb_v2) = colnames(total_comb_pop_v2) = c("Year", "Age", state[2:48])

write.table(total_comb_v2, file = "total_comb_unsmooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
write.table(total_comb_pop_v2, file = "total_comb_pop_unsmooth.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)

total_state_PI_fh_mfts <- function(fh, pcamethod = c("static", "dynamic"), nboot = 1000)
{
  PI_boot_mfts_total = array(, dim = c(101, nboot, (16-fh), 47))
  data_series = read.demogdata("total_comb.txt", "total_comb_pop.txt", 
                                        type = "mortality", label="total_comb", skip = 0)
  data_series_unsmooth = read.demogdata("total_comb_unsmooth.txt", "total_comb_pop_unsmooth.txt", 
                                        type = "mortality", label="total_comb_unsmooth", skip = 0)
  
  for(ij in 1:(16-fh))
  {
    dum_pointwise = PI_total_state_mfts(dat = extract.years(data_series, 1975:(2001+ij)), dat_unsmooth = extract.years(data_series_unsmooth, 1975:(2001+ij)), pcamethod = pcamethod, fh = fh)
    PI_boot_mfts_total[,,ij,]  = dum_pointwise$boot_sample_state_total
  }
  return(list(PI_boot_mfts_total = PI_boot_mfts_total))
}

########################################################
# Prefeture level total series interval scores function
########################################################

interval_score_state_mfts <- function(PI_val, data_series, fh, alpha = 0.8)
{
  test_val_total = extract.years(read.demogdata("total_comb_unsmooth.txt", "total_comb_pop_unsmooth.txt", 
                                        type = "mortality", label="total_comb_unsmooth", skip = 0), (2001+fh):2016)$rate

  # transform back to the original scale
  
  boot_sample_total = exp(PI_val[[fh]]$PI_boot_mfts_total) # an array of dim = c(101, 1000, (16-fh), 47)
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

##########################################################
# Region level total series prediction intervals function
##########################################################

total_region_comb = total_region_comb_pop = matrix(NA, 101*42, 8)
for(iw in 1:8)
{
  total_region_comb[,iw] = as.numeric(get(region[iw])$rate$total)
  total_region_comb_pop[,iw] = as.numeric(get(region[iw])$pop$total)
}
total_region_comb_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_region_comb)
total_region_comb_pop_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_region_comb_pop)
colnames(total_region_comb_v2) = colnames(total_region_comb_pop_v2) = c("Year", "Age", region)

write.table(total_region_comb_v2, file = "total_region_comb_unsmooth.txt", quote = FALSE, row.names = TRUE, col.names = TRUE)  
write.table(total_region_comb_pop_v2, file = "total_region_comb_pop_unsmooth.txt", quote = FALSE, row.names = TRUE, col.names = TRUE)

total_region_PI_fh_mfts <- function(fh, pcamethod = c("static", "dynamic"), nboot = 1000)
{
  PI_boot_mfts_total = array(, dim = c(101, nboot, (16-fh), 8))
  data_series = read.demogdata("total_region_comb.txt", "total_region_comb_pop.txt", 
                                        type = "mortality", label="total_region_comb", skip = 0)
  data_series_unsmooth = read.demogdata("total_region_comb_unsmooth.txt", "total_region_comb_pop_unsmooth.txt", 
                                        type = "mortality", label="total_region_comb_unsmooth", skip = 0)
  
  for(ij in 1:(16-fh))
  {
    dum_pointwise = PI_total_region_mfts(dat = extract.years(data_series, 1975:(2001+ij)), dat_unsmooth = extract.years(data_series_unsmooth, 1975:(2001+ij)), pcamethod = pcamethod, fh = fh)
    PI_boot_mfts_total[,,ij,]  = dum_pointwise$boot_sample_region_total
  }
  return(list(PI_boot_mfts_total = PI_boot_mfts_total))
}

#####################################################
# Region level total series interval scores function
#####################################################

interval_score_region_mfts <- function(PI_val, data_series, fh, alpha = 0.8)
{
  test_val_total = extract.years(read.demogdata("total_region_comb_unsmooth.txt", "total_region_comb_pop_unsmooth.txt", 
                                        type = "mortality", label="total_region_comb_unsmooth", skip = 0), (2001+fh):2016)$rate

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

##########################################################
# Calulating prefecture level total series interval score
##########################################################

cl <- makeCluster(4) 
registerDoParallel(cl)

state_total_fh_PI_mfts_1 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 1)
state_total_fh_PI_mfts_2 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 2)
state_total_fh_PI_mfts_3 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 3)
state_total_fh_PI_mfts_4 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 4)
state_total_fh_PI_mfts_5 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 5)
state_total_fh_PI_mfts_6 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 6)
state_total_fh_PI_mfts_7 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 7)
state_total_fh_PI_mfts_8 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 8)
state_total_fh_PI_mfts_9 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 9)
state_total_fh_PI_mfts_10 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 10)
state_total_fh_PI_mfts_11 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 11)
state_total_fh_PI_mfts_12 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 12)
state_total_fh_PI_mfts_13 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 13)
state_total_fh_PI_mfts_14 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 14)
state_total_fh_PI_mfts_15 = total_state_PI_fh_mfts(pcamethod = "dynamic", fh = 15)

state_total_fh_PI_mfts_1to4 = foreach(ik = 1:4, .packages = c("demography", "ftsa")) %dopar% total_state_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(state_total_fh_PI_mfts_1to4, file = "state_total_fh_PI_mfts_1to4.RData")

state_total_fh_PI_mfts_5to8 = foreach(ik = 5:8, .packages = c("demography", "ftsa")) %dopar% total_state_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(state_total_fh_PI_mfts_5to8, file = "state_total_fh_PI_mfts_5to8.RData")

state_total_fh_PI_mfts_9to12 = foreach(ik = 9:12, .packages = c("demography", "ftsa")) %dopar% total_state_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(state_total_fh_PI_mfts_9to12, file = "state_total_fh_PI_mfts_9to12.RData")

state_total_fh_PI_mfts_13to15 = foreach(ik = 13:15, .packages = c("demography", "ftsa")) %dopar% total_state_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(state_total_fh_PI_mfts_13to15, file = "state_total_fh_PI_mfts_13to15.RData")

state_total_fh_PI_mfts = do.call(c, list(state_total_fh_PI_mfts_1to4, state_total_fh_PI_mfts_5to8, state_total_fh_PI_mfts_9to12, state_total_fh_PI_mfts_13to15))
save(state_total_fh_PI_mfts, file = "state_total_fh_PI_mfts.RData")
###

state_total_fh_PI_mfts = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% total_state_PI_fh_mfts(pcamethod = "dynamic", fh = ik)

# compute interval scores for all forecast horizons

interval_score_state_total_mfts = matrix(NA, ncol = 15, nrow = 47)
for(ij in 1:15)
{
 interval_score_state_total_mfts[,ij] = as.numeric(interval_score_state_mfts(PI_val = state_total_fh_PI_mfts, fh = ij))
 print(ij)
}

stopCluster(cl)
rm(cl)

# separate "state_total_fh_PI_mfts" according to states

for(iw in 2:48)
{
  assign(noquote(paste(state[iw], "_total_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  Hokkaido_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,1]
  Aomori_total_fh_PI_mfts[[ik]]     = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,2]
  Iwate_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,3]
  Miyagi_total_fh_PI_mfts[[ik]]     = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,4]
  Akita_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,5]
  Yamagata_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,6]
  Fukushima_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,7]
  Ibaraki_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,8]
  Tochigi_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,9]
  Gunma_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,10]
  Saitama_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,11]
  Chiba_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,12]
  Tokyo_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,13]
  Kanagawa_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,14]
  Niigata_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,15]
  Toyama_total_fh_PI_mfts[[ik]]     = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,16]
  Ishikawa_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,17]
  Fukui_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,18]
  Yamanashi_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,19]
  Nagano_total_fh_PI_mfts[[ik]]     = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,20]
  Gifu_total_fh_PI_mfts[[ik]]       = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,21]
  Shizuoka_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,22]
  Aichi_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,23]
  Mie_total_fh_PI_mfts[[ik]]        = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,24]
  Shiga_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,25]
  Kyoto_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,26]
  Osaka_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,27]
  Hyogo_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,28]
  Nara_total_fh_PI_mfts[[ik]]       = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,29]
  Wakayama_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,30]
  Tottori_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,31]
  Shimane_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,32]
  Okayama_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,33]
  Hiroshima_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,34]
  Yamaguchi_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,35]
  Tokushima_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,36]
  Kagawa_total_fh_PI_mfts[[ik]]     = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,37]
  Ehime_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,38]
  Kochi_total_fh_PI_mfts[[ik]]      = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,39]
  Fukuoka_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,40]
  Saga_total_fh_PI_mfts[[ik]]       = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,41]
  Nagasaki_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,42]
  Kumamoto_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,43]
  Oita_total_fh_PI_mfts[[ik]]       = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,44]
  Miyazaki_total_fh_PI_mfts[[ik]]   = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,45]
  Kagoshima_total_fh_PI_mfts[[ik]]  = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,46]
  Okinawa_total_fh_PI_mfts[[ik]]    = state_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,47]
}


Japan_total_fh_PI_mfts  = Japan_total_fh_PI
Japan_female_fh_PI_mfts = Japan_female_fh_PI
Japan_male_fh_PI_mfts   = Japan_male_fh_PI

# separate "interval_score_state_total_mfts" into vectors
for(ik in 2:48)
{
  assign(noquote(paste("interval_score_", state[ik], "_total_mfts", sep = "")), interval_score_state_total_mfts[ik-1,])
}

###

for(i in 2:48)
{
  rm(list = c(paste(state[i], "_total_fh_PI_mfts", sep = "")))
}



##########################################
# region level total series interval score
##########################################

cl <- makeCluster(15) 
registerDoParallel(cl)


region_total_fh_PI_mfts_1 = foreach(ik = 1:4, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_1, file = "region_total_fh_PI_mfts_1.RData")

region_total_fh_PI_mfts_2 = foreach(ik = 5:8, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_2, file = "region_total_fh_PI_mfts_2.RData")

region_total_fh_PI_mfts_3 = foreach(ik = 9:12, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_3, file = "region_total_fh_PI_mfts_3.RData")

region_total_fh_PI_mfts_4 = foreach(ik = 13:15, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_4, file = "region_total_fh_PI_mfts_4.RData")


region_total_fh_PI_mfts = do.call(c, list(region_total_fh_PI_mfts_1, region_total_fh_PI_mfts_2, region_total_fh_PI_mfts_3, region_total_fh_PI_mfts_4))
save(region_total_fh_PI_mfts, file = "region_total_fh_PI_mfts.RData")
rm(region_total_fh_PI_mfts_1, region_total_fh_PI_mfts_2, region_total_fh_PI_mfts_3, region_total_fh_PI_mfts_4)


###
region_total_fh_PI_mfts_1to8 = foreach(ik = 1:8, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_1to8, file = "region_total_fh_PI_mfts_1to8.RData")

region_total_fh_PI_mfts_9to15 = foreach(ik = 9:15, .packages = c("demography", "ftsa")) %dopar% total_region_PI_fh_mfts(pcamethod = "dynamic", fh = ik)
save(region_total_fh_PI_mfts_9to15, file = "region_total_fh_PI_mfts_9to15.RData")

region_total_fh_PI_mfts = do.call(c, list(region_total_fh_PI_mfts_1to8, region_total_fh_PI_mfts_9to15))
save(region_total_fh_PI_mfts, file = "region_total_fh_PI_mfts.RData")
####

# compute interval scores for all forecast horizons

interval_score_region_total_mfts = matrix(NA, ncol = 15, nrow = 8)
for(ij in 1:15)
{
 interval_score_region_total_mfts[,ij] = as.numeric(interval_score_region_mfts(PI_val = region_total_fh_PI_mfts, fh = ij))
 print(ij)
}

stopCluster(cl)
rm(cl)

# separate into vectors

for(ik in 1:8)
{
  assign(noquote(paste("interval_score_R", ik, "_total_mfts", sep = "")), interval_score_region_total_mfts[ik,])
}

# separate into female_total_fh_PI_mfts and 

for(iw in 1:8)
{
  assign(noquote(paste("R", iw, "_total_fh_PI_mfts", sep = "")), list())
}

for(ik in 1:15)
{
  R1_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,1]
  R2_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,2]
  R3_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,3]
  R4_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,4]
  R5_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,5]
  R6_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,6]
  R7_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,7]
  R8_total_fh_PI_mfts[[ik]]  = region_total_fh_PI_mfts[[ik]]$PI_boot_mfts_total[,,,8]
  
}

for(i in 1:8)
{
  rm(list = c(paste("R",i, "_total_fh_PI_mfts", sep = "")))
}


save(R1_female_fh_PI_mfts, file = "R1_female_fh_PI_mfts.RData")
save(R2_female_fh_PI_mfts, file = "R2_female_fh_PI_mfts.RData")
save(R3_female_fh_PI_mfts, file = "R3_female_fh_PI_mfts.RData")
save(R4_female_fh_PI_mfts, file = "R4_female_fh_PI_mfts.RData")
save(R5_female_fh_PI_mfts, file = "R5_female_fh_PI_mfts.RData")
save(R6_female_fh_PI_mfts, file = "R6_female_fh_PI_mfts.RData")
save(R7_female_fh_PI_mfts, file = "R7_female_fh_PI_mfts.RData")
save(R8_female_fh_PI_mfts, file = "R8_female_fh_PI_mfts.RData")

save(R1_male_fh_PI_mfts, file = "R1_male_fh_PI_mfts.RData")
save(R2_male_fh_PI_mfts, file = "R2_male_fh_PI_mfts.RData")
save(R3_male_fh_PI_mfts, file = "R3_male_fh_PI_mfts.RData")
save(R4_male_fh_PI_mfts, file = "R4_male_fh_PI_mfts.RData")
save(R5_male_fh_PI_mfts, file = "R5_male_fh_PI_mfts.RData")
save(R6_male_fh_PI_mfts, file = "R6_male_fh_PI_mfts.RData")
save(R7_male_fh_PI_mfts, file = "R7_male_fh_PI_mfts.RData")
save(R8_male_fh_PI_mfts, file = "R8_male_fh_PI_mfts.RData")

for(i in 1:8)
{
  rm(list = c(paste("R",i, "_female_fh_PI_mfts", sep = "")))
  rm(list = c(paste("R",i, "_male_fh_PI_mfts", sep = "")))
}


################################################################
# Calculate interval score at different levels of the hierarchy
################################################################

# Japan (total)

interval_score_Level_0_mfts = interval_score_Japan_total

# Japan (female + male)

interval_score_Level_1_mfts = rowMeans(cbind(interval_score_Japan_female_mfts, interval_score_Japan_male_mfts))

# Japan region (total)

interval_score_Level_2_mfts = rowMeans(cbind(interval_score_R1_total_mfts, interval_score_R2_total_mfts, interval_score_R3_total_mfts, interval_score_R4_total_mfts,
                                        interval_score_R5_total_mfts, interval_score_R6_total_mfts, interval_score_R7_total_mfts, interval_score_R8_total_mfts))


# Japan region (female + male)

interval_score_Level_3_mfts = rowMeans(cbind(interval_score_R1_female_mfts, interval_score_R2_female_mfts, interval_score_R3_female_mfts, interval_score_R4_female_mfts,
                                        interval_score_R5_female_mfts, interval_score_R6_female_mfts, interval_score_R7_female_mfts, interval_score_R8_female_mfts,
                                        interval_score_R1_male_mfts,   interval_score_R2_male_mfts,   interval_score_R3_male_mfts,   interval_score_R4_male_mfts,
                                        interval_score_R5_male_mfts,   interval_score_R6_male_mfts,   interval_score_R7_male_mfts,   interval_score_R8_male_mfts))

# Japan prefecture (total)

interval_score_Level_4_mfts = rowMeans(cbind(interval_score_Hokkaido_total_mfts, interval_score_Aomori_total_mfts,
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


# Japan prefecture (female + male)

interval_score_Level_5_mfts = rowMeans(cbind(interval_score_Hokkaido_female_mfts, interval_score_Aomori_female_mfts,
                                        interval_score_Iwate_female_mfts, interval_score_Miyagi_female_mfts,
                                        interval_score_Akita_female_mfts, interval_score_Yamagata_female_mfts,
                                        interval_score_Fukushima_female_mfts, interval_score_Ibaraki_female_mfts,
                                        interval_score_Tochigi_female_mfts, interval_score_Gunma_female_mfts,
                                        interval_score_Saitama_female_mfts, interval_score_Chiba_female_mfts,
                                        interval_score_Tokyo_female_mfts, interval_score_Kanagawa_female_mfts,
                                        interval_score_Niigata_female_mfts, interval_score_Toyama_female_mfts,
                                        interval_score_Ishikawa_female_mfts, interval_score_Fukui_female_mfts,
                                        interval_score_Yamanashi_female_mfts, interval_score_Nagano_female_mfts,
                                        interval_score_Gifu_female_mfts, interval_score_Shizuoka_female_mfts,
                                        interval_score_Aichi_female_mfts, interval_score_Mie_female_mfts,
                                        interval_score_Shiga_female_mfts, interval_score_Kyoto_female_mfts,
                                        interval_score_Osaka_female_mfts, interval_score_Hyogo_female_mfts,
                                        interval_score_Nara_female_mfts, interval_score_Wakayama_female_mfts,
                                        interval_score_Tottori_female_mfts, interval_score_Shimane_female_mfts,
                                        interval_score_Okayama_female_mfts, interval_score_Hiroshima_female_mfts,
                                        interval_score_Yamaguchi_female_mfts, interval_score_Tokushima_female_mfts,
                                        interval_score_Kagawa_female_mfts, interval_score_Ehime_female_mfts,
                                        interval_score_Kochi_female_mfts, interval_score_Fukuoka_female_mfts,
                                        interval_score_Saga_female_mfts, interval_score_Nagasaki_female_mfts,
                                        interval_score_Kumamoto_female_mfts, interval_score_Oita_female_mfts,
                                        interval_score_Miyazaki_female_mfts, interval_score_Kagoshima_female_mfts,
                                        interval_score_Okinawa_female_mfts,
                                        interval_score_Hokkaido_male_mfts, interval_score_Aomori_male_mfts,
                                        interval_score_Iwate_male_mfts, interval_score_Miyagi_male_mfts,
                                        interval_score_Akita_male_mfts, interval_score_Yamagata_male_mfts,
                                        interval_score_Fukushima_male_mfts, interval_score_Ibaraki_male_mfts,
                                        interval_score_Tochigi_male_mfts, interval_score_Gunma_male_mfts,
                                        interval_score_Saitama_male_mfts, interval_score_Chiba_male_mfts,
                                        interval_score_Tokyo_male_mfts, interval_score_Kanagawa_male_mfts,
                                        interval_score_Niigata_male_mfts, interval_score_Toyama_male_mfts,
                                        interval_score_Ishikawa_male_mfts, interval_score_Fukui_male_mfts,
                                        interval_score_Yamanashi_male_mfts, interval_score_Nagano_male_mfts,
                                        interval_score_Gifu_male_mfts, interval_score_Shizuoka_male_mfts,
                                        interval_score_Aichi_male_mfts, interval_score_Mie_male_mfts,
                                        interval_score_Shiga_male_mfts, interval_score_Kyoto_male_mfts,
                                        interval_score_Osaka_male_mfts, interval_score_Hyogo_male_mfts,
                                        interval_score_Nara_male_mfts, interval_score_Wakayama_male_mfts,
                                        interval_score_Tottori_male_mfts, interval_score_Shimane_male_mfts,
                                        interval_score_Okayama_male_mfts, interval_score_Hiroshima_male_mfts,
                                        interval_score_Yamaguchi_male_mfts, interval_score_Tokushima_male_mfts,
                                        interval_score_Kagawa_male_mfts, interval_score_Ehime_male_mfts,
                                        interval_score_Kochi_male_mfts, interval_score_Fukuoka_male_mfts,
                                        interval_score_Saga_male_mfts, interval_score_Nagasaki_male_mfts,
                                        interval_score_Kumamoto_male_mfts, interval_score_Oita_male_mfts,
                                        interval_score_Miyazaki_male_mfts, interval_score_Kagoshima_male_mfts,
                                        interval_score_Okinawa_male_mfts))

# Averaged interval scores at each level of the hierarchy

interval_score_all_mfts = cbind(interval_score_Level_0_mfts, interval_score_Level_1_mfts, interval_score_Level_2_mfts, interval_score_Level_3_mfts,
                           interval_score_Level_4_mfts, interval_score_Level_5_mfts)
interval_score_all_stats_mfts = rbind(interval_score_all_mfts, colMeans(interval_score_all_mfts), apply(interval_score_all_mfts, 2, median))
colnames(interval_score_all_stats_mfts) = c("Level 0", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
rownames(interval_score_all_stats_mfts) = c(1:15,"Mean","Median")

