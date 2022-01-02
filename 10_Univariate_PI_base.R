##############################################################################################
# Calculate prediction intervals for the univariate forecasting method without reconciliation
##############################################################################################

library(demography)
library(ftsa)

# data_series: specific data series
# series: total, female, male
# fh: forecast horizon
# nboot: number of bootstrap replications

# Define a function to find pointwise prediction intervals by bootstrap

PI_fh <- function(data_series, pcamethod = c("static", "dynamic"), series, fh, nboot = 1000)
{
  PI_boot  = array(NA, dim = c(length(data_series$age), nboot, (16-fh)))
  for(ij in 1:(16-fh))
  {
    dum_pointwise = find_enlarge_val(data_series = extract.years(data_series, 1975:(2001+ij)), 
                                     series = series, fh = fh, pcamethod = pcamethod, transformation = "log")
    PI_boot[,,ij]  = dum_pointwise$boot_sample
  }
  return(list(PI_boot = PI_boot))
}

# Define a function to calculate interval scores for the bootstrapped PIs

## PI_val: one- to 15-step-ahead prediction intervals
## alpha: nominal coverage probability (alpha here is equivalent to 1 - significance level)

interval_score <- function(PI_val, data_series, series, fh, alpha = 0.8)
{
  if(series == "female")
  {
    test_val = extract.years(data_series, (2001+fh):2016)$rate$female
  }
  if(series == "male")
  {
    test_val = extract.years(data_series, (2001+fh):2016)$rate$male
  }
  if(series == "total")
  {
    test_val = extract.years(data_series, (2001+fh):2016)$rate$total
  }
  # transform back to the original scale
  
  boot_sample = exp(PI_val[[fh]]$PI_boot)
  boot_index = which(boot_sample > 1)
  boot_index_below = which(boot_sample < 0)
  if(length(boot_index) > 0)
  {
    boot_sample_v1 = replace(boot_sample, boot_index, 1)
  } else
  {
    boot_sample_v1 = boot_sample
  }
  if(length(boot_index_below) > 0)
  {
    boot_sample_v2 = replace(boot_sample_v1, boot_index_below, 0)
  } else
  {
    boot_sample_v2 = boot_sample_v1
  }
  
  # lower and upper bounds  
  
  dummy = apply(boot_sample, c(1,3), quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
  PI_lb_val = dummy[1,,]
  PI_ub_val = dummy[2,,]
  
  lb_ind = ifelse(test_val < PI_lb_val, 1, 0)
  ub_ind = ifelse(test_val > PI_ub_val, 1, 0)
  score  = mean((PI_ub_val - PI_lb_val) + 2/(1 - alpha) * (PI_lb_val - test_val) * lb_ind + 
                  2/(1 - alpha) * (test_val - PI_ub_val) * ub_ind)
  return(score)
}

########################
# Japan (country level)
########################

# Compute pointwise prediction intervals for all 15 forecast horizons

library(doParallel)

cl <- makeCluster(15) 
registerDoParallel(cl)

Japan_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Japan ,series = "female", pcamethod = "dynamic",  fh = ik)

Japan_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Japan ,series = "male", pcamethod = "dynamic",  fh = ik)

Japan_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Japan, series = "total", pcamethod = "dynamic",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Japan_female_dynamic = interval_score_Japan_male_dynamic = interval_score_Japan_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Japan_female_dynamic[ij] = interval_score(PI_val = Japan_female_fh_PI_dynamic, 
                                                   data_series = Japan, series = "female", fh = ij)
  
  interval_score_Japan_male_dynamic[ij]   = interval_score(PI_val = Japan_male_fh_PI_dynamic, 
                                                 data_series = Japan, series = "male", fh = ij)
  
  interval_score_Japan_total_dynamic[ij]  = interval_score(PI_val = Japan_total_fh_PI_dynamic, 
                                                  data_series = Japan, series = "total", fh = ij)
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

Hokkaido_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Hokkaido, series = "female", fh = ik)

Hokkaido_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Hokkaido, series = "male",   fh = ik)

Hokkaido_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Hokkaido, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hokkaido_female_dynamic = interval_score_Hokkaido_male_dynamic = interval_score_Hokkaido_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Hokkaido_female_dynamic[ij] = interval_score(PI_val = Hokkaido_female_fh_PI_dynamic, 
                                                      data_series = Hokkaido, series = "female", fh = ij)
  
  interval_score_Hokkaido_male_dynamic[ij] = interval_score(PI_val = Hokkaido_male_fh_PI_dynamic, 
                                                    data_series = Hokkaido, series = "male", fh = ij)
  
  interval_score_Hokkaido_total_dynamic[ij] = interval_score(PI_val = Hokkaido_total_fh_PI_dynamic, 
                                                     data_series = Hokkaido, series = "total", fh = ij)
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

Aomori_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Aomori, series = "female", fh = ik)

Aomori_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Aomori, series = "male",   fh = ik)

Aomori_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa")) %dopar% PI_fh(data_series = Aomori, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Aomori_female_dynamic = interval_score_Aomori_male_dynamic = interval_score_Aomori_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Aomori_female_dynamic[ij] = interval_score(PI_val = Aomori_female_fh_PI_dynamic, 
                                                    data_series = Aomori, series = "female", fh = ij)
  
  interval_score_Aomori_male_dynamic[ij] = interval_score(PI_val = Aomori_male_fh_PI_dynamic, 
                                                  data_series = Aomori, series = "male", fh = ij)
  
  interval_score_Aomori_total_dynamic[ij] = interval_score(PI_val = Aomori_total_fh_PI_dynamic, 
                                                   data_series = Aomori, series = "total", fh = ij)
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

Iwate_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Iwate, series = "female", fh = ik)

Iwate_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Iwate, series = "male",   fh = ik)

Iwate_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Iwate, series = "total",  fh = ik)


# compute interval scores for all forecast horizons

interval_score_Iwate_female_dynamic = interval_score_Iwate_male_dynamic = interval_score_Iwate_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Iwate_female_dynamic[ij] = interval_score(PI_val = Iwate_female_fh_PI_dynamic, 
                                                   data_series = Iwate, series = "female", fh = ij)
  
  interval_score_Iwate_male_dynamic[ij] = interval_score(PI_val = Iwate_male_fh_PI_dynamic, 
                                                 data_series = Iwate, series = "male", fh = ij)
  
  interval_score_Iwate_total_dynamic[ij] = interval_score(PI_val = Iwate_total_fh_PI_dynamic, 
                                                  data_series = Iwate, series = "total", fh = ij)
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

Miyagi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyagi, series = "female", fh = ik)

Miyagi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyagi, series = "male",   fh = ik)

Miyagi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyagi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Miyagi_female_dynamic = interval_score_Miyagi_male_dynamic = interval_score_Miyagi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Miyagi_female_dynamic[ij] = interval_score(PI_val = Miyagi_female_fh_PI_dynamic, 
                                                    data_series = Miyagi, series = "female", fh = ij)
  
  interval_score_Miyagi_male_dynamic[ij] = interval_score(PI_val = Miyagi_male_fh_PI_dynamic, 
                                                  data_series = Miyagi, series = "male", fh = ij)
  
  interval_score_Miyagi_total_dynamic[ij] = interval_score(PI_val = Miyagi_total_fh_PI_dynamic, 
                                                   data_series = Miyagi, series = "total", fh = ij)
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

Akita_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Akita, series = "female", fh = ik)

Akita_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Akita, series = "male",   fh = ik)

Akita_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Akita, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Akita_female_dynamic = interval_score_Akita_male_dynamic = interval_score_Akita_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Akita_female_dynamic[ij] = interval_score(PI_val = Akita_female_fh_PI_dynamic, 
                                                   data_series = Akita, series = "female", fh = ij)
  
  interval_score_Akita_male_dynamic[ij] = interval_score(PI_val = Akita_male_fh_PI_dynamic, 
                                                 data_series = Akita, series = "male", fh = ij)
  
  interval_score_Akita_total_dynamic[ij] = interval_score(PI_val = Akita_total_fh_PI_dynamic, 
                                                  data_series = Akita, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

###########
# Yamagata
###########

# pointwise prediction interval for all 15 forecast horizons

cl <- makeCluster(15) 
registerDoParallel(cl)

Yamagata_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamagata, series = "female", fh = ik)

Yamagata_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamagata, series = "male",   fh = ik)

Yamagata_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamagata, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamagata_female_dynamic = interval_score_Yamagata_male_dynamic = interval_score_Yamagata_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Yamagata_female_dynamic[ij] = interval_score(PI_val = Yamagata_female_fh_PI_dynamic, 
                                                      data_series = Yamagata, series = "female", fh = ij)
  
  interval_score_Yamagata_male_dynamic[ij] = interval_score(PI_val = Yamagata_male_fh_PI_dynamic, 
                                                    data_series = Yamagata, series = "male", fh = ij)
  
  interval_score_Yamagata_total_dynamic[ij] = interval_score(PI_val = Yamagata_total_fh_PI_dynamic, 
                                                     data_series = Yamagata, series = "total", fh = ij)
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

Fukushima_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukushima, series = "female", fh = ik)

Fukushima_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukushima, series = "male",   fh = ik)

Fukushima_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukushima, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukushima_female_dynamic = interval_score_Fukushima_male_dynamic = interval_score_Fukushima_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Fukushima_female_dynamic[ij] = interval_score(PI_val = Fukushima_female_fh_PI_dynamic, 
                                                       data_series = Fukushima, series = "female", fh = ij)
  
  interval_score_Fukushima_male_dynamic[ij] = interval_score(PI_val = Fukushima_male_fh_PI_dynamic, 
                                                     data_series = Fukushima, series = "male", fh = ij)
  
  interval_score_Fukushima_total_dynamic[ij] = interval_score(PI_val = Fukushima_total_fh_PI_dynamic, 
                                                      data_series = Fukushima, series = "total", fh = ij)
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

Ibaraki_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ibaraki, series = "female", fh = ik)

Ibaraki_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ibaraki, series = "male",   fh = ik)

Ibaraki_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ibaraki, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ibaraki_female_dynamic = interval_score_Ibaraki_male_dynamic = interval_score_Ibaraki_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Ibaraki_female_dynamic[ij] = interval_score(PI_val = Ibaraki_female_fh_PI_dynamic, 
                                                     data_series = Ibaraki, series = "female", fh = ij)
  
  interval_score_Ibaraki_male_dynamic[ij] = interval_score(PI_val = Ibaraki_male_fh_PI_dynamic, 
                                                   data_series = Ibaraki, series = "male", fh = ij)
  
  interval_score_Ibaraki_total_dynamic[ij] = interval_score(PI_val = Ibaraki_total_fh_PI_dynamic, 
                                                    data_series = Ibaraki, series = "total", fh = ij)
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

Tochigi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tochigi, series = "female", fh = ik)

Tochigi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tochigi, series = "male",   fh = ik)

Tochigi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tochigi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tochigi_female_dynamic = interval_score_Tochigi_male_dynamic = interval_score_Tochigi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Tochigi_female_dynamic[ij] = interval_score(PI_val = Tochigi_female_fh_PI_dynamic, 
                                                     data_series = Tochigi, series = "female", fh = ij)
  
  interval_score_Tochigi_male_dynamic[ij] = interval_score(PI_val = Tochigi_male_fh_PI_dynamic, 
                                                   data_series = Tochigi, series = "male", fh = ij)
  
  interval_score_Tochigi_total_dynamic[ij] = interval_score(PI_val = Tochigi_total_fh_PI_dynamic, 
                                                    data_series = Tochigi, series = "total", fh = ij)
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

Gunma_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gunma, series = "female", fh = ik)

Gunma_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gunma, series = "male",   fh = ik)

Gunma_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gunma, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Gunma_female_dynamic = interval_score_Gunma_male_dynamic = interval_score_Gunma_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Gunma_female_dynamic[ij] = interval_score(PI_val = Gunma_female_fh_PI_dynamic, 
                                                   data_series = Gunma, series = "female", fh = ij)
  
  interval_score_Gunma_male_dynamic[ij] = interval_score(PI_val = Gunma_male_fh_PI_dynamic, 
                                                 data_series = Gunma, series = "male", fh = ij)
  
  interval_score_Gunma_total_dynamic[ij] = interval_score(PI_val = Gunma_total_fh_PI_dynamic, 
                                                  data_series = Gunma, series = "total", fh = ij)
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

Saitama_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saitama, series = "female", fh = ik)

Saitama_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saitama, series = "male",   fh = ik)

Saitama_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saitama, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Saitama_female_dynamic = interval_score_Saitama_male_dynamic = interval_score_Saitama_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Saitama_female_dynamic[ij] = interval_score(PI_val = Saitama_female_fh_PI_dynamic, 
                                                     data_series = Saitama, series = "female", fh = ij)
  
  interval_score_Saitama_male_dynamic[ij] = interval_score(PI_val = Saitama_male_fh_PI_dynamic, 
                                                   data_series = Saitama, series = "male", fh = ij)
  
  interval_score_Saitama_total_dynamic[ij] = interval_score(PI_val = Saitama_total_fh_PI_dynamic, 
                                                    data_series = Saitama, series = "total", fh = ij)
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

Chiba_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Chiba, series = "female", fh = ik)

Chiba_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Chiba, series = "male",   fh = ik)

Chiba_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Chiba, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Chiba_female_dynamic = interval_score_Chiba_male_dynamic = interval_score_Chiba_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Chiba_female_dynamic[ij] = interval_score(PI_val = Chiba_female_fh_PI_dynamic, 
                                                   data_series = Chiba, series = "female", fh = ij)
  
  interval_score_Chiba_male_dynamic[ij] = interval_score(PI_val = Chiba_male_fh_PI_dynamic, 
                                                 data_series = Chiba, series = "male", fh = ij)
  
  interval_score_Chiba_total_dynamic[ij] = interval_score(PI_val = Chiba_total_fh_PI_dynamic, 
                                                  data_series = Chiba, series = "total", fh = ij)
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

Tokyo_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokyo, series = "female", fh = ik)

Tokyo_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokyo, series = "male",   fh = ik)

Tokyo_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokyo, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tokyo_female_dynamic = interval_score_Tokyo_male_dynamic = interval_score_Tokyo_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Tokyo_female_dynamic[ij] = interval_score(PI_val = Tokyo_female_fh_PI_dynamic, 
                                                   data_series = Tokyo, series = "female", fh = ij)
  
  interval_score_Tokyo_male_dynamic[ij] = interval_score(PI_val = Tokyo_male_fh_PI_dynamic, 
                                                 data_series = Tokyo, series = "male", fh = ij)
  
  interval_score_Tokyo_total_dynamic[ij] = interval_score(PI_val = Tokyo_total_fh_PI_dynamic, 
                                                  data_series = Tokyo, series = "total", fh = ij)
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

Kanagawa_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kanagawa, series = "female", fh = ik)

Kanagawa_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kanagawa, series = "male",   fh = ik)

Kanagawa_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kanagawa, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kanagawa_female_dynamic = interval_score_Kanagawa_male_dynamic = interval_score_Kanagawa_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kanagawa_female_dynamic[ij] = interval_score(PI_val = Kanagawa_female_fh_PI_dynamic, 
                                                      data_series = Kanagawa, series = "female", fh = ij)
  
  interval_score_Kanagawa_male_dynamic[ij] = interval_score(PI_val = Kanagawa_male_fh_PI_dynamic, 
                                                    data_series = Kanagawa, series = "male", fh = ij)
  
  interval_score_Kanagawa_total_dynamic[ij] = interval_score(PI_val = Kanagawa_total_fh_PI_dynamic, 
                                                     data_series = Kanagawa, series = "total", fh = ij)
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

Niigata_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Niigata, series = "female", fh = ik)

Niigata_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Niigata, series = "male",   fh = ik)

Niigata_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Niigata, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Niigata_female_dynamic = interval_score_Niigata_male_dynamic = interval_score_Niigata_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Niigata_female_dynamic[ij] = interval_score(PI_val = Niigata_female_fh_PI_dynamic, 
                                                     data_series = Niigata, series = "female", fh = ij)
  
  interval_score_Niigata_male_dynamic[ij] = interval_score(PI_val = Niigata_male_fh_PI_dynamic, 
                                                   data_series = Niigata, series = "male", fh = ij)
  
  interval_score_Niigata_total_dynamic[ij] = interval_score(PI_val = Niigata_total_fh_PI_dynamic, 
                                                    data_series = Niigata, series = "total", fh = ij)
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

Toyama_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Toyama, series = "female", fh = ik)

Toyama_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Toyama, series = "male",   fh = ik)

Toyama_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Toyama, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Toyama_female_dynamic = interval_score_Toyama_male_dynamic = interval_score_Toyama_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Toyama_female_dynamic[ij] = interval_score(PI_val = Toyama_female_fh_PI_dynamic, 
                                                    data_series = Toyama, series = "female", fh = ij)
  
  interval_score_Toyama_male_dynamic[ij] = interval_score(PI_val = Toyama_male_fh_PI_dynamic, 
                                                  data_series = Toyama, series = "male", fh = ij)
  
  interval_score_Toyama_total_dynamic[ij] = interval_score(PI_val = Toyama_total_fh_PI_dynamic, 
                                                   data_series = Toyama, series = "total", fh = ij)
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

Ishikawa_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ishikawa, series = "female", fh = ik)

Ishikawa_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ishikawa, series = "male",   fh = ik)

Ishikawa_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ishikawa, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ishikawa_female_dynamic = interval_score_Ishikawa_male_dynamic = interval_score_Ishikawa_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Ishikawa_female_dynamic[ij] = interval_score(PI_val = Ishikawa_female_fh_PI_dynamic, 
                                                      data_series = Ishikawa, series = "female", fh = ij)
  
  interval_score_Ishikawa_male_dynamic[ij] = interval_score(PI_val = Ishikawa_male_fh_PI_dynamic, 
                                                    data_series = Ishikawa, series = "male", fh = ij)
  
  interval_score_Ishikawa_total_dynamic[ij] = interval_score(PI_val = Ishikawa_total_fh_PI_dynamic, 
                                                     data_series = Ishikawa, series = "total", fh = ij)
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

Fukui_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukui, series = "female", fh = ik)

Fukui_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukui, series = "male",   fh = ik)

Fukui_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukui, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukui_female_dynamic = interval_score_Fukui_male_dynamic = interval_score_Fukui_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Fukui_female_dynamic[ij] = interval_score(PI_val = Fukui_female_fh_PI_dynamic, 
                                                   data_series = Fukui, series = "female", fh = ij)
  
  interval_score_Fukui_male_dynamic[ij] = interval_score(PI_val = Fukui_male_fh_PI_dynamic, 
                                                 data_series = Fukui, series = "male", fh = ij)
  
  interval_score_Fukui_total_dynamic[ij] = interval_score(PI_val = Fukui_total_fh_PI_dynamic, 
                                                  data_series = Fukui, series = "total", fh = ij)
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

Yamanashi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamanashi, series = "female", fh = ik)

Yamanashi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamanashi, series = "male",   fh = ik)

Yamanashi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamanashi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamanashi_female_dynamic = interval_score_Yamanashi_male_dynamic = interval_score_Yamanashi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Yamanashi_female_dynamic[ij] = interval_score(PI_val = Yamanashi_female_fh_PI_dynamic, 
                                                       data_series = Yamanashi, series = "female", fh = ij)
  
  interval_score_Yamanashi_male_dynamic[ij] = interval_score(PI_val = Yamanashi_male_fh_PI_dynamic, 
                                                     data_series = Yamanashi, series = "male", fh = ij)
  
  interval_score_Yamanashi_total_dynamic[ij] = interval_score(PI_val = Yamanashi_total_fh_PI_dynamic, 
                                                      data_series = Yamanashi, series = "total", fh = ij)
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

Nagano_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagano, series = "female", fh = ik)

Nagano_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagano, series = "male",   fh = ik)

Nagano_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagano, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nagano_female_dynamic = interval_score_Nagano_male_dynamic = interval_score_Nagano_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Nagano_female_dynamic[ij] = interval_score(PI_val = Nagano_female_fh_PI_dynamic, 
                                                    data_series = Nagano, series = "female", fh = ij)
  
  interval_score_Nagano_male_dynamic[ij] = interval_score(PI_val = Nagano_male_fh_PI_dynamic, 
                                                  data_series = Nagano, series = "male", fh = ij)
  
  interval_score_Nagano_total_dynamic[ij] = interval_score(PI_val = Nagano_total_fh_PI_dynamic, 
                                                   data_series = Nagano, series = "total", fh = ij)
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

Gifu_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gifu, series = "female", fh = ik)

Gifu_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gifu, series = "male",   fh = ik)

Gifu_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Gifu, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Gifu_female_dynamic = interval_score_Gifu_male_dynamic = interval_score_Gifu_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Gifu_female_dynamic[ij] = interval_score(PI_val = Gifu_female_fh_PI_dynamic, 
                                                  data_series = Gifu, series = "female", fh = ij)
  
  interval_score_Gifu_male_dynamic[ij] = interval_score(PI_val = Gifu_male_fh_PI_dynamic, 
                                                data_series = Gifu, series = "male", fh = ij)
  
  interval_score_Gifu_total_dynamic[ij] = interval_score(PI_val = Gifu_total_fh_PI_dynamic, 
                                                 data_series = Gifu, series = "total", fh = ij)
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

Shizuoka_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shizuoka, series = "female", fh = ik)

Shizuoka_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shizuoka, series = "male",   fh = ik)

Shizuoka_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shizuoka, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shizuoka_female_dynamic = interval_score_Shizuoka_male_dynamic = interval_score_Shizuoka_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Shizuoka_female_dynamic[ij] = interval_score(PI_val = Shizuoka_female_fh_PI_dynamic, 
                                                      data_series = Shizuoka, series = "female", fh = ij)
  
  interval_score_Shizuoka_male_dynamic[ij] = interval_score(PI_val = Shizuoka_male_fh_PI_dynamic, 
                                                    data_series = Shizuoka, series = "male", fh = ij)
  
  interval_score_Shizuoka_total_dynamic[ij] = interval_score(PI_val = Shizuoka_total_fh_PI_dynamic, 
                                                     data_series = Shizuoka, series = "total", fh = ij)
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

Aichi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Aichi, series = "female", fh = ik)

Aichi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Aichi, series = "male",   fh = ik)

Aichi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Aichi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Aichi_female_dynamic = interval_score_Aichi_male_dynamic = interval_score_Aichi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Aichi_female_dynamic[ij] = interval_score(PI_val = Aichi_female_fh_PI_dynamic, 
                                                   data_series = Aichi, series = "female", fh = ij)
  
  interval_score_Aichi_male_dynamic[ij] = interval_score(PI_val = Aichi_male_fh_PI_dynamic, 
                                                 data_series = Aichi, series = "male", fh = ij)
  
  interval_score_Aichi_total_dynamic[ij] = interval_score(PI_val = Aichi_total_fh_PI_dynamic, 
                                                  data_series = Aichi, series = "total", fh = ij)
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

Mie_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Mie, series = "female", fh = ik)

Mie_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Mie, series = "male",   fh = ik)

Mie_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Mie, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Mie_female_dynamic = interval_score_Mie_male_dynamic = interval_score_Mie_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Mie_female_dynamic[ij] = interval_score(PI_val = Mie_female_fh_PI_dynamic, 
                                                 data_series = Mie, series = "female", fh = ij)
  
  interval_score_Mie_male_dynamic[ij] = interval_score(PI_val = Mie_male_fh_PI_dynamic, 
                                               data_series = Mie, series = "male", fh = ij)
  
  interval_score_Mie_total_dynamic[ij] = interval_score(PI_val = Mie_total_fh_PI_dynamic, 
                                                data_series = Mie, series = "total", fh = ij)
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

Shiga_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shiga, series = "female", fh = ik)

Shiga_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shiga, series = "male",   fh = ik)

Shiga_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shiga, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shiga_female_dynamic = interval_score_Shiga_male_dynamic = interval_score_Shiga_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Shiga_female_dynamic[ij] = interval_score(PI_val = Shiga_female_fh_PI_dynamic, 
                                                   data_series = Shiga, series = "female", fh = ij)
  
  interval_score_Shiga_male_dynamic[ij] = interval_score(PI_val = Shiga_male_fh_PI_dynamic, 
                                                 data_series = Shiga, series = "male", fh = ij)
  
  interval_score_Shiga_total_dynamic[ij] = interval_score(PI_val = Shiga_total_fh_PI_dynamic, 
                                                  data_series = Shiga, series = "total", fh = ij)
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

Kyoto_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kyoto, series = "female", fh = ik)

Kyoto_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kyoto, series = "male",   fh = ik)

Kyoto_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kyoto, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kyoto_female_dynamic = interval_score_Kyoto_male_dynamic = interval_score_Kyoto_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kyoto_female_dynamic[ij] = interval_score(PI_val = Kyoto_female_fh_PI_dynamic, 
                                                   data_series = Kyoto, series = "female", fh = ij)
  
  interval_score_Kyoto_male_dynamic[ij] = interval_score(PI_val = Kyoto_male_fh_PI_dynamic, 
                                                 data_series = Kyoto, series = "male", fh = ij)
  
  interval_score_Kyoto_total_dynamic[ij] = interval_score(PI_val = Kyoto_total_fh_PI_dynamic, 
                                                  data_series = Kyoto, series = "total", fh = ij)
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

Osaka_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Osaka, series = "female", fh = ik)

Osaka_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Osaka, series = "male",   fh = ik)

Osaka_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Osaka, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Osaka_female_dynamic = interval_score_Osaka_male_dynamic = interval_score_Osaka_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Osaka_female_dynamic[ij] = interval_score(PI_val = Osaka_female_fh_PI_dynamic, 
                                                   data_series = Osaka, series = "female", fh = ij)
  
  interval_score_Osaka_male_dynamic[ij] = interval_score(PI_val = Osaka_male_fh_PI_dynamic, 
                                                 data_series = Osaka, series = "male", fh = ij)
  
  interval_score_Osaka_total_dynamic[ij] = interval_score(PI_val = Osaka_total_fh_PI_dynamic, 
                                                  data_series = Osaka, series = "total", fh = ij)
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

Hyogo_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hyogo, series = "female", fh = ik)

Hyogo_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hyogo, series = "male",   fh = ik)

Hyogo_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hyogo, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hyogo_female_dynamic = interval_score_Hyogo_male_dynamic = interval_score_Hyogo_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Hyogo_female_dynamic[ij] = interval_score(PI_val = Hyogo_female_fh_PI_dynamic, 
                                                   data_series = Hyogo, series = "female", fh = ij)
  
  interval_score_Hyogo_male_dynamic[ij] = interval_score(PI_val = Hyogo_male_fh_PI_dynamic, 
                                                 data_series = Hyogo, series = "male", fh = ij)
  
  interval_score_Hyogo_total_dynamic[ij] = interval_score(PI_val = Hyogo_total_fh_PI_dynamic, 
                                                  data_series = Hyogo, series = "total", fh = ij)
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

Nara_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nara, series = "female", fh = ik)

Nara_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nara, series = "male",   fh = ik)

Nara_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nara, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nara_female_dynamic = interval_score_Nara_male_dynamic = interval_score_Nara_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Nara_female_dynamic[ij] = interval_score(PI_val = Nara_female_fh_PI_dynamic, 
                                                  data_series = Nara, series = "female", fh = ij)
  
  interval_score_Nara_male_dynamic[ij] = interval_score(PI_val = Nara_male_fh_PI_dynamic, 
                                                data_series = Nara, series = "male", fh = ij)
  
  interval_score_Nara_total_dynamic[ij] = interval_score(PI_val = Nara_total_fh_PI_dynamic, 
                                                 data_series = Nara, series = "total", fh = ij)
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

Wakayama_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Wakayama, series = "female", fh = ik)

Wakayama_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Wakayama, series = "male",   fh = ik)

Wakayama_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Wakayama, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Wakayama_female_dynamic = interval_score_Wakayama_male_dynamic = interval_score_Wakayama_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Wakayama_female_dynamic[ij] = interval_score(PI_val = Wakayama_female_fh_PI_dynamic, 
                                                      data_series = Wakayama, series = "female", fh = ij)
  
  interval_score_Wakayama_male_dynamic[ij] = interval_score(PI_val = Wakayama_male_fh_PI_dynamic, 
                                                    data_series = Wakayama, series = "male", fh = ij)
  
  interval_score_Wakayama_total_dynamic[ij] = interval_score(PI_val = Wakayama_total_fh_PI_dynamic, 
                                                     data_series = Wakayama, series = "total", fh = ij)
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

Tottori_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tottori, series = "female", fh = ik)

Tottori_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tottori, series = "male",   fh = ik)

Tottori_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tottori, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tottori_female_dynamic = interval_score_Tottori_male_dynamic = interval_score_Tottori_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Tottori_female_dynamic[ij] = interval_score(PI_val = Tottori_female_fh_PI_dynamic, 
                                                     data_series = Tottori, series = "female", fh = ij)
  
  interval_score_Tottori_male_dynamic[ij] = interval_score(PI_val = Tottori_male_fh_PI_dynamic, 
                                                   data_series = Tottori, series = "male", fh = ij)
  
  interval_score_Tottori_total_dynamic[ij] = interval_score(PI_val = Tottori_total_fh_PI_dynamic, 
                                                    data_series = Tottori, series = "total", fh = ij)
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

Shimane_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shimane, series = "female", fh = ik)

Shimane_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shimane, series = "male",   fh = ik)

Shimane_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Shimane, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Shimane_female_dynamic = interval_score_Shimane_male_dynamic = interval_score_Shimane_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Shimane_female_dynamic[ij] = interval_score(PI_val = Shimane_female_fh_PI_dynamic, 
                                                     data_series = Shimane, series = "female", fh = ij)
  
  interval_score_Shimane_male_dynamic[ij] = interval_score(PI_val = Shimane_male_fh_PI_dynamic, 
                                                   data_series = Shimane, series = "male", fh = ij)
  
  interval_score_Shimane_total_dynamic[ij] = interval_score(PI_val = Shimane_total_fh_PI_dynamic, 
                                                    data_series = Shimane, series = "total", fh = ij)
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

Okayama_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okayama, series = "female", fh = ik)

Okayama_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okayama, series = "male",   fh = ik)

Okayama_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okayama, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Okayama_female_dynamic = interval_score_Okayama_male_dynamic = interval_score_Okayama_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Okayama_female_dynamic[ij] = interval_score(PI_val = Okayama_female_fh_PI_dynamic, 
                                                     data_series = Okayama, series = "female", fh = ij)
  
  interval_score_Okayama_male_dynamic[ij] = interval_score(PI_val = Okayama_male_fh_PI_dynamic, 
                                                   data_series = Okayama, series = "male", fh = ij)
  
  interval_score_Okayama_total_dynamic[ij] = interval_score(PI_val = Okayama_total_fh_PI_dynamic, 
                                                    data_series = Okayama, series = "total", fh = ij)
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

Hiroshima_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hiroshima, series = "female", fh = ik)

Hiroshima_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hiroshima, series = "male",   fh = ik)

Hiroshima_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Hiroshima, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Hiroshima_female_dynamic = interval_score_Hiroshima_male_dynamic = interval_score_Hiroshima_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Hiroshima_female_dynamic[ij] = interval_score(PI_val = Hiroshima_female_fh_PI_dynamic, 
                                                       data_series = Hiroshima, series = "female", fh = ij)
  
  interval_score_Hiroshima_male_dynamic[ij] = interval_score(PI_val = Hiroshima_male_fh_PI_dynamic, 
                                                     data_series = Hiroshima, series = "male", fh = ij)
  
  interval_score_Hiroshima_total_dynamic[ij] = interval_score(PI_val = Hiroshima_total_fh_PI_dynamic, 
                                                      data_series = Hiroshima, series = "total", fh = ij)
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

Yamaguchi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamaguchi, series = "female", fh = ik)

Yamaguchi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamaguchi, series = "male",   fh = ik)

Yamaguchi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Yamaguchi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Yamaguchi_female_dynamic = interval_score_Yamaguchi_male_dynamic = interval_score_Yamaguchi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Yamaguchi_female_dynamic[ij] = interval_score(PI_val = Yamaguchi_female_fh_PI_dynamic, 
                                                       data_series = Yamaguchi, series = "female", fh = ij)
  
  interval_score_Yamaguchi_male_dynamic[ij] = interval_score(PI_val = Yamaguchi_male_fh_PI_dynamic, 
                                                     data_series = Yamaguchi, series = "male", fh = ij)
  
  interval_score_Yamaguchi_total_dynamic[ij] = interval_score(PI_val = Yamaguchi_total_fh_PI_dynamic, 
                                                      data_series = Yamaguchi, series = "total", fh = ij)
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

Tokushima_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokushima, series = "female", fh = ik)

Tokushima_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokushima, series = "male",   fh = ik)

Tokushima_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Tokushima, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Tokushima_female_dynamic = interval_score_Tokushima_male_dynamic = interval_score_Tokushima_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Tokushima_female_dynamic[ij] = interval_score(PI_val = Tokushima_female_fh_PI_dynamic, 
                                                       data_series = Tokushima, series = "female", fh = ij)
  
  interval_score_Tokushima_male_dynamic[ij] = interval_score(PI_val = Tokushima_male_fh_PI_dynamic, 
                                                     data_series = Tokushima, series = "male", fh = ij)
  
  interval_score_Tokushima_total_dynamic[ij] = interval_score(PI_val = Tokushima_total_fh_PI_dynamic, 
                                                      data_series = Tokushima, series = "total", fh = ij)
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

Kagawa_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagawa, series = "female", fh = ik)

Kagawa_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagawa, series = "male",   fh = ik)

Kagawa_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagawa, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kagawa_female_dynamic = interval_score_Kagawa_male_dynamic = interval_score_Kagawa_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kagawa_female_dynamic[ij] = interval_score(PI_val = Kagawa_female_fh_PI_dynamic, 
                                                    data_series = Kagawa, series = "female", fh = ij)
  
  interval_score_Kagawa_male_dynamic[ij] = interval_score(PI_val = Kagawa_male_fh_PI_dynamic, 
                                                  data_series = Kagawa, series = "male", fh = ij)
  
  interval_score_Kagawa_total_dynamic[ij] = interval_score(PI_val = Kagawa_total_fh_PI_dynamic, 
                                                   data_series = Kagawa, series = "total", fh = ij)
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

Ehime_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ehime, series = "female", fh = ik)

Ehime_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ehime, series = "male",   fh = ik)

Ehime_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Ehime, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Ehime_female_dynamic = interval_score_Ehime_male_dynamic = interval_score_Ehime_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Ehime_female_dynamic[ij] = interval_score(PI_val = Ehime_female_fh_PI_dynamic, 
                                                   data_series = Ehime, series = "female", fh = ij)
  
  interval_score_Ehime_male_dynamic[ij] = interval_score(PI_val = Ehime_male_fh_PI_dynamic, 
                                                 data_series = Ehime, series = "male", fh = ij)
  
  interval_score_Ehime_total_dynamic[ij] = interval_score(PI_val = Ehime_total_fh_PI_dynamic, 
                                                  data_series = Ehime, series = "total", fh = ij)
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

Kochi_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kochi, series = "female", fh = ik)

Kochi_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kochi, series = "male",   fh = ik)

Kochi_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kochi, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kochi_female_dynamic = interval_score_Kochi_male_dynamic = interval_score_Kochi_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kochi_female_dynamic[ij] = interval_score(PI_val = Kochi_female_fh_PI_dynamic, 
                                                   data_series = Kochi, series = "female", fh = ij)
  
  interval_score_Kochi_male_dynamic[ij] = interval_score(PI_val = Kochi_male_fh_PI_dynamic, 
                                                 data_series = Kochi, series = "male", fh = ij)
  
  interval_score_Kochi_total_dynamic[ij] = interval_score(PI_val = Kochi_total_fh_PI_dynamic, 
                                                  data_series = Kochi, series = "total", fh = ij)
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

Fukuoka_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukuoka, series = "female", fh = ik)

Fukuoka_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukuoka, series = "male",   fh = ik)

Fukuoka_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Fukuoka, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Fukuoka_female_dynamic = interval_score_Fukuoka_male_dynamic = interval_score_Fukuoka_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Fukuoka_female_dynamic[ij] = interval_score(PI_val = Fukuoka_female_fh_PI_dynamic, 
                                                     data_series = Fukuoka, series = "female", fh = ij)
  
  interval_score_Fukuoka_male_dynamic[ij] = interval_score(PI_val = Fukuoka_male_fh_PI_dynamic, 
                                                   data_series = Fukuoka, series = "male", fh = ij)
  
  interval_score_Fukuoka_total_dynamic[ij] = interval_score(PI_val = Fukuoka_total_fh_PI_dynamic, 
                                                    data_series = Fukuoka, series = "total", fh = ij)
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

Saga_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saga, series = "female", fh = ik)

Saga_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saga, series = "male",   fh = ik)

Saga_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Saga, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Saga_female_dynamic = interval_score_Saga_male_dynamic = interval_score_Saga_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Saga_female_dynamic[ij] = interval_score(PI_val = Saga_female_fh_PI_dynamic, 
                                                  data_series = Saga, series = "female", fh = ij)
  
  interval_score_Saga_male_dynamic[ij] = interval_score(PI_val = Saga_male_fh_PI_dynamic, 
                                                data_series = Saga, series = "male", fh = ij)
  
  interval_score_Saga_total_dynamic[ij] = interval_score(PI_val = Saga_total_fh_PI_dynamic, 
                                                 data_series = Saga, series = "total", fh = ij)
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

Nagasaki_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagasaki, series = "female", fh = ik)

Nagasaki_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagasaki, series = "male",   fh = ik)

Nagasaki_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Nagasaki, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Nagasaki_female_dynamic = interval_score_Nagasaki_male_dynamic = interval_score_Nagasaki_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Nagasaki_female_dynamic[ij] = interval_score(PI_val = Nagasaki_female_fh_PI_dynamic, 
                                                      data_series = Nagasaki, series = "female", fh = ij)
  
  interval_score_Nagasaki_male_dynamic[ij] = interval_score(PI_val = Nagasaki_male_fh_PI_dynamic, 
                                                    data_series = Nagasaki, series = "male", fh = ij)
  
  interval_score_Nagasaki_total_dynamic[ij] = interval_score(PI_val = Nagasaki_total_fh_PI_dynamic, 
                                                     data_series = Nagasaki, series = "total", fh = ij)
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

Kumamoto_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kumamoto, series = "female", fh = ik)

Kumamoto_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kumamoto, series = "male",   fh = ik)

Kumamoto_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kumamoto, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kumamoto_female_dynamic = interval_score_Kumamoto_male_dynamic = interval_score_Kumamoto_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kumamoto_female_dynamic[ij] = interval_score(PI_val = Kumamoto_female_fh_PI_dynamic, 
                                                      data_series = Kumamoto, series = "female", fh = ij)
  
  interval_score_Kumamoto_male_dynamic[ij] = interval_score(PI_val = Kumamoto_male_fh_PI_dynamic, 
                                                    data_series = Kumamoto, series = "male", fh = ij)
  
  interval_score_Kumamoto_total_dynamic[ij] = interval_score(PI_val = Kumamoto_total_fh_PI_dynamic, 
                                                     data_series = Kumamoto, series = "total", fh = ij)
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

Oita_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Oita, series = "female", fh = ik)

Oita_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Oita, series = "male",   fh = ik)

Oita_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Oita, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Oita_female_dynamic = interval_score_Oita_male_dynamic = interval_score_Oita_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Oita_female_dynamic[ij] = interval_score(PI_val = Oita_female_fh_PI_dynamic, 
                                                  data_series = Oita, series = "female", fh = ij)
  
  interval_score_Oita_male_dynamic[ij] = interval_score(PI_val = Oita_male_fh_PI_dynamic, 
                                                data_series = Oita, series = "male", fh = ij)
  
  interval_score_Oita_total_dynamic[ij] = interval_score(PI_val = Oita_total_fh_PI_dynamic, 
                                                 data_series = Oita, series = "total", fh = ij)
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

Miyazaki_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyazaki, series = "female", fh = ik)

Miyazaki_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyazaki, series = "male",   fh = ik)

Miyazaki_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Miyazaki, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Miyazaki_female_dynamic = interval_score_Miyazaki_male_dynamic = interval_score_Miyazaki_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Miyazaki_female_dynamic[ij] = interval_score(PI_val = Miyazaki_female_fh_PI_dynamic, 
                                                      data_series = Miyazaki, series = "female", fh = ij)
  
  interval_score_Miyazaki_male_dynamic[ij] = interval_score(PI_val = Miyazaki_male_fh_PI_dynamic, 
                                                    data_series = Miyazaki, series = "male", fh = ij)
  
  interval_score_Miyazaki_total_dynamic[ij] = interval_score(PI_val = Miyazaki_total_fh_PI_dynamic, 
                                                     data_series = Miyazaki, series = "total", fh = ij)
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

Kagoshima_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagoshima, series = "female", fh = ik)

Kagoshima_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagoshima, series = "male",   fh = ik)

Kagoshima_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Kagoshima, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Kagoshima_female_dynamic = interval_score_Kagoshima_male_dynamic = interval_score_Kagoshima_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Kagoshima_female_dynamic[ij] = interval_score(PI_val = Kagoshima_female_fh_PI_dynamic, 
                                                       data_series = Kagoshima, series = "female", fh = ij)
  
  interval_score_Kagoshima_male_dynamic[ij] = interval_score(PI_val = Kagoshima_male_fh_PI_dynamic, 
                                                     data_series = Kagoshima, series = "male", fh = ij)
  
  interval_score_Kagoshima_total_dynamic[ij] = interval_score(PI_val = Kagoshima_total_fh_PI_dynamic, 
                                                      data_series = Kagoshima, series = "total", fh = ij)
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

Okinawa_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okinawa, series = "female", fh = ik)

Okinawa_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okinawa, series = "male",   fh = ik)

Okinawa_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = Okinawa, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_Okinawa_female_dynamic = interval_score_Okinawa_male_dynamic = interval_score_Okinawa_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_Okinawa_female_dynamic[ij] = interval_score(PI_val = Okinawa_female_fh_PI_dynamic, 
                                                     data_series = Okinawa, series = "female", fh = ij)
  
  interval_score_Okinawa_male_dynamic[ij] = interval_score(PI_val = Okinawa_male_fh_PI_dynamic, 
                                                   data_series = Okinawa, series = "male", fh = ij)
  
  interval_score_Okinawa_total_dynamic[ij] = interval_score(PI_val = Okinawa_total_fh_PI_dynamic, 
                                                    data_series = Okinawa, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

###############################
# Region level interval scores
###############################

# pointwise prediction interval for R1

cl <- makeCluster(15)  
registerDoParallel(cl)

R1_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R1, series = "female", fh = ik)

R1_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R1, series = "male",   fh = ik)

R1_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R1, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R1_female_dynamic = interval_score_R1_male_dynamic = interval_score_R1_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R1_female_dynamic[ij] = interval_score(PI_val = R1_female_fh_PI_dynamic, 
                                                data_series = mfts_R1, series = "female", fh = ij)
  
  interval_score_R1_male_dynamic[ij] = interval_score(PI_val = R1_male_fh_PI_dynamic, 
                                              data_series = mfts_R1, series = "male", fh = ij)
  
  interval_score_R1_total_dynamic[ij] = interval_score(PI_val = R1_total_fh_PI_dynamic, 
                                               data_series = mfts_R1, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R2

cl <- makeCluster(15)  
registerDoParallel(cl)

R2_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R2, series = "female", fh = ik)

R2_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R2, series = "male",   fh = ik)

R2_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R2, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R2_female_dynamic = interval_score_R2_male_dynamic = interval_score_R2_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R2_female_dynamic[ij] = interval_score(PI_val = R2_female_fh_PI_dynamic, 
                                                data_series = mfts_R2, series = "female", fh = ij)
  
  interval_score_R2_male_dynamic[ij] = interval_score(PI_val = R2_male_fh_PI_dynamic, 
                                              data_series = mfts_R2, series = "male", fh = ij)
  
  interval_score_R2_total_dynamic[ij] = interval_score(PI_val = R2_total_fh_PI_dynamic, 
                                               data_series = mfts_R2, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R3

cl <- makeCluster(15)  
registerDoParallel(cl)

R3_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R3, series = "female", fh = ik)

R3_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R3, series = "male",   fh = ik)

R3_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R3, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R3_female_dynamic = interval_score_R3_male_dynamic = interval_score_R3_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R3_female_dynamic[ij] = interval_score(PI_val = R3_female_fh_PI_dynamic, 
                                                data_series = mfts_R3, series = "female", fh = ij)
  
  interval_score_R3_male_dynamic[ij] = interval_score(PI_val = R3_male_fh_PI_dynamic, 
                                              data_series = mfts_R3, series = "male", fh = ij)
  
  interval_score_R3_total_dynamic[ij] = interval_score(PI_val = R3_total_fh_PI_dynamic, 
                                               data_series = mfts_R3, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R4

cl <- makeCluster(15)  
registerDoParallel(cl)

R4_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R4, series = "female", fh = ik)

R4_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R4, series = "male",   fh = ik)

R4_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R4, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R4_female_dynamic = interval_score_R4_male_dynamic = interval_score_R4_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R4_female_dynamic[ij] = interval_score(PI_val = R4_female_fh_PI_dynamic, 
                                                data_series = mfts_R4, series = "female", fh = ij)
  
  interval_score_R4_male_dynamic[ij] = interval_score(PI_val = R4_male_fh_PI_dynamic, 
                                              data_series = mfts_R4, series = "male", fh = ij)
  
  interval_score_R4_total_dynamic[ij] = interval_score(PI_val = R4_total_fh_PI_dynamic, 
                                               data_series = mfts_R4, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R5

cl <- makeCluster(15)  
registerDoParallel(cl)

R5_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R5, series = "female", fh = ik)

R5_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R5, series = "male",   fh = ik)

R5_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R5, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R5_female_dynamic = interval_score_R5_male_dynamic = interval_score_R5_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R5_female_dynamic[ij] = interval_score(PI_val = R5_female_fh_PI_dynamic, 
                                                data_series = mfts_R5, series = "female", fh = ij)
  
  interval_score_R5_male_dynamic[ij] = interval_score(PI_val = R5_male_fh_PI_dynamic, 
                                              data_series = mfts_R5, series = "male", fh = ij)
  
  interval_score_R5_total_dynamic[ij] = interval_score(PI_val = R5_total_fh_PI_dynamic, 
                                               data_series = mfts_R5, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R6

cl <- makeCluster(15)  
registerDoParallel(cl)

R6_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R6, series = "female", fh = ik)

R6_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R6, series = "male",   fh = ik)

R6_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R6, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R6_female_dynamic = interval_score_R6_male_dynamic = interval_score_R6_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R6_female_dynamic[ij] = interval_score(PI_val = R6_female_fh_PI_dynamic, 
                                                data_series = mfts_R6, series = "female", fh = ij)
  
  interval_score_R6_male_dynamic[ij] = interval_score(PI_val = R6_male_fh_PI_dynamic, 
                                              data_series = mfts_R6, series = "male", fh = ij)
  
  interval_score_R6_total_dynamic[ij] = interval_score(PI_val = R6_total_fh_PI_dynamic, 
                                               data_series = mfts_R6, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R7

cl <- makeCluster(15)  
registerDoParallel(cl)

R7_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R7, series = "female", fh = ik)

R7_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R7, series = "male",   fh = ik)

R7_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R7, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R7_female_dynamic = interval_score_R7_male_dynamic = interval_score_R7_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R7_female_dynamic[ij] = interval_score(PI_val = R7_female_fh_PI_dynamic, 
                                                data_series = mfts_R7, series = "female", fh = ij)
  
  interval_score_R7_male_dynamic[ij] = interval_score(PI_val = R7_male_fh_PI_dynamic, 
                                              data_series = mfts_R7, series = "male", fh = ij)
  
  interval_score_R7_total_dynamic[ij] = interval_score(PI_val = R7_total_fh_PI_dynamic, 
                                               data_series = mfts_R7, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# pointwise prediction interval for R8

cl <- makeCluster(15)  
registerDoParallel(cl)

R8_female_fh_PI_dynamic = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R8, series = "female", fh = ik)

R8_male_fh_PI_dynamic   = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R8, series = "male",   fh = ik)

R8_total_fh_PI_dynamic  = foreach(ik = 1:15, .packages = c("demography", "ftsa"))  %dopar% PI_fh(data_series = mfts_R8, series = "total",  fh = ik)

# compute interval scores for all forecast horizons

interval_score_R8_female_dynamic = interval_score_R8_male_dynamic = interval_score_R8_total_dynamic = rep(0, 15)
for(ij in 1:15)
{
  interval_score_R8_female_dynamic[ij] = interval_score(PI_val = R8_female_fh_PI_dynamic, 
                                                data_series = mfts_R8, series = "female", fh = ij)
  
  interval_score_R8_male_dynamic[ij] = interval_score(PI_val = R8_male_fh_PI_dynamic, 
                                              data_series = mfts_R8, series = "male", fh = ij)
  
  interval_score_R8_total_dynamic[ij] = interval_score(PI_val = R8_total_fh_PI_dynamic, 
                                               data_series = mfts_R8, series = "total", fh = ij)
  print(ij)
}

stopCluster(cl)
rm(cl)

# save results

for (i in 2:48)
{
  save(list = paste0(state[i], "female", "_fh_PI_dynamic"), file =  paste(state[i], "female_fh_PI_dynamic.RData", sep = "_"))
  save(list = paste0(state[i], "male", "_fh_PI_dynamic"), file =  paste(state[i], "male_fh_PI_dynamic.RData", sep = "_"))
  save(list = paste0(state[i], "total", "_fh_PI_dynamic"), file =  paste(state[i], "total_fh_PI_dynamic.RData", sep = "_"))
}

save(R1_total_fh_PI_dynamic, file = "R1_total_fh_PI_dynamic.RData")
save(R2_total_fh_PI_dynamic, file = "R2_total_fh_PI_dynamic.RData")
save(R3_total_fh_PI_dynamic, file = "R3_total_fh_PI_dynamic.RData")
save(R4_total_fh_PI_dynamic, file = "R4_total_fh_PI_dynamic.RData")
save(R5_total_fh_PI_dynamic, file = "R5_total_fh_PI_dynamic.RData")
save(R6_total_fh_PI_dynamic, file = "R6_total_fh_PI_dynamic.RData")
save(R7_total_fh_PI_dynamic, file = "R7_total_fh_PI_dynamic.RData")
save(R8_total_fh_PI_dynamic, file = "R8_total_fh_PI_dynamic.RData")

save(R1_female_fh_PI_dynamic, file = "R1_female_fh_PI_dynamic.RData")
save(R2_female_fh_PI_dynamic, file = "R2_female_fh_PI_dynamic.RData")
save(R3_female_fh_PI_dynamic, file = "R3_female_fh_PI_dynamic.RData")
save(R4_female_fh_PI_dynamic, file = "R4_female_fh_PI_dynamic.RData")
save(R5_female_fh_PI_dynamic, file = "R5_female_fh_PI_dynamic.RData")
save(R6_female_fh_PI_dynamic, file = "R6_female_fh_PI_dynamic.RData")
save(R7_female_fh_PI_dynamic, file = "R7_female_fh_PI_dynamic.RData")
save(R8_female_fh_PI_dynamic, file = "R8_female_fh_PI_dynamic.RData")

save(R1_male_fh_PI_dynamic, file = "R1_male_fh_PI_dynamic.RData")
save(R2_male_fh_PI_dynamic, file = "R2_male_fh_PI_dynamic.RData")
save(R3_male_fh_PI_dynamic, file = "R3_male_fh_PI_dynamic.RData")
save(R4_male_fh_PI_dynamic, file = "R4_male_fh_PI_dynamic.RData")
save(R5_male_fh_PI_dynamic, file = "R5_male_fh_PI_dynamic.RData")
save(R6_male_fh_PI_dynamic, file = "R6_male_fh_PI_dynamic.RData")
save(R7_male_fh_PI_dynamic, file = "R7_male_fh_PI_dynamic.RData")
save(R8_male_fh_PI_dynamic, file = "R8_male_fh_PI_dynamic.RData")

# remove stored objects to release memory

for(i in 1:8)
{
  rm(list = c(paste("R",i, "_total_fh_PI_dynamic", sep = "")))
  rm(list = c(paste("R",i, "_female_fh_PI_dynamic", sep = "")))
  rm(list = c(paste("R",i, "_male_fh_PI_dynamic", sep = "")))
}


#########################################################
# Interval scores at all levels of hierarchy time series
#########################################################

# Japan (total)

interval_score_Level_0_dynamic = interval_score_Japan_total_dynamic

# Japan (female + male)

interval_score_Level_1_dynamic = rowMeans(cbind(interval_score_Japan_female_dynamic, interval_score_Japan_male_dynamic))

# Japan region (total)

interval_score_Level_2_dynamic = rowMeans(cbind(interval_score_R1_total_dynamic, interval_score_R2_total_dynamic, interval_score_R3_total_dynamic, interval_score_R4_total_dynamic, interval_score_R5_total_dynamic, interval_score_R6_total_dynamic, interval_score_R7_total_dynamic, interval_score_R8_total_dynamic))

# Japan region (female + male)

interval_score_Level_3_dynamic = rowMeans(cbind(interval_score_R1_female_dynamic, interval_score_R2_female_dynamic, interval_score_R3_female_dynamic, interval_score_R4_female_dynamic,interval_score_R5_female_dynamic, interval_score_R6_female_dynamic, interval_score_R7_female_dynamic, interval_score_R8_female_dynamic, interval_score_R1_male_dynamic,   interval_score_R2_male_dynamic,   interval_score_R3_male_dynamic,   interval_score_R4_male_dynamic, interval_score_R5_male_dynamic,   interval_score_R6_male_dynamic,   interval_score_R7_male_dynamic,   interval_score_R8_male_dynamic))

# Japan prefecture (total)

interval_score_Level_4_dynamic = rowMeans(cbind(interval_score_Hokkaido_total_dynamic, interval_score_Aomori_total_dynamic,
                                        interval_score_Iwate_total_dynamic, interval_score_Miyagi_total_dynamic,
                                        interval_score_Akita_total_dynamic, interval_score_Yamagata_total_dynamic,
                                        interval_score_Fukushima_total_dynamic, interval_score_Ibaraki_total_dynamic,
                                        interval_score_Tochigi_total_dynamic, interval_score_Gunma_total_dynamic,
                                        interval_score_Saitama_total_dynamic, interval_score_Chiba_total_dynamic,
                                        interval_score_Tokyo_total_dynamic, interval_score_Kanagawa_total_dynamic,
                                        interval_score_Niigata_total_dynamic, interval_score_Toyama_total_dynamic,
                                        interval_score_Ishikawa_total_dynamic, interval_score_Fukui_total_dynamic,
                                        interval_score_Yamanashi_total_dynamic, interval_score_Nagano_total_dynamic,
                                        interval_score_Gifu_total_dynamic, interval_score_Shizuoka_total_dynamic,
                                        interval_score_Aichi_total_dynamic, interval_score_Mie_total_dynamic,
                                        interval_score_Shiga_total_dynamic, interval_score_Kyoto_total_dynamic,
                                        interval_score_Osaka_total_dynamic, interval_score_Hyogo_total_dynamic,
                                        interval_score_Nara_total_dynamic, interval_score_Wakayama_total_dynamic,
                                        interval_score_Tottori_total_dynamic, interval_score_Shimane_total_dynamic,
                                        interval_score_Okayama_total_dynamic, interval_score_Hiroshima_total_dynamic,
                                        interval_score_Yamaguchi_total_dynamic, interval_score_Tokushima_total_dynamic,
                                        interval_score_Kagawa_total_dynamic, interval_score_Ehime_total_dynamic,
                                        interval_score_Kochi_total_dynamic, interval_score_Fukuoka_total_dynamic,
                                        interval_score_Saga_total_dynamic, interval_score_Nagasaki_total_dynamic,
                                        interval_score_Kumamoto_total_dynamic, interval_score_Oita_total_dynamic,
                                        interval_score_Miyazaki_total_dynamic, interval_score_Kagoshima_total_dynamic,
                                        interval_score_Okinawa_total_dynamic))

# Japan prefecture (female + male)

interval_score_Level_5_dynamic = rowMeans(cbind(interval_score_Hokkaido_female_dynamic, interval_score_Aomori_female_dynamic,
                                        interval_score_Iwate_female_dynamic, interval_score_Miyagi_female_dynamic,
                                        interval_score_Akita_female_dynamic, interval_score_Yamagata_female_dynamic,
                                        interval_score_Fukushima_female_dynamic, interval_score_Ibaraki_female_dynamic,
                                        interval_score_Tochigi_female_dynamic, interval_score_Gunma_female_dynamic,
                                        interval_score_Saitama_female_dynamic, interval_score_Chiba_female_dynamic,
                                        interval_score_Tokyo_female_dynamic, interval_score_Kanagawa_female_dynamic,
                                        interval_score_Niigata_female_dynamic, interval_score_Toyama_female_dynamic,
                                        interval_score_Ishikawa_female_dynamic, interval_score_Fukui_female_dynamic,
                                        interval_score_Yamanashi_female_dynamic, interval_score_Nagano_female_dynamic,
                                        interval_score_Gifu_female_dynamic, interval_score_Shizuoka_female_dynamic,
                                        interval_score_Aichi_female_dynamic, interval_score_Mie_female_dynamic,
                                        interval_score_Shiga_female_dynamic, interval_score_Kyoto_female_dynamic,
                                        interval_score_Osaka_female_dynamic, interval_score_Hyogo_female_dynamic,
                                        interval_score_Nara_female_dynamic, interval_score_Wakayama_female_dynamic,
                                        interval_score_Tottori_female_dynamic, interval_score_Shimane_female_dynamic,
                                        interval_score_Okayama_female_dynamic, interval_score_Hiroshima_female_dynamic,
                                        interval_score_Yamaguchi_female_dynamic, interval_score_Tokushima_female_dynamic,
                                        interval_score_Kagawa_female_dynamic, interval_score_Ehime_female_dynamic,
                                        interval_score_Kochi_female_dynamic, interval_score_Fukuoka_female_dynamic,
                                        interval_score_Saga_female_dynamic, interval_score_Nagasaki_female_dynamic,
                                        interval_score_Kumamoto_female_dynamic, interval_score_Oita_female_dynamic,
                                        interval_score_Miyazaki_female_dynamic, interval_score_Kagoshima_female_dynamic,
                                        interval_score_Okinawa_female_dynamic,
                                        interval_score_Hokkaido_male_dynamic, interval_score_Aomori_male_dynamic,
                                        interval_score_Iwate_male_dynamic, interval_score_Miyagi_male_dynamic,
                                        interval_score_Akita_male_dynamic, interval_score_Yamagata_male_dynamic,
                                        interval_score_Fukushima_male_dynamic, interval_score_Ibaraki_male_dynamic,
                                        interval_score_Tochigi_male_dynamic, interval_score_Gunma_male_dynamic,
                                        interval_score_Saitama_male_dynamic, interval_score_Chiba_male_dynamic,
                                        interval_score_Tokyo_male_dynamic, interval_score_Kanagawa_male_dynamic,
                                        interval_score_Niigata_male_dynamic, interval_score_Toyama_male_dynamic,
                                        interval_score_Ishikawa_male_dynamic, interval_score_Fukui_male_dynamic,
                                        interval_score_Yamanashi_male_dynamic, interval_score_Nagano_male_dynamic,
                                        interval_score_Gifu_male_dynamic, interval_score_Shizuoka_male_dynamic,
                                        interval_score_Aichi_male_dynamic, interval_score_Mie_male_dynamic,
                                        interval_score_Shiga_male_dynamic, interval_score_Kyoto_male_dynamic,
                                        interval_score_Osaka_male_dynamic, interval_score_Hyogo_male_dynamic,
                                        interval_score_Nara_male_dynamic, interval_score_Wakayama_male_dynamic,
                                        interval_score_Tottori_male_dynamic, interval_score_Shimane_male_dynamic,
                                        interval_score_Okayama_male_dynamic, interval_score_Hiroshima_male_dynamic,
                                        interval_score_Yamaguchi_male_dynamic, interval_score_Tokushima_male_dynamic,
                                        interval_score_Kagawa_male_dynamic, interval_score_Ehime_male_dynamic,
                                        interval_score_Kochi_male_dynamic, interval_score_Fukuoka_male_dynamic,
                                        interval_score_Saga_male_dynamic, interval_score_Nagasaki_male_dynamic,
                                        interval_score_Kumamoto_male_dynamic, interval_score_Oita_male_dynamic,
                                        interval_score_Miyazaki_male_dynamic, interval_score_Kagoshima_male_dynamic,
                                        interval_score_Okinawa_male_dynamic))

# Averaged interval scores at each level of the hierarchy

interval_score_all_dynamic = cbind(interval_score_Level_0_dynamic, interval_score_Level_1_dynamic, interval_score_Level_2_dynamic, interval_score_Level_3_dynamic, interval_score_Level_4_dynamic, interval_score_Level_5_dynamic)
interval_score_all_stats_dynamic = rbind(interval_score_all_dynamic, colMeans(interval_score_all_dynamic), apply(interval_score_all_dynamic, 2, median))
colnames(interval_score_all_stats_dynamic) = c("Level 0", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
rownames(interval_score_all_stats_dynamic) = c(1:15,"Mean","Median")

