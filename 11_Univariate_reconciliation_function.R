#######################################################################################
# All-level bootstrapped base forecasts (B = 1000) using univariate forecasting method
#######################################################################################

library(demography)
library(ftsa)

# Define a function to compute bootstrapped base forecasts

## fh: forecast horizon

PI_prefecture_dynamic <- function(fh)
{
  prefecture_fh_PI_dynamic = array(0, dim = c(101, (16-fh), 168, 1000))
  for(ij in 1:1000)
  {
    prefecture_fh_PI_dynamic[,,,ij] = exp(cbind(Japan_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        Japan_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Japan_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        R1_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R2_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R3_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R4_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R5_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R6_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R7_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R8_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        R1_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R2_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R3_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R4_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R5_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R6_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R7_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R8_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        R1_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R2_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R3_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R4_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R5_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R6_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        R7_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,], R8_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        Hokkaido_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Aomori_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Iwate_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Miyagi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Akita_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Yamagata_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Fukushima_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Ibaraki_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tochigi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Gunma_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Saitama_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Chiba_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tokyo_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Kanagawa_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Niigata_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Toyama_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Ishikawa_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Fukui_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Yamanashi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Nagano_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Gifu_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],       Shizuoka_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Aichi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Mie_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Shiga_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Kyoto_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Osaka_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Hyogo_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Nara_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],       Wakayama_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tottori_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Shimane_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Okayama_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Hiroshima_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Yamaguchi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Tokushima_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kagawa_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Ehime_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kochi_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Fukuoka_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Saga_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],       Nagasaki_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kumamoto_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Oita_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Miyazaki_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Kagoshima_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Okinawa_total_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        
                                        Hokkaido_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Hokkaido_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Aomori_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Aomori_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Iwate_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Iwate_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Miyagi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Miyagi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Akita_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Akita_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Yamagata_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Yamagata_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Fukushima_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Fukushima_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Ibaraki_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Ibaraki_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tochigi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Tochigi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Gunma_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Gunma_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Saitama_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Saitama_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Chiba_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Chiba_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tokyo_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Tokyo_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kanagawa_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Kanagawa_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Niigata_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Niigata_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Toyama_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Toyama_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Ishikawa_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Ishikawa_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Fukui_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Fukui_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Yamanashi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Yamanashi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Nagano_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Nagano_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Gifu_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Gifu_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Shizuoka_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Shizuoka_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Aichi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Aichi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Mie_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],       Mie_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Shiga_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Shiga_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kyoto_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Kyoto_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Osaka_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Osaka_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Hyogo_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Hyogo_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Nara_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Nara_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Wakayama_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Wakayama_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tottori_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Tottori_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Shimane_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Shimane_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Okayama_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Okayama_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Hiroshima_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Hiroshima_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Yamaguchi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Yamaguchi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Tokushima_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Tokushima_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kagawa_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],    Kagawa_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Ehime_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Ehime_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kochi_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],     Kochi_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Fukuoka_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Fukuoka_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Saga_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Saga_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Nagasaki_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Nagasaki_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kumamoto_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Kumamoto_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Oita_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],      Oita_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Miyazaki_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],  Miyazaki_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Kagoshima_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,], Kagoshima_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,],
                                        Okinawa_female_fh_PI_dynamic[[fh]]$PI_boot[,ij,],   Okinawa_male_fh_PI_dynamic[[fh]]$PI_boot[,ij,]))
  }
  return(prefecture_fh_PI_dynamic)
}

############################################
# Save previous results and release memory
############################################

for (i in 1:48)
{
  dum = list()
  dum[[1]] = get(paste(state[i], "female", "fh_PI_dynamic", sep = "_"))
  dum[[2]] = get(paste(state[i], "male", "fh_PI_dynamic", sep = "_"))
  dum[[3]] = get(paste(state[i], "total", "fh_PI_dynamic", sep = "_"))
  save(dum, file =  paste(state[i], "fh_PI_dynamic.RData", sep = "_"))
  rm(dum)
}

for(i in 1:48)
{
  rm(list = c(paste(state[i], "female", "fh_PI_dynamic", sep = "_"), paste(state[i], "male", "fh_PI_dynamic", sep = "_"), paste(state[i], "total", "fh_PI_dynamic", sep = "_")))
}

##############################################################
# one- to 15-step-ahead bootstrapped base forecasts (B = 1000)
##############################################################

PI_prefecture_h1to15_dynamic = list()
PI_prefecture_h1to15_dynamic[[1]] = PI_prefecture_dynamic(fh = 1)
PI_prefecture_h1to15_dynamic[[2]] = PI_prefecture_dynamic(fh = 2)
PI_prefecture_h1to15_dynamic[[3]] = PI_prefecture_dynamic(fh = 3)
PI_prefecture_h1to15_dynamic[[4]] = PI_prefecture_dynamic(fh = 4)
PI_prefecture_h1to15_dynamic[[5]] = PI_prefecture_dynamic(fh = 5)
PI_prefecture_h1to15_dynamic[[6]] = PI_prefecture_dynamic(fh = 6)
PI_prefecture_h1to15_dynamic[[7]] = PI_prefecture_dynamic(fh = 7)
PI_prefecture_h1to15_dynamic[[8]] = PI_prefecture_dynamic(fh = 8)
PI_prefecture_h1to15_dynamic[[9]] = PI_prefecture_dynamic(fh = 9)
PI_prefecture_h1to15_dynamic[[10]] = PI_prefecture_dynamic(fh = 10)
PI_prefecture_h1to15_dynamic[[11]] = PI_prefecture_dynamic(fh = 11)
PI_prefecture_h1to15_dynamic[[12]] = PI_prefecture_dynamic(fh = 12)
PI_prefecture_h1to15_dynamic[[13]] = PI_prefecture_dynamic(fh = 13)
PI_prefecture_h1to15_dynamic[[14]] = PI_prefecture_dynamic(fh = 14)
PI_prefecture_h1to15_dynamic[[15]] = PI_prefecture_dynamic(fh = 15)


###########################################
# All-level bootstrapped grouped forecasts
###########################################

# Define a functino to reconcile pointwise interval forecasts

## kj: forecast horizon from h = 1 to 15
## age: age indexes from 1 to 101 (ages 0 to 100)

BU_optim_hier_PI_dynamic <- function(kj, age, hier_method = c("BU", "comb_OLS", "mint"))
{
    hier_method = match.arg(hier_method)
    hier_fore = array(0, dim = c(168, (16-kj), 1000))
    
    # Summing matrix for kj horizon at a given age
    
    summ_mat = Smat_fun(kj = kj, age = age)
    
    # ik: number of years in the forecasting period, it depends on kj
    for(ik in 1:(16-kj)) 
    {
        hier = summ_mat[,,ik]
        if(hier_method == "BU")
        {
            for(ij in 1:1000)
            {
                hier_fore[,ik,ij] = hier %*% (PI_prefecture_h1to15_dynamic[[kj]])[age,ik,75:168,ij]
            }
        }
        if(hier_method == "comb_OLS")
        {
            for(ij in 1:1000)
            {
                hier_fore[,ik,ij] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% (PI_prefecture_h1to15_dynamic[[kj]])[age,ik,,ij]
            }
        }
        if(hier_method == "mint")
        {
          wh = wh_fun(kj = kj, age = age)
          for(ij in 1:1000)
          {
            hier_fore[,ik,ij] = hier %*% solve(t(hier) %*% solve(wh) %*% hier) %*% t(hier) %*% solve(wh) %*% (PI_prefecture_h1to15_dynamic[[kj]])[age,ik,,ij]
          }
        }
    }
    return(hier_fore)
}

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "BU" (output: h1 to h15 BU reconciled bootstrap samples)

BU_fun <- function(kj)
{
    dum = array(0, dim = c(101, 168, (16-kj), 1000))
    for(ij in 1:101)
    {
        dum[ij,,,] = BU_optim_hier_PI_dynamic(kj = kj, age = ij)
    }
    return(dum)
}

# Parallel calculation of reconciled interval predictions

library(doParallel)

cl <- makeCluster(4) 
registerDoParallel(cl)

BU_optim_hier_comb = foreach(kj = 1:15) %do% BU_fun(kj = kj)

stopCluster(cl)
rm(cl)


# Computation without parallel

BU_optim_hier_comb_dynamic = list()
for(ik in 1:15)
{
  BU_optim_hier_comb_dynamic[[ik]] = BU_fun(kj = ik)
}
save(BU_optim_hier_comb_dynamic, file = "BU_optim_hier_comb_dynamic.RData")

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "comb_OLS" (output: h1 to h15 OLS reconciled bootstrap samples) 

OLS_fun <- function(kj)  
{
    dum = array(0, dim = c(101, 168, (16-kj), 1000))
    for(ij in 1:101)
    {
        dum[ij,,,] = BU_optim_hier_PI_dynamic(kj = kj, age = ij, hier_method = "comb_OLS")
    }
    return(dum)    
}

# Parallel calculation of reconciled interval predictions

cl <- makeCluster(2) 
registerDoParallel(cl)

OLS_hier_comb = foreach(kj = 1:2) %dopar% OLS_fun(kj = kj)

stopCluster(cl)
rm(cl)

# Altervatively, compute reconciled interval forecasts without using parallel (recommended method for PC with less than 64 GB of memory but very time consuming)

library(MASS)
OLS_hier_comb_dynamic = list()
for(kj in 1:15)
{
  OLS_hier_comb_dynamic[[kj]] = OLS_fun(kj = kj)
}

save(OLS_hier_comb_dynamic, file = "OLS_hier_comb_dynamic_1to8.RData")
save(OLS_hier_comb_dynamic, file = "OLS_hier_comb_dynamic.RData")
save(PI_prefecture_h1to15_dynamic, file = "PI_prefecture_h1to15_dynamic.RData")

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "mint" (output: h1 to h15 MinT reconciled bootstrap samples) 

mint_fun_dynamic <- function(kj)  
{
  dum = array(0, dim = c(101, 168, (16-kj), 1000))
  for(ij in 1:101)
  {
    dum[ij,,,] = BU_optim_hier_PI_dynamic(kj = kj, age = ij, hier_method = "mint")
  }
  return(dum)    
}

# Altervatively, compute reconciled interval forecasts without using parallel (recommended method for PC with less than 64 GB of memory but very time consuming)
library(MASS)
mint_hier_comb_dynamic = list()
for(kj in 1:15)
{
  mint_hier_comb_dynamic[[kj]] = mint_fun_dynamic(kj = kj)
  print(kj)
}

mint_hier_comb_1_dynamic = list()
mint_hier_comb_1_dynamic[[1]] = mint_fun_dynamic(kj = 1)
mint_hier_comb_1_dynamic[[2]] = mint_fun_dynamic(kj = 2)
mint_hier_comb_1_dynamic[[3]] = mint_fun_dynamic(kj = 3)
mint_hier_comb_1_dynamic[[4]] = mint_fun_dynamic(kj = 4)
mint_hier_comb_1_dynamic[[5]] = mint_fun_dynamic(kj = 5)
save(mint_hier_comb_1_dynamic, file = "mint_hier_comb_1_dynamic.RData")

mint_hier_comb_2_dynamic = list()
mint_hier_comb_2_dynamic[[1]] = mint_fun_dynamic(kj = 6)
mint_hier_comb_2_dynamic[[2]] = mint_fun_dynamic(kj = 7)
mint_hier_comb_2_dynamic[[3]] = mint_fun_dynamic(kj = 8)
mint_hier_comb_2_dynamic[[4]] = mint_fun_dynamic(kj = 9)
mint_hier_comb_2_dynamic[[5]] = mint_fun_dynamic(kj = 10)
save(mint_hier_comb_2_dynamic, file = "mint_hier_comb_2_dynamic.RData")

mint_hier_comb_3_dynamic = list()
mint_hier_comb_3_dynamic[[1]] = mint_fun_dynamic(kj = 11)
mint_hier_comb_3_dynamic[[2]] = mint_fun_dynamic(kj = 12)
mint_hier_comb_3_dynamic[[3]] = mint_fun_dynamic(kj = 13)
mint_hier_comb_3_dynamic[[4]] = mint_fun_dynamic(kj = 14)
mint_hier_comb_3_dynamic[[5]] = mint_fun_dynamic(kj = 15)
save(mint_hier_comb_3_dynamic, file = "mint_hier_comb_3_dynamic.RData")

mint_hier_comb_dynamic = c(mint_hier_comb_1_dynamic, mint_hier_comb_2_dynamic, mint_hier_comb_3_dynamic)
save(mint_hier_comb_dynamic, file = "mint_hier_comb_dynamic.RData")

##################################################
# Function used to calculate mean interval scores
##################################################

# Define a function to compute interval scores

interval_score_BU_optim <- function(PI_val, data_series, series, fh, index, alpha = 0.8)
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
  
    boot_sample = PI_val[[fh]]
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
  
    dummy = apply(boot_sample_v2, 1:3, quantile, c((1 - alpha)/2, (1 + alpha)/2), na.rm = TRUE)
    PI_lb_val = dummy[1,,index,]
    PI_ub_val = dummy[2,,index,]
    
    # which holdout samples lie outside lower and upper bounds
    
    lb_ind = ifelse(test_val < PI_lb_val, 1, 0) # indicators
    ub_ind = ifelse(test_val > PI_ub_val, 1, 0) # indicators
  
    score = mean((PI_ub_val - PI_lb_val) + 2/(1 - alpha) * (PI_lb_val - test_val) * lb_ind + 2/(1 - alpha) * (test_val - PI_ub_val) * ub_ind) 
    return(score)
}

