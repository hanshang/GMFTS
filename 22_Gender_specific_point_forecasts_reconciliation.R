#####################################################
# Reconciliation of gender-specific point forecasts
#####################################################

BU_optim_gender <- function(female_data_raw_rate_region, male_data_raw_rate_region, female_data_raw_rate, male_data_raw_rate, 
                            age, kj, hier_method = c("Ind", "BU", "comb_OLS", "comb_GLS", "mint"))
{
  hier_method = match.arg(hier_method)
  
  hier_rate_female = hier_rate_male = matrix(NA,56,(16-kj)) 
  
  ################
  # Female series
  ################
  # Total
  hier_rate_female[1,] = get(female_data_raw_rate[1])[kj,age,1:(16-kj)] 
  # Region
  for(iw in 1:8)
  {
    hier_rate_female[2:9,]  = get(female_data_raw_rate_region[iw])[kj,age,1:(16-kj)]  
  }
  # Prefecture
  for(iw in 2:48)
  {
    hier_rate_female[8+iw,] = get(female_data_raw_rate[iw])[kj,age,1:(16-kj)]
  }  
  
  ################
  # Male series
  ################
  # Total
  hier_rate_male[1,] = get(male_data_raw_rate[1])[kj,age,1:(16-kj)] 
  # Region
  for(iw in 1:8)
  {
    hier_rate_male[2:9,]  = get(male_data_raw_rate_region[iw])[kj,age,1:(16-kj)]  
  }
  # Prefecture
  for(iw in 2:48)
  {
    hier_rate_male[8+iw,] = get(male_data_raw_rate[iw])[kj,age,1:(16-kj)]
  }  
  
  #############################################################################
  # forecast reconciliation via bottom-up, optimal combination or MinT methods
  #############################################################################
  
  hier_fore_F = hier_fore_M = matrix(NA,56,(16-kj))
  summing_mat_F = Smat_F(kj = kj, age = age)
  summing_mat_M = Smat_M(kj = kj, age = age)
  for(ik in 1:(16-kj))
  {
    hier_F = summing_mat_F[,,ik]
    hier_M = summing_mat_M[,,ik]
    if(hier_method == "Ind")
    {
      hier_fore_F[,ik] =  hier_rate_female[,ik]
      hier_fore_M[,ik] =  hier_rate_male[,ik]
    }
    if(hier_method == "BU")
    {
      hier_fore_F[,ik] = (hier_F %*% hier_rate_female[10:56,ik])
      hier_fore_M[,ik] = (hier_M %*% hier_rate_male[10:56,ik])
    }
    if(hier_method == "comb_OLS")
    {
      hier_fore_F[,ik] = hier_F %*% ginv(t(hier_F) %*% hier_F) %*% t(hier_F) %*% hier_rate_female[,ik]
      hier_fore_M[,ik] = hier_M %*% ginv(t(hier_M) %*% hier_M) %*% t(hier_M) %*% hier_rate_male[,ik]
    }
    if(hier_method == "comb_GLS")
    {
      hier_fore_F[,ik] = hier_F %*% ginv(t(hier_F) %*% ginv(diag(rowSums(hier_F^2))) %*% hier_F) %*% t(hier_F) %*% ginv(diag(rowSums(hier_F^2))) %*% hier_rate_female[,ik]
      hier_fore_M[,ik] = hier_M %*% ginv(t(hier_M) %*% ginv(diag(rowSums(hier_M^2))) %*% hier_M) %*% t(hier_M) %*% ginv(diag(rowSums(hier_M^2))) %*% hier_rate_male[,ik]
    }
    if(hier_method == "mint")
    {
      wh_F = wh_fun_F(kj = kj, age = age)
      wh_M = wh_fun_M(kj = kj, age = age)
      hier_fore_F[,ik] = hier_F %*% solve(t(hier_F) %*% solve(wh_F) %*% hier_F) %*% t(hier_F) %*% solve(wh_F) %*% hier_rate_female[,ik]
      hier_fore_M[,ik] = hier_M %*% solve(t(hier_M) %*% solve(wh_M) %*% hier_M) %*% t(hier_M) %*% solve(wh_M) %*% hier_rate_male[,ik]
    }
  }
  return(list(hier_fore_female = hier_fore_F, hier_fore_male = hier_fore_M))
}


BU_optim_gender_err <- function(ik, hier_method, female_data_raw_rate_region,
                                male_data_raw_rate_region, female_data_raw_rate, 
                                male_data_raw_rate)
{
  me = ftsa:::me; mae = ftsa:::mae; rmse = ftsa:::rmse
  BU_optim_female = BU_optim_male = array(NA, dim = c(101,56,(16-ik)))
  for(ik_age in 1:101)
  {
    BU_optim_dum = BU_optim_gender(female_data_raw_rate_region, male_data_raw_rate_region, female_data_raw_rate, male_data_raw_rate, age = ik_age, kj = ik, hier_method = hier_method)
    BU_optim_female[ik_age,,] = BU_optim_dum$hier_fore_female
    BU_optim_male[ik_age,,] = BU_optim_dum$hier_fore_male
  }
  
  #######################
  # Female series errors
  #######################
  
  # Total
  me_total_err_female   = me(BU_optim_female[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
  mae_total_err_female  = mae(BU_optim_female[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
  rmse_total_err_female = rmse(BU_optim_female[,1,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
  
  # Region
  me_female_R_err = mae_female_R_err =  rmse_female_R_err = vector("numeric",8)
  for(iw in 1:8)
  {
    me_female_R_err[iw] = me(BU_optim_female[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
    mae_female_R_err[iw] = mae(BU_optim_female[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
    rmse_female_R_err[iw] = rmse(BU_optim_female[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
  }
  
  # Prefecture
  
  state_female_me = state_female_mae = state_female_rmse = vector("numeric",47)
  
  for(iwk in 2:48)
  {
    state_female_me[iwk-1]   = me(BU_optim_female[,(iwk+8),],    extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
    state_female_mae[iwk-1]  = mae(BU_optim_female[,(iwk+8),],   extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
    state_female_rmse[iwk-1] = rmse(BU_optim_female[,(iwk+8),],  extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
  }      
  
  #######################
  # Male series errors
  #######################
  
  # Total
  me_total_err_male   = me(BU_optim_male[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)
  mae_total_err_male  = mae(BU_optim_male[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)
  rmse_total_err_male = rmse(BU_optim_male[,1,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)
  
  # Region
  me_male_R_err = mae_male_R_err =  rmse_male_R_err = vector("numeric",8)
  for(iw in 1:8)
  {
    me_male_R_err[iw] = me(BU_optim_male[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    mae_male_R_err[iw] = mae(BU_optim_male[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    rmse_male_R_err[iw] = rmse(BU_optim_male[,(iw+1),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
  }
  
  # Prefecture
  
  state_male_me = state_male_mae = state_male_rmse = vector("numeric",47)
  
  for(iwk in 2:48)
  {
    state_male_me[iwk-1]   = me(BU_optim_male[,(iwk+8),],    extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
    state_male_mae[iwk-1]  = mae(BU_optim_male[,(iwk+8),],   extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
    state_male_rmse[iwk-1] = rmse(BU_optim_male[,(iwk+8),],  extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
  }   
  
  
  return(list(me_total_err_female = me_total_err_female, me_total_err_male = me_total_err_male,
              me_female_R_err = me_female_R_err, me_male_R_err = me_male_R_err,
              me_state_female_err = state_female_me, 
              me_state_male_err = state_male_me, 
              
              mae_total_err_female = mae_total_err_female, mae_total_err_male = mae_total_err_male,
              mae_female_R_err = mae_female_R_err, mae_male_R_err = mae_male_R_err,
              mae_state_female_err = state_female_mae, 
              mae_state_male_err = state_male_mae,
              
              rmse_total_err_female = rmse_total_err_female, rmse_total_err_male = rmse_total_err_male,
              rmse_female_R_err = rmse_female_R_err, rmse_male_R_err = rmse_male_R_err,
              rmse_state_female_err = state_female_rmse, 
              rmse_state_male_err = state_male_rmse))
}
########################################
# Point forecast errors; fmethod = "Ind" 
########################################

# create storing objects:

Level_gender_F_err       = Level_gender_M_err       = matrix(NA,15,3)
Level_gender_F_R_err     = Level_gender_M_R_err     = array(NA, dim = c(8,15,3))
Level_gender_State_F_err = Level_gender_State_M_err = array(NA, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_gender_err(ik = ikw, hier_method = "Ind", 
                            female_data_raw_rate_region = mfts_gender_region_forc_female,
                            male_data_raw_rate_region = mfts_gender_region_forc_male,
                            female_data_raw_rate = mfts_gender_state_forc_female, 
                            male_data_raw_rate = mfts_gender_state_forc_male)
  
  # Total
  
  Level_gender_F_err[ikw,1] = dum$me_total_err_female
  Level_gender_M_err[ikw,1] = dum$me_total_err_male
  
  Level_gender_F_err[ikw,2] = dum$mae_total_err_female
  Level_gender_M_err[ikw,2] = dum$mae_total_err_male
  
  Level_gender_F_err[ikw,3] = dum$rmse_total_err_female
  Level_gender_M_err[ikw,3] = dum$rmse_total_err_male
  
  # Region
  
  Level_gender_F_R_err[,ikw,1] = dum$me_female_R_err
  Level_gender_M_R_err[,ikw,1] = dum$me_male_R_err
  
  Level_gender_F_R_err[,ikw,2] = dum$mae_female_R_err
  Level_gender_M_R_err[,ikw,2] = dum$mae_male_R_err
  
  Level_gender_F_R_err[,ikw,3] = dum$rmse_female_R_err
  Level_gender_M_R_err[,ikw,3] = dum$rmse_male_R_err
  
  # Prefecture
  
  Level_gender_State_F_err[,ikw,1] = dum$me_state_female_err
  Level_gender_State_M_err[,ikw,1] = dum$me_state_male_err
  
  Level_gender_State_F_err[,ikw,2] = dum$mae_state_female_err
  Level_gender_State_M_err[,ikw,2] = dum$mae_state_male_err
  
  Level_gender_State_F_err[,ikw,3] = dum$rmse_state_female_err
  Level_gender_State_M_err[,ikw,3] = dum$rmse_state_male_err
}

# summary of results

Ind_gender_female_err_me = cbind(Level_gender_F_err[,1], colMeans(Level_gender_F_R_err[,,1]), colMeans(Level_gender_State_F_err[,,1]))
Ind_gender_female_err_mae = cbind(Level_gender_F_err[,2], colMeans(Level_gender_F_R_err[,,2]), colMeans(Level_gender_State_F_err[,,2]))
Ind_gender_female_err_rmse = cbind(Level_gender_F_err[,3], colMeans(Level_gender_F_R_err[,,3]), colMeans(Level_gender_State_F_err[,,3]))

Ind_gender_male_err_me = cbind(Level_gender_M_err[,1], colMeans(Level_gender_M_R_err[,,1]), colMeans(Level_gender_State_M_err[,,1]))
Ind_gender_male_err_mae = cbind(Level_gender_M_err[,2], colMeans(Level_gender_M_R_err[,,2]), colMeans(Level_gender_State_M_err[,,2]))
Ind_gender_male_err_rmse = cbind(Level_gender_M_err[,3], colMeans(Level_gender_M_R_err[,,3]), colMeans(Level_gender_State_M_err[,,3]))

colnames(Ind_gender_female_err_me) = colnames(Ind_gender_female_err_mae) = colnames(Ind_gender_female_err_rmse) = c("Female Total", "Female Region", "Female Prefecture")
colnames(Ind_gender_male_err_me) = colnames(Ind_gender_male_err_mae) = colnames(Ind_gender_male_err_rmse) = c("Male Total", "Male Region", "Male Prefecture")


########################################
# Point forecast errors; fmethod = "BU" 
########################################

# create storing objects:

Level_gender_F_err       = Level_gender_M_err       = matrix(NA,15,3)
Level_gender_F_R_err     = Level_gender_M_R_err     = array(NA, dim = c(15,3,8))
Level_gender_State_F_err = Level_gender_State_M_err = array(NA, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_gender_err(ik = ikw, hier_method = "BU", 
                            female_data_raw_rate_region = mfts_gender_region_forc_female,
                            male_data_raw_rate_region = mfts_gender_region_forc_male,
                            female_data_raw_rate = mfts_gender_state_forc_female, 
                            male_data_raw_rate = mfts_gender_state_forc_male)
  
  # Total
  
  Level_gender_F_err[ikw,1] = dum$me_total_err_female
  Level_gender_M_err[ikw,1] = dum$me_total_err_male
  
  Level_gender_F_err[ikw,2] = dum$mae_total_err_female
  Level_gender_M_err[ikw,2] = dum$mae_total_err_male
  
  Level_gender_F_err[ikw,3] = dum$rmse_total_err_female
  Level_gender_M_err[ikw,3] = dum$rmse_total_err_male
  
  # Region
  
  Level_F_R_err[ikw,1,] = dum$me_female_R_err
  Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture
  
  Level_State_F_err[,ikw,1] = dum$me_state_female_err
  Level_State_M_err[,ikw,1] = dum$me_state_male_err
  
  Level_State_F_err[,ikw,2] = dum$mae_state_female_err
  Level_State_M_err[,ikw,2] = dum$mae_state_male_err
  
  Level_State_F_err[,ikw,3] = dum$rmse_state_female_err
  Level_State_M_err[,ikw,3] = dum$rmse_state_male_err
}

# summary of results

BU_gender_female_err_me = cbind(Level_gender_F_err[,1], rowMeans(Level_F_R_err[,1,]), colMeans(Level_State_F_err[,,1]))
BU_gender_female_err_mae = cbind(Level_gender_F_err[,2], rowMeans(Level_F_R_err[,2,]), colMeans(Level_State_F_err[,,2]))
BU_gender_female_err_rmse = cbind(Level_gender_F_err[,3], rowMeans(Level_F_R_err[,3,]), colMeans(Level_State_F_err[,,3]))

BU_gender_male_err_me = cbind(Level_gender_M_err[,1], rowMeans(Level_M_R_err[,1,]), colMeans(Level_State_M_err[,,1]))
BU_gender_male_err_mae = cbind(Level_gender_M_err[,2], rowMeans(Level_M_R_err[,2,]), colMeans(Level_State_M_err[,,2]))
BU_gender_male_err_rmse = cbind(Level_gender_M_err[,3], rowMeans(Level_M_R_err[,3,]), colMeans(Level_State_M_err[,,3]))

colnames(BU_gender_female_err_me) = colnames(BU_gender_female_err_mae) = colnames(BU_gender_female_err_rmse) = c("Female Total", "Female Region", "Female Prefecture")
colnames(BU_gender_male_err_me) = colnames(BU_gender_male_err_mae) = colnames(BU_gender_male_err_rmse) = c("Male Total", "Male Region", "Male Prefecture")


##############################################
# Point forecast errors; fmethod = "comb_OLS" 
##############################################

# create storing objects:

optim_Level_gender_F_err       = optim_Level_gender_M_err       = matrix(NA,15,3)
optim_Level_gender_F_R_err     = optim_Level_gender_M_R_err     = array(NA, dim = c(15,3,8))
optim_Level_gender_State_F_err = optim_Level_gender_State_M_err = array(NA, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_gender_err(ik = ikw, hier_method = "comb_OLS", 
                            female_data_raw_rate_region = mfts_gender_region_forc_female,
                            male_data_raw_rate_region = mfts_gender_region_forc_male,
                            female_data_raw_rate = mfts_gender_state_forc_female, 
                            male_data_raw_rate = mfts_gender_state_forc_male)
  
  # Total
  
  optim_Level_gender_F_err[ikw,1] = dum$me_total_err_female
  optim_Level_gender_M_err[ikw,1] = dum$me_total_err_male
  
  optim_Level_gender_F_err[ikw,2] = dum$mae_total_err_female
  optim_Level_gender_M_err[ikw,2] = dum$mae_total_err_male
  
  optim_Level_gender_F_err[ikw,3] = dum$rmse_total_err_female
  optim_Level_gender_M_err[ikw,3] = dum$rmse_total_err_male
  
  # Region
  
  optim_Level_F_R_err[ikw,1,] = dum$me_female_R_err
  optim_Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  optim_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  optim_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  optim_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  optim_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture
  
  optim_Level_State_F_err[,ikw,1] = dum$me_state_female_err
  optim_Level_State_M_err[,ikw,1] = dum$me_state_male_err
  
  optim_Level_State_F_err[,ikw,2] = dum$mae_state_female_err
  optim_Level_State_M_err[,ikw,2] = dum$mae_state_male_err
  
  optim_Level_State_F_err[,ikw,3] = dum$rmse_state_female_err
  optim_Level_State_M_err[,ikw,3] = dum$rmse_state_male_err
}


# summary of results

optim_gender_female_err_me = cbind(optim_Level_gender_F_err[,1], rowMeans(optim_Level_F_R_err[,1,]), colMeans(optim_Level_State_F_err[,,1]))
optim_gender_female_err_mae = cbind(optim_Level_gender_F_err[,2], rowMeans(optim_Level_F_R_err[,2,]), colMeans(optim_Level_State_F_err[,,2]))
optim_gender_female_err_rmse = cbind(optim_Level_gender_F_err[,3], rowMeans(optim_Level_F_R_err[,3,]), colMeans(optim_Level_State_F_err[,,3]))

optim_gender_male_err_me = cbind(optim_Level_gender_M_err[,1], rowMeans(optim_Level_M_R_err[,1,]), colMeans(optim_Level_State_M_err[,,1]))
optim_gender_male_err_mae = cbind(optim_Level_gender_M_err[,2], rowMeans(optim_Level_M_R_err[,2,]), colMeans(optim_Level_State_M_err[,,2]))
optim_gender_male_err_rmse = cbind(optim_Level_gender_M_err[,3], rowMeans(optim_Level_M_R_err[,3,]), colMeans(optim_Level_State_M_err[,,3]))

colnames(optim_gender_female_err_me) = colnames(optim_gender_female_err_mae) = colnames(optim_gender_female_err_rmse) = c("Female Total", "Female Region", "Female Prefecture")
colnames(optim_gender_male_err_me) = colnames(optim_gender_male_err_mae) = colnames(optim_gender_male_err_rmse) = c("Male Total", "Male Region", "Male Prefecture")

##############################################
# Point forecast errors; fmethod = "comb_GLS" 
##############################################

# create storing objects:

optim_GLS_Level_gender_F_err       = optim_GLS_Level_gender_M_err       = matrix(NA,15,3)
optim_GLS_Level_gender_F_R_err     = optim_GLS_Level_gender_M_R_err     = array(NA, dim = c(15,3,8))
optim_GLS_Level_gender_State_F_err = optim_GLS_Level_gender_State_M_err = array(NA, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_gender_err(ik = ikw, hier_method = "comb_GLS", 
                            female_data_raw_rate_region = mfts_gender_region_forc_female,
                            male_data_raw_rate_region = mfts_gender_region_forc_male,
                            female_data_raw_rate = mfts_gender_state_forc_female, 
                            male_data_raw_rate = mfts_gender_state_forc_male)
  
  # Total
  
  optim_GLS_Level_gender_F_err[ikw,1] = dum$me_total_err_female
  optim_GLS_Level_gender_M_err[ikw,1] = dum$me_total_err_male
  
  optim_GLS_Level_gender_F_err[ikw,2] = dum$mae_total_err_female
  optim_GLS_Level_gender_M_err[ikw,2] = dum$mae_total_err_male
  
  optim_GLS_Level_gender_F_err[ikw,3] = dum$rmse_total_err_female
  optim_GLS_Level_gender_M_err[ikw,3] = dum$rmse_total_err_male
  
  # Region
  
  optim_GLS_Level_F_R_err[ikw,1,] = dum$me_female_R_err
  optim_GLS_Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  optim_GLS_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  optim_GLS_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  optim_GLS_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  optim_GLS_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture
  
  optim_GLS_Level_State_F_err[,ikw,1] = dum$me_state_female_err
  optim_GLS_Level_State_M_err[,ikw,1] = dum$me_state_male_err
  
  optim_GLS_Level_State_F_err[,ikw,2] = dum$mae_state_female_err
  optim_GLS_Level_State_M_err[,ikw,2] = dum$mae_state_male_err
  
  optim_GLS_Level_State_F_err[,ikw,3] = dum$rmse_state_female_err
  optim_GLS_Level_State_M_err[,ikw,3] = dum$rmse_state_male_err
}


# summary of results

optim_GLS_gender_female_err_me = cbind(optim_GLS_Level_gender_F_err[,1], rowMeans(optim_GLS_Level_F_R_err[,1,]), colMeans(optim_GLS_Level_State_F_err[,,1]))
optim_GLS_gender_female_err_mae = cbind(optim_GLS_Level_gender_F_err[,2], rowMeans(optim_GLS_Level_F_R_err[,2,]), colMeans(optim_GLS_Level_State_F_err[,,2]))
optim_GLS_gender_female_err_rmse = cbind(optim_GLS_Level_gender_F_err[,3], rowMeans(optim_GLS_Level_F_R_err[,3,]), colMeans(optim_GLS_Level_State_F_err[,,3]))

optim_GLS_gender_male_err_me = cbind(optim_GLS_Level_gender_M_err[,1], rowMeans(optim_GLS_Level_M_R_err[,1,]), colMeans(optim_GLS_Level_State_M_err[,,1]))
optim_GLS_gender_male_err_mae = cbind(optim_GLS_Level_gender_M_err[,2], rowMeans(optim_GLS_Level_M_R_err[,2,]), colMeans(optim_GLS_Level_State_M_err[,,2]))
optim_GLS_gender_male_err_rmse = cbind(optim_GLS_Level_gender_M_err[,3], rowMeans(optim_GLS_Level_M_R_err[,3,]), colMeans(optim_GLS_Level_State_M_err[,,3]))

colnames(optim_GLS_gender_female_err_me) = colnames(optim_GLS_gender_female_err_mae) = colnames(optim_GLS_gender_female_err_rmse) = c("Female Total", "Female Region", "Female Prefecture")
colnames(optim_GLS_gender_male_err_me) = colnames(optim_GLS_gender_male_err_mae) = colnames(optim_GLS_gender_male_err_rmse) = c("Male Total", "Male Region", "Male Prefecture")

##########################################
# Point forecast errors; fmethod = "mint" 
##########################################

# create storing objects:

optim_mint_Level_gender_F_err       = optim_mint_Level_gender_M_err       = matrix(NA,15,3)
optim_mint_Level_gender_F_R_err     = optim_mint_Level_gender_M_R_err     = array(NA, dim = c(15,3,8))
optim_mint_Level_gender_State_F_err = optim_mint_Level_gender_State_M_err = array(NA, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_gender_err(ik = ikw, hier_method = "mint", 
                            female_data_raw_rate_region = mfts_gender_region_forc_female,
                            male_data_raw_rate_region = mfts_gender_region_forc_male,
                            female_data_raw_rate = mfts_gender_state_forc_female, 
                            male_data_raw_rate = mfts_gender_state_forc_male)
  
  # Total
  
  optim_mint_Level_gender_F_err[ikw,1] = dum$me_total_err_female
  optim_mint_Level_gender_M_err[ikw,1] = dum$me_total_err_male
  
  optim_mint_Level_gender_F_err[ikw,2] = dum$mae_total_err_female
  optim_mint_Level_gender_M_err[ikw,2] = dum$mae_total_err_male
  
  optim_mint_Level_gender_F_err[ikw,3] = dum$rmse_total_err_female
  optim_mint_Level_gender_M_err[ikw,3] = dum$rmse_total_err_male
  
  # Region
  
  optim_mint_Level_F_R_err[ikw,1,] = dum$me_female_R_err
  optim_mint_Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  optim_mint_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  optim_mint_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  optim_mint_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  optim_mint_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture
  
  optim_mint_Level_State_F_err[,ikw,1] = dum$me_state_female_err
  optim_mint_Level_State_M_err[,ikw,1] = dum$me_state_male_err
  
  optim_mint_Level_State_F_err[,ikw,2] = dum$mae_state_female_err
  optim_mint_Level_State_M_err[,ikw,2] = dum$mae_state_male_err
  
  optim_mint_Level_State_F_err[,ikw,3] = dum$rmse_state_female_err
  optim_mint_Level_State_M_err[,ikw,3] = dum$rmse_state_male_err
}


# summary of results

optim_mint_gender_female_err_me = cbind(optim_mint_Level_gender_F_err[,1], rowMeans(optim_mint_Level_F_R_err[,1,]), colMeans(optim_mint_Level_State_F_err[,,1]))
optim_mint_gender_female_err_mae = cbind(optim_mint_Level_gender_F_err[,2], rowMeans(optim_mint_Level_F_R_err[,2,]), colMeans(optim_mint_Level_State_F_err[,,2]))
optim_mint_gender_female_err_rmse = cbind(optim_mint_Level_gender_F_err[,3], rowMeans(optim_mint_Level_F_R_err[,3,]), colMeans(optim_mint_Level_State_F_err[,,3]))

optim_mint_gender_male_err_me = cbind(optim_mint_Level_gender_M_err[,1], rowMeans(optim_mint_Level_M_R_err[,1,]), colMeans(optim_mint_Level_State_M_err[,,1]))
optim_mint_gender_male_err_mae = cbind(optim_mint_Level_gender_M_err[,2], rowMeans(optim_mint_Level_M_R_err[,2,]), colMeans(optim_mint_Level_State_M_err[,,2]))
optim_mint_gender_male_err_rmse = cbind(optim_mint_Level_gender_M_err[,3], rowMeans(optim_mint_Level_M_R_err[,3,]), colMeans(optim_mint_Level_State_M_err[,,3]))

colnames(optim_mint_gender_female_err_me) = colnames(optim_mint_gender_female_err_mae) = colnames(optim_mint_gender_female_err_rmse) = c("Female Total", "Female Region", "Female Prefecture")
colnames(optim_mint_gender_male_err_me) = colnames(optim_mint_gender_male_err_mae) = colnames(optim_mint_gender_male_err_rmse) = c("Male Total", "Male Region", "Male Prefecture")



