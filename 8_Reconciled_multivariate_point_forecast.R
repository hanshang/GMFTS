#################################################
# Reconciliation of multivariate point forecasts
#################################################

library(demography)
library(ftsa)

# female_data_raw: Female age-specific exposure to risk
# male_data_raw: Male age-specific exposure to risk 
# female_data_raw_rate: Female age-specific mortality rates (mfts)
# male_data_raw_rate: Male age-specific mortality rates (mfts)
# age: Age variable
# kj: forecast horizon
# hier_method: bottom-up or optimal combination method

# Define a point forecasts reconciliation function 

BU_optim_hier_mfts <- function(total_data_raw_rate_region, female_data_raw_rate_region,
                              male_data_raw_rate_region, female_data_raw_rate, male_data_raw_rate,
                              total_data_raw_rate, age, kj, hier_method = c("BU","Ind", "comb_OLS", "comb_GLS", "mint"))
{
    hier_method = match.arg(hier_method)
    
    hier_rate = matrix(0,168,(16-kj)) # in total 168 series
    # Level_0 (Total)
    hier_rate[1,] = get(total_data_raw_rate[1])[kj,age,1:(16-kj)] # first arg: horizon; # third arg: number of forecasting years 
    # Level_1 (Sex)
    hier_rate[2,] = get(female_data_raw_rate[1])[kj,age,1:(16-kj)]
    hier_rate[3,] = get(male_data_raw_rate[1])[kj,age,1:(16-kj)]
    
    # Level_2 (Region + Total, Female, Male)
    for(iw in 1:8)
    {
        hier_rate[iw+3,]  = get(total_data_raw_rate_region[iw])[kj,age,1:(16-kj)]  
        hier_rate[iw+11,] = get(female_data_raw_rate_region[iw])[kj,age,1:(16-kj)]
        hier_rate[iw+19,] = get(male_data_raw_rate_region[iw])[kj,age,1:(16-kj)]
    }
    
    # Level_3 (Prefecture + Total, Female, Male)
    
    for(iw in 2:48)
    {
       hier_rate[iw+26,] = get(total_data_raw_rate[iw])[kj,age,1:(16-kj)]
    }  
    for(iw in 2:48)
    {
        hier_rate[(2*iw+71),] = get(female_data_raw_rate[iw])[kj,age,1:(16-kj)]
        hier_rate[(2*iw+72),] = get(male_data_raw_rate[iw])[kj,age,1:(16-kj)]   
    }

    # forecast reconciliation via bottom-up, optimal combination or MinT methods
    hier_fore   = matrix(0,168,(16-kj))
    summing_mat = Smat_fun(kj = kj, age = age)
    for(ik in 1:(16-kj))
    {
        hier = summing_mat[,,ik]
        if(hier_method == "Ind")
        {
          hier_fore[,ik] = hier_rate[,ik]
        }
        if(hier_method == "BU")
        {
            hier_fore[,ik] = (hier %*% hier_rate[75:168,ik])
        }
        if(hier_method == "comb_OLS")
        {
            hier_fore[,ik] = hier %*% solve(t(hier) %*% hier) %*% t(hier) %*% hier_rate[,ik]
        }
        if(hier_method == "comb_GLS")
        {
          hier_fore[,ik] = hier %*% solve(t(hier) %*% solve(diag(rowSums(hier^2))) %*% hier) %*% t(hier) %*% solve(diag(rowSums(hier^2))) %*% hier_rate[,ik]
        }
        if(hier_method == "mint")
        {
          wh = wh_fun_mfts(kj = kj, age = age)
          hier_fore[,ik] = hier %*% solve(t(hier) %*% solve(wh) %*% hier) %*% t(hier) %*% solve(wh) %*% hier_rate[,ik]
        }
    }
    return(hier_fore)
}

# Define a function for computing reconciled errors

BU_optim_err <- function(ik, hier_method, total_data_raw_rate_region, female_data_raw_rate_region,
                         male_data_raw_rate_region, female_data_raw_rate, 
                         male_data_raw_rate, total_data_raw_rate)
{
    me = ftsa:::me; mae = ftsa:::mae; rmse = ftsa:::rmse
    BU_optim_hier_comb = array(0, dim = c(101,168,(16-ik)))
    for(ik_age in 1:101)
    {
        BU_optim_hier_comb[ik_age,,] = BU_optim_hier_mfts(total_data_raw_rate_region, female_data_raw_rate_region,
                                                         male_data_raw_rate_region,
                                                         female_data_raw_rate, male_data_raw_rate, total_data_raw_rate,      
                                                         age = ik_age, kj = ik, hier_method = hier_method)
    }
    
    #####################################
    # Errors, including ME, MAE and RMSE
    #####################################
    
    # Level 0 (Total)
    me_total_err    = me(BU_optim_hier_comb[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$total)
    mae_total_err   = mae(BU_optim_hier_comb[,1,],   extract.years(get(state[1]), years = (2001+ik):2016)$rate$total)
    rmse_total_err  = rmse(BU_optim_hier_comb[,1,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$total)
    
    # Level 1 (Sex)
    me_female_err  = me(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
    me_male_err    = me(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)
    
    mae_female_err = mae(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
    mae_male_err   = mae(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)
  
    rmse_female_err = rmse(BU_optim_hier_comb[,2,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$female)
    rmse_male_err   = rmse(BU_optim_hier_comb[,3,],  extract.years(get(state[1]), years = (2001+ik):2016)$rate$male)

    # Level 2 (Region + Total, Female and Male)
    me_total_R_err = me_female_R_err = me_male_R_err = mae_total_R_err = mae_female_R_err = mae_male_R_err = rmse_total_R_err = rmse_female_R_err = rmse_male_R_err = rep(0, 8)
    for(iw in 1:8)
    {
        me_total_R_err[iw]  = me(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
        me_female_R_err[iw] = me(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
        me_male_R_err[iw]   = me(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
      
        mae_total_R_err[iw]  = mae(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
        mae_female_R_err[iw] = mae(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
        mae_male_R_err[iw]   = mae(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)

        rmse_total_R_err[iw]  = rmse(BU_optim_hier_comb[,(iw+3),],  extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
        rmse_female_R_err[iw] = rmse(BU_optim_hier_comb[,(iw+11),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
        rmse_male_R_err[iw]   = rmse(BU_optim_hier_comb[,(iw+19),], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    }
    
    # Level 3 (Prefecture + Total)
      
    me_state_err = bottom_female_me =  bottom_male_me = mae_state_err = bottom_female_mae =  bottom_male_mae = rmse_state_err = bottom_female_rmse = bottom_male_rmse = rep(0, 47)
    
    for(iwk in 2:48)
    {
        me_state_err[iwk-1]    = me(BU_optim_hier_comb[,(iwk+26),],    extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$total)
        mae_state_err[iwk-1]   = mae(BU_optim_hier_comb[,(iwk+26),],   extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$total)
        rmse_state_err[iwk-1]  = rmse(BU_optim_hier_comb[,(iwk+26),],  extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$total)
    }
    
    for(iwk in 2:48)
    {
        bottom_female_me[iwk-1]   = me(BU_optim_hier_comb[,(71+2*iwk),],    extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
        bottom_female_mae[iwk-1]  = mae(BU_optim_hier_comb[,(71+2*iwk),],   extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
        bottom_female_rmse[iwk-1] = rmse(BU_optim_hier_comb[,(71+2*iwk),],  extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$female)
    
        bottom_male_me[iwk-1]     = me(BU_optim_hier_comb[,(72+2*iwk),],    extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
        bottom_male_mae[iwk-1]    = mae(BU_optim_hier_comb[,(72+2*iwk),],   extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
        bottom_male_rmse[iwk-1]   = rmse(BU_optim_hier_comb[,(72+2*iwk),],  extract.years(get(state[iwk]), years = (2001+ik):2016)$rate$male)
    }      
    
    return(list(me_total_err = me_total_err, me_female_err = me_female_err, 
                me_male_err  = me_male_err,  me_total_R_err = me_total_R_err,
                me_female_R_err = me_female_R_err, me_male_R_err = me_male_R_err,
                me_state_err  = me_state_err, me_bottom_female_err = bottom_female_me, 
                me_bottom_male_err = bottom_male_me, 
      
                mae_total_err = mae_total_err, mae_female_err = mae_female_err, 
                mae_male_err  = mae_male_err,  mae_total_R_err = mae_total_R_err,
                mae_female_R_err = mae_female_R_err, mae_male_R_err = mae_male_R_err,
                mae_state_err  = mae_state_err, mae_bottom_female_err = bottom_female_mae, 
                mae_bottom_male_err = bottom_male_mae, 
                
                rmse_total_err = rmse_total_err, rmse_female_err = rmse_female_err, 
                rmse_male_err  = rmse_male_err,  
                rmse_total_R_err = rmse_total_R_err,
                rmse_female_R_err = rmse_female_R_err,
                rmse_male_R_err = rmse_male_R_err,
                rmse_state_err  = rmse_state_err,
                rmse_bottom_female_err = bottom_female_rmse, rmse_bottom_male_err = bottom_male_rmse))
}


########################################
# Point forecast errors; fmethod = "Ind" 
########################################

# Define variables for the base forecasts

mfts_Level_0_err     = mfts_Level_F_err       = mfts_Level_M_err       = matrix(0,15,3)
mfts_Level_T_R_err   = mfts_Level_F_R_err     = mfts_Level_M_R_err     = array(0, dim = c(8,15,3))
mfts_Level_State_err = mfts_Level_State_F_err = mfts_Level_State_M_err = array(0, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_err(ik = ikw, hier_method = "Ind", 
                     total_data_raw_rate_region = mfts_region_forc_total,
                     female_data_raw_rate_region = mfts_region_forc_female,
                     male_data_raw_rate_region = mfts_region_forc_male,
                     female_data_raw_rate = mfts_state_forc_female, 
                     male_data_raw_rate = mfts_state_forc_male, total_data_raw_rate = mfts_state_forc_total)
  
  # Total + Sex
  
  mfts_Level_0_err[ikw,1] = dum$me_total_err
  mfts_Level_F_err[ikw,1] = dum$me_female_err
  mfts_Level_M_err[ikw,1] = dum$me_male_err
  
  mfts_Level_0_err[ikw,2] = dum$mae_total_err
  mfts_Level_F_err[ikw,2] = dum$mae_female_err
  mfts_Level_M_err[ikw,2] = dum$mae_male_err
  
  mfts_Level_0_err[ikw,3] = dum$rmse_total_err
  mfts_Level_F_err[ikw,3] = dum$rmse_female_err
  mfts_Level_M_err[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  mfts_Level_T_R_err[,ikw,1] = dum$me_total_R_err
  mfts_Level_F_R_err[,ikw,1] = dum$me_female_R_err
  mfts_Level_M_R_err[,ikw,1] = dum$me_male_R_err
  
  mfts_Level_T_R_err[,ikw,2] = dum$mae_total_R_err
  mfts_Level_F_R_err[,ikw,2] = dum$mae_female_R_err
  mfts_Level_M_R_err[,ikw,2] = dum$mae_male_R_err
  
  mfts_Level_T_R_err[,ikw,3] = dum$rmse_total_R_err
  mfts_Level_F_R_err[,ikw,3] = dum$rmse_female_R_err
  mfts_Level_M_R_err[,ikw,3] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  mfts_Level_State_err[,ikw,1]   = dum$me_state_err
  mfts_Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
  mfts_Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
  
  mfts_Level_State_err[,ikw,2]   = dum$mae_state_err
  mfts_Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
  mfts_Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
  
  mfts_Level_State_err[,ikw,3]   = dum$rmse_state_err
  mfts_Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
  mfts_Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

mfts_all_level_err_me = cbind(mfts_Level_0_err[,1], rowMeans(cbind(mfts_Level_F_err[,1], mfts_Level_M_err[,1])),
                            colMeans(mfts_Level_T_R_err[,,1]), colMeans(rbind(mfts_Level_F_R_err[,,1], mfts_Level_M_R_err[,,1])),
                            colMeans(mfts_Level_State_err[,,1]), colMeans(rbind(mfts_Level_State_F_err[,,1], mfts_Level_State_M_err[,,1])))
mfts_all_level_err_mae = cbind(mfts_Level_0_err[,2], rowMeans(cbind(mfts_Level_F_err[,2], mfts_Level_M_err[,2])),
                             colMeans(mfts_Level_T_R_err[,,2]), colMeans(rbind(mfts_Level_F_R_err[,,2], mfts_Level_M_R_err[,,2])),
                             colMeans(mfts_Level_State_err[,,2]), colMeans(rbind(mfts_Level_State_F_err[,,2], mfts_Level_State_M_err[,,2])))
mfts_all_level_err_rmse = cbind(mfts_Level_0_err[,3], rowMeans(cbind(mfts_Level_F_err[,3], mfts_Level_M_err[,3])),
                             colMeans(mfts_Level_T_R_err[,,3]), colMeans(rbind(mfts_Level_F_R_err[,,3], mfts_Level_M_R_err[,,3])),
                             colMeans(mfts_Level_State_err[,,3]), colMeans(rbind(mfts_Level_State_F_err[,,3], mfts_Level_State_M_err[,,3])))

colnames(mfts_all_level_err_me) = colnames(mfts_all_level_err_mae) = colnames(mfts_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

########################################
# Point forecast errors; fmethod = "BU" 
########################################

# Define variables for the Bottom-up reconciled forecasts

Level_0_err     = Level_F_err       = Level_M_err       = matrix(0,15,3)
Level_T_R_err   = Level_F_R_err     = Level_M_R_err     = array(0, dim = c(15,3,8))
Level_State_err = Level_State_F_err = Level_State_M_err = array(0, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU", 
                       total_data_raw_rate_region = mfts_region_forc_total,
                       female_data_raw_rate_region = mfts_region_forc_female,
                       male_data_raw_rate_region = mfts_region_forc_male,
                       female_data_raw_rate = mfts_state_forc_female, 
                       male_data_raw_rate = mfts_state_forc_male, total_data_raw_rate = mfts_state_forc_total)
  
    # Total + Sex
    
    Level_0_err[ikw,1] = dum$me_total_err
    Level_F_err[ikw,1] = dum$me_female_err
    Level_M_err[ikw,1] = dum$me_male_err
    
    Level_0_err[ikw,2] = dum$mae_total_err
    Level_F_err[ikw,2] = dum$mae_female_err
    Level_M_err[ikw,2] = dum$mae_male_err
    
    Level_0_err[ikw,3] = dum$rmse_total_err
    Level_F_err[ikw,3] = dum$rmse_female_err
    Level_M_err[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    Level_T_R_err[ikw,1,] = dum$me_total_R_err
    Level_F_R_err[ikw,1,] = dum$me_female_R_err
    Level_M_R_err[ikw,1,] = dum$me_male_R_err
    
    Level_T_R_err[ikw,2,] = dum$mae_total_R_err
    Level_F_R_err[ikw,2,] = dum$mae_female_R_err
    Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
    Level_T_R_err[ikw,3,] = dum$rmse_total_R_err
    Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
    Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    Level_State_err[,ikw,1]   = dum$me_state_err
    Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
    Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
    
    Level_State_err[,ikw,2]   = dum$mae_state_err
    Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
    Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
    
    Level_State_err[,ikw,3]   = dum$rmse_state_err
    Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
    Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

BU_all_level_err_me = cbind(Level_0_err[,1], rowMeans(cbind(Level_F_err[,1], Level_M_err[,1])),
                             rowMeans(Level_T_R_err[,1,]), rowMeans(cbind(rowMeans(Level_F_R_err[,1,]), rowMeans(Level_M_R_err[,1,]))),
                             colMeans(Level_State_err[,,1]), colMeans(rbind(Level_State_F_err[,,1], Level_State_M_err[,,1])))

BU_all_level_err_mae = cbind(Level_0_err[,2], rowMeans(cbind(Level_F_err[,2], Level_M_err[,2])),
                             rowMeans(Level_T_R_err[,2,]), rowMeans(cbind(rowMeans(Level_F_R_err[,2,]), rowMeans(Level_M_R_err[,2,]))),
                             colMeans(Level_State_err[,,2]), colMeans(rbind(Level_State_F_err[,,2], Level_State_M_err[,,2])))

BU_all_level_err_rmse = cbind(Level_0_err[,3], rowMeans(cbind(Level_F_err[,3], Level_M_err[,3])),
                              rowMeans(Level_T_R_err[,3,]), rowMeans(cbind(rowMeans(Level_F_R_err[,3,]), rowMeans(Level_M_R_err[,3,]))),
                              colMeans(Level_State_err[,,3]), colMeans(rbind(Level_State_F_err[,,3], Level_State_M_err[,,3])))
colnames(BU_all_level_err_me) = colnames(BU_all_level_err_mae) = colnames(BU_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


##############################################
# Point forecast errors; fmethod = "comb_OLS" 
##############################################

# Define variables for the Optimal Combination (OLS) reconciled forecasts

optim_Level_0_err     = optim_Level_F_err       = optim_Level_M_err       = matrix(0,15,3)
optim_Level_T_R_err   = optim_Level_F_R_err     = optim_Level_M_R_err     = array(0, dim = c(15,3,8))
optim_Level_State_err = optim_Level_State_F_err = optim_Level_State_M_err = array(0, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS", 
                       total_data_raw_rate_region = mfts_region_forc_total,
                       female_data_raw_rate_region = mfts_region_forc_female,
                       male_data_raw_rate_region = mfts_region_forc_male,
                       female_data_raw_rate = mfts_state_forc_female, 
                       male_data_raw_rate = mfts_state_forc_male, total_data_raw_rate = mfts_state_forc_total)
  
    # Total + Sex
    
    optim_Level_0_err[ikw,1] = dum$me_total_err
    optim_Level_F_err[ikw,1] = dum$me_female_err
    optim_Level_M_err[ikw,1] = dum$me_male_err
    
    optim_Level_0_err[ikw,2] = dum$mae_total_err
    optim_Level_F_err[ikw,2] = dum$mae_female_err
    optim_Level_M_err[ikw,2] = dum$mae_male_err
    
    optim_Level_0_err[ikw,3] = dum$rmse_total_err
    optim_Level_F_err[ikw,3] = dum$rmse_female_err
    optim_Level_M_err[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    optim_Level_T_R_err[ikw,1,] = dum$me_total_R_err
    optim_Level_F_R_err[ikw,1,] = dum$me_female_R_err
    optim_Level_M_R_err[ikw,1,] = dum$me_male_R_err
    
    optim_Level_T_R_err[ikw,2,] = dum$mae_total_R_err
    optim_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
    optim_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
    optim_Level_T_R_err[ikw,3,] = dum$rmse_total_R_err
    optim_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
    optim_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    optim_Level_State_err[,ikw,1]   = dum$me_state_err
    optim_Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
    optim_Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
    
    optim_Level_State_err[,ikw,2]   = dum$mae_state_err
    optim_Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
    optim_Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
    
    optim_Level_State_err[,ikw,3]   = dum$rmse_state_err
    optim_Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
    optim_Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_all_level_err_me = cbind(optim_Level_0_err[,1], rowMeans(cbind(optim_Level_F_err[,1], optim_Level_M_err[,1])),
                             rowMeans(optim_Level_T_R_err[,1,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,1,]), rowMeans(optim_Level_M_R_err[,1,]))),
                             colMeans(optim_Level_State_err[,,1]), colMeans(rbind(optim_Level_State_F_err[,,1], optim_Level_State_M_err[,,1])))

optim_all_level_err_mae = cbind(optim_Level_0_err[,2], rowMeans(cbind(optim_Level_F_err[,2], optim_Level_M_err[,2])),
                             rowMeans(optim_Level_T_R_err[,2,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,2,]), rowMeans(optim_Level_M_R_err[,2,]))),
                             colMeans(optim_Level_State_err[,,2]), colMeans(rbind(optim_Level_State_F_err[,,2], optim_Level_State_M_err[,,2])))

optim_all_level_err_rmse = cbind(optim_Level_0_err[,3], rowMeans(cbind(optim_Level_F_err[,3], optim_Level_M_err[,3])),
                              rowMeans(optim_Level_T_R_err[,3,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err[,3,]), rowMeans(optim_Level_M_R_err[,3,]))),
                              colMeans(optim_Level_State_err[,,3]), colMeans(rbind(optim_Level_State_F_err[,,3], optim_Level_State_M_err[,,3])))
colnames(optim_all_level_err_me) = colnames(optim_all_level_err_mae) = colnames(optim_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

##############################################
# Point forecast errors; fmethod = "comb_GLS" 
##############################################

# Define variables for the Optimal Combination (GLS) reconciled forecasts

optim_GLS_Level_0_err     = optim_GLS_Level_F_err       = optim_GLS_Level_M_err       = matrix(0,15,3)
optim_GLS_Level_T_R_err   = optim_GLS_Level_F_R_err     = optim_GLS_Level_M_R_err     = array(0, dim = c(15,3,8))
optim_GLS_Level_State_err = optim_GLS_Level_State_F_err = optim_GLS_Level_State_M_err = array(0, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS", 
                       total_data_raw_rate_region = mfts_region_forc_total,
                       female_data_raw_rate_region = mfts_region_forc_female,
                       male_data_raw_rate_region = mfts_region_forc_male,
                       female_data_raw_rate = mfts_state_forc_female, 
                       male_data_raw_rate = mfts_state_forc_male, 
                       total_data_raw_rate = mfts_state_forc_total)
  
    # Total + Sex
    
    optim_GLS_Level_0_err[ikw,1] = dum$me_total_err
    optim_GLS_Level_F_err[ikw,1] = dum$me_female_err
    optim_GLS_Level_M_err[ikw,1] = dum$me_male_err
    
    optim_GLS_Level_0_err[ikw,2] = dum$mae_total_err
    optim_GLS_Level_F_err[ikw,2] = dum$mae_female_err
    optim_GLS_Level_M_err[ikw,2] = dum$mae_male_err
    
    optim_GLS_Level_0_err[ikw,3] = dum$rmse_total_err
    optim_GLS_Level_F_err[ikw,3] = dum$rmse_female_err
    optim_GLS_Level_M_err[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    optim_GLS_Level_T_R_err[ikw,1,] = dum$me_total_R_err
    optim_GLS_Level_F_R_err[ikw,1,] = dum$me_female_R_err
    optim_GLS_Level_M_R_err[ikw,1,] = dum$me_male_R_err
    
    optim_GLS_Level_T_R_err[ikw,2,] = dum$mae_total_R_err
    optim_GLS_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
    optim_GLS_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
    optim_GLS_Level_T_R_err[ikw,3,] = dum$rmse_total_R_err
    optim_GLS_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
    optim_GLS_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    optim_GLS_Level_State_err[,ikw,1]   = dum$me_state_err
    optim_GLS_Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
    optim_GLS_Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
    
    optim_GLS_Level_State_err[,ikw,2]   = dum$mae_state_err
    optim_GLS_Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
    optim_GLS_Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
    
    optim_GLS_Level_State_err[,ikw,3]   = dum$rmse_state_err
    optim_GLS_Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
    optim_GLS_Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_GLS_all_level_err_me = cbind(optim_GLS_Level_0_err[,1], rowMeans(cbind(optim_GLS_Level_F_err[,1], optim_GLS_Level_M_err[,1])),
                             rowMeans(optim_GLS_Level_T_R_err[,1,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err[,1,]), rowMeans(optim_GLS_Level_M_R_err[,1,]))),
                             colMeans(optim_GLS_Level_State_err[,,1]), colMeans(rbind(optim_GLS_Level_State_F_err[,,1], optim_GLS_Level_State_M_err[,,1])))

optim_GLS_all_level_err_mae = cbind(optim_GLS_Level_0_err[,2], rowMeans(cbind(optim_GLS_Level_F_err[,2], optim_GLS_Level_M_err[,2])),
                             rowMeans(optim_GLS_Level_T_R_err[,2,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err[,2,]), rowMeans(optim_GLS_Level_M_R_err[,2,]))),
                             colMeans(optim_GLS_Level_State_err[,,2]), colMeans(rbind(optim_GLS_Level_State_F_err[,,2], optim_GLS_Level_State_M_err[,,2])))

optim_GLS_all_level_err_rmse = cbind(optim_GLS_Level_0_err[,3], rowMeans(cbind(optim_GLS_Level_F_err[,3], optim_GLS_Level_M_err[,3])),
                              rowMeans(optim_GLS_Level_T_R_err[,3,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err[,3,]), rowMeans(optim_GLS_Level_M_R_err[,3,]))),
                              colMeans(optim_GLS_Level_State_err[,,3]), colMeans(rbind(optim_GLS_Level_State_F_err[,,3], optim_GLS_Level_State_M_err[,,3])))
colnames(optim_GLS_all_level_err_me) = colnames(optim_GLS_all_level_err_mae) = colnames(optim_GLS_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

##########################################
# Point forecast errors; fmethod = "mint" 
##########################################

# Define variables for the MinT reconciled forecasts

optim_mint_Level_0_err     = optim_mint_Level_F_err       = optim_mint_Level_M_err       = matrix(0,15,3)
optim_mint_Level_T_R_err   = optim_mint_Level_F_R_err     = optim_mint_Level_M_R_err     = array(0, dim = c(15,3,8))
optim_mint_Level_State_err = optim_mint_Level_State_F_err = optim_mint_Level_State_M_err = array(0, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_err(ik = ikw, hier_method = "mint", 
                     total_data_raw_rate_region = mfts_region_forc_total,
                     female_data_raw_rate_region = mfts_region_forc_female,
                     male_data_raw_rate_region = mfts_region_forc_male,
                     female_data_raw_rate = mfts_state_forc_female, 
                     male_data_raw_rate = mfts_state_forc_male, 
                     total_data_raw_rate = mfts_state_forc_total)
  
  # Total + Sex
  
  optim_mint_Level_0_err[ikw,1] = dum$me_total_err
  optim_mint_Level_F_err[ikw,1] = dum$me_female_err
  optim_mint_Level_M_err[ikw,1] = dum$me_male_err
  
  optim_mint_Level_0_err[ikw,2] = dum$mae_total_err
  optim_mint_Level_F_err[ikw,2] = dum$mae_female_err
  optim_mint_Level_M_err[ikw,2] = dum$mae_male_err
  
  optim_mint_Level_0_err[ikw,3] = dum$rmse_total_err
  optim_mint_Level_F_err[ikw,3] = dum$rmse_female_err
  optim_mint_Level_M_err[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  optim_mint_Level_T_R_err[ikw,1,] = dum$me_total_R_err
  optim_mint_Level_F_R_err[ikw,1,] = dum$me_female_R_err
  optim_mint_Level_M_R_err[ikw,1,] = dum$me_male_R_err
  
  optim_mint_Level_T_R_err[ikw,2,] = dum$mae_total_R_err
  optim_mint_Level_F_R_err[ikw,2,] = dum$mae_female_R_err
  optim_mint_Level_M_R_err[ikw,2,] = dum$mae_male_R_err
  
  optim_mint_Level_T_R_err[ikw,3,] = dum$rmse_total_R_err
  optim_mint_Level_F_R_err[ikw,3,] = dum$rmse_female_R_err
  optim_mint_Level_M_R_err[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  optim_mint_Level_State_err[,ikw,1]   = dum$me_state_err
  optim_mint_Level_State_F_err[,ikw,1] = dum$me_bottom_female_err
  optim_mint_Level_State_M_err[,ikw,1] = dum$me_bottom_male_err
  
  optim_mint_Level_State_err[,ikw,2]   = dum$mae_state_err
  optim_mint_Level_State_F_err[,ikw,2] = dum$mae_bottom_female_err
  optim_mint_Level_State_M_err[,ikw,2] = dum$mae_bottom_male_err
  
  optim_mint_Level_State_err[,ikw,3]   = dum$rmse_state_err
  optim_mint_Level_State_F_err[,ikw,3] = dum$rmse_bottom_female_err
  optim_mint_Level_State_M_err[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_mint_all_level_err_me = cbind(optim_mint_Level_0_err[,1], rowMeans(cbind(optim_mint_Level_F_err[,1], optim_mint_Level_M_err[,1])),
                                   rowMeans(optim_mint_Level_T_R_err[,1,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err[,1,]), rowMeans(optim_mint_Level_M_R_err[,1,]))),
                                   colMeans(optim_mint_Level_State_err[,,1]), colMeans(rbind(optim_mint_Level_State_F_err[,,1], optim_mint_Level_State_M_err[,,1])))

optim_mint_all_level_err_mae = cbind(optim_mint_Level_0_err[,2], rowMeans(cbind(optim_mint_Level_F_err[,2], optim_mint_Level_M_err[,2])),
                                    rowMeans(optim_mint_Level_T_R_err[,2,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err[,2,]), rowMeans(optim_mint_Level_M_R_err[,2,]))),
                                    colMeans(optim_mint_Level_State_err[,,2]), colMeans(rbind(optim_mint_Level_State_F_err[,,2], optim_mint_Level_State_M_err[,,2])))

optim_mint_all_level_err_rmse = cbind(optim_mint_Level_0_err[,3], rowMeans(cbind(optim_mint_Level_F_err[,3], optim_mint_Level_M_err[,3])),
                                     rowMeans(optim_mint_Level_T_R_err[,3,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err[,3,]), rowMeans(optim_mint_Level_M_R_err[,3,]))),
                                     colMeans(optim_mint_Level_State_err[,,3]), colMeans(rbind(optim_mint_Level_State_F_err[,,3], optim_mint_Level_State_M_err[,,3])))
colnames(optim_mint_all_level_err_me) = colnames(optim_mint_all_level_err_mae) = colnames(optim_mint_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


########################################
# Summary of reconciled point forecasts
########################################

point_accuracy_me   = cbind(mfts_all_level_err_me,   BU_all_level_err_me,   optim_all_level_err_me,   optim_mint_all_level_err_me)
point_accuracy_mae  = cbind(mfts_all_level_err_mae,  BU_all_level_err_mae,  optim_all_level_err_mae,  optim_mint_all_level_err_mae)
point_accuracy_rmse = cbind(mfts_all_level_err_rmse, BU_all_level_err_rmse, optim_all_level_err_rmse, optim_mint_all_level_err_rmse)

point_forecast_accuracy_me   = rbind(point_accuracy_me,   apply(point_accuracy_me,   2, mean), apply(point_accuracy_me,   2, median))
point_forecast_accuracy_mae  = rbind(point_accuracy_mae,  apply(point_accuracy_mae,  2, mean), apply(point_accuracy_mae,  2, median))
point_forecast_accuracy_rmse = rbind(point_accuracy_rmse, apply(point_accuracy_rmse, 2, mean), apply(point_accuracy_rmse, 2, median))

rownames(point_forecast_accuracy_me) = rownames(point_forecast_accuracy_mae) = rownames(point_forecast_accuracy_rmse) = c(1:15, "Mean", "Median")

