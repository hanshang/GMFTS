##########################################################################################################
# Reconciliation of univariate point forecasts using the bottom-up and the optimal reconciliation methods
##########################################################################################################

# female_data_raw: Female age-specific exposure to risk
# male_data_raw: Male age-specific exposure to risk 
# female_data_raw_rate: Female age-specific mortality rates (ind)
# male_data_raw_rate: Male age-specific mortality rates (ind)
# age: Age variable
# kj: forecast horizon
# hier_method: bottom-up or optimal combination method

########################################
# Point forecast errors; fmethod = "BU" 
########################################

# create storing objects:

Level_0_err_ind_dynamic     = Level_F_err_ind_dynamic       = Level_M_err_ind_dynamic       = matrix(,15,3)
Level_T_R_err_ind_dynamic   = Level_F_R_err_ind_dynamic     = Level_M_R_err_ind_dynamic     = array(, dim = c(15,3,8))
Level_State_err_ind_dynamic = Level_State_F_err_ind_dynamic = Level_State_M_err_ind_dynamic = array(, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "BU", 
                       total_data_raw_rate_region = ind_dynamic_region_forc_total,
                       female_data_raw_rate_region = ind_dynamic_region_forc_female,
                       male_data_raw_rate_region = ind_dynamic_region_forc_male,
                       female_data_raw_rate = ind_dynamic_state_forc_female, 
                       male_data_raw_rate = ind_dynamic_state_forc_male, 
                       total_data_raw_rate = ind_dynamic_state_forc_total)
  
    # Total + Sex
    
    Level_0_err_ind_dynamic[ikw,1] = dum$me_total_err
    Level_F_err_ind_dynamic[ikw,1] = dum$me_female_err
    Level_M_err_ind_dynamic[ikw,1] = dum$me_male_err
    
    Level_0_err_ind_dynamic[ikw,2] = dum$mae_total_err
    Level_F_err_ind_dynamic[ikw,2] = dum$mae_female_err
    Level_M_err_ind_dynamic[ikw,2] = dum$mae_male_err
    
    Level_0_err_ind_dynamic[ikw,3] = dum$rmse_total_err
    Level_F_err_ind_dynamic[ikw,3] = dum$rmse_female_err
    Level_M_err_ind_dynamic[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    Level_T_R_err_ind_dynamic[ikw,1,] = dum$me_total_R_err
    Level_F_R_err_ind_dynamic[ikw,1,] = dum$me_female_R_err
    Level_M_R_err_ind_dynamic[ikw,1,] = dum$me_male_R_err
    
    Level_T_R_err_ind_dynamic[ikw,2,] = dum$mae_total_R_err
    Level_F_R_err_ind_dynamic[ikw,2,] = dum$mae_female_R_err
    Level_M_R_err_ind_dynamic[ikw,2,] = dum$mae_male_R_err
  
    Level_T_R_err_ind_dynamic[ikw,3,] = dum$rmse_total_R_err
    Level_F_R_err_ind_dynamic[ikw,3,] = dum$rmse_female_R_err
    Level_M_R_err_ind_dynamic[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    Level_State_err_ind_dynamic[,ikw,1]   = dum$me_state_err
    Level_State_F_err_ind_dynamic[,ikw,1] = dum$me_bottom_female_err
    Level_State_M_err_ind_dynamic[,ikw,1] = dum$me_bottom_male_err
    
    Level_State_err_ind_dynamic[,ikw,2]   = dum$mae_state_err
    Level_State_F_err_ind_dynamic[,ikw,2] = dum$mae_bottom_female_err
    Level_State_M_err_ind_dynamic[,ikw,2] = dum$mae_bottom_male_err
    
    Level_State_err_ind_dynamic[,ikw,3]   = dum$rmse_state_err
    Level_State_F_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_female_err
    Level_State_M_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

BU_all_level_err_me_ind_dynamic = cbind(Level_0_err_ind_dynamic[,1], rowMeans(cbind(Level_F_err_ind_dynamic[,1], Level_M_err_ind_dynamic[,1])),
                             rowMeans(Level_T_R_err_ind_dynamic[,1,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind_dynamic[,1,]), rowMeans(Level_M_R_err_ind_dynamic[,1,]))),
                             colMeans(Level_State_err_ind_dynamic[,,1]), colMeans(rbind(Level_State_F_err_ind_dynamic[,,1], Level_State_M_err_ind_dynamic[,,1])))

BU_all_level_err_mae_ind_dynamic = cbind(Level_0_err_ind_dynamic[,2], rowMeans(cbind(Level_F_err_ind_dynamic[,2], Level_M_err_ind_dynamic[,2])),
                             rowMeans(Level_T_R_err_ind_dynamic[,2,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind_dynamic[,2,]), rowMeans(Level_M_R_err_ind_dynamic[,2,]))),
                             colMeans(Level_State_err_ind_dynamic[,,2]), colMeans(rbind(Level_State_F_err_ind_dynamic[,,2], Level_State_M_err_ind_dynamic[,,2])))

BU_all_level_err_rmse_ind_dynamic = cbind(Level_0_err_ind_dynamic[,3], rowMeans(cbind(Level_F_err_ind_dynamic[,3], Level_M_err_ind_dynamic[,3])),
                             rowMeans(Level_T_R_err_ind_dynamic[,3,]), rowMeans(cbind(rowMeans(Level_F_R_err_ind_dynamic[,3,]), rowMeans(Level_M_R_err_ind_dynamic[,3,]))),
                             colMeans(Level_State_err_ind_dynamic[,,3]), colMeans(rbind(Level_State_F_err_ind_dynamic[,,3], Level_State_M_err_ind_dynamic[,,3])))
colnames(BU_all_level_err_me_ind_dynamic) = colnames(BU_all_level_err_mae_ind_dynamic) = colnames(BU_all_level_err_rmse_ind_dynamic) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

##############################################
# Point forecast errors; fmethod = "comb_OLS" 
##############################################

# create storing objects:

optim_Level_0_err_ind_dynamic = optim_Level_F_err_ind_dynamic = optim_Level_M_err_ind_dynamic  = matrix(,15,3)
optim_Level_T_R_err_ind_dynamic = optim_Level_F_R_err_ind_dynamic = optim_Level_M_R_err_ind_dynamic = array(, dim = c(15,3,8))
optim_Level_State_err_ind_dynamic = optim_Level_State_F_err_ind_dynamic = optim_Level_State_M_err_ind_dynamic = array(, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_OLS", 
                       total_data_raw_rate_region = ind_region_forc_total,
                       female_data_raw_rate_region = ind_region_forc_female,
                       male_data_raw_rate_region = ind_region_forc_male,
                       female_data_raw_rate = ind_state_forc_female, 
                       male_data_raw_rate = ind_state_forc_male, total_data_raw_rate = ind_state_forc_total)
  
    # Total + Sex
    
    optim_Level_0_err_ind_dynamic[ikw,1] = dum$me_total_err
    optim_Level_F_err_ind_dynamic[ikw,1] = dum$me_female_err
    optim_Level_M_err_ind_dynamic[ikw,1] = dum$me_male_err
    
    optim_Level_0_err_ind_dynamic[ikw,2] = dum$mae_total_err
    optim_Level_F_err_ind_dynamic[ikw,2] = dum$mae_female_err
    optim_Level_M_err_ind_dynamic[ikw,2] = dum$mae_male_err
    
    optim_Level_0_err_ind_dynamic[ikw,3] = dum$rmse_total_err
    optim_Level_F_err_ind_dynamic[ikw,3] = dum$rmse_female_err
    optim_Level_M_err_ind_dynamic[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    optim_Level_T_R_err_ind_dynamic[ikw,1,] = dum$me_total_R_err
    optim_Level_F_R_err_ind_dynamic[ikw,1,] = dum$me_female_R_err
    optim_Level_M_R_err_ind_dynamic[ikw,1,] = dum$me_male_R_err
    
    optim_Level_T_R_err_ind_dynamic[ikw,2,] = dum$mae_total_R_err
    optim_Level_F_R_err_ind_dynamic[ikw,2,] = dum$mae_female_R_err
    optim_Level_M_R_err_ind_dynamic[ikw,2,] = dum$mae_male_R_err
  
    optim_Level_T_R_err_ind_dynamic[ikw,3,] = dum$rmse_total_R_err
    optim_Level_F_R_err_ind_dynamic[ikw,3,] = dum$rmse_female_R_err
    optim_Level_M_R_err_ind_dynamic[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    optim_Level_State_err_ind_dynamic[,ikw,1]   = dum$me_state_err
    optim_Level_State_F_err_ind_dynamic[,ikw,1] = dum$me_bottom_female_err
    optim_Level_State_M_err_ind_dynamic[,ikw,1] = dum$me_bottom_male_err
    
    optim_Level_State_err_ind_dynamic[,ikw,2]   = dum$mae_state_err
    optim_Level_State_F_err_ind_dynamic[,ikw,2] = dum$mae_bottom_female_err
    optim_Level_State_M_err_ind_dynamic[,ikw,2] = dum$mae_bottom_male_err
    
    optim_Level_State_err_ind_dynamic[,ikw,3]   = dum$rmse_state_err
    optim_Level_State_F_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_female_err
    optim_Level_State_M_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_all_level_err_me_ind_dynamic = cbind(optim_Level_0_err_ind_dynamic[,1], rowMeans(cbind(optim_Level_F_err_ind_dynamic[,1], optim_Level_M_err_ind_dynamic[,1])),
                             rowMeans(optim_Level_T_R_err_ind_dynamic[,1,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err_ind_dynamic[,1,]), rowMeans(optim_Level_M_R_err_ind_dynamic[,1,]))),
                             colMeans(optim_Level_State_err_ind_dynamic[,,1]), colMeans(rbind(optim_Level_State_F_err_ind_dynamic[,,1], optim_Level_State_M_err_ind_dynamic[,,1])))

optim_all_level_err_mae_ind_dynamic = cbind(optim_Level_0_err_ind_dynamic[,2], rowMeans(cbind(optim_Level_F_err_ind_dynamic[,2], optim_Level_M_err_ind_dynamic[,2])),
                             rowMeans(optim_Level_T_R_err_ind_dynamic[,2,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err_ind_dynamic[,2,]), rowMeans(optim_Level_M_R_err_ind_dynamic[,2,]))),
                             colMeans(optim_Level_State_err_ind_dynamic[,,2]), colMeans(rbind(optim_Level_State_F_err_ind_dynamic[,,2], optim_Level_State_M_err_ind_dynamic[,,2])))

optim_all_level_err_rmse_ind_dynamic = cbind(optim_Level_0_err_ind_dynamic[,3], rowMeans(cbind(optim_Level_F_err_ind_dynamic[,3], optim_Level_M_err_ind_dynamic[,3])),
                             rowMeans(optim_Level_T_R_err_ind_dynamic[,3,]), rowMeans(cbind(rowMeans(optim_Level_F_R_err_ind_dynamic[,3,]), rowMeans(optim_Level_M_R_err_ind_dynamic[,3,]))),
                             colMeans(optim_Level_State_err_ind_dynamic[,,3]), colMeans(rbind(optim_Level_State_F_err_ind_dynamic[,,3], optim_Level_State_M_err_ind_dynamic[,,3])))
colnames(optim_all_level_err_me_ind_dynamic) = colnames(optim_all_level_err_mae_ind_dynamic) = colnames(optim_all_level_err_rmse_ind_dynamic) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

##############################################
# Point forecast errors; fmethod = "comb_GLS" 
##############################################

# create storing objects:

optim_GLS_Level_0_err_ind_dynamic = optim_GLS_Level_F_err_ind_dynamic = optim_GLS_Level_M_err_ind_dynamic  = matrix(,15,3)
optim_GLS_Level_T_R_err_ind_dynamic = optim_GLS_Level_F_R_err_ind_dynamic = optim_GLS_Level_M_R_err_ind_dynamic = array(, dim = c(15,3,8))
optim_GLS_Level_State_err_ind_dynamic = optim_GLS_Level_State_F_err_ind_dynamic = optim_GLS_Level_State_M_err_ind_dynamic = array(, dim = c(47,15,3))

for(ikw in 1:15)
{
    dum = BU_optim_err(ik = ikw, hier_method = "comb_GLS", 
                       total_data_raw_rate_region = ind_region_forc_total,
                       female_data_raw_rate_region = ind_region_forc_female,
                       male_data_raw_rate_region = ind_region_forc_male,
                       female_data_raw_rate = ind_state_forc_female, 
                       male_data_raw_rate = ind_state_forc_male, total_data_raw_rate = ind_state_forc_total)
  
    # Total + Sex
    
    optim_GLS_Level_0_err_ind_dynamic[ikw,1] = dum$me_total_err
    optim_GLS_Level_F_err_ind_dynamic[ikw,1] = dum$me_female_err
    optim_GLS_Level_M_err_ind_dynamic[ikw,1] = dum$me_male_err
    
    optim_GLS_Level_0_err_ind_dynamic[ikw,2] = dum$mae_total_err
    optim_GLS_Level_F_err_ind_dynamic[ikw,2] = dum$mae_female_err
    optim_GLS_Level_M_err_ind_dynamic[ikw,2] = dum$mae_male_err
    
    optim_GLS_Level_0_err_ind_dynamic[ikw,3] = dum$rmse_total_err
    optim_GLS_Level_F_err_ind_dynamic[ikw,3] = dum$rmse_female_err
    optim_GLS_Level_M_err_ind_dynamic[ikw,3] = dum$rmse_male_err
    
    # Region + Sex
    
    optim_GLS_Level_T_R_err_ind_dynamic[ikw,1,] = dum$me_total_R_err
    optim_GLS_Level_F_R_err_ind_dynamic[ikw,1,] = dum$me_female_R_err
    optim_GLS_Level_M_R_err_ind_dynamic[ikw,1,] = dum$me_male_R_err
    
    optim_GLS_Level_T_R_err_ind_dynamic[ikw,2,] = dum$mae_total_R_err
    optim_GLS_Level_F_R_err_ind_dynamic[ikw,2,] = dum$mae_female_R_err
    optim_GLS_Level_M_R_err_ind_dynamic[ikw,2,] = dum$mae_male_R_err
  
    optim_GLS_Level_T_R_err_ind_dynamic[ikw,3,] = dum$rmse_total_R_err
    optim_GLS_Level_F_R_err_ind_dynamic[ikw,3,] = dum$rmse_female_R_err
    optim_GLS_Level_M_R_err_ind_dynamic[ikw,3,] = dum$rmse_male_R_err
    
    # Prefecture + Sex
  
    optim_GLS_Level_State_err_ind_dynamic[,ikw,1]   = dum$me_state_err
    optim_GLS_Level_State_F_err_ind_dynamic[,ikw,1] = dum$me_bottom_female_err
    optim_GLS_Level_State_M_err_ind_dynamic[,ikw,1] = dum$me_bottom_male_err
    
    optim_GLS_Level_State_err_ind_dynamic[,ikw,2]   = dum$mae_state_err
    optim_GLS_Level_State_F_err_ind_dynamic[,ikw,2] = dum$mae_bottom_female_err
    optim_GLS_Level_State_M_err_ind_dynamic[,ikw,2] = dum$mae_bottom_male_err
    
    optim_GLS_Level_State_err_ind_dynamic[,ikw,3]   = dum$rmse_state_err
    optim_GLS_Level_State_F_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_female_err
    optim_GLS_Level_State_M_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_GLS_all_level_err_me_ind_dynamic = cbind(optim_GLS_Level_0_err_ind_dynamic[,1], rowMeans(cbind(optim_GLS_Level_F_err_ind_dynamic[,1], optim_GLS_Level_M_err_ind_dynamic[,1])),
                             rowMeans(optim_GLS_Level_T_R_err_ind_dynamic[,1,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err_ind_dynamic[,1,]), rowMeans(optim_GLS_Level_M_R_err_ind_dynamic[,1,]))),
                             colMeans(optim_GLS_Level_State_err_ind_dynamic[,,1]), colMeans(rbind(optim_GLS_Level_State_F_err_ind_dynamic[,,1], optim_GLS_Level_State_M_err_ind_dynamic[,,1])))

optim_GLS_all_level_err_mae_ind_dynamic = cbind(optim_GLS_Level_0_err_ind_dynamic[,2], rowMeans(cbind(optim_GLS_Level_F_err_ind_dynamic[,2], optim_GLS_Level_M_err_ind_dynamic[,2])),
                             rowMeans(optim_GLS_Level_T_R_err_ind_dynamic[,2,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err_ind_dynamic[,2,]), rowMeans(optim_GLS_Level_M_R_err_ind_dynamic[,2,]))),
                             colMeans(optim_GLS_Level_State_err_ind_dynamic[,,2]), colMeans(rbind(optim_GLS_Level_State_F_err_ind_dynamic[,,2], optim_GLS_Level_State_M_err_ind_dynamic[,,2])))

optim_GLS_all_level_err_rmse_ind_dynamic = cbind(optim_GLS_Level_0_err_ind_dynamic[,3], rowMeans(cbind(optim_GLS_Level_F_err_ind_dynamic[,3], optim_GLS_Level_M_err_ind_dynamic[,3])),
                             rowMeans(optim_GLS_Level_T_R_err_ind_dynamic[,3,]), rowMeans(cbind(rowMeans(optim_GLS_Level_F_R_err_ind_dynamic[,3,]), rowMeans(optim_GLS_Level_M_R_err_ind_dynamic[,3,]))),
                             colMeans(optim_GLS_Level_State_err_ind_dynamic[,,3]), colMeans(rbind(optim_GLS_Level_State_F_err_ind_dynamic[,,3], optim_GLS_Level_State_M_err_ind_dynamic[,,3])))
colnames(optim_GLS_all_level_err_me_ind_dynamic) = colnames(optim_GLS_all_level_err_mae_ind_dynamic) = colnames(optim_GLS_all_level_err_rmse_ind_dynamic) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")


##############################################
# Point forecast errors; fmethod = "mint" 
##############################################

# create storing objects:

optim_mint_Level_0_err_ind_dynamic = optim_mint_Level_F_err_ind_dynamic = optim_mint_Level_M_err_ind_dynamic  = matrix(,15,3)
optim_mint_Level_T_R_err_ind_dynamic = optim_mint_Level_F_R_err_ind_dynamic = optim_mint_Level_M_R_err_ind_dynamic = array(, dim = c(15,3,8))
optim_mint_Level_State_err_ind_dynamic = optim_mint_Level_State_F_err_ind_dynamic = optim_mint_Level_State_M_err_ind_dynamic = array(, dim = c(47,15,3))

for(ikw in 1:15)
{
  dum = BU_optim_err(ik = ikw, hier_method = "mint", 
                     total_data_raw_rate_region = ind_region_forc_total,
                     female_data_raw_rate_region = ind_region_forc_female,
                     male_data_raw_rate_region = ind_region_forc_male,
                     female_data_raw_rate = ind_state_forc_female, 
                     male_data_raw_rate = ind_state_forc_male, total_data_raw_rate = ind_state_forc_total)
  
  # Total + Sex
  
  optim_mint_Level_0_err_ind_dynamic[ikw,1] = dum$me_total_err
  optim_mint_Level_F_err_ind_dynamic[ikw,1] = dum$me_female_err
  optim_mint_Level_M_err_ind_dynamic[ikw,1] = dum$me_male_err
  
  optim_mint_Level_0_err_ind_dynamic[ikw,2] = dum$mae_total_err
  optim_mint_Level_F_err_ind_dynamic[ikw,2] = dum$mae_female_err
  optim_mint_Level_M_err_ind_dynamic[ikw,2] = dum$mae_male_err
  
  optim_mint_Level_0_err_ind_dynamic[ikw,3] = dum$rmse_total_err
  optim_mint_Level_F_err_ind_dynamic[ikw,3] = dum$rmse_female_err
  optim_mint_Level_M_err_ind_dynamic[ikw,3] = dum$rmse_male_err
  
  # Region + Sex
  
  optim_mint_Level_T_R_err_ind_dynamic[ikw,1,] = dum$me_total_R_err
  optim_mint_Level_F_R_err_ind_dynamic[ikw,1,] = dum$me_female_R_err
  optim_mint_Level_M_R_err_ind_dynamic[ikw,1,] = dum$me_male_R_err
  
  optim_mint_Level_T_R_err_ind_dynamic[ikw,2,] = dum$mae_total_R_err
  optim_mint_Level_F_R_err_ind_dynamic[ikw,2,] = dum$mae_female_R_err
  optim_mint_Level_M_R_err_ind_dynamic[ikw,2,] = dum$mae_male_R_err
  
  optim_mint_Level_T_R_err_ind_dynamic[ikw,3,] = dum$rmse_total_R_err
  optim_mint_Level_F_R_err_ind_dynamic[ikw,3,] = dum$rmse_female_R_err
  optim_mint_Level_M_R_err_ind_dynamic[ikw,3,] = dum$rmse_male_R_err
  
  # Prefecture + Sex
  
  optim_mint_Level_State_err_ind_dynamic[,ikw,1]   = dum$me_state_err
  optim_mint_Level_State_F_err_ind_dynamic[,ikw,1] = dum$me_bottom_female_err
  optim_mint_Level_State_M_err_ind_dynamic[,ikw,1] = dum$me_bottom_male_err
  
  optim_mint_Level_State_err_ind_dynamic[,ikw,2]   = dum$mae_state_err
  optim_mint_Level_State_F_err_ind_dynamic[,ikw,2] = dum$mae_bottom_female_err
  optim_mint_Level_State_M_err_ind_dynamic[,ikw,2] = dum$mae_bottom_male_err
  
  optim_mint_Level_State_err_ind_dynamic[,ikw,3]   = dum$rmse_state_err
  optim_mint_Level_State_F_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_female_err
  optim_mint_Level_State_M_err_ind_dynamic[,ikw,3] = dum$rmse_bottom_male_err
}

# summary of results

optim_mint_all_level_err_me_ind_dynamic = cbind(optim_mint_Level_0_err_ind_dynamic[,1], rowMeans(cbind(optim_mint_Level_F_err_ind_dynamic[,1], optim_mint_Level_M_err_ind_dynamic[,1])),
                                       rowMeans(optim_mint_Level_T_R_err_ind_dynamic[,1,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind_dynamic[,1,]), rowMeans(optim_mint_Level_M_R_err_ind_dynamic[,1,]))),
                                       colMeans(optim_mint_Level_State_err_ind_dynamic[,,1]), colMeans(rbind(optim_mint_Level_State_F_err_ind_dynamic[,,1], optim_mint_Level_State_M_err_ind_dynamic[,,1])))

optim_mint_all_level_err_mae_ind_dynamic = cbind(optim_mint_Level_0_err_ind_dynamic[,2], rowMeans(cbind(optim_mint_Level_F_err_ind_dynamic[,2], optim_mint_Level_M_err_ind_dynamic[,2])),
                                        rowMeans(optim_mint_Level_T_R_err_ind_dynamic[,2,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind_dynamic[,2,]), rowMeans(optim_mint_Level_M_R_err_ind_dynamic[,2,]))),
                                        colMeans(optim_mint_Level_State_err_ind_dynamic[,,2]), colMeans(rbind(optim_mint_Level_State_F_err_ind_dynamic[,,2], optim_mint_Level_State_M_err_ind_dynamic[,,2])))

optim_mint_all_level_err_rmse_ind_dynamic = cbind(optim_mint_Level_0_err_ind_dynamic[,3], rowMeans(cbind(optim_mint_Level_F_err_ind_dynamic[,3], optim_mint_Level_M_err_ind_dynamic[,3])),
                                         rowMeans(optim_mint_Level_T_R_err_ind_dynamic[,3,]), rowMeans(cbind(rowMeans(optim_mint_Level_F_R_err_ind_dynamic[,3,]), rowMeans(optim_mint_Level_M_R_err_ind_dynamic[,3,]))),
                                         colMeans(optim_mint_Level_State_err_ind_dynamic[,,3]), colMeans(rbind(optim_mint_Level_State_F_err_ind_dynamic[,,3], optim_mint_Level_State_M_err_ind_dynamic[,,3])))
colnames(optim_mint_all_level_err_me_ind_dynamic) = colnames(optim_mint_all_level_err_mae_ind_dynamic) = colnames(optim_mint_all_level_err_rmse_ind_dynamic) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

########################################
# Summary of reconciled point forecasts
########################################

point_accuracy_me_ind_dynamic   = cbind(ind_all_level_err_me,   BU_all_level_err_me_ind_dynamic,   optim_all_level_err_me_ind_dynamic,   optim_mint_all_level_err_me_ind_dynamic)
point_accuracy_mae_ind_dynamic  = cbind(ind_all_level_err_mae,  BU_all_level_err_mae_ind_dynamic,  optim_all_level_err_mae_ind_dynamic,  optim_mint_all_level_err_mae_ind_dynamic)
point_accuracy_rmse_ind_dynamic = cbind(ind_all_level_err_rmse, BU_all_level_err_rmse_ind_dynamic, optim_all_level_err_rmse_ind_dynamic, optim_mint_all_level_err_rmse_ind_dynamic)

point_forecast_accuracy_me_ind_dynamic   = rbind(point_accuracy_me_ind_dynamic,   apply(point_accuracy_me_ind_dynamic,   2, mean), apply(point_accuracy_me_ind_dynamic,   2, median))
point_forecast_accuracy_mae_ind_dynamic  = rbind(point_accuracy_mae_ind_dynamic,  apply(point_accuracy_mae_ind_dynamic,  2, mean), apply(point_accuracy_mae_ind_dynamic,  2, median))
point_forecast_accuracy_rmse_ind_dynamic = rbind(point_accuracy_rmse_ind_dynamic, apply(point_accuracy_rmse_ind_dynamic, 2, mean), apply(point_accuracy_rmse_ind_dynamic, 2, median))

rownames(point_forecast_accuracy_me_ind_dynamic) = rownames(point_forecast_accuracy_mae_ind_dynamic) = rownames(point_forecast_accuracy_rmse_ind_dynamic) = c(1:15, "Mean", "Median")

