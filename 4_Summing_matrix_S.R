##########################################################
# Construct summing matrix S by forecast exposure to risk
##########################################################

library(demography)
library(ftsa)

# Define a function for computing the summing matrix S_t

Smat_fun <- function(kj, age, no_prefecture = 47)
{
    level_0 = level_1_a = level_1_b = matrix(0,2*no_prefecture,(16-kj))
    for(iw in 1:no_prefecture)
    {
        # Level 0 
        
        level_0[(2*iw-1),] = get(pop_ratio_P_F_to_T[iw])[kj,age,1:(16-kj)]
        level_0[(2*iw),]   = get(pop_ratio_P_M_to_T[iw])[kj,age,1:(16-kj)]
        
        # Level 1 (disaggregate by sex)
        
        level_1_a[(2*iw-1),] = get(pop_ratio_P_F_to_F[iw])[kj,age,1:(16-kj)]
        level_1_a[(2*iw),]   = rep(0,(16-kj))
        
        level_1_b[(2*iw-1),] = rep(0,(16-kj))
        level_1_b[(2*iw),]   = get(pop_ratio_P_M_to_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_1 = level_2_a_2 = level_2_a_3 = level_2_a_4 = level_2_a_5 = 
    level_2_a_6 = level_2_a_7 = level_2_a_8 = matrix(0,2*no_prefecture,(16-kj))
    
    ###################################  
    # Level 2 (disaggregate by region)
    ###################################
    
    # R1
    
    level_2_a_1[1,] = get(pop_ratio_P_F_to_R1_T[1])[kj,age,1:(16-kj)]  
    level_2_a_1[2,] = get(pop_ratio_P_M_to_R1_T[1])[kj,age,1:(16-kj)]
    level_2_a_1[3:(2*no_prefecture),] = matrix(rep(0,(16-kj)*92), ncol=(16-kj))
    
    # R2
    
    level_2_a_2[1:2, ] = matrix(rep(0,(16-kj)*2), ncol=(16-kj))
    for(iw in 1:6)
    {
        level_2_a_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_T[iw])[kj,age,1:(16-kj)]
        level_2_a_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_T[iw])[kj,age,1:(16-kj)]
    }  
    level_2_a_2[15:(2*no_prefecture),] = matrix(rep(0,(16-kj)*80), ncol=(16-kj))
    
    # R3
    
    level_2_a_3[1:14,] = matrix(rep(0,(16-kj)*14), ncol=(16-kj))  
    for(iw in 1:7)
    {
        level_2_a_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_T[iw])[kj,age,1:(16-kj)]
        level_2_a_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_T[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_3[29:(2*no_prefecture),] = matrix(rep(0,(16-kj)*66), ncol=(16-kj))
    
    # R4
    
    level_2_a_4[1:28,] = matrix(rep(0,(16-kj)*28), ncol=(16-kj))
    for(iw in 1:9)
    {
        level_2_a_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_T[iw])[kj,age,1:(16-kj)]
        level_2_a_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_T[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_4[47:(2*no_prefecture),] = matrix(rep(0,(16-kj)*48), ncol=(16-kj))
    
    # R5
    
    level_2_a_5[1:46,] = matrix(rep(0,(16-kj)*46), ncol=(16-kj))
    for(iw in 1:7)
    {
        level_2_a_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_T[iw])[kj,age,1:(16-kj)]
        level_2_a_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_T[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_5[61:(2*no_prefecture),] = matrix(rep(0,(16-kj)*34), ncol=(16-kj))
    
    # R6
    
    level_2_a_6[1:60,] = matrix(rep(0,(16-kj)*60), ncol=(16-kj))
    for(iw in 1:5)
    {
        level_2_a_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_T[iw])[kj,age,1:(16-kj)]
        level_2_a_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_T[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_6[71:(2*no_prefecture),] = matrix(rep(0,(16-kj)*24), ncol=(16-kj))
    
    # R7
    
    level_2_a_7[1:70,] = matrix(rep(0,(16-kj)*70), ncol=(16-kj))
    for(iw in 1:4)
    {
        level_2_a_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_T[iw])[kj,age,1:(16-kj)]
        level_2_a_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_T[iw])[kj,age,1:(16-kj)]
    }
    level_2_a_7[79:(2*no_prefecture),] = matrix(rep(0,(16-kj)*16), ncol=(16-kj))
    
    # R8
    
    level_2_a_8[1:78,] = matrix(rep(0,(16-kj)*78), ncol=(16-kj))
    for(iw in 1:8)
    {
        level_2_a_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_T[iw])[kj,age,1:(16-kj)]
        level_2_a_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_T[iw])[kj,age,1:(16-kj)] 
    }
    
    level_2_b_1 = level_2_b_2 = level_2_b_3 = level_2_b_4 = level_2_b_5 = level_2_b_6 = 
    level_2_b_7 = level_2_b_8 = matrix(0,(2*no_prefecture),(16-kj))
    
    ##################################################
    # Level 2 (disaggregate by region + sex (female))
    ##################################################
    
    # R1
    
    level_2_b_1[1,] = get(pop_ratio_P_F_to_R1_F[1])[kj,age,1:(16-kj)]  
    level_2_b_1[2:(2*no_prefecture),] = matrix(rep(0,(16-kj)*93), ncol=(16-kj))
    
    # R2
    
    level_2_b_2[1:2, ] = matrix(rep(0,(16-kj)*2), ncol=(16-kj))
    for(iw in 1:6)
    {
        level_2_b_2[(2*iw+1),] = get(pop_ratio_P_F_to_R2_F[iw])[kj,age,1:(16-kj)]
        level_2_b_2[(2*iw+2),] = rep(0, (16-kj))
    }  
    level_2_b_2[15:(2*no_prefecture),] = matrix(rep(0,(16-kj)*80), ncol=(16-kj))
    
    # R3
    
    level_2_b_3[1:14,] = matrix(rep(0,(16-kj)*14), ncol=(16-kj))  
    for(iw in 1:7)
    {
        level_2_b_3[(2*(iw+6)+1),] = get(pop_ratio_P_F_to_R3_F[iw])[kj,age,1:(16-kj)]
        level_2_b_3[(2*(iw+6)+2),] = rep(0, (16-kj))
    }
    level_2_b_3[29:(2*no_prefecture),] = matrix(rep(0,(16-kj)*66), ncol=(16-kj))
    
    # R4
    
    level_2_b_4[1:28,] = matrix(rep(0,(16-kj)*28), ncol=(16-kj))
    for(iw in 1:9)
    {
        level_2_b_4[(2*(iw+13)+1),] = get(pop_ratio_P_F_to_R4_F[iw])[kj,age,1:(16-kj)]
        level_2_b_4[(2*(iw+13)+2),] = rep(0, (16-kj))
    }
    level_2_b_4[47:(2*no_prefecture),] = matrix(rep(0,(16-kj)*48), ncol=(16-kj))
    
    # R5
    
    level_2_b_5[1:46,] = matrix(rep(0,(16-kj)*46), ncol=(16-kj))
    for(iw in 1:7)
    {
        level_2_b_5[(2*(iw+22)+1),] = get(pop_ratio_P_F_to_R5_F[iw])[kj,age,1:(16-kj)]
        level_2_b_5[(2*(iw+22)+2),] = rep(0, (16-kj))
    }
    level_2_b_5[61:(2*no_prefecture),] = matrix(rep(0,(16-kj)*34), ncol=(16-kj))
    
    # R6
    
    level_2_b_6[1:60,] = matrix(rep(0,(16-kj)*60), ncol=(16-kj))
    for(iw in 1:5)
    {
        level_2_b_6[(2*(iw+29)+1),] = get(pop_ratio_P_F_to_R6_F[iw])[kj,age,1:(16-kj)]
        level_2_b_6[(2*(iw+29)+2),] = rep(0, (16-kj))
    }
    level_2_b_6[71:(2*no_prefecture),] = matrix(rep(0,(16-kj)*24), ncol=(16-kj))
    
    # R7
    
    level_2_b_7[1:70,] = matrix(rep(0,(16-kj)*70), ncol=(16-kj))
    for(iw in 1:4)
    {
        level_2_b_7[(2*(iw+34)+1),] = get(pop_ratio_P_F_to_R7_F[iw])[kj,age,1:(16-kj)]
        level_2_b_7[(2*(iw+34)+2),] = rep(0, (16-kj))
    }
    level_2_b_7[79:(2*no_prefecture),] = matrix(rep(0,(16-kj)*16), ncol=(16-kj))
    
    # R8
    
    level_2_b_8[1:78,] = matrix(rep(0,(16-kj)*78), ncol=(16-kj))
    for(iw in 1:8)
    {
        level_2_b_8[(2*(iw+38)+1),] = get(pop_ratio_P_F_to_R8_F[iw])[kj,age,1:(16-kj)]
        level_2_b_8[(2*(iw+38)+2),] = rep(0, (16-kj))
    }
    
    level_2_c_1 = level_2_c_2 = level_2_c_3 = level_2_c_4 = level_2_c_5 = level_2_c_6 = 
    level_2_c_7 = level_2_c_8 = matrix(0,(2*no_prefecture),(16-kj))
    
    ################################################
    # Level 2 (disaggregate by region + sex (male))
    ################################################
    
    # R1
    
    level_2_c_1[1,] = rep(0,(16-kj))
    level_2_c_1[2,] = get(pop_ratio_P_M_to_R1_M[1])[kj,age,1:(16-kj)]
    level_2_c_1[3:(2*no_prefecture),] = matrix(rep(0,(16-kj)*92), ncol=(16-kj))
    
    # R2
    
    level_2_c_2[1:2, ] = matrix(rep(0,(16-kj)*2), ncol=(16-kj))
    for(iw in 1:6)
    {
        level_2_c_2[(2*iw+1),] = rep(0,(16-kj))
        level_2_c_2[(2*iw+2),] = get(pop_ratio_P_M_to_R2_M[iw])[kj,age,1:(16-kj)]
    }  
    level_2_c_2[15:(2*no_prefecture),] = matrix(rep(0,(16-kj)*80), ncol=(16-kj))
    
    # R3
    
    level_2_c_3[1:14,] = matrix(rep(0,(16-kj)*14), ncol=(16-kj))  
    for(iw in 1:7)
    {
        level_2_c_3[(2*(iw+6)+1),] = rep(0,(16-kj))
        level_2_c_3[(2*(iw+6)+2),] = get(pop_ratio_P_M_to_R3_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_c_3[29:(2*no_prefecture),] = matrix(rep(0,(16-kj)*66), ncol=(16-kj))
    
    # R4
    
    level_2_c_4[1:28,] = matrix(rep(0,(16-kj)*28), ncol=(16-kj))
    for(iw in 1:9)
    {
        level_2_c_4[(2*(iw+13)+1),] = rep(0,(16-kj))
        level_2_c_4[(2*(iw+13)+2),] = get(pop_ratio_P_M_to_R4_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_c_4[47:(2*no_prefecture),] = matrix(rep(0,(16-kj)*48), ncol=(16-kj))
    
    # R5
    
    level_2_c_5[1:46,] = matrix(rep(0,(16-kj)*46), ncol=(16-kj))
    for(iw in 1:7)
    {
        level_2_c_5[(2*(iw+22)+1),] = rep(0,(16-kj))
        level_2_c_5[(2*(iw+22)+2),] = get(pop_ratio_P_M_to_R5_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_c_5[61:(2*no_prefecture),] = matrix(rep(0,(16-kj)*34), ncol=(16-kj))
    
    # R6
    
    level_2_c_6[1:60,] = matrix(rep(0,(16-kj)*60), ncol=(16-kj))
    for(iw in 1:5)
    {
        level_2_c_6[(2*(iw+29)+1),] = rep(0,(16-kj))
        level_2_c_6[(2*(iw+29)+2),] = get(pop_ratio_P_M_to_R6_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_c_6[71:(2*no_prefecture),] = matrix(rep(0,(16-kj)*24), ncol=(16-kj))
    
    # R7
    
    level_2_c_7[1:70,] = matrix(rep(0,(16-kj)*70), ncol=(16-kj))
    for(iw in 1:4)
    {
        level_2_c_7[(2*(iw+34)+1),] = rep(0,(16-kj))
        level_2_c_7[(2*(iw+34)+2),] = get(pop_ratio_P_M_to_R7_M[iw])[kj,age,1:(16-kj)]
    }
    level_2_c_7[79:(2*no_prefecture),] = matrix(rep(0,(16-kj)*16), ncol=(16-kj))
    
    # R8
    
    level_2_c_8[1:78,] = matrix(rep(0,(16-kj)*78), ncol=(16-kj))
    for(iw in 1:8)
    {
        level_2_c_8[(2*(iw+38)+1),] = rep(0,(16-kj))
        level_2_c_8[(2*(iw+38)+2),] = get(pop_ratio_P_M_to_R8_M[iw])[kj,age,1:(16-kj)] 
    }
    
    #############################################################
    # Level 3 (disaggregate by prefecture + sex (female & male))
    #############################################################
    
    level_3 = array(0,dim = c((2*no_prefecture),(16-kj),47))
    for(iw in 1:47)
    {
        level_3[2*iw-1,,iw] = get(pop_ratio_P_F_to_P_T[iw])[kj,age,1:(16-kj)]
        level_3[2*iw,,iw]   = get(pop_ratio_P_M_to_P_T[iw])[kj,age,1:(16-kj)]
    }
    
    S_mat = array(0, dim = c(168, (2*no_prefecture), (16-kj)))
    for(ik in 1:(16-kj))
    {
        S_mat[,,ik] = rbind(level_0[,ik],     level_1_a[,ik],   level_1_b[,ik],   level_2_a_1[,ik],
                            level_2_a_2[,ik], level_2_a_3[,ik], level_2_a_4[,ik], level_2_a_5[,ik],
                            level_2_a_6[,ik], level_2_a_7[,ik], level_2_a_8[,ik], level_2_b_1[,ik],
                            level_2_b_2[,ik], level_2_b_3[,ik], level_2_b_4[,ik], level_2_b_5[,ik],
                            level_2_b_6[,ik], level_2_b_7[,ik], level_2_b_8[,ik], level_2_c_1[,ik],
                            level_2_c_2[,ik], level_2_c_3[,ik], level_2_c_4[,ik], level_2_c_5[,ik],
                            level_2_c_6[,ik], level_2_c_7[,ik], level_2_c_8[,ik], t(level_3[,ik,]),
                            diag(94))
    }
    return(S_mat)
}



############################################
# W_h matrix for MinT reconciliation method
############################################

# Define a function for computing the weight matrix W_h used in Mint reconciliation of univariate forecasts

wh_fun <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 168, ncol = (26+kj))
  
  # level 0
  eh_mat[1,] = get(ind_dynamic_state_train_residual_total[1])[[kj]][age,]
  
  # level 1 (disaggregate by sex)
  eh_mat[2,] = get(ind_dynamic_state_train_residual_female[1])[[kj]][age,]
  eh_mat[3,] = get(ind_dynamic_state_train_residual_male[1])[[kj]][age,]
  
  # level 2 & 3 (disaggregate by region & sex)
  for (ikw in 1:8)
  {
    eh_mat[(3+ikw),] = get(ind_dynamic_region_train_residual_total[ikw])[[kj]][age,]
    eh_mat[(11+ikw),] = get(ind_dynamic_region_train_residual_female[ikw])[[kj]][age,]
    eh_mat[(19+ikw),] = get(ind_dynamic_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # level 4 (disaggregate by state)
  for (ikw in 1:47)
  {
    eh_mat[(27+ikw),] = get(ind_dynamic_state_train_residual_total[(ikw+1)])[[kj]][age,]
  }
  
  # level 5 (disaggregate by state & sex)
  for (ikw in 1:47)
  {
    eh_mat[(74+(2*ikw-1)),] = get(ind_dynamic_state_train_residual_female[(ikw+1)])[[kj]][age,]
    eh_mat[(74+(2*ikw)),] = get(ind_dynamic_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}

# Define a function for computing the weight matrix W_h used in Mint reconciliation of multivariate forecasts

wh_fun_mfts <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 168, ncol = (26+kj))
  
  # level 0
  eh_mat[1,] = get(mfts_state_train_residual_total[1])[[kj]][age,]
  
  # level 1 (disaggregate by sex)
  eh_mat[2,] = get(mfts_state_train_residual_female[1])[[kj]][age,]
  eh_mat[3,] = get(mfts_state_train_residual_male[1])[[kj]][age,]
  
  # level 2 & 3 (disaggregate by region & sex)
  for (ikw in 1:8)
  {
    eh_mat[(3+ikw),] = get(mfts_region_train_residual_total[ikw])[[kj]][age,]
    eh_mat[(11+ikw),] = get(mfts_region_train_residual_female[ikw])[[kj]][age,]
    eh_mat[(19+ikw),] = get(mfts_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # level 4 (disaggregate by state)
  for (ikw in 1:47)
  {
    eh_mat[(27+ikw),] = get(mfts_state_train_residual_total[(ikw+1)])[[kj]][age,]
  }
  
  # level 5 (disaggregate by state & sex)
  for (ikw in 1:47)
  {
    eh_mat[(74+(2*ikw-1)),] = get(mfts_state_train_residual_female[(ikw+1)])[[kj]][age,]
    eh_mat[(74+(2*ikw)),] = get(mfts_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}





