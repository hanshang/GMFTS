#########################################
# Summing matrix for sex-specific series
#########################################

library(demography)
library(ftsa)

# Define functions for computing the summing matrix S_t following the gender-specific hierarchy structure

# female series

Smat_F <- function(kj, age, no_prefecture = 47)
{
  ##################################
  # Level 1: female national total
  ##################################
  
  level_1_total = matrix(NA, no_prefecture, (16-kj))
  for (iw in 1:no_prefecture)
  {
    level_1_total[iw,] = get(pop_ratio_P_F_to_F[iw])[kj,age,1:(16-kj)]
  }
  
  #################################
  # Level 2: female regional level
  #################################
  
  level_2_F_1 = level_2_F_2 = level_2_F_3 = level_2_F_4 = level_2_F_5 = level_2_F_6 = level_2_F_7 = level_2_F_8 = matrix(rep(0,(16-kj)*no_prefecture), nrow = no_prefecture, ncol = (16-kj))
  
  # R1
  level_2_F_1[1,] = get(pop_ratio_P_F_to_R1_F[1])[kj,age,1:(16-kj)]

  
  # R2
  for(iw in 1:6)
  {
    level_2_F_2[(iw+1),] = get(pop_ratio_P_F_to_R2_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R3
  for(iw in 1:7)
  {
    level_2_F_3[(iw+7),] = get(pop_ratio_P_F_to_R3_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R4
  for(iw in 1:9)
  {
    level_2_F_4[(iw+14),] = get(pop_ratio_P_F_to_R4_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R5
  for(iw in 1:7)
  {
    level_2_F_5[(iw+23),] = get(pop_ratio_P_F_to_R5_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R6
  for(iw in 1:5)
  {
    level_2_F_6[(iw+30),] = get(pop_ratio_P_F_to_R6_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R7
  for(iw in 1:4)
  {
    level_2_F_7[(iw+35),] = get(pop_ratio_P_F_to_R7_F[iw])[kj,age,1:(16-kj)]
  }
  
  # R8
  for(iw in 1:8)
  {
    level_2_F_8[(iw+39),] = get(pop_ratio_P_F_to_R8_F[iw])[kj,age,1:(16-kj)]
  }
 
  ###################################
  # Level 3: female prefecture level
  ###################################
  
  level_3_F = diag(no_prefecture)
  
  Smat_F = array(NA, dim = c(56, no_prefecture, (16-kj)))
  for(ik in 1:(16-kj))
  {
    Smat_F[,,ik] = rbind(level_1_total[,ik], level_2_F_1[,ik], level_2_F_2[,ik], level_2_F_3[,ik], level_2_F_4[,ik], level_2_F_5[,ik], level_2_F_6[,ik], level_2_F_7[,ik], level_2_F_8[,ik], level_3_F) 
  }
  
  return(Smat_F)
}


# male series

Smat_M <- function(kj, age, no_prefecture = 47)
{
  ##################################
  # Level 1: male national total
  ##################################
  
  level_1_total = matrix(NA, no_prefecture, (16-kj))
  for (iw in 1:no_prefecture)
  {
    level_1_total[iw,] = get(pop_ratio_P_M_to_M[iw])[kj,age,1:(16-kj)]
  }
  
  #################################
  # Level 2: male regional level
  #################################
  
  level_2_M_1 = level_2_M_2 = level_2_M_3 = level_2_M_4 = level_2_M_5 = level_2_M_6 = level_2_M_7 = level_2_M_8 = matrix(rep(0,(16-kj)*no_prefecture), nrow = no_prefecture, ncol = (16-kj))
  
  # R1
  level_2_M_1[1,] = get(pop_ratio_P_M_to_R1_M[1])[kj,age,1:(16-kj)]
  
  
  # R2
  for(iw in 1:6)
  {
    level_2_M_2[(iw+1),] = get(pop_ratio_P_M_to_R2_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R3
  for(iw in 1:7)
  {
    level_2_M_3[(iw+7),] = get(pop_ratio_P_M_to_R3_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R4
  for(iw in 1:9)
  {
    level_2_M_4[(iw+14),] = get(pop_ratio_P_M_to_R4_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R5
  for(iw in 1:7)
  {
    level_2_M_5[(iw+23),] = get(pop_ratio_P_M_to_R5_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R6
  for(iw in 1:5)
  {
    level_2_M_6[(iw+30),] = get(pop_ratio_P_M_to_R6_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R7
  for(iw in 1:4)
  {
    level_2_M_7[(iw+35),] = get(pop_ratio_P_M_to_R7_M[iw])[kj,age,1:(16-kj)]
  }
  
  # R8
  for(iw in 1:8)
  {
    level_2_M_8[(iw+39),] = get(pop_ratio_P_M_to_R8_M[iw])[kj,age,1:(16-kj)]
  }
  
  ###################################
  # Level 3: male prefecture level
  ###################################
  
  level_3_M = diag(no_prefecture)
  
  Smat_M = array(NA, dim = c(56, no_prefecture, (16-kj)))
  for(ik in 1:(16-kj))
  {
    Smat_M[,,ik] = rbind(level_1_total[,ik], level_2_M_1[,ik], level_2_M_2[,ik], level_2_M_3[,ik], level_2_M_4[,ik], level_2_M_5[,ik], level_2_M_6[,ik], level_2_M_7[,ik], level_2_M_8[,ik], level_3_M) 
  }
  
  return(Smat_M)
}

#################################################################
# W_h_F and W_h_M summing matrix for MinT reconciliation method
#################################################################

wh_fun_F <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 56, ncol = (26+kj))
  
  # Total
  eh_mat[1,] = get(mfts_gender_state_train_residual_female[1])[[kj]][age,]
  
  # Region
  for (ikw in 1:8)
  {
    eh_mat[(1+ikw),] = get(mfts_gender_region_train_residual_female[ikw])[[kj]][age,]
  }
  
  # Prefecture
  for (ikw in 1:47)
  {
    eh_mat[(9+ikw),] = get(mfts_gender_state_train_residual_female[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}


wh_fun_M <- function(kj, age)
{
  lowerD = hts:::lowerD
  shrink.estim = hts:::shrink.estim
  
  eh_mat = matrix(NA, nrow = 56, ncol = (26+kj))
  
  # Total
  eh_mat[1,] = get(mfts_gender_state_train_residual_male[1])[[kj]][age,]
  
  # Region
  for (ikw in 1:8)
  {
    eh_mat[(1+ikw),] = get(mfts_gender_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # Prefecture
  for (ikw in 1:47)
  {
    eh_mat[(9+ikw),] = get(mfts_gender_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}


# Define a function for computing the weight matrix W_h used in Mint reconciliation of multivariate forecasts according to the gender-specific hierarchy

wh_fun_gender <- function(kj, age)
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
    eh_mat[(11+ikw),] = get(mfts_gender_region_train_residual_female[ikw])[[kj]][age,]
    eh_mat[(19+ikw),] = get(mfts_gender_region_train_residual_male[ikw])[[kj]][age,]
  }
  
  # level 4 (disaggregate by state)
  for (ikw in 1:47)
  {
    eh_mat[(27+ikw),] = get(mfts_state_train_residual_total[(ikw+1)])[[kj]][age,]
  }
  
  # level 5 (disaggregate by state & sex)
  for (ikw in 1:47)
  {
    eh_mat[(74+(2*ikw-1)),] = get(mfts_gender_state_train_residual_female[(ikw+1)])[[kj]][age,]
    eh_mat[(74+(2*ikw)),] = get(mfts_gender_state_train_residual_male[(ikw+1)])[[kj]][age,]
  }
  
  target = lowerD(t(eh_mat))
  shrink = shrink.estim(t(eh_mat), target)
  wh_mat = shrink[[1]]
  
  return(wh_mat = wh_mat)
}




















