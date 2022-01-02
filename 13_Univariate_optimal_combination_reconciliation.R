################################################################################################
# Calculate mean interval scores for the univariate forecasting with optimal combination method
################################################################################################

library(demography)
library(ftsa)

# PI_val = OLS_optim_hier_comb_lb_ub: All-level lower and upper bounds of forecast mortality rates
# data_series: specific data series
# series: female, male or total
# fh: forecast horizon
# index: corresponding index in the hierarchy

# Compute mean interval scores for the univariate forecasting method
# Japan + Sex

optim_interval_score_Japan_total_dynamic = optim_interval_score_Japan_female_dynamic = optim_interval_score_Japan_male_dynamic = rep(0,15)
for(ik in 1:15)
{
    # Level 0
    
    optim_interval_score_Japan_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Japan, series = "total", 
                                                                   fh = ik, index = 1)
    
    # Level 1
    
    optim_interval_score_Japan_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Japan, series = "female", 
                                                                    fh = ik, index = 2)
    
    optim_interval_score_Japan_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Japan, series = "male", 
                                                                  fh = ik, index = 3)
    print(ik)
}

# Region total

optim_interval_score_R1_total_dynamic = optim_interval_score_R2_total_dynamic = optim_interval_score_R3_total_dynamic = optim_interval_score_R4_total_dynamic = 
optim_interval_score_R5_total_dynamic = optim_interval_score_R6_total_dynamic = optim_interval_score_R7_total_dynamic = optim_interval_score_R8_total_dynamic = rep(0,15)
for(ik in 1:15)
{
    # Level 2
    
    optim_interval_score_R1_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R1, series = "total",
                                                                fh = ik, index = 4)
    
    optim_interval_score_R2_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R2, series = "total",
                                                                fh = ik, index = 5)
    
    optim_interval_score_R3_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R3, series = "total",
                                                                fh = ik, index = 6)
    
    optim_interval_score_R4_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R4, series = "total",
                                                                fh = ik, index = 7)
    
    optim_interval_score_R5_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R5, series = "total",
                                                                fh = ik, index = 8)
    
    optim_interval_score_R6_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R6, series = "total",
                                                                fh = ik, index = 9)
    
    optim_interval_score_R7_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R7, series = "total",
                                                                fh = ik, index = 10)
    
    optim_interval_score_R8_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R8, series = "total",
                                                                fh = ik, index = 11)
    print(ik)
}

# Region + Sex

optim_interval_score_R1_female_dynamic = optim_interval_score_R2_female_dynamic = optim_interval_score_R3_female_dynamic = optim_interval_score_R4_female_dynamic = 
optim_interval_score_R5_female_dynamic = optim_interval_score_R6_female_dynamic = optim_interval_score_R7_female_dynamic = optim_interval_score_R8_female_dynamic = 
optim_interval_score_R1_male_dynamic = optim_interval_score_R2_male_dynamic = optim_interval_score_R3_male_dynamic = optim_interval_score_R4_male_dynamic = 
optim_interval_score_R5_male_dynamic = optim_interval_score_R6_male_dynamic = optim_interval_score_R7_male_dynamic = optim_interval_score_R8_male_dynamic = rep(0,15)

for(ik in 1:15)
{
    # Level 3 (female)
    
    optim_interval_score_R1_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R1, series = "female",
                                                                 fh = ik, index = 12)
    
    optim_interval_score_R2_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R2, series = "female",
                                                                 fh = ik, index = 13)
    
    optim_interval_score_R3_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R3, series = "female",
                                                                 fh = ik, index = 14)
    
    optim_interval_score_R4_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R4, series = "female",
                                                                 fh = ik, index = 15)
    
    optim_interval_score_R5_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R5, series = "female",
                                                                 fh = ik, index = 16)
    
    optim_interval_score_R6_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R6, series = "female",
                                                                 fh = ik, index = 17)
    
    optim_interval_score_R7_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R7, series = "female",
                                                                 fh = ik, index = 18)
    
    optim_interval_score_R8_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R8, series = "female",
                                                                 fh = ik, index = 19)
    
    # Level 3 (male)
    
    optim_interval_score_R1_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R1, series = "male",
                                                               fh = ik, index = 20)
    
    optim_interval_score_R2_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R2, series = "male",
                                                               fh = ik, index = 21)
    
    optim_interval_score_R3_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R3, series = "male",
                                                               fh = ik, index = 22)
    
    optim_interval_score_R4_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R4, series = "male",
                                                               fh = ik, index = 23)
    
    optim_interval_score_R5_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R5, series = "male",
                                                               fh = ik, index = 24)
    
    optim_interval_score_R6_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R6, series = "male",
                                                               fh = ik, index = 25)
    
    optim_interval_score_R7_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R7, series = "male",
                                                               fh = ik, index = 26)
    
    optim_interval_score_R8_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = mfts_R8, series = "male",
                                                               fh = ik, index = 27)
    print(ik)
}

# Prefecture total

optim_interval_score_P1_total_dynamic  = optim_interval_score_P2_total_dynamic  = optim_interval_score_P3_total_dynamic  = optim_interval_score_P4_total_dynamic = 
optim_interval_score_P5_total_dynamic  = optim_interval_score_P6_total_dynamic  = optim_interval_score_P7_total_dynamic  = optim_interval_score_P8_total_dynamic = 
optim_interval_score_P9_total_dynamic  = optim_interval_score_P10_total_dynamic = optim_interval_score_P11_total_dynamic = optim_interval_score_P12_total_dynamic = 
optim_interval_score_P13_total_dynamic = optim_interval_score_P14_total_dynamic = optim_interval_score_P15_total_dynamic = optim_interval_score_P16_total_dynamic = 
optim_interval_score_P17_total_dynamic = optim_interval_score_P18_total_dynamic = optim_interval_score_P19_total_dynamic = optim_interval_score_P20_total_dynamic = 
optim_interval_score_P21_total_dynamic = optim_interval_score_P22_total_dynamic = optim_interval_score_P23_total_dynamic = optim_interval_score_P24_total_dynamic = 
optim_interval_score_P25_total_dynamic = optim_interval_score_P26_total_dynamic = optim_interval_score_P27_total_dynamic = optim_interval_score_P28_total_dynamic = 
optim_interval_score_P29_total_dynamic = optim_interval_score_P30_total_dynamic = optim_interval_score_P31_total_dynamic = optim_interval_score_P32_total_dynamic = 
optim_interval_score_P33_total_dynamic = optim_interval_score_P34_total_dynamic = optim_interval_score_P35_total_dynamic = optim_interval_score_P36_total_dynamic = 
optim_interval_score_P37_total_dynamic = optim_interval_score_P38_total_dynamic = optim_interval_score_P39_total_dynamic = optim_interval_score_P40_total_dynamic = 
optim_interval_score_P41_total_dynamic = optim_interval_score_P42_total_dynamic = optim_interval_score_P43_total_dynamic = optim_interval_score_P44_total_dynamic = 
optim_interval_score_P45_total_dynamic = optim_interval_score_P46_total_dynamic = optim_interval_score_P47_total_dynamic = rep(0,15)

for(ik in 1:15)
{
    optim_interval_score_P1_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hokkaido, series = "total",
                                                                fh = ik, index = 28)
    
    optim_interval_score_P2_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aomori, series = "total",
                                                                fh = ik, index = 29)
    
    optim_interval_score_P3_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Iwate, series = "total",
                                                                fh = ik, index = 30)
    
    optim_interval_score_P4_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyagi, series = "total",
                                                                fh = ik, index = 31)
    
    optim_interval_score_P5_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Akita, series = "total",
                                                                fh = ik, index = 32)
    
    optim_interval_score_P6_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamagata, series = "total",
                                                                fh = ik, index = 33)
    
    optim_interval_score_P7_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukushima, series = "total",
                                                                fh = ik, index = 34)
    
    optim_interval_score_P8_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ibaraki, series = "total",
                                                                fh = ik, index = 35)
    
    optim_interval_score_P9_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tochigi, series = "total",
                                                                fh = ik, index = 36)
    
    optim_interval_score_P10_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gunma, series = "total",
                                                                 fh = ik, index = 37)
    
    optim_interval_score_P11_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saitama, series = "total",
                                                                 fh = ik, index = 38)
    
    optim_interval_score_P12_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Chiba, series = "total",
                                                                 fh = ik, index = 39)
    
    optim_interval_score_P13_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokyo, series = "total",
                                                                 fh = ik, index = 40)
    
    optim_interval_score_P14_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kanagawa, series = "total",
                                                                 fh = ik, index = 41)
    
    optim_interval_score_P15_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Niigata, series = "total",
                                                                 fh = ik, index = 42)
    
    optim_interval_score_P16_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Toyama, series = "total",
                                                                 fh = ik, index = 43)
    
    optim_interval_score_P17_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ishikawa, series = "total",
                                                                 fh = ik, index = 44)
    
    optim_interval_score_P18_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukui, series = "total",
                                                                 fh = ik, index = 45)
    
    optim_interval_score_P19_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamanashi, series = "total",
                                                                 fh = ik, index = 46)
    
    optim_interval_score_P20_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagano, series = "total",
                                                                 fh = ik, index = 47)
    
    optim_interval_score_P21_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gifu, series = "total",
                                                                 fh = ik, index = 48)
    
    optim_interval_score_P22_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shizuoka, series = "total",
                                                                 fh = ik, index = 49)
    
    optim_interval_score_P23_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aichi, series = "total",
                                                                 fh = ik, index = 50)
    
    optim_interval_score_P24_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Mie, series = "total",
                                                                 fh = ik, index = 51)
    
    optim_interval_score_P25_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shiga, series = "total",
                                                                 fh = ik, index = 52)
    
    optim_interval_score_P26_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kyoto, series = "total",
                                                                 fh = ik, index = 53)
    
    optim_interval_score_P27_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Osaka, series = "total",
                                                                 fh = ik, index = 54)
    
    optim_interval_score_P28_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hyogo, series = "total",
                                                                 fh = ik, index = 55)
    
    optim_interval_score_P29_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nara, series = "total",
                                                                 fh = ik, index = 56)
    
    optim_interval_score_P30_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Wakayama, series = "total",
                                                                 fh = ik, index = 57)
    
    optim_interval_score_P31_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tottori, series = "total",
                                                                 fh = ik, index = 58)
    
    optim_interval_score_P32_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shimane, series = "total",
                                                                 fh = ik, index = 59)
    
    optim_interval_score_P33_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okayama, series = "total",
                                                                 fh = ik, index = 60)
    
    optim_interval_score_P34_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hiroshima, series = "total",
                                                                 fh = ik, index = 61)
    
    optim_interval_score_P35_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamaguchi, series = "total",
                                                                 fh = ik, index = 62)
    
    optim_interval_score_P36_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokushima, series = "total",
                                                                 fh = ik, index = 63)
    
    optim_interval_score_P37_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagawa, series = "total",
                                                                 fh = ik, index = 64)
    
    optim_interval_score_P38_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ehime, series = "total",
                                                                 fh = ik, index = 65)
    
    optim_interval_score_P39_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kochi, series = "total",
                                                                 fh = ik, index = 66)
    
    optim_interval_score_P40_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukuoka, series = "total",
                                                                 fh = ik, index = 67)
    
    optim_interval_score_P41_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saga, series = "total",
                                                                 fh = ik, index = 68)
    
    optim_interval_score_P42_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagasaki, series = "total",
                                                                 fh = ik, index = 69)
    
    optim_interval_score_P43_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kumamoto, series = "total",
                                                                 fh = ik, index = 70)
    
    optim_interval_score_P44_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Oita, series = "total",
                                                                 fh = ik, index = 71)
    
    optim_interval_score_P45_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyazaki, series = "total",
                                                                 fh = ik, index = 72)
    
    optim_interval_score_P46_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagoshima, series = "total",
                                                                 fh = ik, index = 73)
    
    optim_interval_score_P47_total_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okinawa, series = "total",
                                                                 fh = ik, index = 74)
    print(ik)
}

# Prefecture + Sex

optim_interval_score_P1_female_dynamic  = optim_interval_score_P2_female_dynamic  = optim_interval_score_P3_female_dynamic  = optim_interval_score_P4_female_dynamic = 
optim_interval_score_P5_female_dynamic  = optim_interval_score_P6_female_dynamic  = optim_interval_score_P7_female_dynamic  = optim_interval_score_P8_female_dynamic = 
optim_interval_score_P9_female_dynamic  = optim_interval_score_P10_female_dynamic = optim_interval_score_P11_female_dynamic = optim_interval_score_P12_female_dynamic = 
optim_interval_score_P13_female_dynamic = optim_interval_score_P14_female_dynamic = optim_interval_score_P15_female_dynamic = optim_interval_score_P16_female_dynamic = 
optim_interval_score_P17_female_dynamic = optim_interval_score_P18_female_dynamic = optim_interval_score_P19_female_dynamic = optim_interval_score_P20_female_dynamic = 
optim_interval_score_P21_female_dynamic = optim_interval_score_P22_female_dynamic = optim_interval_score_P23_female_dynamic = optim_interval_score_P24_female_dynamic = 
optim_interval_score_P25_female_dynamic = optim_interval_score_P26_female_dynamic = optim_interval_score_P27_female_dynamic = optim_interval_score_P28_female_dynamic = 
optim_interval_score_P29_female_dynamic = optim_interval_score_P30_female_dynamic = optim_interval_score_P31_female_dynamic = optim_interval_score_P32_female_dynamic = 
optim_interval_score_P33_female_dynamic = optim_interval_score_P34_female_dynamic = optim_interval_score_P35_female_dynamic = optim_interval_score_P36_female_dynamic = 
optim_interval_score_P37_female_dynamic = optim_interval_score_P38_female_dynamic = optim_interval_score_P39_female_dynamic = optim_interval_score_P40_female_dynamic = 
optim_interval_score_P41_female_dynamic = optim_interval_score_P42_female_dynamic = optim_interval_score_P43_female_dynamic = optim_interval_score_P44_female_dynamic = 
optim_interval_score_P45_female_dynamic = optim_interval_score_P46_female_dynamic = optim_interval_score_P47_female_dynamic = rep(0,15)

for(ik in 1:15)
{
    optim_interval_score_P1_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hokkaido, series = "female",
                                                                 fh = ik, index = 75)
    
    optim_interval_score_P2_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aomori, series = "female",
                                                                 fh = ik, index = 77)
    
    optim_interval_score_P3_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Iwate, series = "female",
                                                                 fh = ik, index = 79)
    
    optim_interval_score_P4_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyagi, series = "female",
                                                                 fh = ik, index = 81)
    
    optim_interval_score_P5_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Akita, series = "female",
                                                                 fh = ik, index = 83)
    
    optim_interval_score_P6_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamagata, series = "female",
                                                                 fh = ik, index = 85)
    
    optim_interval_score_P7_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukushima, series = "female",
                                                                 fh = ik, index = 87)
    
    optim_interval_score_P8_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ibaraki, series = "female",
                                                                 fh = ik, index = 89)
    
    optim_interval_score_P9_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tochigi, series = "female",
                                                                 fh = ik, index = 91)
    
    optim_interval_score_P10_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gunma, series = "female",
                                                                  fh = ik, index = 93)
    
    optim_interval_score_P11_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saitama, series = "female",
                                                                  fh = ik, index = 95)
    
    optim_interval_score_P12_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Chiba, series = "female",
                                                                  fh = ik, index = 97)
    
    optim_interval_score_P13_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokyo, series = "female",
                                                                  fh = ik, index = 99)
    
    optim_interval_score_P14_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kanagawa, series = "female",
                                                                  fh = ik, index = 101)
    
    optim_interval_score_P15_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Niigata, series = "female",
                                                                  fh = ik, index = 103)
    
    optim_interval_score_P16_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Toyama, series = "female",
                                                                  fh = ik, index = 105)
    
    optim_interval_score_P17_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ishikawa, series = "female",
                                                                  fh = ik, index = 107)
    
    optim_interval_score_P18_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukui, series = "female",
                                                                  fh = ik, index = 109)
    
    optim_interval_score_P19_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamanashi, series = "female",
                                                                  fh = ik, index = 111)
    
    optim_interval_score_P20_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagano, series = "female",
                                                                  fh = ik, index = 113)
    
    optim_interval_score_P21_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gifu, series = "female",
                                                                  fh = ik, index = 115)
    
    optim_interval_score_P22_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shizuoka, series = "female",
                                                                  fh = ik, index = 117)
    
    optim_interval_score_P23_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aichi, series = "female",
                                                                  fh = ik, index = 119)
    
    optim_interval_score_P24_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Mie, series = "female",
                                                                  fh = ik, index = 121)
    
    optim_interval_score_P25_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shiga, series = "female",
                                                                  fh = ik, index = 123)
    
    optim_interval_score_P26_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kyoto, series = "female",
                                                                  fh = ik, index = 125)
    
    optim_interval_score_P27_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Osaka, series = "female",
                                                                  fh = ik, index = 127)
    
    optim_interval_score_P28_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hyogo, series = "female",
                                                                  fh = ik, index = 129)
    
    optim_interval_score_P29_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nara, series = "female",
                                                                  fh = ik, index = 131)
    
    optim_interval_score_P30_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Wakayama, series = "female",
                                                                  fh = ik, index = 133)
    
    optim_interval_score_P31_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tottori, series = "female",
                                                                  fh = ik, index = 135)
    
    optim_interval_score_P32_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shimane, series = "female",
                                                                  fh = ik, index = 137)
    
    optim_interval_score_P33_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okayama, series = "female",
                                                                  fh = ik, index = 139)
    
    optim_interval_score_P34_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hiroshima, series = "female",
                                                                  fh = ik, index = 141)
    
    optim_interval_score_P35_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamaguchi, series = "female",
                                                                  fh = ik, index = 143)
    
    optim_interval_score_P36_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokushima, series = "female",
                                                                  fh = ik, index = 145)
    
    optim_interval_score_P37_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagawa, series = "female",
                                                                  fh = ik, index = 147)
    
    optim_interval_score_P38_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ehime, series = "female",
                                                                  fh = ik, index = 149)
    
    optim_interval_score_P39_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kochi, series = "female",
                                                                  fh = ik, index = 151)
    
    optim_interval_score_P40_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukuoka, series = "female",
                                                                  fh = ik, index = 153)
    
    optim_interval_score_P41_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saga, series = "female",
                                                                  fh = ik, index = 155)
    
    optim_interval_score_P42_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagasaki, series = "female",
                                                                  fh = ik, index = 157)
    
    optim_interval_score_P43_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kumamoto, series = "female",
                                                                  fh = ik, index = 159)
    
    optim_interval_score_P44_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Oita, series = "female",
                                                                  fh = ik, index = 161)
    
    optim_interval_score_P45_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyazaki, series = "female",
                                                                  fh = ik, index = 163)
    
    optim_interval_score_P46_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagoshima, series = "female",
                                                                  fh = ik, index = 165)
    
    optim_interval_score_P47_female_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okinawa, series = "female",
                                                                  fh = ik, index = 167)
    print(ik)
}

optim_interval_score_P1_male_dynamic  = optim_interval_score_P2_male_dynamic  = optim_interval_score_P3_male_dynamic  = optim_interval_score_P4_male_dynamic = 
optim_interval_score_P5_male_dynamic  = optim_interval_score_P6_male_dynamic  = optim_interval_score_P7_male_dynamic  = optim_interval_score_P8_male_dynamic = 
optim_interval_score_P9_male_dynamic  = optim_interval_score_P10_male_dynamic = optim_interval_score_P11_male_dynamic = optim_interval_score_P12_male_dynamic = 
optim_interval_score_P13_male_dynamic = optim_interval_score_P14_male_dynamic = optim_interval_score_P15_male_dynamic = optim_interval_score_P16_male_dynamic = 
optim_interval_score_P17_male_dynamic = optim_interval_score_P18_male_dynamic = optim_interval_score_P19_male_dynamic = optim_interval_score_P20_male_dynamic = 
optim_interval_score_P21_male_dynamic = optim_interval_score_P22_male_dynamic = optim_interval_score_P23_male_dynamic = optim_interval_score_P24_male_dynamic = 
optim_interval_score_P25_male_dynamic = optim_interval_score_P26_male_dynamic = optim_interval_score_P27_male_dynamic = optim_interval_score_P28_male_dynamic = 
optim_interval_score_P29_male_dynamic = optim_interval_score_P30_male_dynamic = optim_interval_score_P31_male_dynamic = optim_interval_score_P32_male_dynamic = 
optim_interval_score_P33_male_dynamic = optim_interval_score_P34_male_dynamic = optim_interval_score_P35_male_dynamic = optim_interval_score_P36_male_dynamic = 
optim_interval_score_P37_male_dynamic = optim_interval_score_P38_male_dynamic = optim_interval_score_P39_male_dynamic = optim_interval_score_P40_male_dynamic = 
optim_interval_score_P41_male_dynamic = optim_interval_score_P42_male_dynamic = optim_interval_score_P43_male_dynamic = optim_interval_score_P44_male_dynamic = 
optim_interval_score_P45_male_dynamic = optim_interval_score_P46_male_dynamic = optim_interval_score_P47_male_dynamic = rep(0,15)

for(ik in 1:15)
{
    optim_interval_score_P1_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hokkaido, series = "male",
                                                            fh = ik, index = 76)
    
    optim_interval_score_P2_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aomori, series = "male",
                                                            fh = ik, index = 78)
    
    optim_interval_score_P3_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Iwate, series = "male",
                                                            fh = ik, index = 80)
    
    optim_interval_score_P4_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyagi, series = "male",
                                                            fh = ik, index = 82)
    
    optim_interval_score_P5_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Akita, series = "male",
                                                            fh = ik, index = 84)
    
    optim_interval_score_P6_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamagata, series = "male",
                                                            fh = ik, index = 86)
    
    optim_interval_score_P7_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukushima, series = "male",
                                                            fh = ik, index = 88)
    
    optim_interval_score_P8_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ibaraki, series = "male",
                                                            fh = ik, index = 90)
    
    optim_interval_score_P9_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tochigi, series = "male",
                                                            fh = ik, index = 92)
    
    optim_interval_score_P10_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gunma, series = "male",
                                                             fh = ik, index = 94)
    
    optim_interval_score_P11_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saitama, series = "male",
                                                             fh = ik, index = 96)
    
    optim_interval_score_P12_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Chiba, series = "male",
                                                             fh = ik, index = 98)
    
    optim_interval_score_P13_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokyo, series = "male",
                                                             fh = ik, index = 100)
    
    optim_interval_score_P14_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kanagawa, series = "male",
                                                             fh = ik, index = 102)
    
    optim_interval_score_P15_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Niigata, series = "male",
                                                             fh = ik, index = 104)
    
    optim_interval_score_P16_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Toyama, series = "male",
                                                             fh = ik, index = 106)
    
    optim_interval_score_P17_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ishikawa, series = "male",
                                                             fh = ik, index = 108)
    
    optim_interval_score_P18_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukui, series = "male",
                                                             fh = ik, index = 110)
    
    optim_interval_score_P19_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamanashi, series = "male",
                                                             fh = ik, index = 112)
    
    optim_interval_score_P20_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagano, series = "male",
                                                             fh = ik, index = 114)
    
    optim_interval_score_P21_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Gifu, series = "male",
                                                             fh = ik, index = 116)
    
    optim_interval_score_P22_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shizuoka, series = "male",
                                                             fh = ik, index = 118)
    
    optim_interval_score_P23_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Aichi, series = "male",
                                                             fh = ik, index = 120)
    
    optim_interval_score_P24_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Mie, series = "male",
                                                             fh = ik, index = 122)
    
    optim_interval_score_P25_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shiga, series = "male",
                                                             fh = ik, index = 124)
    
    optim_interval_score_P26_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kyoto, series = "male",
                                                             fh = ik, index = 126)
    
    optim_interval_score_P27_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Osaka, series = "male",
                                                             fh = ik, index = 128)
    
    optim_interval_score_P28_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hyogo, series = "male",
                                                             fh = ik, index = 130)
    
    optim_interval_score_P29_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nara, series = "male",
                                                             fh = ik, index = 132)
    
    optim_interval_score_P30_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Wakayama, series = "male",
                                                             fh = ik, index = 134)
    
    optim_interval_score_P31_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tottori, series = "male",
                                                             fh = ik, index = 136)
    
    optim_interval_score_P32_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Shimane, series = "male",
                                                             fh = ik, index = 138)
    
    optim_interval_score_P33_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okayama, series = "male",
                                                             fh = ik, index = 140)
    
    optim_interval_score_P34_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Hiroshima, series = "male",
                                                             fh = ik, index = 142)
    
    optim_interval_score_P35_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Yamaguchi, series = "male",
                                                             fh = ik, index = 144)
    
    optim_interval_score_P36_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Tokushima, series = "male",
                                                             fh = ik, index = 146)
    
    optim_interval_score_P37_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagawa, series = "male",
                                                             fh = ik, index = 148)
    
    optim_interval_score_P38_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Ehime, series = "male",
                                                             fh = ik, index = 150)
    
    optim_interval_score_P39_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kochi, series = "male",
                                                             fh = ik, index = 152)
    
    optim_interval_score_P40_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Fukuoka, series = "male",
                                                             fh = ik, index = 154)
    
    optim_interval_score_P41_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Saga, series = "male",
                                                             fh = ik, index = 156)
    
    optim_interval_score_P42_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Nagasaki, series = "male",
                                                             fh = ik, index = 158)
    
    optim_interval_score_P43_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kumamoto, series = "male",
                                                             fh = ik, index = 160)
    
    optim_interval_score_P44_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Oita, series = "male",
                                                             fh = ik, index = 162)
    
    optim_interval_score_P45_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Miyazaki, series = "male",
                                                             fh = ik, index = 164)
    
    optim_interval_score_P46_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Kagoshima, series = "male",
                                                             fh = ik, index = 166)
    
    optim_interval_score_P47_male_dynamic[ik] = interval_score_BU_optim(PI_val = OLS_hier_comb_dynamic, data_series = Okinawa, series = "male",
                                                             fh = ik, index = 168)
    print(ik)
}

#########################################################
# Summary results at all levels of hierarchy time series
#########################################################

optim_interval_score_Level_0_dynamic = optim_interval_score_Japan_total_dynamic

optim_interval_score_Level_1_dynamic = rowMeans(cbind(optim_interval_score_Japan_female_dynamic, optim_interval_score_Japan_male_dynamic))

optim_interval_score_Level_2_dynamic = rowMeans(cbind(optim_interval_score_R1_total_dynamic, optim_interval_score_R2_total_dynamic, optim_interval_score_R3_total_dynamic, optim_interval_score_R4_total_dynamic,
                                           optim_interval_score_R5_total_dynamic, optim_interval_score_R6_total_dynamic, optim_interval_score_R7_total_dynamic, optim_interval_score_R8_total_dynamic))

optim_interval_score_Level_3_dynamic = rowMeans(cbind(optim_interval_score_R1_female_dynamic, optim_interval_score_R2_female_dynamic, optim_interval_score_R3_female_dynamic, optim_interval_score_R4_female_dynamic,
                                           optim_interval_score_R5_female_dynamic, optim_interval_score_R6_female_dynamic, optim_interval_score_R7_female_dynamic, optim_interval_score_R8_female_dynamic,
                                           optim_interval_score_R1_male_dynamic, optim_interval_score_R2_male_dynamic, optim_interval_score_R3_male_dynamic, optim_interval_score_R4_male_dynamic,
                                           optim_interval_score_R5_male_dynamic, optim_interval_score_R6_male_dynamic, optim_interval_score_R7_male_dynamic, optim_interval_score_R8_male_dynamic))

optim_interval_score_Level_4_dynamic = rowMeans(cbind(optim_interval_score_P1_total_dynamic,  optim_interval_score_P2_total_dynamic,  optim_interval_score_P3_total_dynamic,  optim_interval_score_P4_total_dynamic,  optim_interval_score_P5_total_dynamic, 
                                           optim_interval_score_P6_total_dynamic,  optim_interval_score_P7_total_dynamic,  optim_interval_score_P8_total_dynamic,  optim_interval_score_P9_total_dynamic,  optim_interval_score_P10_total_dynamic, 
                                           optim_interval_score_P11_total_dynamic, optim_interval_score_P12_total_dynamic, optim_interval_score_P13_total_dynamic, optim_interval_score_P14_total_dynamic, optim_interval_score_P15_total_dynamic, 
                                           optim_interval_score_P16_total_dynamic, optim_interval_score_P17_total_dynamic, optim_interval_score_P18_total_dynamic, optim_interval_score_P19_total_dynamic, optim_interval_score_P20_total_dynamic, 
                                           optim_interval_score_P21_total_dynamic, optim_interval_score_P22_total_dynamic, optim_interval_score_P23_total_dynamic, optim_interval_score_P24_total_dynamic, optim_interval_score_P25_total_dynamic, 
                                           optim_interval_score_P26_total_dynamic, optim_interval_score_P27_total_dynamic, optim_interval_score_P28_total_dynamic, optim_interval_score_P29_total_dynamic, optim_interval_score_P30_total_dynamic, 
                                           optim_interval_score_P31_total_dynamic, optim_interval_score_P32_total_dynamic, optim_interval_score_P33_total_dynamic, optim_interval_score_P34_total_dynamic, optim_interval_score_P35_total_dynamic, 
                                           optim_interval_score_P36_total_dynamic, optim_interval_score_P37_total_dynamic, optim_interval_score_P38_total_dynamic, optim_interval_score_P39_total_dynamic, optim_interval_score_P40_total_dynamic, 
                                           optim_interval_score_P41_total_dynamic, optim_interval_score_P42_total_dynamic, optim_interval_score_P43_total_dynamic, optim_interval_score_P44_total_dynamic, optim_interval_score_P45_total_dynamic, 
                                           optim_interval_score_P46_total_dynamic, optim_interval_score_P47_total_dynamic))

optim_interval_score_Level_5_dynamic = rowMeans(cbind(optim_interval_score_P1_female_dynamic,  optim_interval_score_P2_female_dynamic,  optim_interval_score_P3_female_dynamic,  optim_interval_score_P4_female_dynamic,  optim_interval_score_P5_female_dynamic, 
                                           optim_interval_score_P6_female_dynamic,  optim_interval_score_P7_female_dynamic,  optim_interval_score_P8_female_dynamic,  optim_interval_score_P9_female_dynamic,  optim_interval_score_P10_female_dynamic, 
                                           optim_interval_score_P11_female_dynamic, optim_interval_score_P12_female_dynamic, optim_interval_score_P13_female_dynamic, optim_interval_score_P14_female_dynamic, optim_interval_score_P15_female_dynamic, 
                                           optim_interval_score_P16_female_dynamic, optim_interval_score_P17_female_dynamic, optim_interval_score_P18_female_dynamic, optim_interval_score_P19_female_dynamic, optim_interval_score_P20_female_dynamic, 
                                           optim_interval_score_P21_female_dynamic, optim_interval_score_P22_female_dynamic, optim_interval_score_P23_female_dynamic, optim_interval_score_P24_female_dynamic, optim_interval_score_P25_female_dynamic, 
                                           optim_interval_score_P26_female_dynamic, optim_interval_score_P27_female_dynamic, optim_interval_score_P28_female_dynamic, optim_interval_score_P29_female_dynamic, optim_interval_score_P30_female_dynamic, 
                                           optim_interval_score_P31_female_dynamic, optim_interval_score_P32_female_dynamic, optim_interval_score_P33_female_dynamic, optim_interval_score_P34_female_dynamic, optim_interval_score_P35_female_dynamic, 
                                           optim_interval_score_P36_female_dynamic, optim_interval_score_P37_female_dynamic, optim_interval_score_P38_female_dynamic, optim_interval_score_P39_female_dynamic, optim_interval_score_P40_female_dynamic, 
                                           optim_interval_score_P41_female_dynamic, optim_interval_score_P42_female_dynamic, optim_interval_score_P43_female_dynamic, optim_interval_score_P44_female_dynamic, optim_interval_score_P45_female_dynamic, 
                                           optim_interval_score_P46_female_dynamic, optim_interval_score_P47_female_dynamic,
                                           optim_interval_score_P1_male_dynamic,  optim_interval_score_P2_male_dynamic,  optim_interval_score_P3_male_dynamic,  optim_interval_score_P4_male_dynamic,  optim_interval_score_P5_male_dynamic, 
                                           optim_interval_score_P6_male_dynamic,  optim_interval_score_P7_male_dynamic,  optim_interval_score_P8_male_dynamic,  optim_interval_score_P9_male_dynamic,  optim_interval_score_P10_male_dynamic, 
                                           optim_interval_score_P11_male_dynamic, optim_interval_score_P12_male_dynamic, optim_interval_score_P13_male_dynamic, optim_interval_score_P14_male_dynamic, optim_interval_score_P15_male_dynamic, 
                                           optim_interval_score_P16_male_dynamic, optim_interval_score_P17_male_dynamic, optim_interval_score_P18_male_dynamic, optim_interval_score_P19_male_dynamic, optim_interval_score_P20_male_dynamic, 
                                           optim_interval_score_P21_male_dynamic, optim_interval_score_P22_male_dynamic, optim_interval_score_P23_male_dynamic, optim_interval_score_P24_male_dynamic, optim_interval_score_P25_male_dynamic, 
                                           optim_interval_score_P26_male_dynamic, optim_interval_score_P27_male_dynamic, optim_interval_score_P28_male_dynamic, optim_interval_score_P29_male_dynamic, optim_interval_score_P30_male_dynamic, 
                                           optim_interval_score_P31_male_dynamic, optim_interval_score_P32_male_dynamic, optim_interval_score_P33_male_dynamic, optim_interval_score_P34_male_dynamic, optim_interval_score_P35_male_dynamic, 
                                           optim_interval_score_P36_male_dynamic, optim_interval_score_P37_male_dynamic, optim_interval_score_P38_male_dynamic, optim_interval_score_P39_male_dynamic, optim_interval_score_P40_male_dynamic, 
                                           optim_interval_score_P41_male_dynamic, optim_interval_score_P42_male_dynamic, optim_interval_score_P43_male_dynamic, optim_interval_score_P44_male_dynamic, optim_interval_score_P45_male_dynamic, 
                                           optim_interval_score_P46_male_dynamic, optim_interval_score_P47_male_dynamic))


optim_interval_score_all_dynamic = cbind(optim_interval_score_Level_0_dynamic, optim_interval_score_Level_1_dynamic, optim_interval_score_Level_2_dynamic, optim_interval_score_Level_3_dynamic, optim_interval_score_Level_4_dynamic, optim_interval_score_Level_5_dynamic)
optim_interval_score_all_stats_dynamic = rbind(optim_interval_score_all_dynamic, colMeans(optim_interval_score_all_dynamic), apply(optim_interval_score_all_dynamic, 2, median))
colnames(optim_interval_score_all_stats_dynamic) = c("Level 0", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5")
rownames(optim_interval_score_all_stats_dynamic) = c(1:15,"Mean","Median")

