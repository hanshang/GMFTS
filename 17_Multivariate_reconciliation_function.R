#########################################################################################
# All-level bootstrapped base forecasts (B = 1000) using multivariate forecasting method
#########################################################################################

library(demography)
library(ftsa)

# Define a function to compute bootstrapped base forecasts

## fh: forecast horizon

PI_prefecture_mfts <- function(fh)
{
    prefecture_fh_PI_mfts = array(0, dim = c(101, (16-fh), 168, 1000))
    
    if(fh == 15)
    {
      for(ij in 1:1000)
        {   
          prefecture_fh_PI_mfts[,,,ij] = exp(cbind(Japan_total_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                            Japan_female_fh_PI_mfts[[fh]]$PI_boot[,ij,], 
                                            Japan_male_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                            
                                            R1_total_fh_PI_mfts[[fh]][,ij], R2_total_fh_PI_mfts[[fh]][,ij],
                                            R3_total_fh_PI_mfts[[fh]][,ij], R4_total_fh_PI_mfts[[fh]][,ij],
                                            R5_total_fh_PI_mfts[[fh]][,ij], R6_total_fh_PI_mfts[[fh]][,ij],
                                            R7_total_fh_PI_mfts[[fh]][,ij], R8_total_fh_PI_mfts[[fh]][,ij],
                                            
                                            R1_female_fh_PI_mfts[[fh]][,ij,], R2_female_fh_PI_mfts[[fh]][,ij,],
                                            R3_female_fh_PI_mfts[[fh]][,ij,], R4_female_fh_PI_mfts[[fh]][,ij,],
                                            R5_female_fh_PI_mfts[[fh]][,ij,], R6_female_fh_PI_mfts[[fh]][,ij,],
                                            R7_female_fh_PI_mfts[[fh]][,ij,], R8_female_fh_PI_mfts[[fh]][,ij,],
                                            
                                            R1_male_fh_PI_mfts[[fh]][,ij,], R2_male_fh_PI_mfts[[fh]][,ij,],
                                            R3_male_fh_PI_mfts[[fh]][,ij,], R4_male_fh_PI_mfts[[fh]][,ij,],
                                            R5_male_fh_PI_mfts[[fh]][,ij,], R6_male_fh_PI_mfts[[fh]][,ij,],
                                            R7_male_fh_PI_mfts[[fh]][,ij,], R8_male_fh_PI_mfts[[fh]][,ij,],
          
                                            Hokkaido_total_fh_PI_mfts[[fh]][,ij],   Aomori_total_fh_PI_mfts[[fh]][,ij],
                                            Iwate_total_fh_PI_mfts[[fh]][,ij],      Miyagi_total_fh_PI_mfts[[fh]][,ij],
                                            Akita_total_fh_PI_mfts[[fh]][,ij],      Yamagata_total_fh_PI_mfts[[fh]][,ij],
                                            Fukushima_total_fh_PI_mfts[[fh]][,ij],  Ibaraki_total_fh_PI_mfts[[fh]][,ij],
                                            Tochigi_total_fh_PI_mfts[[fh]][,ij],    Gunma_total_fh_PI_mfts[[fh]][,ij],
                                            Saitama_total_fh_PI_mfts[[fh]][,ij],    Chiba_total_fh_PI_mfts[[fh]][,ij],
                                            Tokyo_total_fh_PI_mfts[[fh]][,ij],      Kanagawa_total_fh_PI_mfts[[fh]][,ij],
                                            Niigata_total_fh_PI_mfts[[fh]][,ij],    Toyama_total_fh_PI_mfts[[fh]][,ij],
                                            Ishikawa_total_fh_PI_mfts[[fh]][,ij],   Fukui_total_fh_PI_mfts[[fh]][,ij],
                                            Yamanashi_total_fh_PI_mfts[[fh]][,ij],  Nagano_total_fh_PI_mfts[[fh]][,ij],
                                            Gifu_total_fh_PI_mfts[[fh]][,ij],       Shizuoka_total_fh_PI_mfts[[fh]][,ij],
                                            Aichi_total_fh_PI_mfts[[fh]][,ij],      Mie_total_fh_PI_mfts[[fh]][,ij],
                                            Shiga_total_fh_PI_mfts[[fh]][,ij],      Kyoto_total_fh_PI_mfts[[fh]][,ij],
                                            Osaka_total_fh_PI_mfts[[fh]][,ij],      Hyogo_total_fh_PI_mfts[[fh]][,ij],
                                            Nara_total_fh_PI_mfts[[fh]][,ij],       Wakayama_total_fh_PI_mfts[[fh]][,ij],
                                            Tottori_total_fh_PI_mfts[[fh]][,ij],    Shimane_total_fh_PI_mfts[[fh]][,ij],
                                            Okayama_total_fh_PI_mfts[[fh]][,ij],    Hiroshima_total_fh_PI_mfts[[fh]][,ij],
                                            Yamaguchi_total_fh_PI_mfts[[fh]][,ij],  Tokushima_total_fh_PI_mfts[[fh]][,ij],
                                            Kagawa_total_fh_PI_mfts[[fh]][,ij],     Ehime_total_fh_PI_mfts[[fh]][,ij],
                                            Kochi_total_fh_PI_mfts[[fh]][,ij],      Fukuoka_total_fh_PI_mfts[[fh]][,ij],
                                            Saga_total_fh_PI_mfts[[fh]][,ij],       Nagasaki_total_fh_PI_mfts[[fh]][,ij],
                                            Kumamoto_total_fh_PI_mfts[[fh]][,ij],   Oita_total_fh_PI_mfts[[fh]][,ij],
                                            Miyazaki_total_fh_PI_mfts[[fh]][,ij],   Kagoshima_total_fh_PI_mfts[[fh]][,ij],
                                            Okinawa_total_fh_PI_mfts[[fh]][,ij],
                                            
                                            Hokkaido_female_fh_PI_mfts[[fh]][,ij,],  Hokkaido_male_fh_PI_mfts[[fh]][,ij,],
                                            Aomori_female_fh_PI_mfts[[fh]][,ij,],    Aomori_male_fh_PI_mfts[[fh]][,ij,],
                                            Iwate_female_fh_PI_mfts[[fh]][,ij,],     Iwate_male_fh_PI_mfts[[fh]][,ij,],
                                            Miyagi_female_fh_PI_mfts[[fh]][,ij,],    Miyagi_male_fh_PI_mfts[[fh]][,ij,],
                                            Akita_female_fh_PI_mfts[[fh]][,ij,],     Akita_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamagata_female_fh_PI_mfts[[fh]][,ij,],  Yamagata_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukushima_female_fh_PI_mfts[[fh]][,ij,], Fukushima_male_fh_PI_mfts[[fh]][,ij,],
                                            Ibaraki_female_fh_PI_mfts[[fh]][,ij,],   Ibaraki_male_fh_PI_mfts[[fh]][,ij,],
                                            Tochigi_female_fh_PI_mfts[[fh]][,ij,],   Tochigi_male_fh_PI_mfts[[fh]][,ij,],
                                            Gunma_female_fh_PI_mfts[[fh]][,ij,],     Gunma_male_fh_PI_mfts[[fh]][,ij,],
                                            Saitama_female_fh_PI_mfts[[fh]][,ij,],   Saitama_male_fh_PI_mfts[[fh]][,ij,],
                                            Chiba_female_fh_PI_mfts[[fh]][,ij,],     Chiba_male_fh_PI_mfts[[fh]][,ij,],
                                            Tokyo_female_fh_PI_mfts[[fh]][,ij,],     Tokyo_male_fh_PI_mfts[[fh]][,ij,],
                                            Kanagawa_female_fh_PI_mfts[[fh]][,ij,],  Kanagawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Niigata_female_fh_PI_mfts[[fh]][,ij,],   Niigata_male_fh_PI_mfts[[fh]][,ij,],
                                            Toyama_female_fh_PI_mfts[[fh]][,ij,],    Toyama_male_fh_PI_mfts[[fh]][,ij,],
                                            Ishikawa_female_fh_PI_mfts[[fh]][,ij,],  Ishikawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukui_female_fh_PI_mfts[[fh]][,ij,],     Fukui_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamanashi_female_fh_PI_mfts[[fh]][,ij,], Yamanashi_male_fh_PI_mfts[[fh]][,ij,],
                                            Nagano_female_fh_PI_mfts[[fh]][,ij,],    Nagano_male_fh_PI_mfts[[fh]][,ij,],
                                            Gifu_female_fh_PI_mfts[[fh]][,ij,],      Gifu_male_fh_PI_mfts[[fh]][,ij,],
                                            Shizuoka_female_fh_PI_mfts[[fh]][,ij,],  Shizuoka_male_fh_PI_mfts[[fh]][,ij,],
                                            Aichi_female_fh_PI_mfts[[fh]][,ij,],     Aichi_male_fh_PI_mfts[[fh]][,ij,],
                                            Mie_female_fh_PI_mfts[[fh]][,ij,],       Mie_male_fh_PI_mfts[[fh]][,ij,],
                                            Shiga_female_fh_PI_mfts[[fh]][,ij,],     Shiga_male_fh_PI_mfts[[fh]][,ij,],
                                            Kyoto_female_fh_PI_mfts[[fh]][,ij,],     Kyoto_male_fh_PI_mfts[[fh]][,ij,],
                                            Osaka_female_fh_PI_mfts[[fh]][,ij,],     Osaka_male_fh_PI_mfts[[fh]][,ij,],
                                            Hyogo_female_fh_PI_mfts[[fh]][,ij,],     Hyogo_male_fh_PI_mfts[[fh]][,ij,],
                                            Nara_female_fh_PI_mfts[[fh]][,ij,],      Nara_male_fh_PI_mfts[[fh]][,ij,],
                                            Wakayama_female_fh_PI_mfts[[fh]][,ij,],  Wakayama_male_fh_PI_mfts[[fh]][,ij,],
                                            Tottori_female_fh_PI_mfts[[fh]][,ij,],   Tottori_male_fh_PI_mfts[[fh]][,ij,],
                                            Shimane_female_fh_PI_mfts[[fh]][,ij,],   Shimane_male_fh_PI_mfts[[fh]][,ij,],
                                            Okayama_female_fh_PI_mfts[[fh]][,ij,],   Okayama_male_fh_PI_mfts[[fh]][,ij,],
                                            Hiroshima_female_fh_PI_mfts[[fh]][,ij,], Hiroshima_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamaguchi_female_fh_PI_mfts[[fh]][,ij,], Yamaguchi_male_fh_PI_mfts[[fh]][,ij,],
                                            Tokushima_female_fh_PI_mfts[[fh]][,ij,], Tokushima_male_fh_PI_mfts[[fh]][,ij,],
                                            Kagawa_female_fh_PI_mfts[[fh]][,ij,],    Kagawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Ehime_female_fh_PI_mfts[[fh]][,ij,],     Ehime_male_fh_PI_mfts[[fh]][,ij,],
                                            Kochi_female_fh_PI_mfts[[fh]][,ij,],     Kochi_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukuoka_female_fh_PI_mfts[[fh]][,ij,],   Fukuoka_male_fh_PI_mfts[[fh]][,ij,],
                                            Saga_female_fh_PI_mfts[[fh]][,ij,],      Saga_male_fh_PI_mfts[[fh]][,ij,],
                                            Nagasaki_female_fh_PI_mfts[[fh]][,ij,],  Nagasaki_male_fh_PI_mfts[[fh]][,ij,],
                                            Kumamoto_female_fh_PI_mfts[[fh]][,ij,],  Kumamoto_male_fh_PI_mfts[[fh]][,ij,],
                                            Oita_female_fh_PI_mfts[[fh]][,ij,],      Oita_male_fh_PI_mfts[[fh]][,ij,],
                                            Miyazaki_female_fh_PI_mfts[[fh]][,ij,],  Miyazaki_male_fh_PI_mfts[[fh]][,ij,],
                                            Kagoshima_female_fh_PI_mfts[[fh]][,ij,], Kagoshima_male_fh_PI_mfts[[fh]][,ij,],
                                            Okinawa_female_fh_PI_mfts[[fh]][,ij,],   Okinawa_male_fh_PI_mfts[[fh]][,ij,]))
          }
    } else {
        for(ij in 1:1000)
        {   
          prefecture_fh_PI_mfts[,,,ij] = exp(cbind(Japan_total_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                            Japan_female_fh_PI_mfts[[fh]]$PI_boot[,ij,], 
                                            Japan_male_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                            
                                            R1_total_fh_PI_mfts[[fh]][,ij,], R2_total_fh_PI_mfts[[fh]][,ij,],
                                            R3_total_fh_PI_mfts[[fh]][,ij,], R4_total_fh_PI_mfts[[fh]][,ij,],
                                            R5_total_fh_PI_mfts[[fh]][,ij,], R6_total_fh_PI_mfts[[fh]][,ij,],
                                            R7_total_fh_PI_mfts[[fh]][,ij,], R8_total_fh_PI_mfts[[fh]][,ij,],
                                            
                                            R1_female_fh_PI_mfts[[fh]][,ij,], R2_female_fh_PI_mfts[[fh]][,ij,],
                                            R3_female_fh_PI_mfts[[fh]][,ij,], R4_female_fh_PI_mfts[[fh]][,ij,],
                                            R5_female_fh_PI_mfts[[fh]][,ij,], R6_female_fh_PI_mfts[[fh]][,ij,],
                                            R7_female_fh_PI_mfts[[fh]][,ij,], R8_female_fh_PI_mfts[[fh]][,ij,],
                                            
                                            R1_male_fh_PI_mfts[[fh]][,ij,], R2_male_fh_PI_mfts[[fh]][,ij,],
                                            R3_male_fh_PI_mfts[[fh]][,ij,], R4_male_fh_PI_mfts[[fh]][,ij,],
                                            R5_male_fh_PI_mfts[[fh]][,ij,], R6_male_fh_PI_mfts[[fh]][,ij,],
                                            R7_male_fh_PI_mfts[[fh]][,ij,], R8_male_fh_PI_mfts[[fh]][,ij,],
          
                                            Hokkaido_total_fh_PI_mfts[[fh]][,ij,],   Aomori_total_fh_PI_mfts[[fh]][,ij,],
                                            Iwate_total_fh_PI_mfts[[fh]][,ij,],      Miyagi_total_fh_PI_mfts[[fh]][,ij,],
                                            Akita_total_fh_PI_mfts[[fh]][,ij,],      Yamagata_total_fh_PI_mfts[[fh]][,ij,],
                                            Fukushima_total_fh_PI_mfts[[fh]][,ij,],  Ibaraki_total_fh_PI_mfts[[fh]][,ij,],
                                            Tochigi_total_fh_PI_mfts[[fh]][,ij,],    Gunma_total_fh_PI_mfts[[fh]][,ij,],
                                            Saitama_total_fh_PI_mfts[[fh]][,ij,],    Chiba_total_fh_PI_mfts[[fh]][,ij,],
                                            Tokyo_total_fh_PI_mfts[[fh]][,ij,],      Kanagawa_total_fh_PI_mfts[[fh]][,ij,],
                                            Niigata_total_fh_PI_mfts[[fh]][,ij,],    Toyama_total_fh_PI_mfts[[fh]][,ij,],
                                            Ishikawa_total_fh_PI_mfts[[fh]][,ij,],   Fukui_total_fh_PI_mfts[[fh]][,ij,],
                                            Yamanashi_total_fh_PI_mfts[[fh]][,ij,],  Nagano_total_fh_PI_mfts[[fh]][,ij,],
                                            Gifu_total_fh_PI_mfts[[fh]][,ij,],       Shizuoka_total_fh_PI_mfts[[fh]][,ij,],
                                            Aichi_total_fh_PI_mfts[[fh]][,ij,],      Mie_total_fh_PI_mfts[[fh]][,ij,],
                                            Shiga_total_fh_PI_mfts[[fh]][,ij,],      Kyoto_total_fh_PI_mfts[[fh]][,ij,],
                                            Osaka_total_fh_PI_mfts[[fh]][,ij,],      Hyogo_total_fh_PI_mfts[[fh]][,ij,],
                                            Nara_total_fh_PI_mfts[[fh]][,ij,],       Wakayama_total_fh_PI_mfts[[fh]][,ij,],
                                            Tottori_total_fh_PI_mfts[[fh]][,ij,],    Shimane_total_fh_PI_mfts[[fh]][,ij,],
                                            Okayama_total_fh_PI_mfts[[fh]][,ij,],    Hiroshima_total_fh_PI_mfts[[fh]][,ij,],
                                            Yamaguchi_total_fh_PI_mfts[[fh]][,ij,],  Tokushima_total_fh_PI_mfts[[fh]][,ij,],
                                            Kagawa_total_fh_PI_mfts[[fh]][,ij,],     Ehime_total_fh_PI_mfts[[fh]][,ij,],
                                            Kochi_total_fh_PI_mfts[[fh]][,ij,],      Fukuoka_total_fh_PI_mfts[[fh]][,ij,],
                                            Saga_total_fh_PI_mfts[[fh]][,ij,],       Nagasaki_total_fh_PI_mfts[[fh]][,ij,],
                                            Kumamoto_total_fh_PI_mfts[[fh]][,ij,],   Oita_total_fh_PI_mfts[[fh]][,ij,],
                                            Miyazaki_total_fh_PI_mfts[[fh]][,ij,],   Kagoshima_total_fh_PI_mfts[[fh]][,ij,],
                                            Okinawa_total_fh_PI_mfts[[fh]][,ij,],
                                            
                                            Hokkaido_female_fh_PI_mfts[[fh]][,ij,],  Hokkaido_male_fh_PI_mfts[[fh]][,ij,],
                                            Aomori_female_fh_PI_mfts[[fh]][,ij,],    Aomori_male_fh_PI_mfts[[fh]][,ij,],
                                            Iwate_female_fh_PI_mfts[[fh]][,ij,],     Iwate_male_fh_PI_mfts[[fh]][,ij,],
                                            Miyagi_female_fh_PI_mfts[[fh]][,ij,],    Miyagi_male_fh_PI_mfts[[fh]][,ij,],
                                            Akita_female_fh_PI_mfts[[fh]][,ij,],     Akita_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamagata_female_fh_PI_mfts[[fh]][,ij,],  Yamagata_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukushima_female_fh_PI_mfts[[fh]][,ij,], Fukushima_male_fh_PI_mfts[[fh]][,ij,],
                                            Ibaraki_female_fh_PI_mfts[[fh]][,ij,],   Ibaraki_male_fh_PI_mfts[[fh]][,ij,],
                                            Tochigi_female_fh_PI_mfts[[fh]][,ij,],   Tochigi_male_fh_PI_mfts[[fh]][,ij,],
                                            Gunma_female_fh_PI_mfts[[fh]][,ij,],     Gunma_male_fh_PI_mfts[[fh]][,ij,],
                                            Saitama_female_fh_PI_mfts[[fh]][,ij,],   Saitama_male_fh_PI_mfts[[fh]][,ij,],
                                            Chiba_female_fh_PI_mfts[[fh]][,ij,],     Chiba_male_fh_PI_mfts[[fh]][,ij,],
                                            Tokyo_female_fh_PI_mfts[[fh]][,ij,],     Tokyo_male_fh_PI_mfts[[fh]][,ij,],
                                            Kanagawa_female_fh_PI_mfts[[fh]][,ij,],  Kanagawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Niigata_female_fh_PI_mfts[[fh]][,ij,],   Niigata_male_fh_PI_mfts[[fh]][,ij,],
                                            Toyama_female_fh_PI_mfts[[fh]][,ij,],    Toyama_male_fh_PI_mfts[[fh]][,ij,],
                                            Ishikawa_female_fh_PI_mfts[[fh]][,ij,],  Ishikawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukui_female_fh_PI_mfts[[fh]][,ij,],     Fukui_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamanashi_female_fh_PI_mfts[[fh]][,ij,], Yamanashi_male_fh_PI_mfts[[fh]][,ij,],
                                            Nagano_female_fh_PI_mfts[[fh]][,ij,],    Nagano_male_fh_PI_mfts[[fh]][,ij,],
                                            Gifu_female_fh_PI_mfts[[fh]][,ij,],      Gifu_male_fh_PI_mfts[[fh]][,ij,],
                                            Shizuoka_female_fh_PI_mfts[[fh]][,ij,],  Shizuoka_male_fh_PI_mfts[[fh]][,ij,],
                                            Aichi_female_fh_PI_mfts[[fh]][,ij,],     Aichi_male_fh_PI_mfts[[fh]][,ij,],
                                            Mie_female_fh_PI_mfts[[fh]][,ij,],       Mie_male_fh_PI_mfts[[fh]][,ij,],
                                            Shiga_female_fh_PI_mfts[[fh]][,ij,],     Shiga_male_fh_PI_mfts[[fh]][,ij,],
                                            Kyoto_female_fh_PI_mfts[[fh]][,ij,],     Kyoto_male_fh_PI_mfts[[fh]][,ij,],
                                            Osaka_female_fh_PI_mfts[[fh]][,ij,],     Osaka_male_fh_PI_mfts[[fh]][,ij,],
                                            Hyogo_female_fh_PI_mfts[[fh]][,ij,],     Hyogo_male_fh_PI_mfts[[fh]][,ij,],
                                            Nara_female_fh_PI_mfts[[fh]][,ij,],      Nara_male_fh_PI_mfts[[fh]][,ij,],
                                            Wakayama_female_fh_PI_mfts[[fh]][,ij,],  Wakayama_male_fh_PI_mfts[[fh]][,ij,],
                                            Tottori_female_fh_PI_mfts[[fh]][,ij,],   Tottori_male_fh_PI_mfts[[fh]][,ij,],
                                            Shimane_female_fh_PI_mfts[[fh]][,ij,],   Shimane_male_fh_PI_mfts[[fh]][,ij,],
                                            Okayama_female_fh_PI_mfts[[fh]][,ij,],   Okayama_male_fh_PI_mfts[[fh]][,ij,],
                                            Hiroshima_female_fh_PI_mfts[[fh]][,ij,], Hiroshima_male_fh_PI_mfts[[fh]][,ij,],
                                            Yamaguchi_female_fh_PI_mfts[[fh]][,ij,], Yamaguchi_male_fh_PI_mfts[[fh]][,ij,],
                                            Tokushima_female_fh_PI_mfts[[fh]][,ij,], Tokushima_male_fh_PI_mfts[[fh]][,ij,],
                                            Kagawa_female_fh_PI_mfts[[fh]][,ij,],    Kagawa_male_fh_PI_mfts[[fh]][,ij,],
                                            Ehime_female_fh_PI_mfts[[fh]][,ij,],     Ehime_male_fh_PI_mfts[[fh]][,ij,],
                                            Kochi_female_fh_PI_mfts[[fh]][,ij,],     Kochi_male_fh_PI_mfts[[fh]][,ij,],
                                            Fukuoka_female_fh_PI_mfts[[fh]][,ij,],   Fukuoka_male_fh_PI_mfts[[fh]][,ij,],
                                            Saga_female_fh_PI_mfts[[fh]][,ij,],      Saga_male_fh_PI_mfts[[fh]][,ij,],
                                            Nagasaki_female_fh_PI_mfts[[fh]][,ij,],  Nagasaki_male_fh_PI_mfts[[fh]][,ij,],
                                            Kumamoto_female_fh_PI_mfts[[fh]][,ij,],  Kumamoto_male_fh_PI_mfts[[fh]][,ij,],
                                            Oita_female_fh_PI_mfts[[fh]][,ij,],      Oita_male_fh_PI_mfts[[fh]][,ij,],
                                            Miyazaki_female_fh_PI_mfts[[fh]][,ij,],  Miyazaki_male_fh_PI_mfts[[fh]][,ij,],
                                            Kagoshima_female_fh_PI_mfts[[fh]][,ij,], Kagoshima_male_fh_PI_mfts[[fh]][,ij,],
                                            Okinawa_female_fh_PI_mfts[[fh]][,ij,],   Okinawa_male_fh_PI_mfts[[fh]][,ij,]))
          }

      }

    return(prefecture_fh_PI_mfts)
}

###############################################################
# one- to 15-step-ahead bootstrapped base forecasts (B = 1000)
###############################################################

### Compute base forecasts via parallel (not recommended for PC with small memory)

cl <- makeCluster(4) 
registerDoParallel(cl)

PI_prefecture_h1to15_mfts_1to2 = foreach(ik = 1:2, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_1to2, file = "PI_prefecture_h1to15_mfts_1to2.RData")
rm(PI_prefecture_h1to15_mfts_1to2)

PI_prefecture_h1to15_mfts_3to4 = foreach(ik = 3:4, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_3to4, file = "PI_prefecture_h1to15_mfts_3to4.RData")
rm(PI_prefecture_h1to15_mfts_3to4)

PI_prefecture_h1to15_mfts_5to6 = foreach(ik = 5:6, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_5to6, file = "PI_prefecture_h1to15_mfts_5to6.RData")
rm(PI_prefecture_h1to15_mfts_5to6)

PI_prefecture_h1to15_mfts_7to8 = foreach(ik = 7:8, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_7to8, file = "PI_prefecture_h1to15_mfts_7to8.RData")
rm(PI_prefecture_h1to15_mfts_7to8)

PI_prefecture_h1to15_mfts_9to12 = foreach(ik = 9:12, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_9to12, file = "PI_prefecture_h1to15_mfts_9to12.RData")
rm(PI_prefecture_h1to15_mfts_9to12)

PI_prefecture_h1to15_mfts_13to15 = foreach(ik = 13:15, .packages = c("demography", "ftsa")) %dopar% PI_prefecture_mfts(fh = ik)
save(PI_prefecture_h1to15_mfts_13to15, file = "PI_prefecture_h1to15_mfts_13to15.RData")
rm(PI_prefecture_h1to15_mfts_13to15)

stopCluster(cl)
rm(cl)

# Compute base forecasts step-by-step

PI_prefecture_h1to15_mfts_1 = PI_prefecture_mfts(fh = 1)
save(PI_prefecture_h1to15_mfts_1, file = "PI_prefecture_h1to15_mfts_1.RData")
rm(PI_prefecture_h1to15_mfts_1)
gc()

PI_prefecture_h1to15_mfts_2 = PI_prefecture_mfts(fh = 2)
save(PI_prefecture_h1to15_mfts_2, file = "PI_prefecture_h1to15_mfts_2.RData")
rm(PI_prefecture_h1to15_mfts_2)
gc()

PI_prefecture_h1to15_mfts_3 = PI_prefecture_mfts(fh = 3)
save(PI_prefecture_h1to15_mfts_3, file = "PI_prefecture_h1to15_mfts_3.RData")
rm(PI_prefecture_h1to15_mfts_3)
gc()

PI_prefecture_h1to15_mfts_4 = PI_prefecture_mfts(fh = 4)
save(PI_prefecture_h1to15_mfts_4, file = "PI_prefecture_h1to15_mfts_4.RData")
rm(PI_prefecture_h1to15_mfts_4)
gc()

PI_prefecture_h1to15_mfts_5 = PI_prefecture_mfts(fh = 5)
save(PI_prefecture_h1to15_mfts_5, file = "PI_prefecture_h1to15_mfts_5.RData")
rm(PI_prefecture_h1to15_mfts_5)
gc()

PI_prefecture_h1to15_mfts_6 = PI_prefecture_mfts(fh = 6)
save(PI_prefecture_h1to15_mfts_6, file = "PI_prefecture_h1to15_mfts_6.RData")
rm(PI_prefecture_h1to15_mfts_6)
gc()

PI_prefecture_h1to15_mfts_7 = PI_prefecture_mfts(fh = 7)
save(PI_prefecture_h1to15_mfts_7, file = "PI_prefecture_h1to15_mfts_7.RData")
rm(PI_prefecture_h1to15_mfts_7)
gc()

PI_prefecture_h1to15_mfts_8 = PI_prefecture_mfts(fh = 8)
save(PI_prefecture_h1to15_mfts_8, file = "PI_prefecture_h1to15_mfts_8.RData")
rm(PI_prefecture_h1to15_mfts_8)
gc()

PI_prefecture_h1to15_mfts_9 = PI_prefecture_mfts(fh = 9)
save(PI_prefecture_h1to15_mfts_9, file = "PI_prefecture_h1to15_mfts_9.RData")
rm(PI_prefecture_h1to15_mfts_9)
gc()

PI_prefecture_h1to15_mfts_10 = PI_prefecture_mfts(fh = 10)
save(PI_prefecture_h1to15_mfts_10, file = "PI_prefecture_h1to15_mfts_10.RData")
rm(PI_prefecture_h1to15_mfts_10)
gc()

PI_prefecture_h1to15_mfts_11 = PI_prefecture_mfts(fh = 11)
save(PI_prefecture_h1to15_mfts_11, file = "PI_prefecture_h1to15_mfts_11.RData")
rm(PI_prefecture_h1to15_mfts_11)
gc()

PI_prefecture_h1to15_mfts_12 = PI_prefecture_mfts(fh = 12)
save(PI_prefecture_h1to15_mfts_12, file = "PI_prefecture_h1to15_mfts_12.RData")
rm(PI_prefecture_h1to15_mfts_12)
gc()

PI_prefecture_h1to15_mfts_13 = PI_prefecture_mfts(fh = 13)
save(PI_prefecture_h1to15_mfts_13, file = "PI_prefecture_h1to15_mfts_13.RData")
rm(PI_prefecture_h1to15_mfts_13)
gc()

PI_prefecture_h1to15_mfts_14 = PI_prefecture_mfts(fh = 14)
save(PI_prefecture_h1to15_mfts_14, file = "PI_prefecture_h1to15_mfts_14.RData")
rm(PI_prefecture_h1to15_mfts_14)
gc()

PI_prefecture_h1to15_mfts_15 = PI_prefecture_mfts(fh = 15)
save(PI_prefecture_h1to15_mfts_15, file = "PI_prefecture_h1to15_mfts_15.RData")
rm(PI_prefecture_h1to15_mfts_15)
gc()


###########################################
# All-level bootstrapped grouped forecasts
###########################################

# Define a functino to reconcile pointwise interval forecasts

# kj: forecast horizon from h = 1 to 15
# age: age indexes from 1 to 101 (ages 0 to 100)

BU_optim_hier_PI_mfts <- function(kj, age, hier_method = c("BU", "comb_OLS", "mint"))
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
          load(paste("PI_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
          
          for(ij in 1:1000)
          {
            #hier_fore[,ik,ij] = hier %*% (PI_prefecture_h1to15_mfts[[kj]])[age,ik,75:168,ij]
            hier_fore[,ik,ij] = hier %*% (get(paste("PI_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,75:168,ij]
          }
            
          rm(list = paste("PI_prefecture_h1to15_mfts_", kj, sep = ""))
        }
        
        if(hier_method == "comb_OLS")
        {
          load(paste("PI_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
          
          for(ij in 1:1000)
          {
            #hier_fore[,ik,ij] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% (PI_prefecture_h1to15_mfts[[kj]])[age,ik,,ij]
            hier_fore[,ik,ij] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% (get(paste("PI_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,,ij]
          }
          
          rm(list = paste("PI_prefecture_h1to15_mfts_", kj, sep = ""))
        }
        
        if(hier_method == "mint")
        {
          load(paste("PI_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
          wh = wh_fun_mfts(kj = kj, age = age)
          for(ij in 1:1000)
          {
            
            hier_fore[,ik,ij] = hier %*% solve(t(hier) %*% solve(wh) %*% hier) %*% t(hier) %*% solve(wh) %*%  (get(paste("PI_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,,ij]
          }
          
          rm(list = paste("PI_prefecture_h1to15_mfts_", kj, sep = ""))
        }
        
    }
    return(hier_fore)
}

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "BU" (output: h1 to h15 BU reconciled bootstrap samples)

BU_fun_mfts <- function(kj)
{
    dum = array(0, dim = c(101, 168, (16-kj), 1000))
    for(ij in 1:101)
    {
        dum[ij,,,] = BU_optim_hier_PI_mfts(kj = kj, age = ij)
    }
    return(dum)
}

# Parallel calculation of reconciled interval predictions

library(doParallel)

cl <- makeCluster(4) 
registerDoParallel(cl)

BU_optim_hier_comb_mfts = foreach(kj = 1:15) %dopar% BU_fun_mfts(kj = kj)

stopCluster(cl)
rm(cl)

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "comb_OLS" (output: h1 to h15 OLS reconciled bootstrap samples) 

OLS_fun_mfts <- function(kj)  
{
    dum = array(0, dim = c(101, 168, (16-kj), 1000))
    for(ij in 1:101)
    {
      print(paste("age =", ij))
      dum[ij,,,] = BU_optim_hier_PI_mfts(kj = kj, age = ij, hier_method = "comb_OLS")
    }
    return(dum)    
}

# Parallel calculation of reconciled interval predictions

cl <- makeCluster(8) 
registerDoParallel(cl)

OLS_hier_comb_mfts = foreach(kj = 1:15) %dopar% OLS_fun_mfts(kj = kj)
save(OLS_hier_comb_mfts, file = "OLS_hier_comb_mfts.RData")

stopCluster(cl)
rm(cl)

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "mint" (output: h1 to h15 MinT reconciled bootstrap samples) 

mint_fun_mfts <- function(kj)  
{
  dum = array(0, dim = c(101, 168, (16-kj), 1000))
  for(ij in 1:101)
  {
    print(paste("age =", ij))
    dum[ij,,,] = BU_optim_hier_PI_mfts(kj = kj, age = ij, hier_method = "mint")
  }
  return(dum)    
}

# Parallel calculation of reconciled interval predictions

cl <- makeCluster(4) 
registerDoParallel(cl)

mint_hier_comb_mfts = foreach(kj = 1:15, .export = c(ls()[pop_ratio_index], ls()[state_residual_index], ls()[state_residual_index]), .packages = c("MASS")) %dopar% mint_fun_mfts(kj = kj)
save(mint_hier_comb_mfts, file = "mint_hier_comb_mfts.RData")

stopCluster(cl)
rm(cl)

##################################################
# Function used to calculate mean interval scores
##################################################

# Define a function to compute interval scores

interval_score_BU_optim_mfts <- function(PI_val, data_series, series, fh, index, alpha = 0.8)
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
    }
    else
    {
        boot_sample_v1 = boot_sample
    }
    if(length(boot_index_below) > 0)
    {
        boot_sample_v2 = replace(boot_sample_v1, boot_index_below, 0) 
    }
    else
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

