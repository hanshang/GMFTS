###################################################################################
# All-level bootstrapped base forecasts (B = 1000) for the new hierarchy structure
###################################################################################

library(demography)
library(ftsa)

# Define a function for nonparametric bootstrap of gender-specific series

PI_gender_prefecture_mfts <- function(fh)
{
  prefecture_gender_fh_PI_mfts = array(0, dim = c(101, (16-fh), 168, 1000))
  
  if(fh == 15)
  {
    for(ij in 1:1000)
    {   
      prefecture_gender_fh_PI_mfts[,,,ij] = exp(cbind(Japan_total_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                               Japan_female_fh_PI_mfts[[fh]]$PI_boot[,ij,], 
                                               Japan_male_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                               
                                               R1_total_fh_PI_mfts[[fh]][,ij], R2_total_fh_PI_mfts[[fh]][,ij],
                                               R3_total_fh_PI_mfts[[fh]][,ij], R4_total_fh_PI_mfts[[fh]][,ij],
                                               R5_total_fh_PI_mfts[[fh]][,ij], R6_total_fh_PI_mfts[[fh]][,ij],
                                               R7_total_fh_PI_mfts[[fh]][,ij], R8_total_fh_PI_mfts[[fh]][,ij],
                                               
                                               R1_gender_female_fh_PI_mfts[[fh]][,ij], R2_gender_female_fh_PI_mfts[[fh]][,ij],
                                               R3_gender_female_fh_PI_mfts[[fh]][,ij], R4_gender_female_fh_PI_mfts[[fh]][,ij],
                                               R5_gender_female_fh_PI_mfts[[fh]][,ij], R6_gender_female_fh_PI_mfts[[fh]][,ij],
                                               R7_gender_female_fh_PI_mfts[[fh]][,ij], R8_gender_female_fh_PI_mfts[[fh]][,ij],
                                               
                                               R1_gender_male_fh_PI_mfts[[fh]][,ij], R2_gender_male_fh_PI_mfts[[fh]][,ij],
                                               R3_gender_male_fh_PI_mfts[[fh]][,ij], R4_gender_male_fh_PI_mfts[[fh]][,ij],
                                               R5_gender_male_fh_PI_mfts[[fh]][,ij], R6_gender_male_fh_PI_mfts[[fh]][,ij],
                                               R7_gender_male_fh_PI_mfts[[fh]][,ij], R8_gender_male_fh_PI_mfts[[fh]][,ij],
                                               
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
                                               
                                               Hokkaido_gender_female_fh_PI_mfts[[fh]][,ij],  Hokkaido_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Aomori_gender_female_fh_PI_mfts[[fh]][,ij],    Aomori_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Iwate_gender_female_fh_PI_mfts[[fh]][,ij],     Iwate_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Miyagi_gender_female_fh_PI_mfts[[fh]][,ij],    Miyagi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Akita_gender_female_fh_PI_mfts[[fh]][,ij],     Akita_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Yamagata_gender_female_fh_PI_mfts[[fh]][,ij],  Yamagata_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Fukushima_gender_female_fh_PI_mfts[[fh]][,ij], Fukushima_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Ibaraki_gender_female_fh_PI_mfts[[fh]][,ij],   Ibaraki_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Tochigi_gender_female_fh_PI_mfts[[fh]][,ij],   Tochigi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Gunma_gender_female_fh_PI_mfts[[fh]][,ij],     Gunma_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Saitama_gender_female_fh_PI_mfts[[fh]][,ij],   Saitama_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Chiba_gender_female_fh_PI_mfts[[fh]][,ij],     Chiba_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Tokyo_gender_female_fh_PI_mfts[[fh]][,ij],     Tokyo_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kanagawa_gender_female_fh_PI_mfts[[fh]][,ij],  Kanagawa_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Niigata_gender_female_fh_PI_mfts[[fh]][,ij],   Niigata_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Toyama_gender_female_fh_PI_mfts[[fh]][,ij],    Toyama_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Ishikawa_gender_female_fh_PI_mfts[[fh]][,ij],  Ishikawa_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Fukui_gender_female_fh_PI_mfts[[fh]][,ij],     Fukui_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Yamanashi_gender_female_fh_PI_mfts[[fh]][,ij], Yamanashi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Nagano_gender_female_fh_PI_mfts[[fh]][,ij],    Nagano_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Gifu_gender_female_fh_PI_mfts[[fh]][,ij],      Gifu_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Shizuoka_gender_female_fh_PI_mfts[[fh]][,ij],  Shizuoka_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Aichi_gender_female_fh_PI_mfts[[fh]][,ij],     Aichi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Mie_gender_female_fh_PI_mfts[[fh]][,ij],       Mie_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Shiga_gender_female_fh_PI_mfts[[fh]][,ij],     Shiga_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kyoto_gender_female_fh_PI_mfts[[fh]][,ij],     Kyoto_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Osaka_gender_female_fh_PI_mfts[[fh]][,ij],     Osaka_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Hyogo_gender_female_fh_PI_mfts[[fh]][,ij],     Hyogo_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Nara_gender_female_fh_PI_mfts[[fh]][,ij],      Nara_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Wakayama_gender_female_fh_PI_mfts[[fh]][,ij],  Wakayama_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Tottori_gender_female_fh_PI_mfts[[fh]][,ij],   Tottori_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Shimane_gender_female_fh_PI_mfts[[fh]][,ij],   Shimane_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Okayama_gender_female_fh_PI_mfts[[fh]][,ij],   Okayama_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Hiroshima_gender_female_fh_PI_mfts[[fh]][,ij], Hiroshima_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Yamaguchi_gender_female_fh_PI_mfts[[fh]][,ij], Yamaguchi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Tokushima_gender_female_fh_PI_mfts[[fh]][,ij], Tokushima_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kagawa_gender_female_fh_PI_mfts[[fh]][,ij],    Kagawa_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Ehime_gender_female_fh_PI_mfts[[fh]][,ij],     Ehime_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kochi_gender_female_fh_PI_mfts[[fh]][,ij],     Kochi_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Fukuoka_gender_female_fh_PI_mfts[[fh]][,ij],   Fukuoka_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Saga_gender_female_fh_PI_mfts[[fh]][,ij],      Saga_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Nagasaki_gender_female_fh_PI_mfts[[fh]][,ij],  Nagasaki_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kumamoto_gender_female_fh_PI_mfts[[fh]][,ij],  Kumamoto_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Oita_gender_female_fh_PI_mfts[[fh]][,ij],      Oita_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Miyazaki_gender_female_fh_PI_mfts[[fh]][,ij],  Miyazaki_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Kagoshima_gender_female_fh_PI_mfts[[fh]][,ij], Kagoshima_gender_male_fh_PI_mfts[[fh]][,ij],
                                               Okinawa_gender_female_fh_PI_mfts[[fh]][,ij],   Okinawa_gender_male_fh_PI_mfts[[fh]][,ij]))
    }
  } else {
    for(ij in 1:1000)
    {   
      prefecture_gender_fh_PI_mfts[,,,ij] = exp(cbind(Japan_total_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                               Japan_female_fh_PI_mfts[[fh]]$PI_boot[,ij,], 
                                               Japan_male_fh_PI_mfts[[fh]]$PI_boot[,ij,],
                                               
                                               R1_total_fh_PI_mfts[[fh]][,ij,], R2_total_fh_PI_mfts[[fh]][,ij,],
                                               R3_total_fh_PI_mfts[[fh]][,ij,], R4_total_fh_PI_mfts[[fh]][,ij,],
                                               R5_total_fh_PI_mfts[[fh]][,ij,], R6_total_fh_PI_mfts[[fh]][,ij,],
                                               R7_total_fh_PI_mfts[[fh]][,ij,], R8_total_fh_PI_mfts[[fh]][,ij,],
                                               
                                               R1_gender_female_fh_PI_mfts[[fh]][,ij,], R2_gender_female_fh_PI_mfts[[fh]][,ij,],
                                               R3_gender_female_fh_PI_mfts[[fh]][,ij,], R4_gender_female_fh_PI_mfts[[fh]][,ij,],
                                               R5_gender_female_fh_PI_mfts[[fh]][,ij,], R6_gender_female_fh_PI_mfts[[fh]][,ij,],
                                               R7_gender_female_fh_PI_mfts[[fh]][,ij,], R8_gender_female_fh_PI_mfts[[fh]][,ij,],
                                               
                                               R1_gender_male_fh_PI_mfts[[fh]][,ij,], R2_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               R3_gender_male_fh_PI_mfts[[fh]][,ij,], R4_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               R5_gender_male_fh_PI_mfts[[fh]][,ij,], R6_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               R7_gender_male_fh_PI_mfts[[fh]][,ij,], R8_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               
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
                                               
                                               Hokkaido_gender_female_fh_PI_mfts[[fh]][,ij,],  Hokkaido_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Aomori_gender_female_fh_PI_mfts[[fh]][,ij,],    Aomori_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Iwate_gender_female_fh_PI_mfts[[fh]][,ij,],     Iwate_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Miyagi_gender_female_fh_PI_mfts[[fh]][,ij,],    Miyagi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Akita_gender_female_fh_PI_mfts[[fh]][,ij,],     Akita_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Yamagata_gender_female_fh_PI_mfts[[fh]][,ij,],  Yamagata_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Fukushima_gender_female_fh_PI_mfts[[fh]][,ij,], Fukushima_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Ibaraki_gender_female_fh_PI_mfts[[fh]][,ij,],   Ibaraki_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Tochigi_gender_female_fh_PI_mfts[[fh]][,ij,],   Tochigi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Gunma_gender_female_fh_PI_mfts[[fh]][,ij,],     Gunma_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Saitama_gender_female_fh_PI_mfts[[fh]][,ij,],   Saitama_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Chiba_gender_female_fh_PI_mfts[[fh]][,ij,],     Chiba_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Tokyo_gender_female_fh_PI_mfts[[fh]][,ij,],     Tokyo_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kanagawa_gender_female_fh_PI_mfts[[fh]][,ij,],  Kanagawa_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Niigata_gender_female_fh_PI_mfts[[fh]][,ij,],   Niigata_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Toyama_gender_female_fh_PI_mfts[[fh]][,ij,],    Toyama_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Ishikawa_gender_female_fh_PI_mfts[[fh]][,ij,],  Ishikawa_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Fukui_gender_female_fh_PI_mfts[[fh]][,ij,],     Fukui_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Yamanashi_gender_female_fh_PI_mfts[[fh]][,ij,], Yamanashi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Nagano_gender_female_fh_PI_mfts[[fh]][,ij,],    Nagano_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Gifu_gender_female_fh_PI_mfts[[fh]][,ij,],      Gifu_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Shizuoka_gender_female_fh_PI_mfts[[fh]][,ij,],  Shizuoka_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Aichi_gender_female_fh_PI_mfts[[fh]][,ij,],     Aichi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Mie_gender_female_fh_PI_mfts[[fh]][,ij,],       Mie_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Shiga_gender_female_fh_PI_mfts[[fh]][,ij,],     Shiga_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kyoto_gender_female_fh_PI_mfts[[fh]][,ij,],     Kyoto_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Osaka_gender_female_fh_PI_mfts[[fh]][,ij,],     Osaka_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Hyogo_gender_female_fh_PI_mfts[[fh]][,ij,],     Hyogo_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Nara_gender_female_fh_PI_mfts[[fh]][,ij,],      Nara_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Wakayama_gender_female_fh_PI_mfts[[fh]][,ij,],  Wakayama_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Tottori_gender_female_fh_PI_mfts[[fh]][,ij,],   Tottori_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Shimane_gender_female_fh_PI_mfts[[fh]][,ij,],   Shimane_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Okayama_gender_female_fh_PI_mfts[[fh]][,ij,],   Okayama_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Hiroshima_gender_female_fh_PI_mfts[[fh]][,ij,], Hiroshima_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Yamaguchi_gender_female_fh_PI_mfts[[fh]][,ij,], Yamaguchi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Tokushima_gender_female_fh_PI_mfts[[fh]][,ij,], Tokushima_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kagawa_gender_female_fh_PI_mfts[[fh]][,ij,],    Kagawa_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Ehime_gender_female_fh_PI_mfts[[fh]][,ij,],     Ehime_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kochi_gender_female_fh_PI_mfts[[fh]][,ij,],     Kochi_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Fukuoka_gender_female_fh_PI_mfts[[fh]][,ij,],   Fukuoka_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Saga_gender_female_fh_PI_mfts[[fh]][,ij,],      Saga_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Nagasaki_gender_female_fh_PI_mfts[[fh]][,ij,],  Nagasaki_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kumamoto_gender_female_fh_PI_mfts[[fh]][,ij,],  Kumamoto_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Oita_gender_female_fh_PI_mfts[[fh]][,ij,],      Oita_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Miyazaki_gender_female_fh_PI_mfts[[fh]][,ij,],  Miyazaki_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Kagoshima_gender_female_fh_PI_mfts[[fh]][,ij,], Kagoshima_gender_male_fh_PI_mfts[[fh]][,ij,],
                                               Okinawa_gender_female_fh_PI_mfts[[fh]][,ij,],   Okinawa_gender_male_fh_PI_mfts[[fh]][,ij,]))
    }
    
  }
  
  return(prefecture_gender_fh_PI_mfts)
}


###############################################################
# one- to 15-step-ahead bootstrapped base forecasts (B = 1000)
###############################################################

# Compute pointwise prediction intervals step-by-step

PI_gender_prefecture_h1to15_mfts_1 = PI_gender_prefecture_mfts(fh = 1)
save(PI_gender_prefecture_h1to15_mfts_1, file = "PI_gender_prefecture_h1to15_mfts_1.RData"); rm(PI_gender_prefecture_h1to15_mfts_1)
PI_gender_prefecture_h1to15_mfts_2 = PI_gender_prefecture_mfts(fh = 2)
save(PI_gender_prefecture_h1to15_mfts_2, file = "PI_gender_prefecture_h1to15_mfts_2.RData"); rm(PI_gender_prefecture_h1to15_mfts_2)
PI_gender_prefecture_h1to15_mfts_3 = PI_gender_prefecture_mfts(fh = 3)
save(PI_gender_prefecture_h1to15_mfts_3, file = "PI_gender_prefecture_h1to15_mfts_3.RData"); rm(PI_gender_prefecture_h1to15_mfts_3)
PI_gender_prefecture_h1to15_mfts_4 = PI_gender_prefecture_mfts(fh = 4)
save(PI_gender_prefecture_h1to15_mfts_4, file = "PI_gender_prefecture_h1to15_mfts_4.RData"); rm(PI_gender_prefecture_h1to15_mfts_4)
PI_gender_prefecture_h1to15_mfts_5 = PI_gender_prefecture_mfts(fh = 5)
save(PI_gender_prefecture_h1to15_mfts_5, file = "PI_gender_prefecture_h1to15_mfts_5.RData"); rm(PI_gender_prefecture_h1to15_mfts_5)
PI_gender_prefecture_h1to15_mfts_6 = PI_gender_prefecture_mfts(fh = 6)
save(PI_gender_prefecture_h1to15_mfts_6, file = "PI_gender_prefecture_h1to15_mfts_6.RData"); rm(PI_gender_prefecture_h1to15_mfts_6)
PI_gender_prefecture_h1to15_mfts_7 = PI_gender_prefecture_mfts(fh = 7)
save(PI_gender_prefecture_h1to15_mfts_7, file = "PI_gender_prefecture_h1to15_mfts_7.RData"); rm(PI_gender_prefecture_h1to15_mfts_7)
PI_gender_prefecture_h1to15_mfts_8 = PI_gender_prefecture_mfts(fh = 8)
save(PI_gender_prefecture_h1to15_mfts_8, file = "PI_gender_prefecture_h1to15_mfts_8.RData"); rm(PI_gender_prefecture_h1to15_mfts_8)
PI_gender_prefecture_h1to15_mfts_9 = PI_gender_prefecture_mfts(fh = 9)
save(PI_gender_prefecture_h1to15_mfts_9, file = "PI_gender_prefecture_h1to15_mfts_9.RData"); rm(PI_gender_prefecture_h1to15_mfts_9)
PI_gender_prefecture_h1to15_mfts_10 = PI_gender_prefecture_mfts(fh = 10)
save(PI_gender_prefecture_h1to15_mfts_10, file = "PI_gender_prefecture_h1to15_mfts_10.RData"); rm(PI_gender_prefecture_h1to15_mfts_10)
PI_gender_prefecture_h1to15_mfts_11 = PI_gender_prefecture_mfts(fh = 11)
save(PI_gender_prefecture_h1to15_mfts_11, file = "PI_gender_prefecture_h1to15_mfts_11.RData"); rm(PI_gender_prefecture_h1to15_mfts_11)
PI_gender_prefecture_h1to15_mfts_12 = PI_gender_prefecture_mfts(fh = 12)
save(PI_gender_prefecture_h1to15_mfts_12, file = "PI_gender_prefecture_h1to15_mfts_12.RData"); rm(PI_gender_prefecture_h1to15_mfts_12)
PI_gender_prefecture_h1to15_mfts_13 = PI_gender_prefecture_mfts(fh = 13)
save(PI_gender_prefecture_h1to15_mfts_13, file = "PI_gender_prefecture_h1to15_mfts_13.RData"); rm(PI_gender_prefecture_h1to15_mfts_13)
PI_gender_prefecture_h1to15_mfts_14 = PI_gender_prefecture_mfts(fh = 14)
save(PI_gender_prefecture_h1to15_mfts_14, file = "PI_gender_prefecture_h1to15_mfts_14.RData"); rm(PI_gender_prefecture_h1to15_mfts_14)
PI_gender_prefecture_h1to15_mfts_15 = PI_gender_prefecture_mfts(fh = 15)
save(PI_gender_prefecture_h1to15_mfts_15, file = "PI_gender_prefecture_h1to15_mfts_15.RData"); rm(PI_gender_prefecture_h1to15_mfts_15)

for (i in 1:15)
{
  load(paste("PI_gender_prefecture_h1to15_mfts_",i,".RData", sep=""))
}

for (i in 1:15)
{
  rm(list = c(paste("PI_gender_prefecture_h1to15_mfts_",i, sep="")))
}

PI_gender_prefecture_h1to15_mfts = list()
PI_gender_prefecture_h1to15_mfts[[1]] = PI_gender_prefecture_h1to15_mfts_1
PI_gender_prefecture_h1to15_mfts[[2]] = PI_gender_prefecture_h1to15_mfts_2
PI_gender_prefecture_h1to15_mfts[[3]] = PI_gender_prefecture_h1to15_mfts_3
PI_gender_prefecture_h1to15_mfts[[4]] = PI_gender_prefecture_h1to15_mfts_4
PI_gender_prefecture_h1to15_mfts[[5]] = PI_gender_prefecture_h1to15_mfts_5
PI_gender_prefecture_h1to15_mfts[[6]] = PI_gender_prefecture_h1to15_mfts_6
PI_gender_prefecture_h1to15_mfts[[7]] = PI_gender_prefecture_h1to15_mfts_7
PI_gender_prefecture_h1to15_mfts[[8]] = PI_gender_prefecture_h1to15_mfts_8
PI_gender_prefecture_h1to15_mfts[[9]] = PI_gender_prefecture_h1to15_mfts_9
PI_gender_prefecture_h1to15_mfts[[10]] = PI_gender_prefecture_h1to15_mfts_10
PI_gender_prefecture_h1to15_mfts[[11]] = PI_gender_prefecture_h1to15_mfts_11
PI_gender_prefecture_h1to15_mfts[[12]] = PI_gender_prefecture_h1to15_mfts_12
PI_gender_prefecture_h1to15_mfts[[13]] = PI_gender_prefecture_h1to15_mfts_13
PI_gender_prefecture_h1to15_mfts[[14]] = PI_gender_prefecture_h1to15_mfts_14
PI_gender_prefecture_h1to15_mfts[[15]] = PI_gender_prefecture_h1to15_mfts_15

save(PI_gender_prefecture_h1to15_mfts, file = "PI_gender_prefecture_h1to15_mfts.RData")

###########################################
# All-level bootstrapped grouped forecasts
###########################################

# kj: forecast horizon from h = 1 to 15
# age: age indexes from 1 to 101 (ages 0 to 100)

BU_gender_optim_hier_PI_mfts <- function(kj, age, hier_method = c("BU", "comb_OLS", "mint"))
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
      load(paste("PI_gender_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
      
      for(ij in 1:1000)
      {
        #hier_fore[,ik,ij] = hier %*% (PI_prefecture_h1to15_mfts[[kj]])[age,ik,75:168,ij]
        hier_fore[,ik,ij] = hier %*% (get(paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,75:168,ij]
      }
      
      rm(list = paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = ""))
    }
    
    if(hier_method == "comb_OLS")
    {
      load(paste("PI_gender_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
      
      for(ij in 1:1000)
      {
        #hier_fore[,ik,ij] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% (PI_prefecture_h1to15_mfts[[kj]])[age,ik,,ij]
        hier_fore[,ik,ij] = hier %*% ginv(t(hier) %*% hier) %*% t(hier) %*% (get(paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,,ij]
      }
      
      rm(list = paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = ""))
    }
    
    if(hier_method == "mint")
    {
      load(paste("PI_gender_prefecture_h1to15_mfts_", kj, ".RData", sep = ""))
      wh = wh_fun_mfts(kj = kj, age = age)
      for(ij in 1:1000)
      {
        
        hier_fore[,ik,ij] = hier %*% solve(t(hier) %*% solve(wh) %*% hier) %*% t(hier) %*% solve(wh) %*%  (get(paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = "")))[age,ik,,ij]
      }
      
      rm(list = paste("PI_gender_prefecture_h1to15_mfts_", kj, sep = ""))
    }
    
  }
  return(hier_fore)
}

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "BU" (output: h1 to h15 BU reconciled bootstrap samples)

BU_gender_fun_mfts <- function(kj)
{
  dum = array(0, dim = c(101, 168, (16-kj), 1000))
  for(ij in 1:101)
  {
    dum[ij,,,] = BU_gender_optim_hier_PI_mfts(kj = kj, age = ij)
  }
  return(dum)
}

# Parallel calculation of reconciled interval predictions

library(doParallel)

cl <- makeCluster(5) 
registerDoParallel(cl)

BU_gender_hier_comb_mfts = foreach(kj = 1:15, .export = ls()[pop_ratio_index], .packages = c("MASS")) %dopar% BU_gender_fun_mfts(kj = kj)
save(BU_gender_hier_comb_mfts, file = "BU_gender_hier_comb_mfts.RData")

stopCluster(cl)
rm(cl)

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "comb_OLS" (output: h1 to h15 OLS reconciled bootstrap samples) 

OLS_gender_fun_mfts <- function(kj)  
{
  dum = array(0, dim = c(101, 168, (16-kj), 1000))
  for(ij in 1:101)
  {
    print(paste("age =", ij))
    dum[ij,,,] = BU_gender_optim_hier_PI_mfts(kj = kj, age = ij, hier_method = "comb_OLS")
  }
  return(dum)    
}

# Parallel calculation of reconciled interval predictions

cl <- makeCluster(8) 
registerDoParallel(cl)

pop_ratio_index = c(match(pop_ratio_P_F_to_T, ls()), match(pop_ratio_P_M_to_T, ls()), match(pop_ratio_P_F_to_F, ls()),match(pop_ratio_P_M_to_M, ls()), match(pop_ratio_P_F_to_R1_T, ls()), match(pop_ratio_P_F_to_R2_T, ls()), match(pop_ratio_P_F_to_R3_T, ls()), match(pop_ratio_P_F_to_R4_T, ls()), match(pop_ratio_P_F_to_R5_T, ls()), match(pop_ratio_P_F_to_R6_T, ls()), match(pop_ratio_P_F_to_R7_T, ls()), match(pop_ratio_P_F_to_R8_T, ls()), match(pop_ratio_P_M_to_R1_T, ls()), match(pop_ratio_P_M_to_R2_T, ls()), match(pop_ratio_P_M_to_R3_T, ls()), match(pop_ratio_P_M_to_R4_T, ls()), match(pop_ratio_P_M_to_R5_T, ls()), match(pop_ratio_P_M_to_R6_T, ls()), match(pop_ratio_P_M_to_R7_T, ls()), match(pop_ratio_P_M_to_R8_T, ls()), match(pop_ratio_P_F_to_R1_F, ls()), match(pop_ratio_P_F_to_R2_F, ls()), match(pop_ratio_P_F_to_R3_F, ls()), match(pop_ratio_P_F_to_R4_F, ls()), match(pop_ratio_P_F_to_R5_F, ls()), match(pop_ratio_P_F_to_R6_F, ls()), match(pop_ratio_P_F_to_R7_F, ls()), match(pop_ratio_P_F_to_R8_F, ls()), match(pop_ratio_P_M_to_R1_M, ls()), match(pop_ratio_P_M_to_R2_M, ls()), match(pop_ratio_P_M_to_R3_M, ls()), match(pop_ratio_P_M_to_R4_M, ls()), match(pop_ratio_P_M_to_R5_M, ls()), match(pop_ratio_P_M_to_R6_M, ls()), match(pop_ratio_P_M_to_R7_M, ls()), match(pop_ratio_P_M_to_R8_M, ls()), match(pop_ratio_P_F_to_P_T, ls()), match(pop_ratio_P_M_to_P_T, ls()))

state_residual_index = c(match(mfts_state_train_residual_total, ls()), match(mfts_state_train_residual_female, ls()), match(mfts_state_train_residual_male, ls()), match(mfts_region_train_residual_total, ls()), match(mfts_region_train_residual_female, ls()), match(mfts_region_train_residual_male, ls()))


OLS_gender_hier_comb_mfts = foreach(kj = 1:15, .export = ls()[pop_ratio_index], .packages = c("MASS")) %dopar% OLS_gender_fun_mfts(kj = kj)
save(OLS_gender_hier_comb_mfts, file = "OLS_gender_hier_comb_mfts.RData")
rm(OLS_gender_hier_comb_mfts)

stopCluster(cl)
rm(cl)

# Define a function to enable parallel computation of interval forecasts reconciliation
## hier_method = "mint" (output: h1 to h15 MinT reconciled bootstrap samples) 

mint_gender_fun_mfts <- function(kj)  
{
  dum = array(0, dim = c(101, 168, (16-kj), 1000))
  for(ij in 1:101)
  {
    print(paste("age =", ij))
    dum[ij,,,] = BU_gender_optim_hier_PI_mfts(kj = kj, age = ij, hier_method = "mint")
  }
  return(dum)    
}

# Parallel calculation of reconciled interval predictions

cl <- makeCluster(8) 
registerDoParallel(cl)

pop_ratio_index = c(match(pop_ratio_P_F_to_T, ls()), match(pop_ratio_P_M_to_T, ls()), match(pop_ratio_P_F_to_F, ls()),match(pop_ratio_P_M_to_M, ls()), match(pop_ratio_P_F_to_R1_T, ls()), match(pop_ratio_P_F_to_R2_T, ls()), match(pop_ratio_P_F_to_R3_T, ls()), match(pop_ratio_P_F_to_R4_T, ls()), match(pop_ratio_P_F_to_R5_T, ls()), match(pop_ratio_P_F_to_R6_T, ls()), match(pop_ratio_P_F_to_R7_T, ls()), match(pop_ratio_P_F_to_R8_T, ls()), match(pop_ratio_P_M_to_R1_T, ls()), match(pop_ratio_P_M_to_R2_T, ls()), match(pop_ratio_P_M_to_R3_T, ls()), match(pop_ratio_P_M_to_R4_T, ls()), match(pop_ratio_P_M_to_R5_T, ls()), match(pop_ratio_P_M_to_R6_T, ls()), match(pop_ratio_P_M_to_R7_T, ls()), match(pop_ratio_P_M_to_R8_T, ls()), match(pop_ratio_P_F_to_R1_F, ls()), match(pop_ratio_P_F_to_R2_F, ls()), match(pop_ratio_P_F_to_R3_F, ls()), match(pop_ratio_P_F_to_R4_F, ls()), match(pop_ratio_P_F_to_R5_F, ls()), match(pop_ratio_P_F_to_R6_F, ls()), match(pop_ratio_P_F_to_R7_F, ls()), match(pop_ratio_P_F_to_R8_F, ls()), match(pop_ratio_P_M_to_R1_M, ls()), match(pop_ratio_P_M_to_R2_M, ls()), match(pop_ratio_P_M_to_R3_M, ls()), match(pop_ratio_P_M_to_R4_M, ls()), match(pop_ratio_P_M_to_R5_M, ls()), match(pop_ratio_P_M_to_R6_M, ls()), match(pop_ratio_P_M_to_R7_M, ls()), match(pop_ratio_P_M_to_R8_M, ls()), match(pop_ratio_P_F_to_P_T, ls()), match(pop_ratio_P_M_to_P_T, ls()))

state_residual_index = c(match(mfts_state_train_residual_total, ls()), match(mfts_state_train_residual_female, ls()), match(mfts_state_train_residual_male, ls()), match(mfts_region_train_residual_total, ls()), match(mfts_region_train_residual_female, ls()), match(mfts_region_train_residual_male, ls()))

mint_gender_hier_comb_mfts = foreach(kj = 1:15, .export = c(ls()[pop_ratio_index], ls()[state_residual_index], ls()[state_residual_index]), .packages = c("MASS")) %dopar% mint_gender_fun_mfts(kj = kj)
save(mint_gender_hier_comb_mfts, file = "mint_gender_hier_comb_mfts.RData")
rm(mint_gender_hier_comb_mfts)

stopCluster(cl)
rm(cl)




























