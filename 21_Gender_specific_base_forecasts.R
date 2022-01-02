#########################################
# Point forecasts gender-specific series 
#########################################

library(demography)
library(ftsa)

# state level training residuals

mfts_gender_state_train_residual_female = c("mfts_gender_Japan_train_residual_female", "mfts_gender_Hokkaido_train_residual_female", "mfts_gender_Aomori_train_residual_female", 
                                     "mfts_gender_Iwate_train_residual_female", "mfts_gender_Miyagi_train_residual_female", "mfts_gender_Akita_train_residual_female", 
                                     "mfts_gender_Yamagata_train_residual_female", "mfts_gender_Fukushima_train_residual_female", "mfts_gender_Ibaraki_train_residual_female", 
                                     "mfts_gender_Tochigi_train_residual_female", "mfts_gender_Gunma_train_residual_female", "mfts_gender_Saitama_train_residual_female", 
                                     "mfts_gender_Chiba_train_residual_female", "mfts_gender_Tokyo_train_residual_female", "mfts_gender_Kanagawa_train_residual_female",
                                     "mfts_gender_Niigata_train_residual_female", "mfts_gender_Toyama_train_residual_female", "mfts_gender_Ishikawa_train_residual_female", 
                                     "mfts_gender_Fukui_train_residual_female", "mfts_gender_Yamanashi_train_residual_female", "mfts_gender_Nagano_train_residual_female", 
                                     "mfts_gender_Gifu_train_residual_female", "mfts_gender_Shizuoka_train_residual_female", "mfts_gender_Aichi_train_residual_female",
                                     "mfts_gender_Mie_train_residual_female", "mfts_gender_Shiga_train_residual_female", "mfts_gender_Kyoto_train_residual_female", 
                                     "mfts_gender_Osaka_train_residual_female", "mfts_gender_Hyogo_train_residual_female", "mfts_gender_Nara_train_residual_female", 
                                     "mfts_gender_Wakayama_train_residual_female", "mfts_gender_Tottori_train_residual_female", "mfts_gender_Shimane_train_residual_female",
                                     "mfts_gender_Okayama_train_residual_female", "mfts_gender_Hiroshima_train_residual_female", "mfts_gender_Yamaguchi_train_residual_female", 
                                     "mfts_gender_Tokushima_train_residual_female", "mfts_gender_Kagawa_train_residual_female", "mfts_gender_Ehime_train_residual_female", 
                                     "mfts_gender_Kochi_train_residual_female", "mfts_gender_Fukuoka_train_residual_female", "mfts_gender_Saga_train_residual_female", 
                                     "mfts_gender_Nagasaki_train_residual_female", "mfts_gender_Kumamoto_train_residual_female", "mfts_gender_Oita_train_residual_female", 
                                     "mfts_gender_Miyazaki_train_residual_female", "mfts_gender_Kagoshima_train_residual_female", "mfts_gender_Okinawa_train_residual_female")

mfts_gender_state_train_residual_male = c("mfts_gender_Japan_train_residual_male", "mfts_gender_Hokkaido_train_residual_male", "mfts_gender_Aomori_train_residual_male", 
                                   "mfts_gender_Iwate_train_residual_male", "mfts_gender_Miyagi_train_residual_male", "mfts_gender_Akita_train_residual_male", 
                                   "mfts_gender_Yamagata_train_residual_male", "mfts_gender_Fukushima_train_residual_male", "mfts_gender_Ibaraki_train_residual_male", 
                                   "mfts_gender_Tochigi_train_residual_male", "mfts_gender_Gunma_train_residual_male", "mfts_gender_Saitama_train_residual_male", 
                                   "mfts_gender_Chiba_train_residual_male", "mfts_gender_Tokyo_train_residual_male", "mfts_gender_Kanagawa_train_residual_male",
                                   "mfts_gender_Niigata_train_residual_male", "mfts_gender_Toyama_train_residual_male", "mfts_gender_Ishikawa_train_residual_male", 
                                   "mfts_gender_Fukui_train_residual_male", "mfts_gender_Yamanashi_train_residual_male", "mfts_gender_Nagano_train_residual_male", 
                                   "mfts_gender_Gifu_train_residual_male", "mfts_gender_Shizuoka_train_residual_male", "mfts_gender_Aichi_train_residual_male",
                                   "mfts_gender_Mie_train_residual_male", "mfts_gender_Shiga_train_residual_male", "mfts_gender_Kyoto_train_residual_male", 
                                   "mfts_gender_Osaka_train_residual_male", "mfts_gender_Hyogo_train_residual_male", "mfts_gender_Nara_train_residual_male", 
                                   "mfts_gender_Wakayama_train_residual_male", "mfts_gender_Tottori_train_residual_male", "mfts_gender_Shimane_train_residual_male",
                                   "mfts_gender_Okayama_train_residual_male", "mfts_gender_Hiroshima_train_residual_male", "mfts_gender_Yamaguchi_train_residual_male", 
                                   "mfts_gender_Tokushima_train_residual_male", "mfts_gender_Kagawa_train_residual_male", "mfts_gender_Ehime_train_residual_male", 
                                   "mfts_gender_Kochi_train_residual_male", "mfts_gender_Fukuoka_train_residual_male", "mfts_gender_Saga_train_residual_male", 
                                   "mfts_gender_Nagasaki_train_residual_male", "mfts_gender_Kumamoto_train_residual_male", "mfts_gender_Oita_train_residual_male", 
                                   "mfts_gender_Miyazaki_train_residual_male", "mfts_gender_Kagoshima_train_residual_male", "mfts_gender_Okinawa_train_residual_male")

# state level forecasts

mfts_gender_state_forc_female = c("mfts_gender_Japan_forc_female",     "mfts_gender_Hokkaido_forc_female",  "mfts_gender_Aomori_forc_female", 
                           "mfts_gender_Iwate_forc_female",     "mfts_gender_Miyagi_forc_female",    "mfts_gender_Akita_forc_female", 
                           "mfts_gender_Yamagata_forc_female",  "mfts_gender_Fukushima_forc_female", "mfts_gender_Ibaraki_forc_female", 
                           "mfts_gender_Tochigi_forc_female",   "mfts_gender_Gunma_forc_female",     "mfts_gender_Saitama_forc_female", 
                           "mfts_gender_Chiba_forc_female",     "mfts_gender_Tokyo_forc_female",     "mfts_gender_Kanagawa_forc_female",
                           "mfts_gender_Niigata_forc_female",   "mfts_gender_Toyama_forc_female",    "mfts_gender_Ishikawa_forc_female", 
                           "mfts_gender_Fukui_forc_female",     "mfts_gender_Yamanashi_forc_female", "mfts_gender_Nagano_forc_female", 
                           "mfts_gender_Gifu_forc_female",      "mfts_gender_Shizuoka_forc_female",  "mfts_gender_Aichi_forc_female",
                           "mfts_gender_Mie_forc_female",       "mfts_gender_Shiga_forc_female",     "mfts_gender_Kyoto_forc_female", 
                           "mfts_gender_Osaka_forc_female",     "mfts_gender_Hyogo_forc_female",     "mfts_gender_Nara_forc_female", 
                           "mfts_gender_Wakayama_forc_female",  "mfts_gender_Tottori_forc_female",   "mfts_gender_Shimane_forc_female",
                           "mfts_gender_Okayama_forc_female",   "mfts_gender_Hiroshima_forc_female", "mfts_gender_Yamaguchi_forc_female", 
                           "mfts_gender_Tokushima_forc_female", "mfts_gender_Kagawa_forc_female",    "mfts_gender_Ehime_forc_female", 
                           "mfts_gender_Kochi_forc_female",     "mfts_gender_Fukuoka_forc_female",   "mfts_gender_Saga_forc_female", 
                           "mfts_gender_Nagasaki_forc_female",  "mfts_gender_Kumamoto_forc_female",  "mfts_gender_Oita_forc_female", 
                           "mfts_gender_Miyazaki_forc_female",  "mfts_gender_Kagoshima_forc_female", "mfts_gender_Okinawa_forc_female")

mfts_gender_state_forc_male = c("mfts_gender_Japan_forc_male",     "mfts_gender_Hokkaido_forc_male",  "mfts_gender_Aomori_forc_male", 
                         "mfts_gender_Iwate_forc_male",     "mfts_gender_Miyagi_forc_male",    "mfts_gender_Akita_forc_male", 
                         "mfts_gender_Yamagata_forc_male",  "mfts_gender_Fukushima_forc_male", "mfts_gender_Ibaraki_forc_male", 
                         "mfts_gender_Tochigi_forc_male",   "mfts_gender_Gunma_forc_male",     "mfts_gender_Saitama_forc_male", 
                         "mfts_gender_Chiba_forc_male",     "mfts_gender_Tokyo_forc_male",     "mfts_gender_Kanagawa_forc_male",
                         "mfts_gender_Niigata_forc_male",   "mfts_gender_Toyama_forc_male",    "mfts_gender_Ishikawa_forc_male", 
                         "mfts_gender_Fukui_forc_male",     "mfts_gender_Yamanashi_forc_male", "mfts_gender_Nagano_forc_male", 
                         "mfts_gender_Gifu_forc_male",      "mfts_gender_Shizuoka_forc_male",  "mfts_gender_Aichi_forc_male",
                         "mfts_gender_Mie_forc_male",       "mfts_gender_Shiga_forc_male",     "mfts_gender_Kyoto_forc_male", 
                         "mfts_gender_Osaka_forc_male",     "mfts_gender_Hyogo_forc_male",     "mfts_gender_Nara_forc_male", 
                         "mfts_gender_Wakayama_forc_male",  "mfts_gender_Tottori_forc_male",   "mfts_gender_Shimane_forc_male",
                         "mfts_gender_Okayama_forc_male",   "mfts_gender_Hiroshima_forc_male", "mfts_gender_Yamaguchi_forc_male", 
                         "mfts_gender_Tokushima_forc_male", "mfts_gender_Kagawa_forc_male",    "mfts_gender_Ehime_forc_male", 
                         "mfts_gender_Kochi_forc_male",     "mfts_gender_Fukuoka_forc_male",   "mfts_gender_Saga_forc_male", 
                         "mfts_gender_Nagasaki_forc_male",  "mfts_gender_Kumamoto_forc_male",  "mfts_gender_Oita_forc_male", 
                         "mfts_gender_Miyazaki_forc_male",  "mfts_gender_Kagoshima_forc_male", "mfts_gender_Okinawa_forc_male")

# Mean Error of state forecasts

mfts_gender_state_me_female = c("mfts_gender_Japan_me_female", "mfts_gender_Hokkaido_me_female", "mfts_gender_Aomori_me_female", 
                         "mfts_gender_Iwate_me_female", "mfts_gender_Miyagi_me_female", "mfts_gender_Akita_me_female", 
                         "mfts_gender_Yamagata_me_female", "mfts_gender_Fukushima_me_female", "mfts_gender_Ibaraki_me_female", 
                         "mfts_gender_Tochigi_me_female", "mfts_gender_Gunma_me_female", "mfts_gender_Saitama_me_female", 
                         "mfts_gender_Chiba_me_female", "mfts_gender_Tokyo_me_female", "mfts_gender_Kanagawa_me_female",
                         "mfts_gender_Niigata_me_female", "mfts_gender_Toyama_me_female", "mfts_gender_Ishikawa_me_female", 
                         "mfts_gender_Fukui_me_female", "mfts_gender_Yamanashi_me_female", "mfts_gender_Nagano_me_female", 
                         "mfts_gender_Gifu_me_female", "mfts_gender_Shizuoka_me_female", "mfts_gender_Aichi_me_female",
                         "mfts_gender_Mie_me_female", "mfts_gender_Shiga_me_female", "mfts_gender_Kyoto_me_female", 
                         "mfts_gender_Osaka_me_female", "mfts_gender_Hyogo_me_female", "mfts_gender_Nara_me_female", 
                         "mfts_gender_Wakayama_me_female", "mfts_gender_Tottori_me_female", "mfts_gender_Shimane_me_female",
                         "mfts_gender_Okayama_me_female", "mfts_gender_Hiroshima_me_female", "mfts_gender_Yamaguchi_me_female", 
                         "mfts_gender_Tokushima_me_female", "mfts_gender_Kagawa_me_female", "mfts_gender_Ehime_me_female", 
                         "mfts_gender_Kochi_me_female", "mfts_gender_Fukuoka_me_female", "mfts_gender_Saga_me_female", 
                         "mfts_gender_Nagasaki_me_female", "mfts_gender_Kumamoto_me_female", "mfts_gender_Oita_me_female", 
                         "mfts_gender_Miyazaki_me_female", "mfts_gender_Kagoshima_me_female", "mfts_gender_Okinawa_me_female")

mfts_gender_state_me_male = c("mfts_gender_Japan_me_male", "mfts_gender_Hokkaido_me_male", "mfts_gender_Aomori_me_male", 
                       "mfts_gender_Iwate_me_male", "mfts_gender_Miyagi_me_male", "mfts_gender_Akita_me_male", 
                       "mfts_gender_Yamagata_me_male", "mfts_gender_Fukushima_me_male", "mfts_gender_Ibaraki_me_male", 
                       "mfts_gender_Tochigi_me_male", "mfts_gender_Gunma_me_male", "mfts_gender_Saitama_me_male", 
                       "mfts_gender_Chiba_me_male", "mfts_gender_Tokyo_me_male", "mfts_gender_Kanagawa_me_male",
                       "mfts_gender_Niigata_me_male", "mfts_gender_Toyama_me_male", "mfts_gender_Ishikawa_me_male", 
                       "mfts_gender_Fukui_me_male", "mfts_gender_Yamanashi_me_male", "mfts_gender_Nagano_me_male", 
                       "mfts_gender_Gifu_me_male", "mfts_gender_Shizuoka_me_male", "mfts_gender_Aichi_me_male",
                       "mfts_gender_Mie_me_male", "mfts_gender_Shiga_me_male", "mfts_gender_Kyoto_me_male", 
                       "mfts_gender_Osaka_me_male", "mfts_gender_Hyogo_me_male", "mfts_gender_Nara_me_male", 
                       "mfts_gender_Wakayama_me_male", "mfts_gender_Tottori_me_male", "mfts_gender_Shimane_me_male",
                       "mfts_gender_Okayama_me_male", "mfts_gender_Hiroshima_me_male", "mfts_gender_Yamaguchi_me_male", 
                       "mfts_gender_Tokushima_me_male", "mfts_gender_Kagawa_me_male", "mfts_gender_Ehime_me_male", 
                       "mfts_gender_Kochi_me_male", "mfts_gender_Fukuoka_me_male", "mfts_gender_Saga_me_male", 
                       "mfts_gender_Nagasaki_me_male", "mfts_gender_Kumamoto_me_male", "mfts_gender_Oita_me_male", 
                       "mfts_gender_Miyazaki_me_male", "mfts_gender_Kagoshima_me_male", "mfts_gender_Okinawa_me_male")

# Mean Absolute Error of state forecasts

mfts_gender_state_mae_female = c("mfts_gender_Japan_mae_female", "mfts_gender_Hokkaido_mae_female", "mfts_gender_Aomori_mae_female", 
                          "mfts_gender_Iwate_mae_female", "mfts_gender_Miyagi_mae_female", "mfts_gender_Akita_mae_female", 
                          "mfts_gender_Yamagata_mae_female", "mfts_gender_Fukushima_mae_female", "mfts_gender_Ibaraki_mae_female", 
                          "mfts_gender_Tochigi_mae_female", "mfts_gender_Gunma_mae_female", "mfts_gender_Saitama_mae_female", 
                          "mfts_gender_Chiba_mae_female", "mfts_gender_Tokyo_mae_female", "mfts_gender_Kanagawa_mae_female",
                          "mfts_gender_Niigata_mae_female", "mfts_gender_Toyama_mae_female", "mfts_gender_Ishikawa_mae_female", 
                          "mfts_gender_Fukui_mae_female", "mfts_gender_Yamanashi_mae_female", "mfts_gender_Nagano_mae_female", 
                          "mfts_gender_Gifu_mae_female", "mfts_gender_Shizuoka_mae_female", "mfts_gender_Aichi_mae_female",
                          "mfts_gender_Mie_mae_female", "mfts_gender_Shiga_mae_female", "mfts_gender_Kyoto_mae_female", 
                          "mfts_gender_Osaka_mae_female", "mfts_gender_Hyogo_mae_female", "mfts_gender_Nara_mae_female", 
                          "mfts_gender_Wakayama_mae_female", "mfts_gender_Tottori_mae_female", "mfts_gender_Shimane_mae_female",
                          "mfts_gender_Okayama_mae_female", "mfts_gender_Hiroshima_mae_female", "mfts_gender_Yamaguchi_mae_female", 
                          "mfts_gender_Tokushima_mae_female", "mfts_gender_Kagawa_mae_female", "mfts_gender_Ehime_mae_female", 
                          "mfts_gender_Kochi_mae_female", "mfts_gender_Fukuoka_mae_female", "mfts_gender_Saga_mae_female", 
                          "mfts_gender_Nagasaki_mae_female", "mfts_gender_Kumamoto_mae_female", "mfts_gender_Oita_mae_female", 
                          "mfts_gender_Miyazaki_mae_female", "mfts_gender_Kagoshima_mae_female", "mfts_gender_Okinawa_mae_female")

mfts_gender_state_mae_male = c("mfts_gender_Japan_mae_male", "mfts_gender_Hokkaido_mae_male", "mfts_gender_Aomori_mae_male", 
                        "mfts_gender_Iwate_mae_male", "mfts_gender_Miyagi_mae_male", "mfts_gender_Akita_mae_male", 
                        "mfts_gender_Yamagata_mae_male", "mfts_gender_Fukushima_mae_male", "mfts_gender_Ibaraki_mae_male", 
                        "mfts_gender_Tochigi_mae_male", "mfts_gender_Gunma_mae_male", "mfts_gender_Saitama_mae_male", 
                        "mfts_gender_Chiba_mae_male", "mfts_gender_Tokyo_mae_male", "mfts_gender_Kanagawa_mae_male",
                        "mfts_gender_Niigata_mae_male", "mfts_gender_Toyama_mae_male", "mfts_gender_Ishikawa_mae_male", 
                        "mfts_gender_Fukui_mae_male", "mfts_gender_Yamanashi_mae_male", "mfts_gender_Nagano_mae_male", 
                        "mfts_gender_Gifu_mae_male", "mfts_gender_Shizuoka_mae_male", "mfts_gender_Aichi_mae_male",
                        "mfts_gender_Mie_mae_male", "mfts_gender_Shiga_mae_male", "mfts_gender_Kyoto_mae_male", 
                        "mfts_gender_Osaka_mae_male", "mfts_gender_Hyogo_mae_male", "mfts_gender_Nara_mae_male", 
                        "mfts_gender_Wakayama_mae_male", "mfts_gender_Tottori_mae_male", "mfts_gender_Shimane_mae_male",
                        "mfts_gender_Okayama_mae_male", "mfts_gender_Hiroshima_mae_male", "mfts_gender_Yamaguchi_mae_male", 
                        "mfts_gender_Tokushima_mae_male", "mfts_gender_Kagawa_mae_male", "mfts_gender_Ehime_mae_male", 
                        "mfts_gender_Kochi_mae_male", "mfts_gender_Fukuoka_mae_male", "mfts_gender_Saga_mae_male", 
                        "mfts_gender_Nagasaki_mae_male", "mfts_gender_Kumamoto_mae_male", "mfts_gender_Oita_mae_male", 
                        "mfts_gender_Miyazaki_mae_male", "mfts_gender_Kagoshima_mae_male", "mfts_gender_Okinawa_mae_male")


# Root Mean Square Error of state forecasts

mfts_gender_state_rmse_female = c("mfts_gender_Japan_rmse_female", "mfts_gender_Hokkaido_rmse_female", "mfts_gender_Aomori_rmse_female", 
                           "mfts_gender_Iwate_rmse_female", "mfts_gender_Miyagi_rmse_female", "mfts_gender_Akita_rmse_female", 
                           "mfts_gender_Yamagata_rmse_female", "mfts_gender_Fukushima_rmse_female", "mfts_gender_Ibaraki_rmse_female", 
                           "mfts_gender_Tochigi_rmse_female", "mfts_gender_Gunma_rmse_female", "mfts_gender_Saitama_rmse_female", 
                           "mfts_gender_Chiba_rmse_female", "mfts_gender_Tokyo_rmse_female", "mfts_gender_Kanagawa_rmse_female",
                           "mfts_gender_Niigata_rmse_female", "mfts_gender_Toyama_rmse_female", "mfts_gender_Ishikawa_rmse_female", 
                           "mfts_gender_Fukui_rmse_female", "mfts_gender_Yamanashi_rmse_female", "mfts_gender_Nagano_rmse_female", 
                           "mfts_gender_Gifu_rmse_female", "mfts_gender_Shizuoka_rmse_female", "mfts_gender_Aichi_rmse_female",
                           "mfts_gender_Mie_rmse_female", "mfts_gender_Shiga_rmse_female", "mfts_gender_Kyoto_rmse_female", 
                           "mfts_gender_Osaka_rmse_female", "mfts_gender_Hyogo_rmse_female", "mfts_gender_Nara_rmse_female", 
                           "mfts_gender_Wakayama_rmse_female", "mfts_gender_Tottori_rmse_female", "mfts_gender_Shimane_rmse_female",
                           "mfts_gender_Okayama_rmse_female", "mfts_gender_Hiroshima_rmse_female", "mfts_gender_Yamaguchi_rmse_female", 
                           "mfts_gender_Tokushima_rmse_female", "mfts_gender_Kagawa_rmse_female", "mfts_gender_Ehime_rmse_female", 
                           "mfts_gender_Kochi_rmse_female", "mfts_gender_Fukuoka_rmse_female", "mfts_gender_Saga_rmse_female", 
                           "mfts_gender_Nagasaki_rmse_female", "mfts_gender_Kumamoto_rmse_female", "mfts_gender_Oita_rmse_female", 
                           "mfts_gender_Miyazaki_rmse_female", "mfts_gender_Kagoshima_rmse_female", "mfts_gender_Okinawa_rmse_female")

mfts_gender_state_rmse_male = c("mfts_gender_Japan_rmse_male", "mfts_gender_Hokkaido_rmse_male", "mfts_gender_Aomori_rmse_male", 
                         "mfts_gender_Iwate_rmse_male", "mfts_gender_Miyagi_rmse_male", "mfts_gender_Akita_rmse_male", 
                         "mfts_gender_Yamagata_rmse_male", "mfts_gender_Fukushima_rmse_male", "mfts_gender_Ibaraki_rmse_male", 
                         "mfts_gender_Tochigi_rmse_male", "mfts_gender_Gunma_rmse_male", "mfts_gender_Saitama_rmse_male", 
                         "mfts_gender_Chiba_rmse_male", "mfts_gender_Tokyo_rmse_male", "mfts_gender_Kanagawa_rmse_male",
                         "mfts_gender_Niigata_rmse_male", "mfts_gender_Toyama_rmse_male", "mfts_gender_Ishikawa_rmse_male", 
                         "mfts_gender_Fukui_rmse_male", "mfts_gender_Yamanashi_rmse_male", "mfts_gender_Nagano_rmse_male", 
                         "mfts_gender_Gifu_rmse_male", "mfts_gender_Shizuoka_rmse_male", "mfts_gender_Aichi_rmse_male",
                         "mfts_gender_Mie_rmse_male", "mfts_gender_Shiga_rmse_male", "mfts_gender_Kyoto_rmse_male", 
                         "mfts_gender_Osaka_rmse_male", "mfts_gender_Hyogo_rmse_male", "mfts_gender_Nara_rmse_male", 
                         "mfts_gender_Wakayama_rmse_male", "mfts_gender_Tottori_rmse_male", "mfts_gender_Shimane_rmse_male",
                         "mfts_gender_Okayama_rmse_male", "mfts_gender_Hiroshima_rmse_male", "mfts_gender_Yamaguchi_rmse_male", 
                         "mfts_gender_Tokushima_rmse_male", "mfts_gender_Kagawa_rmse_male", "mfts_gender_Ehime_rmse_male", 
                         "mfts_gender_Kochi_rmse_male", "mfts_gender_Fukuoka_rmse_male", "mfts_gender_Saga_rmse_male", 
                         "mfts_gender_Nagasaki_rmse_male", "mfts_gender_Kumamoto_rmse_male", "mfts_gender_Oita_rmse_male", 
                         "mfts_gender_Miyazaki_rmse_male", "mfts_gender_Kagoshima_rmse_male", "mfts_gender_Okinawa_rmse_male")


# region level training residuals

mfts_gender_region_train_residual_female = c("mfts_gender_region_R1_train_residual_female", "mfts_gender_region_R2_train_residual_female", "mfts_gender_region_R3_train_residual_female", 
                                      "mfts_gender_region_R4_train_residual_female", "mfts_gender_region_R5_train_residual_female", "mfts_gender_region_R6_train_residual_female", 
                                      "mfts_gender_region_R7_train_residual_female", "mfts_gender_region_R8_train_residual_female")

mfts_gender_region_train_residual_male = c("mfts_gender_region_R1_train_residual_male", "mfts_gender_region_R2_train_residual_male", "mfts_gender_region_R3_train_residual_male", 
                                    "mfts_gender_region_R4_train_residual_male", "mfts_gender_region_R5_train_residual_male", "mfts_gender_region_R6_train_residual_male", 
                                    "mfts_gender_region_R7_train_residual_male", "mfts_gender_region_R8_train_residual_male")


# region level forecasts

mfts_gender_region_forc_female = c("mfts_gender_region_R1_forc_female", "mfts_gender_region_R2_forc_female", "mfts_gender_region_R3_forc_female", 
                            "mfts_gender_region_R4_forc_female", "mfts_gender_region_R5_forc_female", "mfts_gender_region_R6_forc_female", 
                            "mfts_gender_region_R7_forc_female", "mfts_gender_region_R8_forc_female")

mfts_gender_region_forc_male = c("mfts_gender_region_R1_forc_male", "mfts_gender_region_R2_forc_male", "mfts_gender_region_R3_forc_male", 
                          "mfts_gender_region_R4_forc_male", "mfts_gender_region_R5_forc_male", "mfts_gender_region_R6_forc_male", 
                          "mfts_gender_region_R7_forc_male", "mfts_gender_region_R8_forc_male")

# Mean Error of region forecasts

mfts_gender_region_me_female = c("mfts_gender_region_R1_me_female", "mfts_gender_region_R2_me_female", "mfts_gender_region_R3_me_female", 
                          "mfts_gender_region_R4_me_female", "mfts_gender_region_R5_me_female", "mfts_gender_region_R6_me_female", 
                          "mfts_gender_region_R7_me_female", "mfts_gender_region_R8_me_female")

mfts_gender_region_me_male = c("mfts_gender_region_R1_me_male", "mfts_gender_region_R2_me_male", "mfts_gender_region_R3_me_male", 
                        "mfts_gender_region_R4_me_male", "mfts_gender_region_R5_me_male", "mfts_gender_region_R6_me_male", 
                        "mfts_gender_region_R7_me_male", "mfts_gender_region_R8_me_male")

# Mean Absolute Error of region forecasts

mfts_gender_region_mae_female = c("mfts_gender_region_R1_mae_female", "mfts_gender_region_R2_mae_female", "mfts_gender_region_R3_mae_female", 
                           "mfts_gender_region_R4_mae_female", "mfts_gender_region_R5_mae_female", "mfts_gender_region_R6_mae_female", 
                           "mfts_gender_region_R7_mae_female", "mfts_gender_region_R8_mae_female")

mfts_gender_region_mae_male = c("mfts_gender_region_R1_mae_male", "mfts_gender_region_R2_mae_male", "mfts_gender_region_R3_mae_male", 
                         "mfts_gender_region_R4_mae_male", "mfts_gender_region_R5_mae_male", "mfts_gender_region_R6_mae_male", 
                         "mfts_gender_region_R7_mae_male", "mfts_gender_region_R8_mae_male")

# Root Mean Square Error of region forecasts

mfts_gender_region_rmse_female = c("mfts_gender_region_R1_rmse_female", "mfts_gender_region_R2_rmse_female", "mfts_gender_region_R3_rmse_female", 
                            "mfts_gender_region_R4_rmse_female", "mfts_gender_region_R5_rmse_female", "mfts_gender_region_R6_rmse_female", 
                            "mfts_gender_region_R7_rmse_female", "mfts_gender_region_R8_rmse_female")

mfts_gender_region_rmse_male = c("mfts_gender_region_R1_rmse_male", "mfts_gender_region_R2_rmse_male", "mfts_gender_region_R3_rmse_male", 
                          "mfts_gender_region_R4_rmse_male", "mfts_gender_region_R5_rmse_male", "mfts_gender_region_R6_rmse_male", 
                          "mfts_gender_region_R7_rmse_male", "mfts_gender_region_R8_rmse_male")


#######################################################################
# Multivariate point forecast of female and male series by prefectures
#######################################################################

# Define a function for multivariate FTS forecasting

mfts_gender_back_test <- function(pcamethod = c("static", "dynamic"), year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(state_smooth[2])$year,1) - forecast_year
  
  state_female_comb = state_male_comb = state_female_comb_pop = state_male_comb_pop = matrix(NA, 101*42, 47)
  for(iw in 2:48)
  {
    state_female_comb[,iw-1] = as.numeric(get(state_smooth[iw])$rate$female)
    state_female_comb_pop[,iw-1] = as.numeric(get(state_smooth[iw])$pop$female)
    
    state_male_comb[,iw-1] = as.numeric(get(state_smooth[iw])$rate$male)
    state_male_comb_pop[,iw-1] = as.numeric(get(state_smooth[iw])$pop$male)
  }
  
  total_comb_female = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_comb)
  total_comb_pop_female = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_female_comb_pop)
  colnames(total_comb_female) = colnames(total_comb_pop_female) = c("Year", "Age", state[2:48])
  
  total_comb_male = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_comb)
  total_comb_pop_male = cbind(rep(1975:2016, each=101), rep(0:100, 42), state_male_comb_pop)
  colnames(total_comb_male) = colnames(total_comb_pop_male) = c("Year", "Age", state[2:48])
  
  write.table(total_comb_female, file = "total_comb_female.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  write.table(total_comb_pop_female, file = "total_comb_pop_female.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  
  write.table(total_comb_male, file = "total_comb_male.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  write.table(total_comb_pop_male, file = "total_comb_pop_male.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  
  state_comb_demogdata_female = read.demogdata("total_comb_female.txt", "total_comb_pop_female.txt", 
                                        type = "mortality", label="state_comb_female", skip = 0)
  state_comb_demogdata_male = read.demogdata("total_comb_male.txt", "total_comb_pop_male.txt", 
                                               type = "mortality", label="state_comb_male", skip = 0)
  
  
  res_prefecture_female = res_prefecture_male = array(NA, dim = c(year_horizon, 101, year_horizon, 47))
  train_residual_female = train_residual_male = list()
  
  
  for(ij in 1:year_horizon)
  {
    ind_dat_female = extract.years(state_comb_demogdata_female, years = 1975:(n_year+ij))
    ind_dat_male = extract.years(state_comb_demogdata_male, years = 1975:(n_year+ij))
    
    pcamethod = match.arg(pcamethod)
    fun_forc_female = mfts(ind_dat_female, pcamethod = pcamethod, year_horizon = year_horizon)
    fun_forc_male = mfts(ind_dat_male, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_female[[ij]] = train_residual_male[[ij]] = array(NA, dim = c(101, (26+ij), 47))
    for(iwk in 1:47)
    {
      res_prefecture_female[,,ij,iwk] = t(fun_forc_female$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_female[[ij]][,,iwk] = fun_forc_female$train_residual[(101*(iwk-1)+1):(101*iwk),]
      
      res_prefecture_male[,,ij,iwk] = t(fun_forc_male$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_male[[ij]][,,iwk] = fun_forc_male$train_residual[(101*(iwk-1)+1):(101*iwk),]
    }
  }
  
  # MAE & RMSE
  state_prefecture_female_me = state_prefecture_female_mae = state_prefecture_female_rmse = matrix(NA,year_horizon,47)
  state_prefecture_male_me = state_prefecture_male_mae = state_prefecture_male_rmse = matrix(NA,year_horizon,47)
  
  for(iw in 1:47)
  {
    for(ik in 1:year_horizon)
    {
      # me
      state_prefecture_female_me[ik,iw] = me(res_prefecture_female[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$female)
      state_prefecture_male_me[ik,iw] = me(res_prefecture_male[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$male)
      
      # mae
      state_prefecture_female_mae[ik,iw] = mae(res_prefecture_female[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$female)
      state_prefecture_male_mae[ik,iw] = mae(res_prefecture_male[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$male)
      
      # rmse
      state_prefecture_female_rmse[ik,iw] = rmse(res_prefecture_female[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$female)
      state_prefecture_male_rmse[ik,iw] = rmse(res_prefecture_male[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$male)
    }
  }
  return(list(res_prefecture_female = res_prefecture_female, state_prefecture_female_me = state_prefecture_female_me, state_prefecture_female_mae = state_prefecture_female_mae,
              state_prefecture_female_rmse = state_prefecture_female_rmse, train_residual_female = train_residual_female, 
              res_prefecture_male = res_prefecture_male, state_prefecture_male_me = state_prefecture_male_me, state_prefecture_male_mae = state_prefecture_male_mae,
              state_prefecture_male_rmse = state_prefecture_male_rmse, train_residual_male = train_residual_male))
}

# Point forecasts of gender-specific series at prefecture level (Please be aware that this part of computation takes a long time to execute.)
dum_state_gender = mfts_gender_back_test(pcamethod = c("dynamic"), year_horizon = 15) 

# save results
save(dum_state_gender, file = "gender-specific_state_mfts.RData")

mfts_gender_state_me_female = dum_state_gender$state_prefecture_female_me
mfts_gender_state_mae_female = dum_state_gender$state_prefecture_female_mae
mfts_gender_state_rmse_female = dum_state_gender$state_prefecture_female_rmse
res_gender_prefecture_female = dum_state_gender$res_prefecture_female
mfts_gender_state_residual_female = dum_state_gender$train_residual_female
colnames(mfts_gender_state_me_female) = colnames(mfts_gender_state_mae_female) = colnames(mfts_gender_state_rmse_female) = state[2:48]

mfts_gender_state_me_male = dum_state_gender$state_prefecture_male_me
mfts_gender_state_mae_male = dum_state_gender$state_prefecture_male_mae
mfts_gender_state_rmse_male = dum_state_gender$state_prefecture_male_rmse
res_gender_prefecture_male = dum_state_gender$res_prefecture_male
mfts_gender_state_residual_male = dum_state_gender$train_residual_male
colnames(mfts_gender_state_me_male) = colnames(mfts_gender_state_mae_male) = colnames(mfts_gender_state_rmse_male) = state[2:48]

for(ik in 1:47)
{
  assign(mfts_gender_state_forc_female[ik+1], res_gender_prefecture_female[,,,ik])
  assign(mfts_gender_state_forc_male[ik+1], res_gender_prefecture_male[,,,ik])
}

assign(mfts_gender_state_forc_female[1], get(mfts_state_forc_female[1]))
assign(mfts_gender_state_forc_male[1], get(mfts_state_forc_male[1]))

for(ik in 1:47)
{
  tmp_female = tmp_male = list()
  for(ih in 1:15)
  {
    tmp_female[[ih]] = mfts_gender_state_residual_female[[ih]][,,ik]
    tmp_male[[ih]] = mfts_gender_state_residual_male[[ih]][,,ik]
  }
  assign(mfts_gender_state_train_residual_female[ik+1], tmp_female)
  assign(mfts_gender_state_train_residual_male[ik+1], tmp_male)
}

# total forecast by univariate forecasting method
assign(mfts_gender_state_train_residual_female[1], mfts_Japan_train_residual_female)
assign(mfts_gender_state_train_residual_male[1], mfts_Japan_train_residual_male)

###################################################################
# Multivariate point forecast of female and male series by regions
###################################################################

mfts_gender_back_region_test <- function(pcamethod = c("static", "dynamic"), year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(region_smooth[2])$year,1) - forecast_year
  
  region_female_comb = region_male_comb = region_female_comb_pop = region_male_comb_pop = matrix(NA, 101*42, 8)
  for(iw in 1:8)
  {
    region_female_comb[,iw] = as.numeric(get(region_smooth[iw])$rate$female)
    region_female_comb_pop[,iw] = as.numeric(get(region_smooth[iw])$pop$female)
    
    region_male_comb[,iw] = as.numeric(get(region_smooth[iw])$rate$male)
    region_male_comb_pop[,iw] = as.numeric(get(region_smooth[iw])$pop$male)
  }
  
  total_comb_region_female = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_comb)
  total_comb_pop_region_female = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_female_comb_pop)
  colnames(total_comb_region_female) = colnames(total_comb_pop_region_female) = c("Year", "Age", region[1:8])
  
  total_comb_region_male = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_comb)
  total_comb_pop_region_male = cbind(rep(1975:2016, each=101), rep(0:100, 42), region_male_comb_pop)
  colnames(total_comb_region_male) = colnames(total_comb_pop_region_male) = c("Year", "Age", region[1:8])
  
  write.table(total_comb_region_female, file = "total_comb_region_female.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  write.table(total_comb_pop_region_female, file = "total_comb_pop_region_female.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  
  write.table(total_comb_region_male, file = "total_comb_region_male.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  write.table(total_comb_pop_region_male, file = "total_comb_pop_region_male.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  
  region_comb_demogdata_female = read.demogdata("total_comb_region_female.txt", "total_comb_pop_region_female.txt", 
                                               type = "mortality", label="region_comb_female", skip = 0)
  region_comb_demogdata_male = read.demogdata("total_comb_region_male.txt", "total_comb_pop_region_male.txt", 
                                             type = "mortality", label="region_comb_male", skip = 0)
  
  
  res_region_female = res_region_male = array(NA, dim = c(year_horizon, 101, year_horizon, 8))
  train_residual_female = train_residual_male = list()
  
  
  for(ij in 1:year_horizon)
  {
    ind_dat_female = extract.years(region_comb_demogdata_female, years = 1975:(n_year+ij))
    ind_dat_male = extract.years(region_comb_demogdata_male, years = 1975:(n_year+ij))
    
    pcamethod = match.arg(pcamethod)
    fun_forc_female = mfts(ind_dat_female, pcamethod = pcamethod, year_horizon = year_horizon)
    fun_forc_male = mfts(ind_dat_male, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_female[[ij]] = train_residual_male[[ij]] = array(NA, dim = c(101, (26+ij), 8))
    for(iwk in 1:8)
    {
      res_region_female[,,ij,iwk] = t(fun_forc_female$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_female[[ij]][,,iwk] = fun_forc_female$train_residual[(101*(iwk-1)+1):(101*iwk),]
      
      res_region_male[,,ij,iwk] = t(fun_forc_male$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_male[[ij]][,,iwk] = fun_forc_male$train_residual[(101*(iwk-1)+1):(101*iwk),]
    }
  }
  
  # MAE & RMSE
  region_female_me = region_female_mae = region_female_rmse = matrix(NA,year_horizon,8)
  region_male_me = region_male_mae = region_male_rmse = matrix(NA,year_horizon,8)
  
  for(iw in 1:8)
  {
    for(ik in 1:year_horizon)
    {
      # me
      region_female_me[ik,iw] = me(res_region_female[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
      region_male_me[ik,iw] = me(res_region_male[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
      
      # mae
      region_female_mae[ik,iw] = mae(res_region_female[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
      region_male_mae[ik,iw] = mae(res_region_male[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
      
      # rmse
      region_female_rmse[ik,iw] = rmse(res_region_female[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
      region_male_rmse[ik,iw] = rmse(res_region_male[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    }
  }
  return(list(res_region_female = res_region_female, region_female_me = region_female_me, region_female_mae = region_female_mae,
              region_female_rmse = region_female_rmse, train_residual_female = train_residual_female, 
              res_region_male = res_region_male, region_male_me = region_male_me, region_male_mae = region_male_mae,
              region_male_rmse = region_male_rmse, train_residual_male = train_residual_male))
}

# Point forecasts of gender-specific series at region level

dum_region_gender = mfts_gender_back_region_test(pcamethod = c("dynamic"), year_horizon = 15) # save(dum_region_gender, file = "gender-specific_region_mfts_2.R")
mfts_gender_region_me_female = dum_region_gender$region_female_me
mfts_gender_region_mae_female = dum_region_gender$region_female_mae
mfts_gender_region_rmse_female = dum_region_gender$region_female_rmse
res_gender_region_female = dum_region_gender$res_region_female
mfts_gender_region_residual_female = dum_region_gender$train_residual_female
colnames(mfts_gender_region_me_female) = colnames(mfts_gender_region_mae_female) = colnames(mfts_gender_region_rmse_female) = region[1:8]

mfts_gender_region_me_male = dum_region_gender$region_male_me
mfts_gender_region_mae_male = dum_region_gender$region_male_mae
mfts_gender_region_rmse_male = dum_region_gender$region_male_rmse
res_gender_region_male = dum_region_gender$res_region_male
mfts_gender_region_residual_male = dum_region_gender$train_residual_male
colnames(mfts_gender_region_me_male) = colnames(mfts_gender_region_mae_male) = colnames(mfts_gender_region_rmse_male) = region[1:8]

for(ik in 1:8)
{
  assign(mfts_gender_region_forc_female[ik], res_gender_region_female[,,,ik])
  assign(mfts_gender_region_forc_male[ik], res_gender_region_male[,,,ik])
}

for(ik in 1:8)
{
  tmp_female = tmp_male = list()
  for(ih in 1:15)
  {
    tmp_female[[ih]] = mfts_gender_region_residual_female[[ih]][,,ik]
    tmp_male[[ih]] = mfts_gender_region_residual_male[[ih]][,,ik]
  }
  assign(mfts_gender_region_train_residual_female[ik], tmp_female)
  assign(mfts_gender_region_train_residual_male[ik], tmp_male)
}


#####################
# Summary of results
#####################

# Female gender specific series

rowmeans_mfts_gender_state_me_female = apply(mfts_gender_state_me_female, 1, mean)
rowmeans_mfts_gender_state_mae_female = apply(mfts_gender_state_mae_female, 1, mean)
rowmeans_mfts_gender_state_rmse_female = apply(mfts_gender_state_rmse_female, 1, mean)

rowmeans_mfts_gender_region_me_female = apply(mfts_gender_region_me_female, 1, mean)
rowmeans_mfts_gender_region_mae_female = apply(mfts_gender_region_mae_female, 1, mean)
rowmeans_mfts_gender_region_rmse_female = apply(mfts_gender_region_rmse_female, 1, mean)

rowmeans_mfts_gender_total_me_female = get(mfts_state_me_female[1])
rowmeans_mfts_gender_total_mae_female = get(mfts_state_mae_female[1])
rowmeans_mfts_gender_total_rmse_female = get(mfts_state_rmse_female[1])

# ME
mfts_gender_female_me_all = cbind(rowmeans_mfts_gender_total_me_female, rowmeans_mfts_gender_region_me_female, rowmeans_mfts_gender_state_me_female)
mfts_gender_female_me_all = rbind(mfts_gender_female_me_all, colMeans(mfts_gender_female_me_all), apply(mfts_gender_female_me_all, 2, median))
colnames(mfts_gender_female_me_all) = c("Female Total", "Female Region", "Female Prefecture")

# MAE
mfts_gender_female_mae_all = cbind(rowmeans_mfts_gender_total_mae_female, rowmeans_mfts_gender_region_mae_female, rowmeans_mfts_gender_state_mae_female)
mfts_gender_female_mae_all = rbind(mfts_gender_female_mae_all, colMeans(mfts_gender_female_mae_all), apply(mfts_gender_female_mae_all, 2, median))
colnames(mfts_gender_female_mae_all) = c("Female Total", "Female Region", "Female Prefecture")

# RMSE
mfts_gender_female_rmse_all = cbind(rowmeans_mfts_gender_total_rmse_female, rowmeans_mfts_gender_region_rmse_female, rowmeans_mfts_gender_state_rmse_female)
mfts_gender_female_rmse_all = rbind(mfts_gender_female_rmse_all, colMeans(mfts_gender_female_rmse_all), apply(mfts_gender_female_rmse_all, 2, median))
colnames(mfts_gender_female_rmse_all) = c("Female Total", "Female Region", "Female Prefecture")

# Male gender specific series

rowmeans_mfts_gender_state_me_male = apply(mfts_gender_state_me_male, 1, mean)
rowmeans_mfts_gender_state_mae_male = apply(mfts_gender_state_mae_male, 1, mean)
rowmeans_mfts_gender_state_rmse_male = apply(mfts_gender_state_rmse_male, 1, mean)

rowmeans_mfts_gender_region_me_male = apply(mfts_gender_region_me_male, 1, mean)
rowmeans_mfts_gender_region_mae_male = apply(mfts_gender_region_mae_male, 1, mean)
rowmeans_mfts_gender_region_rmse_male = apply(mfts_gender_region_rmse_male, 1, mean)

rowmeans_mfts_gender_total_me_male = get(mfts_state_me_male[1])
rowmeans_mfts_gender_total_mae_male = get(mfts_state_mae_male[1])
rowmeans_mfts_gender_total_rmse_male = get(mfts_state_rmse_male[1])

# ME
mfts_gender_male_me_all = cbind(rowmeans_mfts_gender_total_me_male, rowmeans_mfts_gender_region_me_male, rowmeans_mfts_gender_state_me_male)
mfts_gender_male_me_all = rbind(mfts_gender_male_me_all, colMeans(mfts_gender_male_me_all), apply(mfts_gender_male_me_all, 2, median))
colnames(mfts_gender_male_me_all) = c("Male Total", "Male Region", "Male Prefecture")

# MAE
mfts_gender_male_mae_all = cbind(rowmeans_mfts_gender_total_mae_male, rowmeans_mfts_gender_region_mae_male, rowmeans_mfts_gender_state_mae_male)
mfts_gender_male_mae_all = rbind(mfts_gender_male_mae_all, colMeans(mfts_gender_male_mae_all), apply(mfts_gender_male_mae_all, 2, median))
colnames(mfts_gender_male_mae_all) = c("Male Total", "Male Region", "Male Prefecture")

# RMSE
mfts_gender_male_rmse_all = cbind(rowmeans_mfts_gender_total_rmse_male, rowmeans_mfts_gender_region_rmse_male, rowmeans_mfts_gender_state_rmse_male)
mfts_gender_male_rmse_all = rbind(mfts_gender_male_rmse_all, colMeans(mfts_gender_male_rmse_all), apply(mfts_gender_male_rmse_all, 2, median))
colnames(mfts_gender_male_rmse_all) = c("Male Total", "Male Region", "Male Prefecture")


#####################
# Summary of results
#####################


# Mean Errors
mfts_gender_all_level_err_me = cbind(apply(cbind(mfts_gender_female_me_all[,1], mfts_gender_male_me_all[,1]), 1, mean),
                                     apply(cbind(mfts_gender_female_me_all[,2], mfts_gender_male_me_all[,2]), 1, mean),
                                     apply(cbind(mfts_gender_female_me_all[,3], mfts_gender_male_me_all[,3]), 1, mean))

# Mean Absolute Errors
mfts_gender_all_level_err_mae = cbind(apply(cbind(mfts_gender_female_mae_all[,1], mfts_gender_male_mae_all[,1]), 1, mean),
                                     apply(cbind(mfts_gender_female_mae_all[,2], mfts_gender_male_mae_all[,2]), 1, mean),
                                     apply(cbind(mfts_gender_female_mae_all[,3], mfts_gender_male_mae_all[,3]), 1, mean))

# Root Mean Square Errors
mfts_gender_all_level_err_rmse = cbind(apply(cbind(mfts_gender_female_rmse_all[,1], mfts_gender_male_rmse_all[,1]), 1, mean),
                                     apply(cbind(mfts_gender_female_rmse_all[,2], mfts_gender_male_rmse_all[,2]), 1, mean),
                                     apply(cbind(mfts_gender_female_rmse_all[,3], mfts_gender_male_rmse_all[,3]), 1, mean))










