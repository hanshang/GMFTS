###########################################################################
# Multivariate functional time series point forecast (the proposed method)
###########################################################################

library(demography)
library(ftsa)

# Define variables for prefecture level training residuals

mfts_state_train_residual_female = c("mfts_Japan_train_residual_female", "mfts_Hokkaido_train_residual_female", "mfts_Aomori_train_residual_female", 
                                    "mfts_Iwate_train_residual_female", "mfts_Miyagi_train_residual_female", "mfts_Akita_train_residual_female", 
                                    "mfts_Yamagata_train_residual_female", "mfts_Fukushima_train_residual_female", "mfts_Ibaraki_train_residual_female", 
                                    "mfts_Tochigi_train_residual_female", "mfts_Gunma_train_residual_female", "mfts_Saitama_train_residual_female", 
                                    "mfts_Chiba_train_residual_female", "mfts_Tokyo_train_residual_female", "mfts_Kanagawa_train_residual_female",
                                    "mfts_Niigata_train_residual_female", "mfts_Toyama_train_residual_female", "mfts_Ishikawa_train_residual_female", 
                                    "mfts_Fukui_train_residual_female", "mfts_Yamanashi_train_residual_female", "mfts_Nagano_train_residual_female", 
                                    "mfts_Gifu_train_residual_female", "mfts_Shizuoka_train_residual_female", "mfts_Aichi_train_residual_female",
                                    "mfts_Mie_train_residual_female", "mfts_Shiga_train_residual_female", "mfts_Kyoto_train_residual_female", 
                                    "mfts_Osaka_train_residual_female", "mfts_Hyogo_train_residual_female", "mfts_Nara_train_residual_female", 
                                    "mfts_Wakayama_train_residual_female", "mfts_Tottori_train_residual_female", "mfts_Shimane_train_residual_female",
                                    "mfts_Okayama_train_residual_female", "mfts_Hiroshima_train_residual_female", "mfts_Yamaguchi_train_residual_female", 
                                    "mfts_Tokushima_train_residual_female", "mfts_Kagawa_train_residual_female", "mfts_Ehime_train_residual_female", 
                                    "mfts_Kochi_train_residual_female", "mfts_Fukuoka_train_residual_female", "mfts_Saga_train_residual_female", 
                                    "mfts_Nagasaki_train_residual_female", "mfts_Kumamoto_train_residual_female", "mfts_Oita_train_residual_female", 
                                    "mfts_Miyazaki_train_residual_female", "mfts_Kagoshima_train_residual_female", "mfts_Okinawa_train_residual_female")

mfts_state_train_residual_male = c("mfts_Japan_train_residual_male", "mfts_Hokkaido_train_residual_male", "mfts_Aomori_train_residual_male", 
                                  "mfts_Iwate_train_residual_male", "mfts_Miyagi_train_residual_male", "mfts_Akita_train_residual_male", 
                                  "mfts_Yamagata_train_residual_male", "mfts_Fukushima_train_residual_male", "mfts_Ibaraki_train_residual_male", 
                                  "mfts_Tochigi_train_residual_male", "mfts_Gunma_train_residual_male", "mfts_Saitama_train_residual_male", 
                                  "mfts_Chiba_train_residual_male", "mfts_Tokyo_train_residual_male", "mfts_Kanagawa_train_residual_male",
                                  "mfts_Niigata_train_residual_male", "mfts_Toyama_train_residual_male", "mfts_Ishikawa_train_residual_male", 
                                  "mfts_Fukui_train_residual_male", "mfts_Yamanashi_train_residual_male", "mfts_Nagano_train_residual_male", 
                                  "mfts_Gifu_train_residual_male", "mfts_Shizuoka_train_residual_male", "mfts_Aichi_train_residual_male",
                                  "mfts_Mie_train_residual_male", "mfts_Shiga_train_residual_male", "mfts_Kyoto_train_residual_male", 
                                  "mfts_Osaka_train_residual_male", "mfts_Hyogo_train_residual_male", "mfts_Nara_train_residual_male", 
                                  "mfts_Wakayama_train_residual_male", "mfts_Tottori_train_residual_male", "mfts_Shimane_train_residual_male",
                                  "mfts_Okayama_train_residual_male", "mfts_Hiroshima_train_residual_male", "mfts_Yamaguchi_train_residual_male", 
                                  "mfts_Tokushima_train_residual_male", "mfts_Kagawa_train_residual_male", "mfts_Ehime_train_residual_male", 
                                  "mfts_Kochi_train_residual_male", "mfts_Fukuoka_train_residual_male", "mfts_Saga_train_residual_male", 
                                  "mfts_Nagasaki_train_residual_male", "mfts_Kumamoto_train_residual_male", "mfts_Oita_train_residual_male", 
                                  "mfts_Miyazaki_train_residual_male", "mfts_Kagoshima_train_residual_male", "mfts_Okinawa_train_residual_male")

mfts_state_train_residual_total = c("mfts_Japan_train_residual_total", "mfts_Hokkaido_train_residual_total", "mfts_Aomori_train_residual_total", 
                                   "mfts_Iwate_train_residual_total", "mfts_Miyagi_train_residual_total", "mfts_Akita_train_residual_total", 
                                   "mfts_Yamagata_train_residual_total", "mfts_Fukushima_train_residual_total", "mfts_Ibaraki_train_residual_total", 
                                   "mfts_Tochigi_train_residual_total", "mfts_Gunma_train_residual_total", "mfts_Saitama_train_residual_total", 
                                   "mfts_Chiba_train_residual_total", "mfts_Tokyo_train_residual_total", "mfts_Kanagawa_train_residual_total",
                                   "mfts_Niigata_train_residual_total", "mfts_Toyama_train_residual_total", "mfts_Ishikawa_train_residual_total", 
                                   "mfts_Fukui_train_residual_total", "mfts_Yamanashi_train_residual_total", "mfts_Nagano_train_residual_total", 
                                   "mfts_Gifu_train_residual_total", "mfts_Shizuoka_train_residual_total", "mfts_Aichi_train_residual_total",
                                   "mfts_Mie_train_residual_total", "mfts_Shiga_train_residual_total", "mfts_Kyoto_train_residual_total", 
                                   "mfts_Osaka_train_residual_total", "mfts_Hyogo_train_residual_total", "mfts_Nara_train_residual_total", 
                                   "mfts_Wakayama_train_residual_total", "mfts_Tottori_train_residual_total", "mfts_Shimane_train_residual_total",
                                   "mfts_Okayama_train_residual_total", "mfts_Hiroshima_train_residual_total", "mfts_Yamaguchi_train_residual_total", 
                                   "mfts_Tokushima_train_residual_total", "mfts_Kagawa_train_residual_total", "mfts_Ehime_train_residual_total", 
                                   "mfts_Kochi_train_residual_total", "mfts_Fukuoka_train_residual_total", "mfts_Saga_train_residual_total", 
                                   "mfts_Nagasaki_train_residual_total", "mfts_Kumamoto_train_residual_total", "mfts_Oita_train_residual_total", 
                                   "mfts_Miyazaki_train_residual_total", "mfts_Kagoshima_train_residual_total", "mfts_Okinawa_train_residual_total")


# Define variables for prefecture level forecasts

mfts_state_forc_female = c("mfts_Japan_forc_female",     "mfts_Hokkaido_forc_female",  "mfts_Aomori_forc_female", 
                           "mfts_Iwate_forc_female",     "mfts_Miyagi_forc_female",    "mfts_Akita_forc_female", 
                           "mfts_Yamagata_forc_female",  "mfts_Fukushima_forc_female", "mfts_Ibaraki_forc_female", 
                           "mfts_Tochigi_forc_female",   "mfts_Gunma_forc_female",     "mfts_Saitama_forc_female", 
                           "mfts_Chiba_forc_female",     "mfts_Tokyo_forc_female",     "mfts_Kanagawa_forc_female",
                           "mfts_Niigata_forc_female",   "mfts_Toyama_forc_female",    "mfts_Ishikawa_forc_female", 
                           "mfts_Fukui_forc_female",     "mfts_Yamanashi_forc_female", "mfts_Nagano_forc_female", 
                           "mfts_Gifu_forc_female",      "mfts_Shizuoka_forc_female",  "mfts_Aichi_forc_female",
                           "mfts_Mie_forc_female",       "mfts_Shiga_forc_female",     "mfts_Kyoto_forc_female", 
                           "mfts_Osaka_forc_female",     "mfts_Hyogo_forc_female",     "mfts_Nara_forc_female", 
                           "mfts_Wakayama_forc_female",  "mfts_Tottori_forc_female",   "mfts_Shimane_forc_female",
                           "mfts_Okayama_forc_female",   "mfts_Hiroshima_forc_female", "mfts_Yamaguchi_forc_female", 
                           "mfts_Tokushima_forc_female", "mfts_Kagawa_forc_female",    "mfts_Ehime_forc_female", 
                           "mfts_Kochi_forc_female",     "mfts_Fukuoka_forc_female",   "mfts_Saga_forc_female", 
                           "mfts_Nagasaki_forc_female",  "mfts_Kumamoto_forc_female",  "mfts_Oita_forc_female", 
                           "mfts_Miyazaki_forc_female",  "mfts_Kagoshima_forc_female", "mfts_Okinawa_forc_female")

mfts_state_forc_male = c("mfts_Japan_forc_male",     "mfts_Hokkaido_forc_male",  "mfts_Aomori_forc_male", 
                         "mfts_Iwate_forc_male",     "mfts_Miyagi_forc_male",    "mfts_Akita_forc_male", 
                         "mfts_Yamagata_forc_male",  "mfts_Fukushima_forc_male", "mfts_Ibaraki_forc_male", 
                         "mfts_Tochigi_forc_male",   "mfts_Gunma_forc_male",     "mfts_Saitama_forc_male", 
                         "mfts_Chiba_forc_male",     "mfts_Tokyo_forc_male",     "mfts_Kanagawa_forc_male",
                         "mfts_Niigata_forc_male",   "mfts_Toyama_forc_male",    "mfts_Ishikawa_forc_male", 
                         "mfts_Fukui_forc_male",     "mfts_Yamanashi_forc_male", "mfts_Nagano_forc_male", 
                         "mfts_Gifu_forc_male",      "mfts_Shizuoka_forc_male",  "mfts_Aichi_forc_male",
                         "mfts_Mie_forc_male",       "mfts_Shiga_forc_male",     "mfts_Kyoto_forc_male", 
                         "mfts_Osaka_forc_male",     "mfts_Hyogo_forc_male",     "mfts_Nara_forc_male", 
                         "mfts_Wakayama_forc_male",  "mfts_Tottori_forc_male",   "mfts_Shimane_forc_male",
                         "mfts_Okayama_forc_male",   "mfts_Hiroshima_forc_male", "mfts_Yamaguchi_forc_male", 
                         "mfts_Tokushima_forc_male", "mfts_Kagawa_forc_male",    "mfts_Ehime_forc_male", 
                         "mfts_Kochi_forc_male",     "mfts_Fukuoka_forc_male",   "mfts_Saga_forc_male", 
                         "mfts_Nagasaki_forc_male",  "mfts_Kumamoto_forc_male",  "mfts_Oita_forc_male", 
                         "mfts_Miyazaki_forc_male",  "mfts_Kagoshima_forc_male", "mfts_Okinawa_forc_male")

mfts_state_forc_total = c("mfts_Japan_forc_total",     "mfts_Hokkaido_forc_total",  "mfts_Aomori_forc_total", 
                          "mfts_Iwate_forc_total",     "mfts_Miyagi_forc_total",    "mfts_Akita_forc_total", 
                          "mfts_Yamagata_forc_total",  "mfts_Fukushima_forc_total", "mfts_Ibaraki_forc_total", 
                          "mfts_Tochigi_forc_total",   "mfts_Gunma_forc_total",     "mfts_Saitama_forc_total", 
                          "mfts_Chiba_forc_total",     "mfts_Tokyo_forc_total",     "mfts_Kanagawa_forc_total",
                          "mfts_Niigata_forc_total",   "mfts_Toyama_forc_total",    "mfts_Ishikawa_forc_total", 
                          "mfts_Fukui_forc_total",     "mfts_Yamanashi_forc_total", "mfts_Nagano_forc_total", 
                          "mfts_Gifu_forc_total",      "mfts_Shizuoka_forc_total",  "mfts_Aichi_forc_total",
                          "mfts_Mie_forc_total",       "mfts_Shiga_forc_total",     "mfts_Kyoto_forc_total", 
                          "mfts_Osaka_forc_total",     "mfts_Hyogo_forc_total",     "mfts_Nara_forc_total", 
                          "mfts_Wakayama_forc_total",  "mfts_Tottori_forc_total",   "mfts_Shimane_forc_total",
                          "mfts_Okayama_forc_total",   "mfts_Hiroshima_forc_total", "mfts_Yamaguchi_forc_total", 
                          "mfts_Tokushima_forc_total", "mfts_Kagawa_forc_total",    "mfts_Ehime_forc_total", 
                          "mfts_Kochi_forc_total",     "mfts_Fukuoka_forc_total",   "mfts_Saga_forc_total", 
                          "mfts_Nagasaki_forc_total",  "mfts_Kumamoto_forc_total",  "mfts_Oita_forc_total", 
                          "mfts_Miyazaki_forc_total",  "mfts_Kagoshima_forc_total", "mfts_Okinawa_forc_total")

# Mean Error of state forecasts

mfts_state_me_female = c("mfts_Japan_me_female", "mfts_Hokkaido_me_female", "mfts_Aomori_me_female", 
                         "mfts_Iwate_me_female", "mfts_Miyagi_me_female", "mfts_Akita_me_female", 
                         "mfts_Yamagata_me_female", "mfts_Fukushima_me_female", "mfts_Ibaraki_me_female", 
                         "mfts_Tochigi_me_female", "mfts_Gunma_me_female", "mfts_Saitama_me_female", 
                         "mfts_Chiba_me_female", "mfts_Tokyo_me_female", "mfts_Kanagawa_me_female",
                         "mfts_Niigata_me_female", "mfts_Toyama_me_female", "mfts_Ishikawa_me_female", 
                         "mfts_Fukui_me_female", "mfts_Yamanashi_me_female", "mfts_Nagano_me_female", 
                         "mfts_Gifu_me_female", "mfts_Shizuoka_me_female", "mfts_Aichi_me_female",
                         "mfts_Mie_me_female", "mfts_Shiga_me_female", "mfts_Kyoto_me_female", 
                         "mfts_Osaka_me_female", "mfts_Hyogo_me_female", "mfts_Nara_me_female", 
                         "mfts_Wakayama_me_female", "mfts_Tottori_me_female", "mfts_Shimane_me_female",
                         "mfts_Okayama_me_female", "mfts_Hiroshima_me_female", "mfts_Yamaguchi_me_female", 
                         "mfts_Tokushima_me_female", "mfts_Kagawa_me_female", "mfts_Ehime_me_female", 
                         "mfts_Kochi_me_female", "mfts_Fukuoka_me_female", "mfts_Saga_me_female", 
                         "mfts_Nagasaki_me_female", "mfts_Kumamoto_me_female", "mfts_Oita_me_female", 
                         "mfts_Miyazaki_me_female", "mfts_Kagoshima_me_female", "mfts_Okinawa_me_female")

mfts_state_me_male = c("mfts_Japan_me_male", "mfts_Hokkaido_me_male", "mfts_Aomori_me_male", 
                       "mfts_Iwate_me_male", "mfts_Miyagi_me_male", "mfts_Akita_me_male", 
                       "mfts_Yamagata_me_male", "mfts_Fukushima_me_male", "mfts_Ibaraki_me_male", 
                       "mfts_Tochigi_me_male", "mfts_Gunma_me_male", "mfts_Saitama_me_male", 
                       "mfts_Chiba_me_male", "mfts_Tokyo_me_male", "mfts_Kanagawa_me_male",
                       "mfts_Niigata_me_male", "mfts_Toyama_me_male", "mfts_Ishikawa_me_male", 
                       "mfts_Fukui_me_male", "mfts_Yamanashi_me_male", "mfts_Nagano_me_male", 
                       "mfts_Gifu_me_male", "mfts_Shizuoka_me_male", "mfts_Aichi_me_male",
                       "mfts_Mie_me_male", "mfts_Shiga_me_male", "mfts_Kyoto_me_male", 
                       "mfts_Osaka_me_male", "mfts_Hyogo_me_male", "mfts_Nara_me_male", 
                       "mfts_Wakayama_me_male", "mfts_Tottori_me_male", "mfts_Shimane_me_male",
                       "mfts_Okayama_me_male", "mfts_Hiroshima_me_male", "mfts_Yamaguchi_me_male", 
                       "mfts_Tokushima_me_male", "mfts_Kagawa_me_male", "mfts_Ehime_me_male", 
                       "mfts_Kochi_me_male", "mfts_Fukuoka_me_male", "mfts_Saga_me_male", 
                       "mfts_Nagasaki_me_male", "mfts_Kumamoto_me_male", "mfts_Oita_me_male", 
                       "mfts_Miyazaki_me_male", "mfts_Kagoshima_me_male", "mfts_Okinawa_me_male")

mfts_state_me_total = c("mfts_Japan_me_total", "mfts_Hokkaido_me_total", "mfts_Aomori_me_total", 
                        "mfts_Iwate_me_total", "mfts_Miyagi_me_total", "mfts_Akita_me_total", 
                        "mfts_Yamagata_me_total", "mfts_Fukushima_me_total", "mfts_Ibaraki_me_total", 
                        "mfts_Tochigi_me_total", "mfts_Gunma_me_total", "mfts_Saitama_me_total", 
                        "mfts_Chiba_me_total", "mfts_Tokyo_me_total", "mfts_Kanagawa_me_total",
                        "mfts_Niigata_me_total", "mfts_Toyama_me_total", "mfts_Ishikawa_me_total", 
                        "mfts_Fukui_me_total", "mfts_Yamanashi_me_total", "mfts_Nagano_me_total", 
                        "mfts_Gifu_me_total", "mfts_Shizuoka_me_total", "mfts_Aichi_me_total",
                        "mfts_Mie_me_total", "mfts_Shiga_me_total", "mfts_Kyoto_me_total", 
                        "mfts_Osaka_me_total", "mfts_Hyogo_me_total", "mfts_Nara_me_total", 
                        "mfts_Wakayama_me_total", "mfts_Tottori_me_total", "mfts_Shimane_me_total",
                        "mfts_Okayama_me_total", "mfts_Hiroshima_me_total", "mfts_Yamaguchi_me_total", 
                        "mfts_Tokushima_me_total", "mfts_Kagawa_me_total", "mfts_Ehime_me_total", 
                        "mfts_Kochi_me_total", "mfts_Fukuoka_me_total", "mfts_Saga_me_total", 
                        "mfts_Nagasaki_me_total", "mfts_Kumamoto_me_total", "mfts_Oita_me_total", 
                        "mfts_Miyazaki_me_total", "mfts_Kagoshima_me_total", "mfts_Okinawa_me_total")

# Mean Absolute Error of state forecasts

mfts_state_mae_female = c("mfts_Japan_mae_female", "mfts_Hokkaido_mae_female", "mfts_Aomori_mae_female", 
                          "mfts_Iwate_mae_female", "mfts_Miyagi_mae_female", "mfts_Akita_mae_female", 
                          "mfts_Yamagata_mae_female", "mfts_Fukushima_mae_female", "mfts_Ibaraki_mae_female", 
                          "mfts_Tochigi_mae_female", "mfts_Gunma_mae_female", "mfts_Saitama_mae_female", 
                          "mfts_Chiba_mae_female", "mfts_Tokyo_mae_female", "mfts_Kanagawa_mae_female",
                          "mfts_Niigata_mae_female", "mfts_Toyama_mae_female", "mfts_Ishikawa_mae_female", 
                          "mfts_Fukui_mae_female", "mfts_Yamanashi_mae_female", "mfts_Nagano_mae_female", 
                          "mfts_Gifu_mae_female", "mfts_Shizuoka_mae_female", "mfts_Aichi_mae_female",
                          "mfts_Mie_mae_female", "mfts_Shiga_mae_female", "mfts_Kyoto_mae_female", 
                          "mfts_Osaka_mae_female", "mfts_Hyogo_mae_female", "mfts_Nara_mae_female", 
                          "mfts_Wakayama_mae_female", "mfts_Tottori_mae_female", "mfts_Shimane_mae_female",
                          "mfts_Okayama_mae_female", "mfts_Hiroshima_mae_female", "mfts_Yamaguchi_mae_female", 
                          "mfts_Tokushima_mae_female", "mfts_Kagawa_mae_female", "mfts_Ehime_mae_female", 
                          "mfts_Kochi_mae_female", "mfts_Fukuoka_mae_female", "mfts_Saga_mae_female", 
                          "mfts_Nagasaki_mae_female", "mfts_Kumamoto_mae_female", "mfts_Oita_mae_female", 
                          "mfts_Miyazaki_mae_female", "mfts_Kagoshima_mae_female", "mfts_Okinawa_mae_female")

mfts_state_mae_male = c("mfts_Japan_mae_male", "mfts_Hokkaido_mae_male", "mfts_Aomori_mae_male", 
                        "mfts_Iwate_mae_male", "mfts_Miyagi_mae_male", "mfts_Akita_mae_male", 
                        "mfts_Yamagata_mae_male", "mfts_Fukushima_mae_male", "mfts_Ibaraki_mae_male", 
                        "mfts_Tochigi_mae_male", "mfts_Gunma_mae_male", "mfts_Saitama_mae_male", 
                        "mfts_Chiba_mae_male", "mfts_Tokyo_mae_male", "mfts_Kanagawa_mae_male",
                        "mfts_Niigata_mae_male", "mfts_Toyama_mae_male", "mfts_Ishikawa_mae_male", 
                        "mfts_Fukui_mae_male", "mfts_Yamanashi_mae_male", "mfts_Nagano_mae_male", 
                        "mfts_Gifu_mae_male", "mfts_Shizuoka_mae_male", "mfts_Aichi_mae_male",
                        "mfts_Mie_mae_male", "mfts_Shiga_mae_male", "mfts_Kyoto_mae_male", 
                        "mfts_Osaka_mae_male", "mfts_Hyogo_mae_male", "mfts_Nara_mae_male", 
                        "mfts_Wakayama_mae_male", "mfts_Tottori_mae_male", "mfts_Shimane_mae_male",
                        "mfts_Okayama_mae_male", "mfts_Hiroshima_mae_male", "mfts_Yamaguchi_mae_male", 
                        "mfts_Tokushima_mae_male", "mfts_Kagawa_mae_male", "mfts_Ehime_mae_male", 
                        "mfts_Kochi_mae_male", "mfts_Fukuoka_mae_male", "mfts_Saga_mae_male", 
                        "mfts_Nagasaki_mae_male", "mfts_Kumamoto_mae_male", "mfts_Oita_mae_male", 
                        "mfts_Miyazaki_mae_male", "mfts_Kagoshima_mae_male", "mfts_Okinawa_mae_male")

mfts_state_mae_total = c("mfts_Japan_mae_total", "mfts_Hokkaido_mae_total", "mfts_Aomori_mae_total", 
                         "mfts_Iwate_mae_total", "mfts_Miyagi_mae_total", "mfts_Akita_mae_total", 
                         "mfts_Yamagata_mae_total", "mfts_Fukushima_mae_total", "mfts_Ibaraki_mae_total", 
                         "mfts_Tochigi_mae_total", "mfts_Gunma_mae_total", "mfts_Saitama_mae_total", 
                         "mfts_Chiba_mae_total", "mfts_Tokyo_mae_total", "mfts_Kanagawa_mae_total",
                         "mfts_Niigata_mae_total", "mfts_Toyama_mae_total", "mfts_Ishikawa_mae_total", 
                         "mfts_Fukui_mae_total", "mfts_Yamanashi_mae_total", "mfts_Nagano_mae_total", 
                         "mfts_Gifu_mae_total", "mfts_Shizuoka_mae_total", "mfts_Aichi_mae_total",
                         "mfts_Mie_mae_total", "mfts_Shiga_mae_total", "mfts_Kyoto_mae_total", 
                         "mfts_Osaka_mae_total", "mfts_Hyogo_mae_total", "mfts_Nara_mae_total", 
                         "mfts_Wakayama_mae_total", "mfts_Tottori_mae_total", "mfts_Shimane_mae_total",
                         "mfts_Okayama_mae_total", "mfts_Hiroshima_mae_total", "mfts_Yamaguchi_mae_total", 
                         "mfts_Tokushima_mae_total", "mfts_Kagawa_mae_total", "mfts_Ehime_mae_total", 
                         "mfts_Kochi_mae_total", "mfts_Fukuoka_mae_total", "mfts_Saga_mae_total", 
                         "mfts_Nagasaki_mae_total", "mfts_Kumamoto_mae_total", "mfts_Oita_mae_total", 
                         "mfts_Miyazaki_mae_total", "mfts_Kagoshima_mae_total", "mfts_Okinawa_mae_total")

# Root Mean Square Error of state forecasts

mfts_state_rmse_female = c("mfts_Japan_rmse_female", "mfts_Hokkaido_rmse_female", "mfts_Aomori_rmse_female", 
                           "mfts_Iwate_rmse_female", "mfts_Miyagi_rmse_female", "mfts_Akita_rmse_female", 
                           "mfts_Yamagata_rmse_female", "mfts_Fukushima_rmse_female", "mfts_Ibaraki_rmse_female", 
                           "mfts_Tochigi_rmse_female", "mfts_Gunma_rmse_female", "mfts_Saitama_rmse_female", 
                           "mfts_Chiba_rmse_female", "mfts_Tokyo_rmse_female", "mfts_Kanagawa_rmse_female",
                           "mfts_Niigata_rmse_female", "mfts_Toyama_rmse_female", "mfts_Ishikawa_rmse_female", 
                           "mfts_Fukui_rmse_female", "mfts_Yamanashi_rmse_female", "mfts_Nagano_rmse_female", 
                           "mfts_Gifu_rmse_female", "mfts_Shizuoka_rmse_female", "mfts_Aichi_rmse_female",
                           "mfts_Mie_rmse_female", "mfts_Shiga_rmse_female", "mfts_Kyoto_rmse_female", 
                           "mfts_Osaka_rmse_female", "mfts_Hyogo_rmse_female", "mfts_Nara_rmse_female", 
                           "mfts_Wakayama_rmse_female", "mfts_Tottori_rmse_female", "mfts_Shimane_rmse_female",
                           "mfts_Okayama_rmse_female", "mfts_Hiroshima_rmse_female", "mfts_Yamaguchi_rmse_female", 
                           "mfts_Tokushima_rmse_female", "mfts_Kagawa_rmse_female", "mfts_Ehime_rmse_female", 
                           "mfts_Kochi_rmse_female", "mfts_Fukuoka_rmse_female", "mfts_Saga_rmse_female", 
                           "mfts_Nagasaki_rmse_female", "mfts_Kumamoto_rmse_female", "mfts_Oita_rmse_female", 
                           "mfts_Miyazaki_rmse_female", "mfts_Kagoshima_rmse_female", "mfts_Okinawa_rmse_female")

mfts_state_rmse_male = c("mfts_Japan_rmse_male", "mfts_Hokkaido_rmse_male", "mfts_Aomori_rmse_male", 
                         "mfts_Iwate_rmse_male", "mfts_Miyagi_rmse_male", "mfts_Akita_rmse_male", 
                         "mfts_Yamagata_rmse_male", "mfts_Fukushima_rmse_male", "mfts_Ibaraki_rmse_male", 
                         "mfts_Tochigi_rmse_male", "mfts_Gunma_rmse_male", "mfts_Saitama_rmse_male", 
                         "mfts_Chiba_rmse_male", "mfts_Tokyo_rmse_male", "mfts_Kanagawa_rmse_male",
                         "mfts_Niigata_rmse_male", "mfts_Toyama_rmse_male", "mfts_Ishikawa_rmse_male", 
                         "mfts_Fukui_rmse_male", "mfts_Yamanashi_rmse_male", "mfts_Nagano_rmse_male", 
                         "mfts_Gifu_rmse_male", "mfts_Shizuoka_rmse_male", "mfts_Aichi_rmse_male",
                         "mfts_Mie_rmse_male", "mfts_Shiga_rmse_male", "mfts_Kyoto_rmse_male", 
                         "mfts_Osaka_rmse_male", "mfts_Hyogo_rmse_male", "mfts_Nara_rmse_male", 
                         "mfts_Wakayama_rmse_male", "mfts_Tottori_rmse_male", "mfts_Shimane_rmse_male",
                         "mfts_Okayama_rmse_male", "mfts_Hiroshima_rmse_male", "mfts_Yamaguchi_rmse_male", 
                         "mfts_Tokushima_rmse_male", "mfts_Kagawa_rmse_male", "mfts_Ehime_rmse_male", 
                         "mfts_Kochi_rmse_male", "mfts_Fukuoka_rmse_male", "mfts_Saga_rmse_male", 
                         "mfts_Nagasaki_rmse_male", "mfts_Kumamoto_rmse_male", "mfts_Oita_rmse_male", 
                         "mfts_Miyazaki_rmse_male", "mfts_Kagoshima_rmse_male", "mfts_Okinawa_rmse_male")

mfts_state_rmse_total = c("mfts_Japan_rmse_total", "mfts_Hokkaido_rmse_total", "mfts_Aomori_rmse_total", 
                          "mfts_Iwate_rmse_total", "mfts_Miyagi_rmse_total", "mfts_Akita_rmse_total", 
                          "mfts_Yamagata_rmse_total", "mfts_Fukushima_rmse_total", "mfts_Ibaraki_rmse_total", 
                          "mfts_Tochigi_rmse_total", "mfts_Gunma_rmse_total", "mfts_Saitama_rmse_total", 
                          "mfts_Chiba_rmse_total", "mfts_Tokyo_rmse_total", "mfts_Kanagawa_rmse_total",
                          "mfts_Niigata_rmse_total", "mfts_Toyama_rmse_total", "mfts_Ishikawa_rmse_total", 
                          "mfts_Fukui_rmse_total", "mfts_Yamanashi_rmse_total", "mfts_Nagano_rmse_total", 
                          "mfts_Gifu_rmse_total", "mfts_Shizuoka_rmse_total", "mfts_Aichi_rmse_total",
                          "mfts_Mie_rmse_total", "mfts_Shiga_rmse_total", "mfts_Kyoto_rmse_total", 
                          "mfts_Osaka_rmse_total", "mfts_Hyogo_rmse_total", "mfts_Nara_rmse_total", 
                          "mfts_Wakayama_rmse_total", "mfts_Tottori_rmse_total", "mfts_Shimane_rmse_total",
                          "mfts_Okayama_rmse_total", "mfts_Hiroshima_rmse_total", "mfts_Yamaguchi_rmse_total", 
                          "mfts_Tokushima_rmse_total", "mfts_Kagawa_rmse_total", "mfts_Ehime_rmse_total", 
                          "mfts_Kochi_rmse_total", "mfts_Fukuoka_rmse_total", "mfts_Saga_rmse_total", 
                          "mfts_Nagasaki_rmse_total", "mfts_Kumamoto_rmse_total", "mfts_Oita_rmse_total", 
                          "mfts_Miyazaki_rmse_total", "mfts_Kagoshima_rmse_total", "mfts_Okinawa_rmse_total")

# Define variables for region level training residuals

mfts_region_train_residual_female = c("mfts_region_R1_train_residual_female", "mfts_region_R2_train_residual_female", "mfts_region_R3_train_residual_female", 
                                     "mfts_region_R4_train_residual_female", "mfts_region_R5_train_residual_female", "mfts_region_R6_train_residual_female", 
                                     "mfts_region_R7_train_residual_female", "mfts_region_R8_train_residual_female")

mfts_region_train_residual_male = c("mfts_region_R1_train_residual_male", "mfts_region_R2_train_residual_male", "mfts_region_R3_train_residual_male", 
                                   "mfts_region_R4_train_residual_male", "mfts_region_R5_train_residual_male", "mfts_region_R6_train_residual_male", 
                                   "mfts_region_R7_train_residual_male", "mfts_region_R8_train_residual_male")

mfts_region_train_residual_total = c("mfts_region_R1_train_residual_total", "mfts_region_R2_train_residual_total", "mfts_region_R3_train_residual_total", 
                                    "mfts_region_R4_train_residual_total", "mfts_region_R5_train_residual_total", "mfts_region_R6_train_residual_total", 
                                    "mfts_region_R7_train_residual_total", "mfts_region_R8_train_residual_total")


# Define variables for region level forecasts

mfts_region_forc_female = c("mfts_region_R1_forc_female", "mfts_region_R2_forc_female", "mfts_region_R3_forc_female", 
                            "mfts_region_R4_forc_female", "mfts_region_R5_forc_female", "mfts_region_R6_forc_female", 
                            "mfts_region_R7_forc_female", "mfts_region_R8_forc_female")

mfts_region_forc_male = c("mfts_region_R1_forc_male", "mfts_region_R2_forc_male", "mfts_region_R3_forc_male", 
                          "mfts_region_R4_forc_male", "mfts_region_R5_forc_male", "mfts_region_R6_forc_male", 
                          "mfts_region_R7_forc_male", "mfts_region_R8_forc_male")

mfts_region_forc_total = c("mfts_region_R1_forc_total", "mfts_region_R2_forc_total", "mfts_region_R3_forc_total", 
                           "mfts_region_R4_forc_total", "mfts_region_R5_forc_total", "mfts_region_R6_forc_total", 
                           "mfts_region_R7_forc_total", "mfts_region_R8_forc_total")

# Mean Error of region forecasts

mfts_region_me_female = c("mfts_region_R1_me_female", "mfts_region_R2_me_female", "mfts_region_R3_me_female", 
                          "mfts_region_R4_me_female", "mfts_region_R5_me_female", "mfts_region_R6_me_female", 
                          "mfts_region_R7_me_female", "mfts_region_R8_me_female")

mfts_region_me_male = c("mfts_region_R1_me_male", "mfts_region_R2_me_male", "mfts_region_R3_me_male", 
                        "mfts_region_R4_me_male", "mfts_region_R5_me_male", "mfts_region_R6_me_male", 
                        "mfts_region_R7_me_male", "mfts_region_R8_me_male")

mfts_region_me_total = c("mfts_region_R1_me_total", "mfts_region_R2_me_total", "mfts_region_R3_me_total", 
                         "mfts_region_R4_me_total", "mfts_region_R5_me_total", "mfts_region_R6_me_total", 
                         "mfts_region_R7_me_total", "mfts_region_R8_me_total")

# Mean Absolute Error of region forecasts

mfts_region_mae_female = c("mfts_region_R1_mae_female", "mfts_region_R2_mae_female", "mfts_region_R3_mae_female", 
                           "mfts_region_R4_mae_female", "mfts_region_R5_mae_female", "mfts_region_R6_mae_female", 
                           "mfts_region_R7_mae_female", "mfts_region_R8_mae_female")

mfts_region_mae_male = c("mfts_region_R1_mae_male", "mfts_region_R2_mae_male", "mfts_region_R3_mae_male", 
                         "mfts_region_R4_mae_male", "mfts_region_R5_mae_male", "mfts_region_R6_mae_male", 
                         "mfts_region_R7_mae_male", "mfts_region_R8_mae_male")

mfts_region_mae_total = c("mfts_region_R1_mae_total", "mfts_region_R2_mae_total", "mfts_region_R3_mae_total", 
                          "mfts_region_R4_mae_total", "mfts_region_R5_mae_total", "mfts_region_R6_mae_total", 
                          "mfts_region_R7_mae_total", "mfts_region_R8_mae_total")

# Root Mean Square Error of region forecasts

mfts_region_rmse_female = c("mfts_region_R1_rmse_female", "mfts_region_R2_rmse_female", "mfts_region_R3_rmse_female", 
                            "mfts_region_R4_rmse_female", "mfts_region_R5_rmse_female", "mfts_region_R6_rmse_female", 
                            "mfts_region_R7_rmse_female", "mfts_region_R8_rmse_female")

mfts_region_rmse_male = c("mfts_region_R1_rmse_male", "mfts_region_R2_rmse_male", "mfts_region_R3_rmse_male", 
                          "mfts_region_R4_rmse_male", "mfts_region_R5_rmse_male", "mfts_region_R6_rmse_male", 
                          "mfts_region_R7_rmse_male", "mfts_region_R8_rmse_male")

mfts_region_rmse_total = c("mfts_region_R1_rmse_total", "mfts_region_R2_rmse_total", "mfts_region_R3_rmse_total", 
                           "mfts_region_R4_rmse_total", "mfts_region_R5_rmse_total", "mfts_region_R6_rmse_total", 
                           "mfts_region_R7_rmse_total", "mfts_region_R8_rmse_total")


# Define a function for multivariate FTS forecasting

mfts <- function(dat, pcamethod = c("static", "dynamic"), year_horizon)
{
  n = length(names(dat$rate))
  # if the data contains female, male and total, then we consider only female and male
  if(n == 3)
  {
    n_pop = 2
  }else
  {
    n_pop = n
  }
  
  rowmeans_object = sd_object = decenter_object = list()
  for(ik in 1:n_pop)
  {
    # compute mean and standard deviation functions
    rowmeans_object[[ik]] = rowMeans(log(dat$rate[[ik]]), na.rm=TRUE)
    sd_object[[ik]] = apply(log(dat$rate[[ik]]), 1, sd, na.rm=TRUE)
    
    # de-center functional data
    decenter_object[[ik]] = t(scale(t(log(dat$rate[[ik]])), center = TRUE, scale = TRUE))
  }
  comb_object = do.call(rbind, decenter_object)
  
  pcamethod = match.arg(pcamethod)
  
  if (pcamethod == "static")
  {
    ncomp_comb = head(which(cumsum(ftsm(fts(1:nrow(comb_object), comb_object), order = 25)$varprop) >= 0.95), 1)
    fit_ftsm = ftsm(fts(1:nrow(comb_object), comb_object), order = ncomp_comb)
    train_residual = exp(comb_object * do.call(c,sd_object) + do.call(c, rowmeans_object)) - exp(fit_ftsm$fitted$y * do.call(c,sd_object) + do.call(c, rowmeans_object))
    fore_ftsm = forecast(fit_ftsm, h = year_horizon)
    fore_res = exp(fore_ftsm$mean$y * do.call(c,sd_object) + do.call(c, rowmeans_object))
  }
  
  
  if (pcamethod == "dynamic")
  {
    data_dum = comb_object

    C_0 = long_run_covariance_estimation(data_dum, H = 3, C0 = 3)
    eigen_decomp = eigen(C_0)
    dynamic_order = head(which(cumsum(eigen_decomp$values)/sum(eigen_decomp$values) >= 0.95),1)
    dynamic_basis = as.matrix(eigen_decomp$vectors[,1:dynamic_order])
    dynamic_scores = t(dynamic_basis) %*% data_dum
    
    train_residual = exp(comb_object * do.call(c,sd_object) + do.call(c, rowmeans_object)) - exp(dynamic_basis %*%dynamic_scores * do.call(c,sd_object) + do.call(c, rowmeans_object))
    
    # making forecasts
    scores_fit = scores_fore = list()
    fore_ftsm_dyn = matrix(NA, nrow = nrow(data_dum), ncol = year_horizon)
    
    for(ik in 1:dynamic_order)
    {
      scores_fit[[ik]] = auto.arima(dynamic_scores[ik,])
      scores_fore[[ik]] = forecast(scores_fit[[ik]], h = year_horizon)$mean
    }
    
    for(ih in 1:year_horizon)
    {
      fore_ftsm_dyn[,ih] = dynamic_basis%*% unlist(lapply(scores_fore,`[[`,ih))
    }
    
    fore_res = exp(fore_ftsm_dyn * do.call(c,sd_object) + do.call(c, rowmeans_object))
  }

  return(list(fore_res = fore_res, train_residual = train_residual))
}



# Define a function to make point forecasts at prefecture level

me   = ftsa:::me
mae  = ftsa:::mae
rmse = ftsa:::rmse

mfts_back_test <- function(iw,  pcamethod = c("static", "dynamic"), fmethod = "classical", year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(state_smooth[1])$year,1) - forecast_year
  res_male = res_female = array(NA, dim = c(year_horizon, 101, year_horizon))
  train_residual_female = train_residual_male = list()
  
  pcamethod = match.arg(pcamethod)
  
  for(ij in 1:year_horizon)
  {
    ind_dat = extract.years(get(state_smooth[iw]), years = 1975:(n_year+ij))
    fun_forc = mfts(ind_dat, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_female[[ij]] = fun_forc$train_residual[1:101,]
    train_residual_male[[ij]] =  fun_forc$train_residual[102:202,]
    
    res_female[,,ij] = t(fun_forc$fore_res[1:101,])
    res_male[,,ij]   = t(fun_forc$fore_res[102:202,])
    
  }
  # MAE & RMSE
  female_me = male_me = female_mae = male_mae = female_rmse = male_rmse = vector("numeric",year_horizon)
  for(ik in 1:year_horizon)
  {
    ## mortality rate
    
    # me 
    female_me[ik] = me(res_female[ik,,1:(forecast_year-ik)], extract.years(get(state[iw]), years = (2001+ik):2016)$rate$female)
    male_me[ik]   = me(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(state[iw]), years = (2001+ik):2016)$rate$male)
    
    # mae
    female_mae[ik] = mae(res_female[ik,,1:(forecast_year-ik)], extract.years(get(state[iw]), years = (2001+ik):2016)$rate$female)
    male_mae[ik]   = mae(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(state[iw]), years = (2001+ik):2016)$rate$male)
    
    # rmse
    female_rmse[ik] = rmse(res_female[ik,,1:(forecast_year-ik)], extract.years(get(state[iw]), years = (2001+ik):2016)$rate$female)
    male_rmse[ik]   = rmse(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(state[iw]), years = (2001+ik):2016)$rate$male)
    
  }
  return(list(res_female = res_female, res_male = res_male,
              train_residual_female = train_residual_female, train_residual_male = train_residual_male,
              female_me = female_me, male_me = male_me, 
              female_mae = female_mae, male_mae = male_mae,
              female_rmse = female_rmse, male_rmse = male_rmse))
}

# Define a function to make forecasts at region level

mfts_back_test_region <- function(iw, pcamethod = c("static", "dynamic"), year_horizon, fmethod = "classical")
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(region_smooth[1])$year,1) - forecast_year
  
  res_male = res_female = array(NA, dim = c(year_horizon, 101, year_horizon))
  
  train_residual_female = train_residual_male = list()
  
  pcamethod = match.arg(pcamethod)
  
  for(ij in 1:year_horizon)
  {
    ind_dat = extract.years(get(region_smooth[iw]), years = 1975:(n_year+ij))
    fun_forc = mfts(ind_dat, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_female[[ij]] = fun_forc$train_residual[1:101,]
    train_residual_male[[ij]] =  fun_forc$train_residual[102:202,]
    
    res_female[,,ij] = t(fun_forc$fore_res[1:101,])
    res_male[,,ij]   = t(fun_forc$fore_res[102:202,])
    
  }
  # MAE
  female_me = male_me = female_mae = male_mae = female_rmse = male_rmse = 
    e0_female_me = e0_male_me = e0_female_mae = e0_male_mae = e0_female_rmse = e0_male_rmse = vector("numeric", year_horizon)
  for(ik in 1:year_horizon)
  {
    # mortality rate
    female_me[ik] = me(res_female[ik,,1:(forecast_year-ik)], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
    male_me[ik]   = me(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    
    female_mae[ik] = mae(res_female[ik,,1:(forecast_year-ik)], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
    male_mae[ik]   = mae(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    
    female_rmse[ik] = rmse(res_female[ik,,1:(forecast_year-ik)], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$female)
    male_rmse[ik]   = rmse(res_male[ik,,1:(forecast_year-ik)],   extract.years(get(region[iw]), years = (2001+ik):2016)$rate$male)
    
  }
  return(list(res_female = res_female, res_male = res_male,
              train_residual_female = train_residual_female, train_residual_male = train_residual_male,
              female_me = female_me, male_me = male_me,
              female_mae = female_mae, male_mae = male_mae,
              female_rmse = female_rmse, male_rmse = male_rmse))
}

#######################################################################
# Multivariate forecast of age-specific mortality rates by prefectures
#######################################################################

# Compute point forecasts for Japan and all prefectures

for(ik in 1:48)
{
  dum = mfts_back_test(iw = ik, pcamethod = c("dynamic"), fmethod = "classical", year_horizon = 15)
  
  ## mortality rate
  assign(mfts_state_forc_female[ik], dum$res_female)
  assign(mfts_state_forc_male[ik],   dum$res_male)
  
  assign(mfts_state_train_residual_female[ik], dum$train_residual_female)
  assign(mfts_state_train_residual_male[ik], dum$train_residual_male)
  
  assign(mfts_state_me_female[ik], dum$female_me)
  assign(mfts_state_me_male[ik],   dum$male_me)
  
  assign(mfts_state_mae_female[ik], dum$female_mae)
  assign(mfts_state_mae_male[ik],   dum$male_mae)
  
  assign(mfts_state_rmse_female[ik], dum$female_rmse)
  assign(mfts_state_rmse_male[ik],   dum$male_rmse)
  
  rm(dum)
}

###################################################################
# Multivariate forecast of age-specific mortality rates by regions
###################################################################

# Compute point forecasts for all regions

for(ik in 1:8)
{
  dum = mfts_back_test_region(iw = ik, pcamethod = c("dynamic"), year_horizon = 15)
  
  # mortality rate
  assign(mfts_region_forc_female[ik], dum$res_female)
  assign(mfts_region_forc_male[ik],   dum$res_male)
  
  assign(mfts_region_train_residual_female[ik], dum$train_residual_female)
  assign(mfts_region_train_residual_male[ik], dum$train_residual_male)
  
  assign(mfts_region_me_female[ik], dum$female_me)
  assign(mfts_region_me_male[ik],   dum$male_me)
  
  assign(mfts_region_mae_female[ik], dum$female_mae)
  assign(mfts_region_mae_male[ik],   dum$male_mae)
  
  assign(mfts_region_rmse_female[ik], dum$female_rmse)
  assign(mfts_region_rmse_male[ik],   dum$male_rmse)
  
}

############################################################################
# Multivariate forecast of age-specific mortality rates of prefecture total
############################################################################

# Define a function to make forecasts for prefecture total series  

mfts_back_test_total <- function(fmethod = "classical", pcamethod = c("static", "dynamic"), year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(region_smooth[1])$year,1) - forecast_year
  
  total_comb = total_comb_pop =  matrix(NA, 101*42, 47)
  for(iw in 2:48)
  {
    total_comb[,iw-1] = as.numeric(get(state_smooth[iw])$rate$total)
    total_comb_pop[,iw-1] = as.numeric(get(state_smooth[iw])$pop$total)
  }
  total_comb_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_comb)
  total_comb_pop_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_comb_pop)
  colnames(total_comb_v2) = colnames(total_comb_pop_v2) = c("Year", "Age", state[2:48])
  
  write.table(total_comb_v2, file = "total_comb.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  write.table(total_comb_pop_v2, file = "total_comb_pop.txt", quote = FALSE, row.names = TRUE, col.names=TRUE)
  
  total_comb_demogdata = read.demogdata("total_comb.txt", "total_comb_pop.txt", 
                                        type = "mortality", label="total_comb", skip = 0)
  
  res_prefecture = array(NA, dim = c(year_horizon, 101, year_horizon, 47))
  train_residual_total = list()
  for(ij in 1:year_horizon)
  {
    ind_dat = extract.years(total_comb_demogdata, years = 1975:(n_year+ij))
    
    pcamethod = match.arg(pcamethod)
    fun_forc = mfts(ind_dat, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_total[[ij]] = array(NA, dim = c(101, (26+ij), 47))
    for(iwk in 1:47)
    {
      res_prefecture[,,ij,iwk] = t(fun_forc$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_total[[ij]][,,iwk] = fun_forc$train_residual[(101*(iwk-1)+1):(101*iwk),]
    }

  }
  
  # MAE & RMSE
  total_prefecture_me = total_prefecture_mae = total_prefecture_rmse = matrix(NA,year_horizon,47)
  for(iw in 1:47)
  {
    for(ik in 1:year_horizon)
    {
      # me
      total_prefecture_me[ik,iw] = me(res_prefecture[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$total)
      
      # mae
      total_prefecture_mae[ik,iw] = mae(res_prefecture[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$total)
      
      # rmse
      total_prefecture_rmse[ik,iw] = rmse(res_prefecture[ik,,1:(forecast_year-ik),iw], extract.years(get(state[iw+1]), years = (2001+ik):2016)$rate$total)
    }
  }
  return(list(res_prefecture = res_prefecture, total_prefecture_me = total_prefecture_me, total_prefecture_mae = total_prefecture_mae,
              total_prefecture_rmse = total_prefecture_rmse, train_residual_total = train_residual_total))
}

# Compute forecasts of total series at prefecture total

dum_state = mfts_back_test_total(pcamethod = c("dynamic"), year_horizon = 15)
mfts_state_me_total = dum_state$total_prefecture_me
mfts_state_mae_total = dum_state$total_prefecture_mae
mfts_state_rmse_total = dum_state$total_prefecture_rmse
res_prefecture = dum_state$res_prefecture
mfts_state_residual = dum_state$train_residual_total
colnames(mfts_state_me_total) = colnames(mfts_state_mae_total) = colnames(mfts_state_rmse_total) = state[2:48]

for(ik in 1:47)
{
  assign(mfts_state_forc_total[ik+1], res_prefecture[,,,ik])
}

for(ik in 1:47)
{
  tmp = list()
  for(ih in 1:15)
  {
    tmp[[ih]] = mfts_state_residual[[ih]][,,ik]
  }
  assign(mfts_state_train_residual_total[ik+1], tmp)
}

# Independent and multivariate forecasts are the same for the national total series.
assign(mfts_state_forc_total[1], ind_dynamic_Japan_forc_total)
assign(mfts_state_train_residual_total[1], ind_dynamic_Japan_train_residual_total)

########################################################################
# Multivariate forecast of age-specific mortality rates of region total
########################################################################

# Define a function to make forecasts for region total series  

mfts_back_test_region_total = function(fmethod = "classical", pcamethod = c("static", "dynamic"), year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(region_smooth[1])$year,1) - forecast_year
  
  total_region_comb = total_region_comb_pop = matrix(NA, 101*42, 8)
  for(iw in 1:8)
  {
    total_region_comb[,iw] = as.numeric(get(region_smooth[iw])$rate$total)
    total_region_comb_pop[,iw] = as.numeric(get(region_smooth[iw])$pop$total)
  }
  total_region_comb_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_region_comb)
  total_region_comb_pop_v2 = cbind(rep(1975:2016, each=101), rep(0:100, 42), total_region_comb_pop)
  colnames(total_region_comb_v2) = colnames(total_region_comb_pop_v2) = c("Year", "Age", region)
  
  write.table(total_region_comb_v2, file = "total_region_comb.txt", quote = FALSE, row.names = TRUE,
              col.names = TRUE)  
  write.table(total_region_comb_pop_v2, file = "total_region_comb_pop.txt", quote = FALSE, row.names = TRUE,
              col.names = TRUE)
  
  total_region_comb_demogdata = read.demogdata("total_region_comb.txt", "total_region_comb_pop.txt",
                                               type = "mortality", label = "total_region_comb", skip = 0)
  
  res_region = array(NA, dim = c(year_horizon, 101, year_horizon, 8))
  train_residual_total = list()
  for(ij in 1:year_horizon)
  {
    ind_dat = extract.years(total_region_comb_demogdata, years = 1975:(n_year+ij))
    pcamethod = match.arg(pcamethod)
    fun_forc = mfts(ind_dat, pcamethod = pcamethod, year_horizon = year_horizon)
    
    train_residual_total[[ij]] = array(NA, dim = c(101, (26+ij), 8))
    for(iwk in 1:8)
    {
      res_region[,,ij,iwk] = t(fun_forc$fore_res[(101*(iwk-1)+1):(101*iwk),])
      train_residual_total[[ij]][,,iwk] = fun_forc$train_residual[(101*(iwk-1)+1):(101*iwk),]
    }
  }
  # Errors
  total_region_me = total_region_mae = total_region_rmse = matrix(NA, year_horizon, 8)
  for(iw in 1:8)
  {
    for(ik in 1:year_horizon)
    {
      total_region_me[ik,iw] = me(res_region[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
      total_region_mae[ik,iw] = mae(res_region[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
      total_region_rmse[ik,iw] = rmse(res_region[ik,,1:(forecast_year-ik),iw], extract.years(get(region[iw]), years = (2001+ik):2016)$rate$total)
      
    }
  }
  return(list(res_region = res_region, 
              train_residual_total = train_residual_total,
              total_region_me = total_region_me, 
              total_region_mae = total_region_mae,
              total_region_rmse = total_region_rmse))    
}

# Compute forecasts of total series at region total

dum_region = mfts_back_test_region_total(pcamethod = "dynamic", year_horizon = 15)
mfts_region_me_total = dum_region$total_region_me
mfts_region_mae_total = dum_region$total_region_mae
mfts_region_rmse_total = dum_region$total_region_rmse
mfts_region_residual = dum_region$train_residual_total
colnames(mfts_region_me_total) = colnames(mfts_region_rmse_total) = region

for(ik in 1:8)
{
  assign(mfts_region_forc_total[ik], dum_region$res_region[,,,ik])
  
}

for(ik in 1:8)
{
  tmp = list()
  for(ih in 1:15)
  {
    tmp[[ih]] = mfts_region_residual[[ih]][,,ik]
  }
  assign(mfts_region_train_residual_total[ik], tmp)
}


#######################################
# Calculation of point forecast errors
#######################################

#  prefecture level

mfts_me_female_mean_overall = mfts_me_male_mean_overall =
  mfts_mae_female_mean_overall = mfts_mae_male_mean_overall = 
  mfts_rmse_female_mean_overall = mfts_rmse_male_mean_overall = NULL
for(ik in 1:48)
{
  # me
  mfts_me_female_mean_overall  = cbind(mfts_me_female_mean_overall, get(mfts_state_me_female[ik]))
  mfts_me_male_mean_overall    = cbind(mfts_me_male_mean_overall,   get(mfts_state_me_male[ik]))
  
  # me
  mfts_mae_female_mean_overall  = cbind(mfts_mae_female_mean_overall, get(mfts_state_mae_female[ik]))
  mfts_mae_male_mean_overall    = cbind(mfts_mae_male_mean_overall,   get(mfts_state_mae_male[ik]))
  
  # rmse
  mfts_rmse_female_mean_overall  = cbind(mfts_rmse_female_mean_overall, get(mfts_state_rmse_female[ik]))
  mfts_rmse_male_mean_overall    = cbind(mfts_rmse_male_mean_overall,   get(mfts_state_rmse_male[ik]))
}

# take an average

rowmeans_mfts_me_female_mean_overall = apply(mfts_me_female_mean_overall[,2:48], 1, mean)
rowmeans_mfts_me_male_mean_overall   = apply(mfts_me_male_mean_overall[,2:48], 1, mean)
rowmeans_mfts_me_total_mean_overall  = apply(mfts_state_me_total, 1, mean)

rowmeans_mfts_mae_female_mean_overall = apply(mfts_mae_female_mean_overall[,2:48], 1, mean)
rowmeans_mfts_mae_male_mean_overall   = apply(mfts_mae_male_mean_overall[,2:48], 1, mean)
rowmeans_mfts_mae_total_mean_overall  = apply(mfts_state_mae_total, 1, mean)

rowmeans_mfts_rmse_female_mean_overall = apply(mfts_rmse_female_mean_overall[,2:48], 1, mean)
rowmeans_mfts_rmse_male_mean_overall   = apply(mfts_rmse_male_mean_overall[,2:48], 1, mean)
rowmeans_mfts_rmse_total_mean_overall  = apply(mfts_state_rmse_total, 1, mean)

# region level

mfts_region_me_female_mean_overall  = mfts_region_me_male_mean_overall  =  
  mfts_region_mae_female_mean_overall  = mfts_region_mae_male_mean_overall  = 
  mfts_region_rmse_female_mean_overall = mfts_region_rmse_male_mean_overall = NULL
for(ik in 1:8)
{
  # me
  mfts_region_me_female_mean_overall = cbind(mfts_region_me_female_mean_overall, get(mfts_region_me_female[ik]))
  mfts_region_me_male_mean_overall   = cbind(mfts_region_me_male_mean_overall,   get(mfts_region_me_male[ik]))
  
  # mae
  mfts_region_mae_female_mean_overall = cbind(mfts_region_mae_female_mean_overall, get(mfts_region_mae_female[ik]))
  mfts_region_mae_male_mean_overall   = cbind(mfts_region_mae_male_mean_overall,   get(mfts_region_mae_male[ik]))
  
  # rmse
  mfts_region_rmse_female_mean_overall = cbind(mfts_region_rmse_female_mean_overall, get(mfts_region_rmse_female[ik]))
  mfts_region_rmse_male_mean_overall   = cbind(mfts_region_rmse_male_mean_overall,   get(mfts_region_rmse_male[ik]))
}

# take an average

rowmeans_mfts_region_me_female_mean_overall = apply(mfts_region_me_female_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_me_male_mean_overall   = apply(mfts_region_me_male_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_me_total_mean_overall  = apply(mfts_region_me_total, 1, mean)

rowmeans_mfts_region_mae_female_mean_overall = apply(mfts_region_mae_female_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_mae_male_mean_overall   = apply(mfts_region_mae_male_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_mae_total_mean_overall  = apply(mfts_region_mae_total, 1, mean)

rowmeans_mfts_region_rmse_female_mean_overall = apply(mfts_region_rmse_female_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_rmse_male_mean_overall   = apply(mfts_region_rmse_male_mean_overall[,1:8], 1, mean)
rowmeans_mfts_region_rmse_total_mean_overall  = apply(mfts_region_rmse_total, 1, mean)

#####################
# Summary of results
#####################

# Mean Errors

mfts_me_total_mean_overall =  ind_dynamic_Japan_me_total 

mfts_all_level_err_me = cbind(mfts_me_total_mean_overall, 
                              apply(cbind(mfts_me_female_mean_overall[,1], mfts_me_male_mean_overall[,1]), 1,mean),
                              apply(mfts_region_me_total, 1, mean), 
                              apply(cbind(mfts_region_me_female_mean_overall, mfts_region_me_male_mean_overall),1,mean),
                              rowmeans_mfts_me_total_mean_overall, 
                              apply(cbind(rowmeans_mfts_me_female_mean_overall, rowmeans_mfts_me_male_mean_overall),1,mean))

mfts_all_level_err_me_all = rbind(mfts_all_level_err_me, colMeans(mfts_all_level_err_me), apply(mfts_all_level_err_me, 2, median))

# Mean Absolute Errors

mfts_mae_total_mean_overall =  ind_dynamic_Japan_mae_total 

mfts_all_level_err_mae = cbind(mfts_mae_total_mean_overall, 
                               apply(cbind(mfts_mae_female_mean_overall[,1], mfts_mae_male_mean_overall[,1]), 1,mean),
                               apply(mfts_region_mae_total, 1, mean), 
                               apply(cbind(mfts_region_mae_female_mean_overall, mfts_region_mae_male_mean_overall),1,mean),
                               rowmeans_mfts_mae_total_mean_overall, 
                               apply(cbind(rowmeans_mfts_mae_female_mean_overall, rowmeans_mfts_mae_male_mean_overall),1,mean))

mfts_all_level_err_mae_all = rbind(mfts_all_level_err_mae, colMeans(mfts_all_level_err_mae), apply(mfts_all_level_err_mae, 2, median))

# Root Mean Square Errors

mfts_rmse_total_mean_overall = ind_dynamic_Japan_rmse_total 

mfts_all_level_err_rmse = cbind(mfts_rmse_total_mean_overall, 
                                apply(cbind(mfts_rmse_female_mean_overall[,1], mfts_rmse_male_mean_overall[,1]), 1,mean),
                                apply(mfts_region_rmse_total, 1, mean), 
                                apply(cbind(mfts_region_rmse_female_mean_overall, mfts_region_rmse_male_mean_overall),1,mean),
                                rowmeans_mfts_rmse_total_mean_overall, 
                                apply(cbind(rowmeans_mfts_rmse_female_mean_overall, rowmeans_mfts_rmse_male_mean_overall),1,mean))

mfts_all_level_err_rmse_all = rbind(mfts_all_level_err_rmse, colMeans(mfts_all_level_err_rmse), apply(mfts_all_level_err_rmse, 2, median))

colnames(mfts_all_level_err_me) = colnames(mfts_all_level_err_mae) = colnames(mfts_all_level_err_rmse) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
colnames(mfts_all_level_err_me_all) = colnames(mfts_all_level_err_mae_all) = colnames(mfts_all_level_err_rmse_all) = c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
rownames(mfts_all_level_err_me_all) = rownames(mfts_all_level_err_mae_all) = rownames(mfts_all_level_err_rmse_all) = c(1:15, "Mean", "Median")

