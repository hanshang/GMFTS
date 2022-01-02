#######################################################################
# Univariate functional time series point forecast (benchmark method)
#######################################################################

require(demography)
require(ftsa)

# define variables for prefecture level forecasts

ind_dynamic_state_train_residual_female = c("ind_dynamic_Japan_train_residual_female", "ind_dynamic_Hokkaido_train_residual_female", "ind_dynamic_Aomori_train_residual_female", 
                                    "ind_dynamic_Iwate_train_residual_female", "ind_dynamic_Miyagi_train_residual_female", "ind_dynamic_Akita_train_residual_female", 
                                    "ind_dynamic_Yamagata_train_residual_female", "ind_dynamic_Fukushima_train_residual_female", "ind_dynamic_Ibaraki_train_residual_female", 
                                    "ind_dynamic_Tochigi_train_residual_female", "ind_dynamic_Gunma_train_residual_female", "ind_dynamic_Saitama_train_residual_female", 
                                    "ind_dynamic_Chiba_train_residual_female", "ind_dynamic_Tokyo_train_residual_female", "ind_dynamic_Kanagawa_train_residual_female",
                                    "ind_dynamic_Niigata_train_residual_female", "ind_dynamic_Toyama_train_residual_female", "ind_dynamic_Ishikawa_train_residual_female", 
                                    "ind_dynamic_Fukui_train_residual_female", "ind_dynamic_Yamanashi_train_residual_female", "ind_dynamic_Nagano_train_residual_female", 
                                    "ind_dynamic_Gifu_train_residual_female", "ind_dynamic_Shizuoka_train_residual_female", "ind_dynamic_Aichi_train_residual_female",
                                    "ind_dynamic_Mie_train_residual_female", "ind_dynamic_Shiga_train_residual_female", "ind_dynamic_Kyoto_train_residual_female", 
                                    "ind_dynamic_Osaka_train_residual_female", "ind_dynamic_Hyogo_train_residual_female", "ind_dynamic_Nara_train_residual_female", 
                                    "ind_dynamic_Wakayama_train_residual_female", "ind_dynamic_Tottori_train_residual_female", "ind_dynamic_Shimane_train_residual_female",
                                    "ind_dynamic_Okayama_train_residual_female", "ind_dynamic_Hiroshima_train_residual_female", "ind_dynamic_Yamaguchi_train_residual_female", 
                                    "ind_dynamic_Tokushima_train_residual_female", "ind_dynamic_Kagawa_train_residual_female", "ind_dynamic_Ehime_train_residual_female", 
                                    "ind_dynamic_Kochi_train_residual_female", "ind_dynamic_Fukuoka_train_residual_female", "ind_dynamic_Saga_train_residual_female", 
                                    "ind_dynamic_Nagasaki_train_residual_female", "ind_dynamic_Kumamoto_train_residual_female", "ind_dynamic_Oita_train_residual_female", 
                                    "ind_dynamic_Miyazaki_train_residual_female", "ind_dynamic_Kagoshima_train_residual_female", "ind_dynamic_Okinawa_train_residual_female")

ind_dynamic_state_train_residual_male = c("ind_dynamic_Japan_train_residual_male", "ind_dynamic_Hokkaido_train_residual_male", "ind_dynamic_Aomori_train_residual_male", 
                                  "ind_dynamic_Iwate_train_residual_male", "ind_dynamic_Miyagi_train_residual_male", "ind_dynamic_Akita_train_residual_male", 
                                  "ind_dynamic_Yamagata_train_residual_male", "ind_dynamic_Fukushima_train_residual_male", "ind_dynamic_Ibaraki_train_residual_male", 
                                  "ind_dynamic_Tochigi_train_residual_male", "ind_dynamic_Gunma_train_residual_male", "ind_dynamic_Saitama_train_residual_male", 
                                  "ind_dynamic_Chiba_train_residual_male", "ind_dynamic_Tokyo_train_residual_male", "ind_dynamic_Kanagawa_train_residual_male",
                                  "ind_dynamic_Niigata_train_residual_male", "ind_dynamic_Toyama_train_residual_male", "ind_dynamic_Ishikawa_train_residual_male", 
                                  "ind_dynamic_Fukui_train_residual_male", "ind_dynamic_Yamanashi_train_residual_male", "ind_dynamic_Nagano_train_residual_male", 
                                  "ind_dynamic_Gifu_train_residual_male", "ind_dynamic_Shizuoka_train_residual_male", "ind_dynamic_Aichi_train_residual_male",
                                  "ind_dynamic_Mie_train_residual_male", "ind_dynamic_Shiga_train_residual_male", "ind_dynamic_Kyoto_train_residual_male", 
                                  "ind_dynamic_Osaka_train_residual_male", "ind_dynamic_Hyogo_train_residual_male", "ind_dynamic_Nara_train_residual_male", 
                                  "ind_dynamic_Wakayama_train_residual_male", "ind_dynamic_Tottori_train_residual_male", "ind_dynamic_Shimane_train_residual_male",
                                  "ind_dynamic_Okayama_train_residual_male", "ind_dynamic_Hiroshima_train_residual_male", "ind_dynamic_Yamaguchi_train_residual_male", 
                                  "ind_dynamic_Tokushima_train_residual_male", "ind_dynamic_Kagawa_train_residual_male", "ind_dynamic_Ehime_train_residual_male", 
                                  "ind_dynamic_Kochi_train_residual_male", "ind_dynamic_Fukuoka_train_residual_male", "ind_dynamic_Saga_train_residual_male", 
                                  "ind_dynamic_Nagasaki_train_residual_male", "ind_dynamic_Kumamoto_train_residual_male", "ind_dynamic_Oita_train_residual_male", 
                                  "ind_dynamic_Miyazaki_train_residual_male", "ind_dynamic_Kagoshima_train_residual_male", "ind_dynamic_Okinawa_train_residual_male")

ind_dynamic_state_train_residual_total = c("ind_dynamic_Japan_train_residual_total", "ind_dynamic_Hokkaido_train_residual_total", "ind_dynamic_Aomori_train_residual_total", 
                                   "ind_dynamic_Iwate_train_residual_total", "ind_dynamic_Miyagi_train_residual_total", "ind_dynamic_Akita_train_residual_total", 
                                   "ind_dynamic_Yamagata_train_residual_total", "ind_dynamic_Fukushima_train_residual_total", "ind_dynamic_Ibaraki_train_residual_total", 
                                   "ind_dynamic_Tochigi_train_residual_total", "ind_dynamic_Gunma_train_residual_total", "ind_dynamic_Saitama_train_residual_total", 
                                   "ind_dynamic_Chiba_train_residual_total", "ind_dynamic_Tokyo_train_residual_total", "ind_dynamic_Kanagawa_train_residual_total",
                                   "ind_dynamic_Niigata_train_residual_total", "ind_dynamic_Toyama_train_residual_total", "ind_dynamic_Ishikawa_train_residual_total", 
                                   "ind_dynamic_Fukui_train_residual_total", "ind_dynamic_Yamanashi_train_residual_total", "ind_dynamic_Nagano_train_residual_total", 
                                   "ind_dynamic_Gifu_train_residual_total", "ind_dynamic_Shizuoka_train_residual_total", "ind_dynamic_Aichi_train_residual_total",
                                   "ind_dynamic_Mie_train_residual_total", "ind_dynamic_Shiga_train_residual_total", "ind_dynamic_Kyoto_train_residual_total", 
                                   "ind_dynamic_Osaka_train_residual_total", "ind_dynamic_Hyogo_train_residual_total", "ind_dynamic_Nara_train_residual_total", 
                                   "ind_dynamic_Wakayama_train_residual_total", "ind_dynamic_Tottori_train_residual_total", "ind_dynamic_Shimane_train_residual_total",
                                   "ind_dynamic_Okayama_train_residual_total", "ind_dynamic_Hiroshima_train_residual_total", "ind_dynamic_Yamaguchi_train_residual_total", 
                                   "ind_dynamic_Tokushima_train_residual_total", "ind_dynamic_Kagawa_train_residual_total", "ind_dynamic_Ehime_train_residual_total", 
                                   "ind_dynamic_Kochi_train_residual_total", "ind_dynamic_Fukuoka_train_residual_total", "ind_dynamic_Saga_train_residual_total", 
                                   "ind_dynamic_Nagasaki_train_residual_total", "ind_dynamic_Kumamoto_train_residual_total", "ind_dynamic_Oita_train_residual_total", 
                                   "ind_dynamic_Miyazaki_train_residual_total", "ind_dynamic_Kagoshima_train_residual_total", "ind_dynamic_Okinawa_train_residual_total")


ind_dynamic_state_forc_female = c("ind_dynamic_Japan_forc_female", "ind_dynamic_Hokkaido_forc_female", "ind_dynamic_Aomori_forc_female", 
                          "ind_dynamic_Iwate_forc_female", "ind_dynamic_Miyagi_forc_female", "ind_dynamic_Akita_forc_female", 
                          "ind_dynamic_Yamagata_forc_female", "ind_dynamic_Fukushima_forc_female", "ind_dynamic_Ibaraki_forc_female", 
                          "ind_dynamic_Tochigi_forc_female", "ind_dynamic_Gunma_forc_female", "ind_dynamic_Saitama_forc_female", 
                          "ind_dynamic_Chiba_forc_female", "ind_dynamic_Tokyo_forc_female", "ind_dynamic_Kanagawa_forc_female",
                          "ind_dynamic_Niigata_forc_female", "ind_dynamic_Toyama_forc_female", "ind_dynamic_Ishikawa_forc_female", 
                          "ind_dynamic_Fukui_forc_female", "ind_dynamic_Yamanashi_forc_female", "ind_dynamic_Nagano_forc_female", 
                          "ind_dynamic_Gifu_forc_female", "ind_dynamic_Shizuoka_forc_female", "ind_dynamic_Aichi_forc_female",
                          "ind_dynamic_Mie_forc_female", "ind_dynamic_Shiga_forc_female", "ind_dynamic_Kyoto_forc_female", 
                          "ind_dynamic_Osaka_forc_female", "ind_dynamic_Hyogo_forc_female", "ind_dynamic_Nara_forc_female", 
                          "ind_dynamic_Wakayama_forc_female", "ind_dynamic_Tottori_forc_female", "ind_dynamic_Shimane_forc_female",
                          "ind_dynamic_Okayama_forc_female", "ind_dynamic_Hiroshima_forc_female", "ind_dynamic_Yamaguchi_forc_female", 
                          "ind_dynamic_Tokushima_forc_female", "ind_dynamic_Kagawa_forc_female", "ind_dynamic_Ehime_forc_female", 
                          "ind_dynamic_Kochi_forc_female", "ind_dynamic_Fukuoka_forc_female", "ind_dynamic_Saga_forc_female", 
                          "ind_dynamic_Nagasaki_forc_female", "ind_dynamic_Kumamoto_forc_female", "ind_dynamic_Oita_forc_female", 
                          "ind_dynamic_Miyazaki_forc_female", "ind_dynamic_Kagoshima_forc_female", "ind_dynamic_Okinawa_forc_female")

ind_dynamic_state_forc_male = c("ind_dynamic_Japan_forc_male", "ind_dynamic_Hokkaido_forc_male", "ind_dynamic_Aomori_forc_male", 
                        "ind_dynamic_Iwate_forc_male", "ind_dynamic_Miyagi_forc_male", "ind_dynamic_Akita_forc_male", 
                        "ind_dynamic_Yamagata_forc_male", "ind_dynamic_Fukushima_forc_male", "ind_dynamic_Ibaraki_forc_male", 
                        "ind_dynamic_Tochigi_forc_male", "ind_dynamic_Gunma_forc_male", "ind_dynamic_Saitama_forc_male", 
                        "ind_dynamic_Chiba_forc_male", "ind_dynamic_Tokyo_forc_male", "ind_dynamic_Kanagawa_forc_male",
                        "ind_dynamic_Niigata_forc_male", "ind_dynamic_Toyama_forc_male", "ind_dynamic_Ishikawa_forc_male", 
                        "ind_dynamic_Fukui_forc_male", "ind_dynamic_Yamanashi_forc_male", "ind_dynamic_Nagano_forc_male", 
                        "ind_dynamic_Gifu_forc_male", "ind_dynamic_Shizuoka_forc_male", "ind_dynamic_Aichi_forc_male",
                        "ind_dynamic_Mie_forc_male", "ind_dynamic_Shiga_forc_male", "ind_dynamic_Kyoto_forc_male", 
                        "ind_dynamic_Osaka_forc_male", "ind_dynamic_Hyogo_forc_male", "ind_dynamic_Nara_forc_male", 
                        "ind_dynamic_Wakayama_forc_male", "ind_dynamic_Tottori_forc_male", "ind_dynamic_Shimane_forc_male",
                        "ind_dynamic_Okayama_forc_male", "ind_dynamic_Hiroshima_forc_male", "ind_dynamic_Yamaguchi_forc_male", 
                        "ind_dynamic_Tokushima_forc_male", "ind_dynamic_Kagawa_forc_male", "ind_dynamic_Ehime_forc_male", 
                        "ind_dynamic_Kochi_forc_male", "ind_dynamic_Fukuoka_forc_male", "ind_dynamic_Saga_forc_male", 
                        "ind_dynamic_Nagasaki_forc_male", "ind_dynamic_Kumamoto_forc_male", "ind_dynamic_Oita_forc_male", 
                        "ind_dynamic_Miyazaki_forc_male", "ind_dynamic_Kagoshima_forc_male", "ind_dynamic_Okinawa_forc_male")

ind_dynamic_state_forc_total = c("ind_dynamic_Japan_forc_total", "ind_dynamic_Hokkaido_forc_total", "ind_dynamic_Aomori_forc_total", 
                         "ind_dynamic_Iwate_forc_total", "ind_dynamic_Miyagi_forc_total", "ind_dynamic_Akita_forc_total", 
                         "ind_dynamic_Yamagata_forc_total", "ind_dynamic_Fukushima_forc_total", "ind_dynamic_Ibaraki_forc_total", 
                         "ind_dynamic_Tochigi_forc_total", "ind_dynamic_Gunma_forc_total", "ind_dynamic_Saitama_forc_total", 
                         "ind_dynamic_Chiba_forc_total", "ind_dynamic_Tokyo_forc_total", "ind_dynamic_Kanagawa_forc_total",
                         "ind_dynamic_Niigata_forc_total", "ind_dynamic_Toyama_forc_total", "ind_dynamic_Ishikawa_forc_total", 
                         "ind_dynamic_Fukui_forc_total", "ind_dynamic_Yamanashi_forc_total", "ind_dynamic_Nagano_forc_total", 
                         "ind_dynamic_Gifu_forc_total", "ind_dynamic_Shizuoka_forc_total", "ind_dynamic_Aichi_forc_total",
                         "ind_dynamic_Mie_forc_total", "ind_dynamic_Shiga_forc_total", "ind_dynamic_Kyoto_forc_total", 
                         "ind_dynamic_Osaka_forc_total", "ind_dynamic_Hyogo_forc_total", "ind_dynamic_Nara_forc_total", 
                         "ind_dynamic_Wakayama_forc_total", "ind_dynamic_Tottori_forc_total", "ind_dynamic_Shimane_forc_total",
                         "ind_dynamic_Okayama_forc_total", "ind_dynamic_Hiroshima_forc_total", "ind_dynamic_Yamaguchi_forc_total", 
                         "ind_dynamic_Tokushima_forc_total", "ind_dynamic_Kagawa_forc_total", "ind_dynamic_Ehime_forc_total", 
                         "ind_dynamic_Kochi_forc_total", "ind_dynamic_Fukuoka_forc_total", "ind_dynamic_Saga_forc_total", 
                         "ind_dynamic_Nagasaki_forc_total", "ind_dynamic_Kumamoto_forc_total", "ind_dynamic_Oita_forc_total", 
                         "ind_dynamic_Miyazaki_forc_total", "ind_dynamic_Kagoshima_forc_total", "ind_dynamic_Okinawa_forc_total")

# Mean Errors at prefecture level

ind_dynamic_state_me_female = c("ind_dynamic_Japan_me_female", "ind_dynamic_Hokkaido_me_female", "ind_dynamic_Aomori_me_female", 
                        "ind_dynamic_Iwate_me_female", "ind_dynamic_Miyagi_me_female", "ind_dynamic_Akita_me_female", 
                        "ind_dynamic_Yamagata_me_female", "ind_dynamic_Fukushima_me_female", "ind_dynamic_Ibaraki_me_female", 
                        "ind_dynamic_Tochigi_me_female", "ind_dynamic_Gunma_me_female", "ind_dynamic_Saitama_me_female", 
                        "ind_dynamic_Chiba_me_female", "ind_dynamic_Tokyo_me_female", "ind_dynamic_Kanagawa_me_female",
                        "ind_dynamic_Niigata_me_female", "ind_dynamic_Toyama_me_female", "ind_dynamic_Ishikawa_me_female", 
                        "ind_dynamic_Fukui_me_female", "ind_dynamic_Yamanashi_me_female", "ind_dynamic_Nagano_me_female", 
                        "ind_dynamic_Gifu_me_female", "ind_dynamic_Shizuoka_me_female", "ind_dynamic_Aichi_me_female",
                        "ind_dynamic_Mie_me_female", "ind_dynamic_Shiga_me_female", "ind_dynamic_Kyoto_me_female", 
                        "ind_dynamic_Osaka_me_female", "ind_dynamic_Hyogo_me_female", "ind_dynamic_Nara_me_female", 
                        "ind_dynamic_Wakayama_me_female", "ind_dynamic_Tottori_me_female", "ind_dynamic_Shimane_me_female",
                        "ind_dynamic_Okayama_me_female", "ind_dynamic_Hiroshima_me_female", "ind_dynamic_Yamaguchi_me_female", 
                        "ind_dynamic_Tokushima_me_female", "ind_dynamic_Kagawa_me_female", "ind_dynamic_Ehime_me_female", 
                        "ind_dynamic_Kochi_me_female", "ind_dynamic_Fukuoka_me_female", "ind_dynamic_Saga_me_female", 
                        "ind_dynamic_Nagasaki_me_female", "ind_dynamic_Kumamoto_me_female", "ind_dynamic_Oita_me_female", 
                        "ind_dynamic_Miyazaki_me_female", "ind_dynamic_Kagoshima_me_female", "ind_dynamic_Okinawa_me_female")

ind_dynamic_state_me_male = c("ind_dynamic_Japan_me_male", "ind_dynamic_Hokkaido_me_male", "ind_dynamic_Aomori_me_male", 
                      "ind_dynamic_Iwate_me_male", "ind_dynamic_Miyagi_me_male", "ind_dynamic_Akita_me_male", 
                      "ind_dynamic_Yamagata_me_male", "ind_dynamic_Fukushima_me_male", "ind_dynamic_Ibaraki_me_male", 
                      "ind_dynamic_Tochigi_me_male", "ind_dynamic_Gunma_me_male", "ind_dynamic_Saitama_me_male", 
                      "ind_dynamic_Chiba_me_male", "ind_dynamic_Tokyo_me_male", "ind_dynamic_Kanagawa_me_male",
                      "ind_dynamic_Niigata_me_male", "ind_dynamic_Toyama_me_male", "ind_dynamic_Ishikawa_me_male", 
                      "ind_dynamic_Fukui_me_male", "ind_dynamic_Yamanashi_me_male", "ind_dynamic_Nagano_me_male", 
                      "ind_dynamic_Gifu_me_male", "ind_dynamic_Shizuoka_me_male", "ind_dynamic_Aichi_me_male",
                      "ind_dynamic_Mie_me_male", "ind_dynamic_Shiga_me_male", "ind_dynamic_Kyoto_me_male", 
                      "ind_dynamic_Osaka_me_male", "ind_dynamic_Hyogo_me_male", "ind_dynamic_Nara_me_male", 
                      "ind_dynamic_Wakayama_me_male", "ind_dynamic_Tottori_me_male", "ind_dynamic_Shimane_me_male",
                      "ind_dynamic_Okayama_me_male", "ind_dynamic_Hiroshima_me_male", "ind_dynamic_Yamaguchi_me_male", 
                      "ind_dynamic_Tokushima_me_male", "ind_dynamic_Kagawa_me_male", "ind_dynamic_Ehime_me_male", 
                      "ind_dynamic_Kochi_me_male", "ind_dynamic_Fukuoka_me_male", "ind_dynamic_Saga_me_male", 
                      "ind_dynamic_Nagasaki_me_male", "ind_dynamic_Kumamoto_me_male", "ind_dynamic_Oita_me_male", 
                      "ind_dynamic_Miyazaki_me_male", "ind_dynamic_Kagoshima_me_male", "ind_dynamic_Okinawa_me_male")


ind_dynamic_state_me_total = c("ind_dynamic_Japan_me_total", "ind_dynamic_Hokkaido_me_total", "ind_dynamic_Aomori_me_total", 
                       "ind_dynamic_Iwate_me_total", "ind_dynamic_Miyagi_me_total", "ind_dynamic_Akita_me_total", 
                       "ind_dynamic_Yamagata_me_total", "ind_dynamic_Fukushima_me_total", "ind_dynamic_Ibaraki_me_total", 
                       "ind_dynamic_Tochigi_me_total", "ind_dynamic_Gunma_me_total", "ind_dynamic_Saitama_me_total", 
                       "ind_dynamic_Chiba_me_total", "ind_dynamic_Tokyo_me_total", "ind_dynamic_Kanagawa_me_total",
                       "ind_dynamic_Niigata_me_total", "ind_dynamic_Toyama_me_total", "ind_dynamic_Ishikawa_me_total", 
                       "ind_dynamic_Fukui_me_total", "ind_dynamic_Yamanashi_me_total", "ind_dynamic_Nagano_me_total", 
                       "ind_dynamic_Gifu_me_total", "ind_dynamic_Shizuoka_me_total", "ind_dynamic_Aichi_me_total",
                       "ind_dynamic_Mie_me_total", "ind_dynamic_Shiga_me_total", "ind_dynamic_Kyoto_me_total", 
                       "ind_dynamic_Osaka_me_total", "ind_dynamic_Hyogo_me_total", "ind_dynamic_Nara_me_total", 
                       "ind_dynamic_Wakayama_me_total", "ind_dynamic_Tottori_me_total", "ind_dynamic_Shimane_me_total",
                       "ind_dynamic_Okayama_me_total", "ind_dynamic_Hiroshima_me_total", "ind_dynamic_Yamaguchi_me_total", 
                       "ind_dynamic_Tokushima_me_total", "ind_dynamic_Kagawa_me_total", "ind_dynamic_Ehime_me_total", 
                       "ind_dynamic_Kochi_me_total", "ind_dynamic_Fukuoka_me_total", "ind_dynamic_Saga_me_total", 
                       "ind_dynamic_Nagasaki_me_total", "ind_dynamic_Kumamoto_me_total", "ind_dynamic_Oita_me_total", 
                       "ind_dynamic_Miyazaki_me_total", "ind_dynamic_Kagoshima_me_total", "ind_dynamic_Okinawa_me_total")


# Mean Absolute Errors at state level 

ind_dynamic_state_mae_female = c("ind_dynamic_Japan_mae_female", "ind_dynamic_Hokkaido_mae_female", "ind_dynamic_Aomori_mae_female", 
                         "ind_dynamic_Iwate_mae_female", "ind_dynamic_Miyagi_mae_female", "ind_dynamic_Akita_mae_female", 
                         "ind_dynamic_Yamagata_mae_female", "ind_dynamic_Fukushima_mae_female", "ind_dynamic_Ibaraki_mae_female", 
                         "ind_dynamic_Tochigi_mae_female", "ind_dynamic_Gunma_mae_female", "ind_dynamic_Saitama_mae_female", 
                         "ind_dynamic_Chiba_mae_female", "ind_dynamic_Tokyo_mae_female", "ind_dynamic_Kanagawa_mae_female",
                         "ind_dynamic_Niigata_mae_female", "ind_dynamic_Toyama_mae_female", "ind_dynamic_Ishikawa_mae_female", 
                         "ind_dynamic_Fukui_mae_female", "ind_dynamic_Yamanashi_mae_female", "ind_dynamic_Nagano_mae_female", 
                         "ind_dynamic_Gifu_mae_female", "ind_dynamic_Shizuoka_mae_female", "ind_dynamic_Aichi_mae_female",
                         "ind_dynamic_Mie_mae_female", "ind_dynamic_Shiga_mae_female", "ind_dynamic_Kyoto_mae_female", 
                         "ind_dynamic_Osaka_mae_female", "ind_dynamic_Hyogo_mae_female", "ind_dynamic_Nara_mae_female", 
                         "ind_dynamic_Wakayama_mae_female", "ind_dynamic_Tottori_mae_female", "ind_dynamic_Shimane_mae_female",
                         "ind_dynamic_Okayama_mae_female", "ind_dynamic_Hiroshima_mae_female", "ind_dynamic_Yamaguchi_mae_female", 
                         "ind_dynamic_Tokushima_mae_female", "ind_dynamic_Kagawa_mae_female", "ind_dynamic_Ehime_mae_female", 
                         "ind_dynamic_Kochi_mae_female", "ind_dynamic_Fukuoka_mae_female", "ind_dynamic_Saga_mae_female", 
                         "ind_dynamic_Nagasaki_mae_female", "ind_dynamic_Kumamoto_mae_female", "ind_dynamic_Oita_mae_female", 
                         "ind_dynamic_Miyazaki_mae_female", "ind_dynamic_Kagoshima_mae_female", "ind_dynamic_Okinawa_mae_female")

ind_dynamic_state_mae_male = c("ind_dynamic_Japan_mae_male", "ind_dynamic_Hokkaido_mae_male", "ind_dynamic_Aomori_mae_male", 
                       "ind_dynamic_Iwate_mae_male", "ind_dynamic_Miyagi_mae_male", "ind_dynamic_Akita_mae_male", 
                       "ind_dynamic_Yamagata_mae_male", "ind_dynamic_Fukushima_mae_male", "ind_dynamic_Ibaraki_mae_male", 
                       "ind_dynamic_Tochigi_mae_male", "ind_dynamic_Gunma_mae_male", "ind_dynamic_Saitama_mae_male", 
                       "ind_dynamic_Chiba_mae_male", "ind_dynamic_Tokyo_mae_male", "ind_dynamic_Kanagawa_mae_male",
                       "ind_dynamic_Niigata_mae_male", "ind_dynamic_Toyama_mae_male", "ind_dynamic_Ishikawa_mae_male", 
                       "ind_dynamic_Fukui_mae_male", "ind_dynamic_Yamanashi_mae_male", "ind_dynamic_Nagano_mae_male", 
                       "ind_dynamic_Gifu_mae_male", "ind_dynamic_Shizuoka_mae_male", "ind_dynamic_Aichi_mae_male",
                       "ind_dynamic_Mie_mae_male", "ind_dynamic_Shiga_mae_male", "ind_dynamic_Kyoto_mae_male", 
                       "ind_dynamic_Osaka_mae_male", "ind_dynamic_Hyogo_mae_male", "ind_dynamic_Nara_mae_male", 
                       "ind_dynamic_Wakayama_mae_male", "ind_dynamic_Tottori_mae_male", "ind_dynamic_Shimane_mae_male",
                       "ind_dynamic_Okayama_mae_male", "ind_dynamic_Hiroshima_mae_male", "ind_dynamic_Yamaguchi_mae_male", 
                       "ind_dynamic_Tokushima_mae_male", "ind_dynamic_Kagawa_mae_male", "ind_dynamic_Ehime_mae_male", 
                       "ind_dynamic_Kochi_mae_male", "ind_dynamic_Fukuoka_mae_male", "ind_dynamic_Saga_mae_male", 
                       "ind_dynamic_Nagasaki_mae_male", "ind_dynamic_Kumamoto_mae_male", "ind_dynamic_Oita_mae_male", 
                       "ind_dynamic_Miyazaki_mae_male", "ind_dynamic_Kagoshima_mae_male", "ind_dynamic_Okinawa_mae_male")


ind_dynamic_state_mae_total = c("ind_dynamic_Japan_mae_total", "ind_dynamic_Hokkaido_mae_total", "ind_dynamic_Aomori_mae_total", 
                        "ind_dynamic_Iwate_mae_total", "ind_dynamic_Miyagi_mae_total", "ind_dynamic_Akita_mae_total", 
                        "ind_dynamic_Yamagata_mae_total", "ind_dynamic_Fukushima_mae_total", "ind_dynamic_Ibaraki_mae_total", 
                        "ind_dynamic_Tochigi_mae_total", "ind_dynamic_Gunma_mae_total", "ind_dynamic_Saitama_mae_total", 
                        "ind_dynamic_Chiba_mae_total", "ind_dynamic_Tokyo_mae_total", "ind_dynamic_Kanagawa_mae_total",
                        "ind_dynamic_Niigata_mae_total", "ind_dynamic_Toyama_mae_total", "ind_dynamic_Ishikawa_mae_total", 
                        "ind_dynamic_Fukui_mae_total", "ind_dynamic_Yamanashi_mae_total", "ind_dynamic_Nagano_mae_total", 
                        "ind_dynamic_Gifu_mae_total", "ind_dynamic_Shizuoka_mae_total", "ind_dynamic_Aichi_mae_total",
                        "ind_dynamic_Mie_mae_total", "ind_dynamic_Shiga_mae_total", "ind_dynamic_Kyoto_mae_total", 
                        "ind_dynamic_Osaka_mae_total", "ind_dynamic_Hyogo_mae_total", "ind_dynamic_Nara_mae_total", 
                        "ind_dynamic_Wakayama_mae_total", "ind_dynamic_Tottori_mae_total", "ind_dynamic_Shimane_mae_total",
                        "ind_dynamic_Okayama_mae_total", "ind_dynamic_Hiroshima_mae_total", "ind_dynamic_Yamaguchi_mae_total", 
                        "ind_dynamic_Tokushima_mae_total", "ind_dynamic_Kagawa_mae_total", "ind_dynamic_Ehime_mae_total", 
                        "ind_dynamic_Kochi_mae_total", "ind_dynamic_Fukuoka_mae_total", "ind_dynamic_Saga_mae_total", 
                        "ind_dynamic_Nagasaki_mae_total", "ind_dynamic_Kumamoto_mae_total", "ind_dynamic_Oita_mae_total", 
                        "ind_dynamic_Miyazaki_mae_total", "ind_dynamic_Kagoshima_mae_total", "ind_dynamic_Okinawa_mae_total")


# Root Mean Square Errors at state level

ind_dynamic_state_rmse_female = c("ind_dynamic_Japan_rmse_female", "ind_dynamic_Hokkaido_rmse_female", "ind_dynamic_Aomori_rmse_female", 
                          "ind_dynamic_Iwate_rmse_female", "ind_dynamic_Miyagi_rmse_female", "ind_dynamic_Akita_rmse_female", 
                          "ind_dynamic_Yamagata_rmse_female", "ind_dynamic_Fukushima_rmse_female", "ind_dynamic_Ibaraki_rmse_female", 
                          "ind_dynamic_Tochigi_rmse_female", "ind_dynamic_Gunma_rmse_female", "ind_dynamic_Saitama_rmse_female", 
                          "ind_dynamic_Chiba_rmse_female", "ind_dynamic_Tokyo_rmse_female", "ind_dynamic_Kanagawa_rmse_female",
                          "ind_dynamic_Niigata_rmse_female", "ind_dynamic_Toyama_rmse_female", "ind_dynamic_Ishikawa_rmse_female", 
                          "ind_dynamic_Fukui_rmse_female", "ind_dynamic_Yamanashi_rmse_female", "ind_dynamic_Nagano_rmse_female", 
                          "ind_dynamic_Gifu_rmse_female", "ind_dynamic_Shizuoka_rmse_female", "ind_dynamic_Aichi_rmse_female",
                          "ind_dynamic_Mie_rmse_female", "ind_dynamic_Shiga_rmse_female", "ind_dynamic_Kyoto_rmse_female", 
                          "ind_dynamic_Osaka_rmse_female", "ind_dynamic_Hyogo_rmse_female", "ind_dynamic_Nara_rmse_female", 
                          "ind_dynamic_Wakayama_rmse_female", "ind_dynamic_Tottori_rmse_female", "ind_dynamic_Shimane_rmse_female",
                          "ind_dynamic_Okayama_rmse_female", "ind_dynamic_Hiroshima_rmse_female", "ind_dynamic_Yamaguchi_rmse_female", 
                          "ind_dynamic_Tokushima_rmse_female", "ind_dynamic_Kagawa_rmse_female", "ind_dynamic_Ehime_rmse_female", 
                          "ind_dynamic_Kochi_rmse_female", "ind_dynamic_Fukuoka_rmse_female", "ind_dynamic_Saga_rmse_female", 
                          "ind_dynamic_Nagasaki_rmse_female", "ind_dynamic_Kumamoto_rmse_female", "ind_dynamic_Oita_rmse_female", 
                          "ind_dynamic_Miyazaki_rmse_female", "ind_dynamic_Kagoshima_rmse_female", "ind_dynamic_Okinawa_rmse_female")

ind_dynamic_state_rmse_male = c("ind_dynamic_Japan_rmse_male", "ind_dynamic_Hokkaido_rmse_male", "ind_dynamic_Aomori_rmse_male", 
                        "ind_dynamic_Iwate_rmse_male", "ind_dynamic_Miyagi_rmse_male", "ind_dynamic_Akita_rmse_male", 
                        "ind_dynamic_Yamagata_rmse_male", "ind_dynamic_Fukushima_rmse_male", "ind_dynamic_Ibaraki_rmse_male", 
                        "ind_dynamic_Tochigi_rmse_male", "ind_dynamic_Gunma_rmse_male", "ind_dynamic_Saitama_rmse_male", 
                        "ind_dynamic_Chiba_rmse_male", "ind_dynamic_Tokyo_rmse_male", "ind_dynamic_Kanagawa_rmse_male",
                        "ind_dynamic_Niigata_rmse_male", "ind_dynamic_Toyama_rmse_male", "ind_dynamic_Ishikawa_rmse_male", 
                        "ind_dynamic_Fukui_rmse_male", "ind_dynamic_Yamanashi_rmse_male", "ind_dynamic_Nagano_rmse_male", 
                        "ind_dynamic_Gifu_rmse_male", "ind_dynamic_Shizuoka_rmse_male", "ind_dynamic_Aichi_rmse_male",
                        "ind_dynamic_Mie_rmse_male", "ind_dynamic_Shiga_rmse_male", "ind_dynamic_Kyoto_rmse_male", 
                        "ind_dynamic_Osaka_rmse_male", "ind_dynamic_Hyogo_rmse_male", "ind_dynamic_Nara_rmse_male", 
                        "ind_dynamic_Wakayama_rmse_male", "ind_dynamic_Tottori_rmse_male", "ind_dynamic_Shimane_rmse_male",
                        "ind_dynamic_Okayama_rmse_male", "ind_dynamic_Hiroshima_rmse_male", "ind_dynamic_Yamaguchi_rmse_male", 
                        "ind_dynamic_Tokushima_rmse_male", "ind_dynamic_Kagawa_rmse_male", "ind_dynamic_Ehime_rmse_male", 
                        "ind_dynamic_Kochi_rmse_male", "ind_dynamic_Fukuoka_rmse_male", "ind_dynamic_Saga_rmse_male", 
                        "ind_dynamic_Nagasaki_rmse_male", "ind_dynamic_Kumamoto_rmse_male", "ind_dynamic_Oita_rmse_male", 
                        "ind_dynamic_Miyazaki_rmse_male", "ind_dynamic_Kagoshima_rmse_male", "ind_dynamic_Okinawa_rmse_male")

ind_dynamic_state_rmse_total = c("ind_dynamic_Japan_rmse_total", "ind_dynamic_Hokkaido_rmse_total", "ind_dynamic_Aomori_rmse_total", 
                         "ind_dynamic_Iwate_rmse_total", "ind_dynamic_Miyagi_rmse_total", "ind_dynamic_Akita_rmse_total", 
                         "ind_dynamic_Yamagata_rmse_total", "ind_dynamic_Fukushima_rmse_total", "ind_dynamic_Ibaraki_rmse_total", 
                         "ind_dynamic_Tochigi_rmse_total", "ind_dynamic_Gunma_rmse_total", "ind_dynamic_Saitama_rmse_total", 
                         "ind_dynamic_Chiba_rmse_total", "ind_dynamic_Tokyo_rmse_total", "ind_dynamic_Kanagawa_rmse_total",
                         "ind_dynamic_Niigata_rmse_total", "ind_dynamic_Toyama_rmse_total", "ind_dynamic_Ishikawa_rmse_total", 
                         "ind_dynamic_Fukui_rmse_total", "ind_dynamic_Yamanashi_rmse_total", "ind_dynamic_Nagano_rmse_total", 
                         "ind_dynamic_Gifu_rmse_total", "ind_dynamic_Shizuoka_rmse_total", "ind_dynamic_Aichi_rmse_total",
                         "ind_dynamic_Mie_rmse_total", "ind_dynamic_Shiga_rmse_total", "ind_dynamic_Kyoto_rmse_total", 
                         "ind_dynamic_Osaka_rmse_total", "ind_dynamic_Hyogo_rmse_total", "ind_dynamic_Nara_rmse_total", 
                         "ind_dynamic_Wakayama_rmse_total", "ind_dynamic_Tottori_rmse_total", "ind_dynamic_Shimane_rmse_total",
                         "ind_dynamic_Okayama_rmse_total", "ind_dynamic_Hiroshima_rmse_total", "ind_dynamic_Yamaguchi_rmse_total", 
                         "ind_dynamic_Tokushima_rmse_total", "ind_dynamic_Kagawa_rmse_total", "ind_dynamic_Ehime_rmse_total", 
                         "ind_dynamic_Kochi_rmse_total", "ind_dynamic_Fukuoka_rmse_total", "ind_dynamic_Saga_rmse_total", 
                         "ind_dynamic_Nagasaki_rmse_total", "ind_dynamic_Kumamoto_rmse_total", "ind_dynamic_Oita_rmse_total", 
                         "ind_dynamic_Miyazaki_rmse_total", "ind_dynamic_Kagoshima_rmse_total", "ind_dynamic_Okinawa_rmse_total")

# region level training residuals

ind_dynamic_region_train_residual_female = c("ind_dynamic_region_R1_train_residual_female", "ind_dynamic_region_R2_train_residual_female", "ind_dynamic_region_R3_train_residual_female", 
                                     "ind_dynamic_region_R4_train_residual_female", "ind_dynamic_region_R5_train_residual_female", "ind_dynamic_region_R6_train_residual_female", 
                                     "ind_dynamic_region_R7_train_residual_female", "ind_dynamic_region_R8_train_residual_female")

ind_dynamic_region_train_residual_male = c("ind_dynamic_region_R1_train_residual_male", "ind_dynamic_region_R2_train_residual_male", "ind_dynamic_region_R3_train_residual_male", 
                                   "ind_dynamic_region_R4_train_residual_male", "ind_dynamic_region_R5_train_residual_male", "ind_dynamic_region_R6_train_residual_male", 
                                   "ind_dynamic_region_R7_train_residual_male", "ind_dynamic_region_R8_train_residual_male")

ind_dynamic_region_train_residual_total = c("ind_dynamic_region_R1_train_residual_total", "ind_dynamic_region_R2_train_residual_total", "ind_dynamic_region_R3_train_residual_total", 
                                    "ind_dynamic_region_R4_train_residual_total", "ind_dynamic_region_R5_train_residual_total", "ind_dynamic_region_R6_train_residual_total", 
                                    "ind_dynamic_region_R7_train_residual_total", "ind_dynamic_region_R8_train_residual_total")


# region level forecasts

ind_dynamic_region_forc_female = c("ind_dynamic_region_R1_forc_female", "ind_dynamic_region_R2_forc_female", "ind_dynamic_region_R3_forc_female", 
                           "ind_dynamic_region_R4_forc_female", "ind_dynamic_region_R5_forc_female", "ind_dynamic_region_R6_forc_female", 
                           "ind_dynamic_region_R7_forc_female", "ind_dynamic_region_R8_forc_female")

ind_dynamic_region_forc_male = c("ind_dynamic_region_R1_forc_male", "ind_dynamic_region_R2_forc_male", "ind_dynamic_region_R3_forc_male", 
                         "ind_dynamic_region_R4_forc_male", "ind_dynamic_region_R5_forc_male", "ind_dynamic_region_R6_forc_male", 
                         "ind_dynamic_region_R7_forc_male", "ind_dynamic_region_R8_forc_male")

ind_dynamic_region_forc_total = c("ind_dynamic_region_R1_forc_total", "ind_dynamic_region_R2_forc_total", "ind_dynamic_region_R3_forc_total", 
                          "ind_dynamic_region_R4_forc_total", "ind_dynamic_region_R5_forc_total", "ind_dynamic_region_R6_forc_total", 
                          "ind_dynamic_region_R7_forc_total", "ind_dynamic_region_R8_forc_total")

# Mean Errors at region level

ind_dynamic_region_me_female = c("ind_dynamic_region_R1_me_female", "ind_dynamic_region_R2_me_female", "ind_dynamic_region_R3_me_female", 
                         "ind_dynamic_region_R4_me_female", "ind_dynamic_region_R5_me_female", "ind_dynamic_region_R6_me_female", 
                         "ind_dynamic_region_R7_me_female", "ind_dynamic_region_R8_me_female")

ind_dynamic_region_me_male = c("ind_dynamic_region_R1_me_male", "ind_dynamic_region_R2_me_male", "ind_dynamic_region_R3_me_male", 
                       "ind_dynamic_region_R4_me_male", "ind_dynamic_region_R5_me_male", "ind_dynamic_region_R6_me_male", 
                       "ind_dynamic_region_R7_me_male", "ind_dynamic_region_R8_me_male")

ind_dynamic_region_me_total = c("ind_dynamic_region_R1_me_total", "ind_dynamic_region_R2_me_total", "ind_dynamic_region_R3_me_total", 
                        "ind_dynamic_region_R4_me_total", "ind_dynamic_region_R5_me_total", "ind_dynamic_region_R6_me_total", 
                        "ind_dynamic_region_R7_me_total", "ind_dynamic_region_R8_me_total")

# Mean Absolute Errors at region level

ind_dynamic_region_mae_female = c("ind_dynamic_region_R1_mae_female", "ind_dynamic_region_R2_mae_female", "ind_dynamic_region_R3_mae_female", 
                          "ind_dynamic_region_R4_mae_female", "ind_dynamic_region_R5_mae_female", "ind_dynamic_region_R6_mae_female", 
                          "ind_dynamic_region_R7_mae_female", "ind_dynamic_region_R8_mae_female")

ind_dynamic_region_mae_male = c("ind_dynamic_region_R1_mae_male", "ind_dynamic_region_R2_mae_male", "ind_dynamic_region_R3_mae_male", 
                        "ind_dynamic_region_R4_mae_male", "ind_dynamic_region_R5_mae_male", "ind_dynamic_region_R6_mae_male", 
                        "ind_dynamic_region_R7_mae_male", "ind_dynamic_region_R8_mae_male")

ind_dynamic_region_mae_total = c("ind_dynamic_region_R1_mae_total", "ind_dynamic_region_R2_mae_total", "ind_dynamic_region_R3_mae_total", 
                         "ind_dynamic_region_R4_mae_total", "ind_dynamic_region_R5_mae_total", "ind_dynamic_region_R6_mae_total", 
                         "ind_dynamic_region_R7_mae_total", "ind_dynamic_region_R8_mae_total")

# Root Mean Square Errors at region level

ind_dynamic_region_rmse_female = c("ind_dynamic_region_R1_rmse_female", "ind_dynamic_region_R2_rmse_female", "ind_dynamic_region_R3_rmse_female", 
                           "ind_dynamic_region_R4_rmse_female", "ind_dynamic_region_R5_rmse_female", "ind_dynamic_region_R6_rmse_female", 
                           "ind_dynamic_region_R7_rmse_female", "ind_dynamic_region_R8_rmse_female")

ind_dynamic_region_rmse_male = c("ind_dynamic_region_R1_rmse_male", "ind_dynamic_region_R2_rmse_male", "ind_dynamic_region_R3_rmse_male", 
                         "ind_dynamic_region_R4_rmse_male", "ind_dynamic_region_R5_rmse_male", "ind_dynamic_region_R6_rmse_male", 
                         "ind_dynamic_region_R7_rmse_male", "ind_dynamic_region_R8_rmse_male")

ind_dynamic_region_rmse_total = c("ind_dynamic_region_R1_rmse_total", "ind_dynamic_region_R2_rmse_total", "ind_dynamic_region_R3_rmse_total", 
                          "ind_dynamic_region_R4_rmse_total", "ind_dynamic_region_R5_rmse_total", "ind_dynamic_region_R6_rmse_total", 
                          "ind_dynamic_region_R7_rmse_total", "ind_dynamic_region_R8_rmse_total")

##########################
# Compute forecast errors 
##########################

me   = ftsa:::me
mae  = ftsa:::mae
rmse = ftsa:::rmse

# iw: region index
# year_horizon: forecast horizon
# fmethod: "classical" or "M" (standard vs robust functional principal component analysis)
# alpha: 1 - nominal coverage probability

# Define a function for computing forecasts at the region level

ind_back_test_region <- function(iw, year_horizon, fmethod = c("classical", "M"),  pcamethod = c("static", "dynamic"), alpha = 0.2)
{
  fmethod = match.arg(fmethod)
  pcamethod = match.arg(pcamethod)
  
  train_residual_female = train_residual_male = train_residual_total = list()
  
  res_male = res_male_lb = res_male_ub = 
    res_female = res_female_lb = res_female_ub = 
    res_total = res_total_lb = res_total_ub = array(NA, dim = c(year_horizon, 101, year_horizon))

  if (pcamethod == "static")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(region_smooth[iw]), years = 1975:(2000+j))
      
      fdm_female_order = head(which(round(cumsum(fdm(ind_dat, series = "female", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_female = fdm(ind_dat, series = "female", order = fdm_female_order, method = fmethod, lambda = 2.33)
      train_residual_female[[j]] = ind_dat$rate$female - exp(fdm_female$fitted$y)
      fun_forc_female  = forecast(fdm_female, h = year_horizon)        
      
      fdm_male_order = head(which(round(cumsum(fdm(ind_dat, series = "male", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_male = fdm(ind_dat, series = "male", order = fdm_male_order, method = fmethod, lambda = 2.33)
      train_residual_male[[j]] = ind_dat$rate$male - exp(fdm_male$fitted$y)
      fun_forc_male  = forecast(fdm_male, h = year_horizon)
      
      fdm_total_order = head(which(round(cumsum(fdm(ind_dat, series = "total", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_total = fdm(ind_dat, series = "total", order = fdm_total_order, method = fmethod, lambda = 2.33)
      train_residual_total[[j]] = ind_dat$rate$total - exp(fdm_total$fitted$y)
      fun_forc_total  = forecast(fdm_total, h = year_horizon)
      
      res_female[,,j]    = t(fun_forc_female$rate$female)
      res_male[,,j]    = t(fun_forc_male$rate$male)
      res_total[,,j]    = t(fun_forc_total$rate$total)
    }
  }
  
  if (pcamethod == "dynamic")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(region_smooth[iw]), years = 1975:(2000+j))
      
      # female
      data_dum_mean_female = rowMeans(log(ind_dat$rate$female), na.rm = TRUE)
      data_dum_sd_female = apply(log(ind_dat$rate$female), 1, sd, na.rm = TRUE)
      data_dum_female = t(scale(t(log(ind_dat$rate$female)), center = TRUE, scale = TRUE))
      
      C_0_female = long_run_covariance_estimation(data_dum_female, H = 3, C0 = 3)
      eigen_decomp_female = eigen(C_0_female)
      dynamic_order_female = head(which(cumsum(eigen_decomp_female$values)/sum(eigen_decomp_female$values) >= 0.95),1)
      dynamic_basis_female = as.matrix(eigen_decomp_female$vectors[,1:dynamic_order_female])
      dynamic_scores_female = t(dynamic_basis_female) %*% data_dum_female
      
      train_residual_female[[j]] = ind_dat$rate$female - exp(dynamic_basis_female %*% dynamic_scores_female*data_dum_sd_female + data_dum_mean_female)
      
      scores_fit_female = scores_fore_female = list()
      fore_ftsm_dyn_female = matrix(NA, nrow = nrow(data_dum_female), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_female)
      {
        scores_fit_female[[ik]] = auto.arima(dynamic_scores_female[ik,])
        scores_fore_female[[ik]] = forecast(scores_fit_female[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_female[,ih] = dynamic_basis_female%*% unlist(lapply(scores_fore_female,`[[`,ih))
      }
      
      fore_res_female = exp(fore_ftsm_dyn_female * data_dum_sd_female + data_dum_mean_female)
      res_female[,,j]    = t(fore_res_female)
      
      # male
      data_dum_mean_male = rowMeans(log(ind_dat$rate$male), na.rm = TRUE)
      data_dum_sd_male = apply(log(ind_dat$rate$male), 1, sd, na.rm = TRUE)
      data_dum_male = t(scale(t(log(ind_dat$rate$male)), center = TRUE, scale = TRUE))
      
      C_0_male = long_run_covariance_estimation(data_dum_male, H = 3, C0 = 3)
      eigen_decomp_male = eigen(C_0_male)
      dynamic_order_male = head(which(cumsum(eigen_decomp_male$values)/sum(eigen_decomp_male$values) >= 0.95),1)
      dynamic_basis_male = as.matrix(eigen_decomp_male$vectors[,1:dynamic_order_male])
      dynamic_scores_male = t(dynamic_basis_male) %*% data_dum_male
      
      train_residual_male[[j]] = ind_dat$rate$male - exp(dynamic_basis_male %*% dynamic_scores_male*data_dum_sd_male + data_dum_mean_male)
      
      scores_fit_male = scores_fore_male = list()
      fore_ftsm_dyn_male = matrix(NA, nrow = nrow(data_dum_male), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_male)
      {
        scores_fit_male[[ik]] = auto.arima(dynamic_scores_male[ik,])
        scores_fore_male[[ik]] = forecast(scores_fit_male[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_male[,ih] = dynamic_basis_male%*% unlist(lapply(scores_fore_male,`[[`,ih))
      }
      
      fore_res_male = exp(fore_ftsm_dyn_male * data_dum_sd_male + data_dum_mean_male)
      res_male[,,j]    = t(fore_res_male)
      
      # total
      data_dum_mean_total = rowMeans(log(ind_dat$rate$total), na.rm = TRUE)
      data_dum_sd_total = apply(log(ind_dat$rate$total), 1, sd, na.rm = TRUE)
      data_dum_total = t(scale(t(log(ind_dat$rate$total)), center = TRUE, scale = TRUE))
      
      C_0_total = long_run_covariance_estimation(data_dum_total, H = 3, C0 = 3)
      eigen_decomp_total = eigen(C_0_total)
      dynamic_order_total = head(which(cumsum(eigen_decomp_total$values)/sum(eigen_decomp_total$values) >= 0.95),1)
      dynamic_basis_total = as.matrix(eigen_decomp_total$vectors[,1:dynamic_order_total])
      dynamic_scores_total = t(dynamic_basis_total) %*% data_dum_total
      
      train_residual_total[[j]] = ind_dat$rate$total - exp(dynamic_basis_total %*% dynamic_scores_total*data_dum_sd_total + data_dum_mean_total)
      
      scores_fit_total = scores_fore_total = list()
      fore_ftsm_dyn_total = matrix(NA, nrow = nrow(data_dum_total), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_total)
      {
        scores_fit_total[[ik]] = auto.arima(dynamic_scores_total[ik,])
        scores_fore_total[[ik]] = forecast(scores_fit_total[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_total[,ih] = dynamic_basis_total%*% unlist(lapply(scores_fore_total,`[[`,ih))
      }
      
      fore_res_total = exp(fore_ftsm_dyn_total * data_dum_sd_total + data_dum_mean_total)
      res_total[,,j]    = t(fore_res_total)
    }
  }
  
  # Errors
  female_me = male_me = total_me = female_mae = male_mae = total_mae = female_rmse = male_rmse = total_rmse = vector("numeric",year_horizon)
  for(k in 1:year_horizon)
  {
    female_me[k]  = me(res_female[k,,1:(16-k)],  extract.years(get(region[iw]), years = (2001+k):2016)$rate$female)
    male_me[k]    = me(res_male[k,,1:(16-k)],    extract.years(get(region[iw]), years = (2001+k):2016)$rate$male)
    total_me[k]   = me(res_total[k,,1:(16-k)],   extract.years(get(region[iw]), years = (2001+k):2016)$rate$total)
    
    female_mae[k]  = mae(res_female[k,,1:(16-k)],  extract.years(get(region[iw]), years = (2001+k):2016)$rate$female)
    male_mae[k]    = mae(res_male[k,,1:(16-k)],    extract.years(get(region[iw]), years = (2001+k):2016)$rate$male)
    total_mae[k]   = mae(res_total[k,,1:(16-k)],   extract.years(get(region[iw]), years = (2001+k):2016)$rate$total)
    
    female_rmse[k]  = rmse(res_female[k,,1:(16-k)],  extract.years(get(region[iw]), years = (2001+k):2016)$rate$female)
    male_rmse[k]    = rmse(res_male[k,,1:(16-k)],  extract.years(get(region[iw]), years = (2001+k):2016)$rate$male)
    total_rmse[k]   = rmse(res_total[k,,1:(16-k)], extract.years(get(region[iw]), years = (2001+k):2016)$rate$total)
  }

  return(list(res_female = res_female, res_male = res_male, res_total = res_total,
              train_residual_female = train_residual_female, 
              train_residual_male = train_residual_male, train_residual_total = train_residual_total,
              female_me = female_me, male_me = male_me, total_me = total_me,
              female_mae = female_mae, male_mae = male_mae, total_mae = total_mae,
              female_rmse = female_rmse, male_rmse = male_rmse, total_rmse = total_rmse))
}

# Define a function for computing forecasts at the prefecture level

ind_back_test <- function(iw, year_horizon, fmethod = c("classical", "M"), pcamethod = c("static", "dynamic"), alpha = 0.2)
{
  fmethod = match.arg(fmethod)
  pcamethod = match.arg(pcamethod)
  
  train_residual_female = train_residual_male = train_residual_total = list()
  
  res_male  = res_female = res_total = array(NA, dim = c(year_horizon, 101, year_horizon))
  
  if (pcamethod == "static")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(state_smooth[iw]), years = 1975:(2000+j))
      
      fdm_female_order = head(which(round(cumsum(fdm(ind_dat, series = "female", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_female = fdm(ind_dat, series = "female", order = fdm_female_order, method = fmethod, lambda = 2.33)
      train_residual_female[[j]] = ind_dat$rate$female - exp(fdm_female$fitted$y)
      fun_forc_female  = forecast(fdm_female, h = year_horizon)   
      
      fdm_male_order = head(which(round(cumsum(fdm(ind_dat, series = "male", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_male = fdm(ind_dat, series = "male", order = fdm_male_order, method = fmethod, lambda = 2.33)
      train_residual_male[[j]] = ind_dat$rate$male - exp(fdm_male$fitted$y)
      fun_forc_male  = forecast(fdm_male, h = year_horizon)
      
      fdm_total_order = head(which(round(cumsum(fdm(ind_dat, series = "total", method = fmethod, lambda = 2.33, order = length(ind_dat$year))$varprop),3)>=0.95),1)
      fdm_total = fdm(ind_dat, series = "total", order = fdm_total_order, method = fmethod, lambda = 2.33)
      train_residual_total[[j]] = ind_dat$rate$total - exp(fdm_total$fitted$y)
      fun_forc_total  = forecast(fdm_total, h = year_horizon)
      
      res_female[,,j] = t(fun_forc_female$rate$female)
      res_male[,,j] = t(fun_forc_male$rate$male)
      res_total[,,j] = t(fun_forc_total$rate$total)
    }
  }
    
  if (pcamethod == "dynamic")
  {
    for(j in 1:year_horizon)
    {
      ind_dat = extract.years(get(state_smooth[iw]), years = 1975:(2000+j))
      # female
      data_dum_mean_female = rowMeans(log(ind_dat$rate$female), na.rm = TRUE)
      data_dum_sd_female = apply(log(ind_dat$rate$female), 1, sd, na.rm = TRUE)
      data_dum_female = t(scale(t(log(ind_dat$rate$female)), center = TRUE, scale = TRUE))
      
      C_0_female = long_run_covariance_estimation(data_dum_female, H = 3, C0 = 3)
      eigen_decomp_female = eigen(C_0_female)
      dynamic_order_female = head(which(cumsum(eigen_decomp_female$values)/sum(eigen_decomp_female$values) >= 0.95),1)
      dynamic_basis_female = as.matrix(eigen_decomp_female$vectors[,1:dynamic_order_female])
      dynamic_scores_female = t(dynamic_basis_female) %*% data_dum_female
      
      train_residual_female[[j]] = ind_dat$rate$female - exp(dynamic_basis_female %*% dynamic_scores_female*data_dum_sd_female + data_dum_mean_female)
      
      scores_fit_female = scores_fore_female = list()
      fore_ftsm_dyn_female = matrix(NA, nrow = nrow(data_dum_female), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_female)
      {
        scores_fit_female[[ik]] = auto.arima(dynamic_scores_female[ik,])
        scores_fore_female[[ik]] = forecast(scores_fit_female[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_female[,ih] = dynamic_basis_female%*% unlist(lapply(scores_fore_female,`[[`,ih))
      }
      
      fore_res_female = exp(fore_ftsm_dyn_female * data_dum_sd_female + data_dum_mean_female)
      res_female[,,j]    = t(fore_res_female)
      
      # male
      data_dum_mean_male = rowMeans(log(ind_dat$rate$male), na.rm = TRUE)
      data_dum_sd_male = apply(log(ind_dat$rate$male), 1, sd, na.rm = TRUE)
      data_dum_male = t(scale(t(log(ind_dat$rate$male)), center = TRUE, scale = TRUE))
      
      C_0_male = long_run_covariance_estimation(data_dum_male, H = 3, C0 = 3)
      eigen_decomp_male = eigen(C_0_male)
      dynamic_order_male = head(which(cumsum(eigen_decomp_male$values)/sum(eigen_decomp_male$values) >= 0.95),1)
      dynamic_basis_male = as.matrix(eigen_decomp_male$vectors[,1:dynamic_order_male])
      dynamic_scores_male = t(dynamic_basis_male) %*% data_dum_male
      
      train_residual_male[[j]] = ind_dat$rate$male - exp(dynamic_basis_male %*% dynamic_scores_male*data_dum_sd_male + data_dum_mean_male)
      
      scores_fit_male = scores_fore_male = list()
      fore_ftsm_dyn_male = matrix(NA, nrow = nrow(data_dum_male), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_male)
      {
        scores_fit_male[[ik]] = auto.arima(dynamic_scores_male[ik,])
        scores_fore_male[[ik]] = forecast(scores_fit_male[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_male[,ih] = dynamic_basis_male%*% unlist(lapply(scores_fore_male,`[[`,ih))
      }
      
      fore_res_male = exp(fore_ftsm_dyn_male * data_dum_sd_male + data_dum_mean_male)
      res_male[,,j]    = t(fore_res_male)
      
      # total
      data_dum_mean_total = rowMeans(log(ind_dat$rate$total), na.rm = TRUE)
      data_dum_sd_total = apply(log(ind_dat$rate$total), 1, sd, na.rm = TRUE)
      data_dum_total = t(scale(t(log(ind_dat$rate$total)), center = TRUE, scale = TRUE))
      
      C_0_total = long_run_covariance_estimation(data_dum_total, H = 3, C0 = 3)
      eigen_decomp_total = eigen(C_0_total)
      dynamic_order_total = head(which(cumsum(eigen_decomp_total$values)/sum(eigen_decomp_total$values) >= 0.95),1)
      dynamic_basis_total = as.matrix(eigen_decomp_total$vectors[,1:dynamic_order_total])
      dynamic_scores_total = t(dynamic_basis_total) %*% data_dum_total
      
      train_residual_total[[j]] = ind_dat$rate$total - exp(dynamic_basis_total %*% dynamic_scores_total*data_dum_sd_total + data_dum_mean_total)
      
      scores_fit_total = scores_fore_total = list()
      fore_ftsm_dyn_total = matrix(NA, nrow = nrow(data_dum_total), ncol = year_horizon)
      
      for(ik in 1:dynamic_order_total)
      {
        scores_fit_total[[ik]] = auto.arima(dynamic_scores_total[ik,])
        scores_fore_total[[ik]] = forecast(scores_fit_total[[ik]], h = year_horizon)$mean
      }
      
      for(ih in 1:year_horizon)
      {
        fore_ftsm_dyn_total[,ih] = dynamic_basis_total%*% unlist(lapply(scores_fore_total,`[[`,ih))
      }
      
      fore_res_total = exp(fore_ftsm_dyn_total * data_dum_sd_total + data_dum_mean_total)
      res_total[,,j] = t(fore_res_total)
    }
  }
  
  # Errors
  female_me = male_me = total_me = female_mae = male_mae = total_mae = female_rmse = male_rmse = total_rmse = vector("numeric", year_horizon)
  for(k in 1:year_horizon)
  {    
    female_me[k]  = me(res_female[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$female)
    male_me[k]    = me(res_male[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$male)
    total_me[k]   = me(res_total[k,,1:(16-k)], extract.years(get(state[iw]), years = (2001+k):2016)$rate$total)
      
    female_mae[k]  = mae(res_female[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$female)
    male_mae[k]    = mae(res_male[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$male)
    total_mae[k]   = mae(res_total[k,,1:(16-k)], extract.years(get(state[iw]), years = (2001+k):2016)$rate$total)
      
    female_rmse[k]  = rmse(res_female[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$female)
    male_rmse[k]    = rmse(res_male[k,,1:(16-k)],  extract.years(get(state[iw]), years = (2001+k):2016)$rate$male)
    total_rmse[k]   = rmse(res_total[k,,1:(16-k)], extract.years(get(state[iw]), years = (2001+k):2016)$rate$total)
  }	
    

  return(list(res_female = res_female, res_male = res_male, res_total = res_total,
              train_residual_female = train_residual_female, 
              train_residual_male = train_residual_male, train_residual_total = train_residual_total,
              female_me = female_me, male_me = male_me, total_me = total_me,
              female_mae = female_mae, male_mae = male_mae, total_mae = total_mae,
              female_rmse = female_rmse, male_rmse = male_rmse, total_rmse = total_rmse))
}


#####################################################################
# Univariate forecast of age-specific mortality rates by prefectures
#####################################################################

# Compute independent point forecasts for Japan and all prefectures

for(ik in 1:48)
{
  dum = ind_back_test(iw = ik,  fmethod = "classical", pcamethod = "dynamic", year_horizon = 15)
  assign(ind_dynamic_state_forc_female[ik], dum$res_female)
  assign(ind_dynamic_state_forc_male[ik],   dum$res_male)
  assign(ind_dynamic_state_forc_total[ik],  dum$res_total)
  
  assign(ind_dynamic_state_train_residual_female[ik], dum$train_residual_female)
  assign(ind_dynamic_state_train_residual_male[ik], dum$train_residual_male)
  assign(ind_dynamic_state_train_residual_total[ik], dum$train_residual_total)
  
  assign(ind_dynamic_state_me_female[ik], dum$female_me)
  assign(ind_dynamic_state_me_male[ik],   dum$male_me)
  assign(ind_dynamic_state_me_total[ik],  dum$total_me) 
  
  assign(ind_dynamic_state_mae_female[ik], dum$female_mae)
  assign(ind_dynamic_state_mae_male[ik],   dum$male_mae)
  assign(ind_dynamic_state_mae_total[ik],  dum$total_mae)   
  
  assign(ind_dynamic_state_rmse_female[ik], dum$female_rmse)
  assign(ind_dynamic_state_rmse_male[ik],   dum$male_rmse)
  assign(ind_dynamic_state_rmse_total[ik],  dum$total_rmse)   
  
  print(ik)
}

###########################################################
# Univariate forecast of age-specific mortality by regions
###########################################################

# Compute independent point forecasts for all regions

for(ik in 1:8)
{
  dum = ind_back_test_region(iw = ik, fmethod = "classic", pcamethod = "dynamic", year_horizon = 15)
  assign(ind_dynamic_region_forc_female[ik], dum$res_female)
  assign(ind_dynamic_region_forc_male[ik], dum$res_male)
  assign(ind_dynamic_region_forc_total[ik], dum$res_total)
  
  assign(ind_dynamic_region_train_residual_female[ik], dum$train_residual_female)
  assign(ind_dynamic_region_train_residual_male[ik], dum$train_residual_male)
  assign(ind_dynamic_region_train_residual_total[ik], dum$train_residual_total)
  
  assign(ind_dynamic_region_me_female[ik], dum$female_me)
  assign(ind_dynamic_region_me_male[ik],   dum$male_me)
  assign(ind_dynamic_region_me_total[ik],  dum$total_me)
  
  assign(ind_dynamic_region_mae_female[ik], dum$female_mae)
  assign(ind_dynamic_region_mae_male[ik],   dum$male_mae)
  assign(ind_dynamic_region_mae_total[ik],  dum$total_mae)
  
  assign(ind_dynamic_region_rmse_female[ik], dum$female_rmse)
  assign(ind_dynamic_region_rmse_male[ik],   dum$male_rmse)
  assign(ind_dynamic_region_rmse_total[ik],  dum$total_rmse)
  
  print(ik)
}

#####################
# Summary of results
#####################

# Region + Sex

ind_dynamic_region_me_female_mean_overall = ind_dynamic_region_me_male_mean_overall = ind_dynamic_region_me_total_mean_overall =
  ind_dynamic_region_mae_female_mean_overall = ind_dynamic_region_mae_male_mean_overall = ind_dynamic_region_mae_total_mean_overall = 
  ind_dynamic_region_rmse_female_mean_overall = ind_dynamic_region_rmse_male_mean_overall = ind_dynamic_region_rmse_total_mean_overall = NULL

for(ik in 1:8)  
{
  ind_dynamic_region_me_female_mean_overall = cbind(ind_dynamic_region_me_female_mean_overall, get(ind_dynamic_region_me_female[ik]))
  ind_dynamic_region_me_male_mean_overall   = cbind(ind_dynamic_region_me_male_mean_overall,   get(ind_dynamic_region_me_male[ik]))
  ind_dynamic_region_me_total_mean_overall  = cbind(ind_dynamic_region_me_total_mean_overall,   get(ind_dynamic_region_me_total[ik]))
  
  ind_dynamic_region_mae_female_mean_overall = cbind(ind_dynamic_region_mae_female_mean_overall, get(ind_dynamic_region_mae_female[ik]))
  ind_dynamic_region_mae_male_mean_overall   = cbind(ind_dynamic_region_mae_male_mean_overall,   get(ind_dynamic_region_mae_male[ik]))
  ind_dynamic_region_mae_total_mean_overall  = cbind(ind_dynamic_region_mae_total_mean_overall,   get(ind_dynamic_region_mae_total[ik]))
  
  ind_dynamic_region_rmse_female_mean_overall = cbind(ind_dynamic_region_rmse_female_mean_overall, get(ind_dynamic_region_rmse_female[ik]))
  ind_dynamic_region_rmse_male_mean_overall   = cbind(ind_dynamic_region_rmse_male_mean_overall,   get(ind_dynamic_region_rmse_male[ik]))
  ind_dynamic_region_rmse_total_mean_overall  = cbind(ind_dynamic_region_rmse_total_mean_overall,   get(ind_dynamic_region_rmse_total[ik]))
}

# Prefecture + Sex

ind_dynamic_me_female_mean_overall = ind_dynamic_me_male_mean_overall = ind_dynamic_me_total_mean_overall =
  ind_dynamic_mae_female_mean_overall = ind_dynamic_mae_male_mean_overall = ind_dynamic_mae_total_mean_overall = 
  ind_dynamic_rmse_female_mean_overall = ind_dynamic_rmse_male_mean_overall = ind_dynamic_rmse_total_mean_overall = NULL

for(ik in 1:48)
{
  ind_dynamic_me_female_mean_overall  = cbind(ind_dynamic_me_female_mean_overall, get(ind_dynamic_state_me_female[ik]))
  ind_dynamic_me_male_mean_overall    = cbind(ind_dynamic_me_male_mean_overall,   get(ind_dynamic_state_me_male[ik]))
  ind_dynamic_me_total_mean_overall   = cbind(ind_dynamic_me_total_mean_overall,   get(ind_dynamic_state_me_total[ik]))
  
  ind_dynamic_mae_female_mean_overall  = cbind(ind_dynamic_mae_female_mean_overall, get(ind_dynamic_state_mae_female[ik]))
  ind_dynamic_mae_male_mean_overall    = cbind(ind_dynamic_mae_male_mean_overall,   get(ind_dynamic_state_mae_male[ik]))
  ind_dynamic_mae_total_mean_overall   = cbind(ind_dynamic_mae_total_mean_overall,   get(ind_dynamic_state_mae_total[ik]))
  
  ind_dynamic_rmse_female_mean_overall  = cbind(ind_dynamic_rmse_female_mean_overall, get(ind_dynamic_state_rmse_female[ik]))
  ind_dynamic_rmse_male_mean_overall    = cbind(ind_dynamic_rmse_male_mean_overall,   get(ind_dynamic_state_rmse_male[ik]))
  ind_dynamic_rmse_total_mean_overall   = cbind(ind_dynamic_rmse_total_mean_overall,   get(ind_dynamic_state_rmse_total[ik]))
}

################################
# Averaging over 47 prefectures
################################

rowmeans_ind_dynamic_me_female_mean_overall = apply(ind_dynamic_me_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_me_male_mean_overall   = apply(ind_dynamic_me_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_me_total_mean_overall  = apply(ind_dynamic_me_total_mean_overall[,2:48], 1, mean)

rowmeans_ind_dynamic_mae_female_mean_overall = apply(ind_dynamic_mae_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_mae_male_mean_overall   = apply(ind_dynamic_mae_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_mae_total_mean_overall  = apply(ind_dynamic_mae_total_mean_overall[,2:48], 1, mean)

rowmeans_ind_dynamic_rmse_female_mean_overall = apply(ind_dynamic_rmse_female_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_rmse_male_mean_overall   = apply(ind_dynamic_rmse_male_mean_overall[,2:48], 1, mean)
rowmeans_ind_dynamic_rmse_total_mean_overall  = apply(ind_dynamic_rmse_total_mean_overall[,2:48], 1, mean)

#####################
# Summary of results
#####################

ind_dynamic_all_level_err_me = cbind(ind_dynamic_me_total_mean_overall[,1], apply(cbind(ind_dynamic_me_female_mean_overall[,1], ind_dynamic_me_male_mean_overall[,1]), 1,mean),
                              apply(ind_dynamic_region_me_total_mean_overall, 1, mean), apply(cbind(ind_dynamic_region_me_female_mean_overall, ind_dynamic_region_me_male_mean_overall),1,mean),
                              rowmeans_ind_dynamic_me_total_mean_overall, apply(cbind(rowmeans_ind_dynamic_me_female_mean_overall, rowmeans_ind_dynamic_me_male_mean_overall),1,mean))
ind_dynamic_all_level_err_me_all = rbind(ind_dynamic_all_level_err_me, colMeans(ind_dynamic_all_level_err_me), apply(ind_dynamic_all_level_err_me, 2, median))


ind_dynamic_all_level_err_mae = cbind(ind_dynamic_mae_total_mean_overall[,1], apply(cbind(ind_dynamic_mae_female_mean_overall[,1], ind_dynamic_mae_male_mean_overall[,1]), 1,mean),
                              apply(ind_dynamic_region_mae_total_mean_overall, 1, mean), apply(cbind(ind_dynamic_region_mae_female_mean_overall, ind_dynamic_region_mae_male_mean_overall),1,mean),
                              rowmeans_ind_dynamic_mae_total_mean_overall, apply(cbind(rowmeans_ind_dynamic_mae_female_mean_overall, rowmeans_ind_dynamic_mae_male_mean_overall),1,mean))
ind_dynamic_all_level_err_mae_all = rbind(ind_dynamic_all_level_err_mae, colMeans(ind_dynamic_all_level_err_mae), apply(ind_dynamic_all_level_err_mae, 2, median))


ind_dynamic_all_level_err_rmse = cbind(ind_dynamic_rmse_total_mean_overall[,1], apply(cbind(ind_dynamic_rmse_female_mean_overall[,1], ind_dynamic_rmse_male_mean_overall[,1]), 1,mean),
                               apply(ind_dynamic_region_rmse_total_mean_overall, 1, mean), apply(cbind(ind_dynamic_region_rmse_female_mean_overall, ind_dynamic_region_rmse_male_mean_overall),1,mean),
                               rowmeans_ind_dynamic_rmse_total_mean_overall, apply(cbind(rowmeans_ind_dynamic_rmse_female_mean_overall, rowmeans_ind_dynamic_rmse_male_mean_overall),1,mean))
ind_dynamic_all_level_err_rmse_all = rbind(ind_dynamic_all_level_err_rmse, colMeans(ind_dynamic_all_level_err_rmse), apply(ind_dynamic_all_level_err_rmse, 2, median))

colnames(ind_dynamic_all_level_err_rmse) = colnames(ind_dynamic_all_level_err_mae) = colnames(ind_dynamic_all_level_err_me)  =  c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")

colnames(ind_dynamic_all_level_err_rmse_all) = colnames(ind_dynamic_all_level_err_mae_all) = colnames(ind_dynamic_all_level_err_me_all)  =  c("Total", "Sex", "Region", "Region + Sex", "Prefecture", "Prefecture + Sex")
rownames(ind_dynamic_all_level_err_me_all) = rownames(ind_dynamic_all_level_err_mae_all) = rownames(ind_dynamic_all_level_err_rmse_all)=c(1:15, "Mean", "Median")

