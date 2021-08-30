#######################################################################
# Univariate functional time series point forecast (benchmark method)
#######################################################################

# state level training residuals

ind_state_train_residual_female = c("ind_Japan_train_residual_female", "ind_Hokkaido_train_residual_female", "ind_Aomori_train_residual_female", 
                          "ind_Iwate_train_residual_female", "ind_Miyagi_train_residual_female", "ind_Akita_train_residual_female", 
                          "ind_Yamagata_train_residual_female", "ind_Fukushima_train_residual_female", "ind_Ibaraki_train_residual_female", 
                          "ind_Tochigi_train_residual_female", "ind_Gunma_train_residual_female", "ind_Saitama_train_residual_female", 
                          "ind_Chiba_train_residual_female", "ind_Tokyo_train_residual_female", "ind_Kanagawa_train_residual_female",
                          "ind_Niigata_train_residual_female", "ind_Toyama_train_residual_female", "ind_Ishikawa_train_residual_female", 
                          "ind_Fukui_train_residual_female", "ind_Yamanashi_train_residual_female", "ind_Nagano_train_residual_female", 
                          "ind_Gifu_train_residual_female", "ind_Shizuoka_train_residual_female", "ind_Aichi_train_residual_female",
                          "ind_Mie_train_residual_female", "ind_Shiga_train_residual_female", "ind_Kyoto_train_residual_female", 
                          "ind_Osaka_train_residual_female", "ind_Hyogo_train_residual_female", "ind_Nara_train_residual_female", 
                          "ind_Wakayama_train_residual_female", "ind_Tottori_train_residual_female", "ind_Shimane_train_residual_female",
                          "ind_Okayama_train_residual_female", "ind_Hiroshima_train_residual_female", "ind_Yamaguchi_train_residual_female", 
                          "ind_Tokushima_train_residual_female", "ind_Kagawa_train_residual_female", "ind_Ehime_train_residual_female", 
                          "ind_Kochi_train_residual_female", "ind_Fukuoka_train_residual_female", "ind_Saga_train_residual_female", 
                          "ind_Nagasaki_train_residual_female", "ind_Kumamoto_train_residual_female", "ind_Oita_train_residual_female", 
                          "ind_Miyazaki_train_residual_female", "ind_Kagoshima_train_residual_female", "ind_Okinawa_train_residual_female")

ind_state_train_residual_male = c("ind_Japan_train_residual_male", "ind_Hokkaido_train_residual_male", "ind_Aomori_train_residual_male", 
                        "ind_Iwate_train_residual_male", "ind_Miyagi_train_residual_male", "ind_Akita_train_residual_male", 
                        "ind_Yamagata_train_residual_male", "ind_Fukushima_train_residual_male", "ind_Ibaraki_train_residual_male", 
                        "ind_Tochigi_train_residual_male", "ind_Gunma_train_residual_male", "ind_Saitama_train_residual_male", 
                        "ind_Chiba_train_residual_male", "ind_Tokyo_train_residual_male", "ind_Kanagawa_train_residual_male",
                        "ind_Niigata_train_residual_male", "ind_Toyama_train_residual_male", "ind_Ishikawa_train_residual_male", 
                        "ind_Fukui_train_residual_male", "ind_Yamanashi_train_residual_male", "ind_Nagano_train_residual_male", 
                        "ind_Gifu_train_residual_male", "ind_Shizuoka_train_residual_male", "ind_Aichi_train_residual_male",
                        "ind_Mie_train_residual_male", "ind_Shiga_train_residual_male", "ind_Kyoto_train_residual_male", 
                        "ind_Osaka_train_residual_male", "ind_Hyogo_train_residual_male", "ind_Nara_train_residual_male", 
                        "ind_Wakayama_train_residual_male", "ind_Tottori_train_residual_male", "ind_Shimane_train_residual_male",
                        "ind_Okayama_train_residual_male", "ind_Hiroshima_train_residual_male", "ind_Yamaguchi_train_residual_male", 
                        "ind_Tokushima_train_residual_male", "ind_Kagawa_train_residual_male", "ind_Ehime_train_residual_male", 
                        "ind_Kochi_train_residual_male", "ind_Fukuoka_train_residual_male", "ind_Saga_train_residual_male", 
                        "ind_Nagasaki_train_residual_male", "ind_Kumamoto_train_residual_male", "ind_Oita_train_residual_male", 
                        "ind_Miyazaki_train_residual_male", "ind_Kagoshima_train_residual_male", "ind_Okinawa_train_residual_male")

ind_state_train_residual_total = c("ind_Japan_train_residual_total", "ind_Hokkaido_train_residual_total", "ind_Aomori_train_residual_total", 
                         "ind_Iwate_train_residual_total", "ind_Miyagi_train_residual_total", "ind_Akita_train_residual_total", 
                         "ind_Yamagata_train_residual_total", "ind_Fukushima_train_residual_total", "ind_Ibaraki_train_residual_total", 
                         "ind_Tochigi_train_residual_total", "ind_Gunma_train_residual_total", "ind_Saitama_train_residual_total", 
                         "ind_Chiba_train_residual_total", "ind_Tokyo_train_residual_total", "ind_Kanagawa_train_residual_total",
                         "ind_Niigata_train_residual_total", "ind_Toyama_train_residual_total", "ind_Ishikawa_train_residual_total", 
                         "ind_Fukui_train_residual_total", "ind_Yamanashi_train_residual_total", "ind_Nagano_train_residual_total", 
                         "ind_Gifu_train_residual_total", "ind_Shizuoka_train_residual_total", "ind_Aichi_train_residual_total",
                         "ind_Mie_train_residual_total", "ind_Shiga_train_residual_total", "ind_Kyoto_train_residual_total", 
                         "ind_Osaka_train_residual_total", "ind_Hyogo_train_residual_total", "ind_Nara_train_residual_total", 
                         "ind_Wakayama_train_residual_total", "ind_Tottori_train_residual_total", "ind_Shimane_train_residual_total",
                         "ind_Okayama_train_residual_total", "ind_Hiroshima_train_residual_total", "ind_Yamaguchi_train_residual_total", 
                         "ind_Tokushima_train_residual_total", "ind_Kagawa_train_residual_total", "ind_Ehime_train_residual_total", 
                         "ind_Kochi_train_residual_total", "ind_Fukuoka_train_residual_total", "ind_Saga_train_residual_total", 
                         "ind_Nagasaki_train_residual_total", "ind_Kumamoto_train_residual_total", "ind_Oita_train_residual_total", 
                         "ind_Miyazaki_train_residual_total", "ind_Kagoshima_train_residual_total", "ind_Okinawa_train_residual_total")

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
 
# state level forecasts

ind_state_forc_female = c("ind_Japan_forc_female", "ind_Hokkaido_forc_female", "ind_Aomori_forc_female", 
                          "ind_Iwate_forc_female", "ind_Miyagi_forc_female", "ind_Akita_forc_female", 
                          "ind_Yamagata_forc_female", "ind_Fukushima_forc_female", "ind_Ibaraki_forc_female", 
                          "ind_Tochigi_forc_female", "ind_Gunma_forc_female", "ind_Saitama_forc_female", 
                          "ind_Chiba_forc_female", "ind_Tokyo_forc_female", "ind_Kanagawa_forc_female",
                          "ind_Niigata_forc_female", "ind_Toyama_forc_female", "ind_Ishikawa_forc_female", 
                          "ind_Fukui_forc_female", "ind_Yamanashi_forc_female", "ind_Nagano_forc_female", 
                          "ind_Gifu_forc_female", "ind_Shizuoka_forc_female", "ind_Aichi_forc_female",
                          "ind_Mie_forc_female", "ind_Shiga_forc_female", "ind_Kyoto_forc_female", 
                          "ind_Osaka_forc_female", "ind_Hyogo_forc_female", "ind_Nara_forc_female", 
                          "ind_Wakayama_forc_female", "ind_Tottori_forc_female", "ind_Shimane_forc_female",
                          "ind_Okayama_forc_female", "ind_Hiroshima_forc_female", "ind_Yamaguchi_forc_female", 
                          "ind_Tokushima_forc_female", "ind_Kagawa_forc_female", "ind_Ehime_forc_female", 
                          "ind_Kochi_forc_female", "ind_Fukuoka_forc_female", "ind_Saga_forc_female", 
                          "ind_Nagasaki_forc_female", "ind_Kumamoto_forc_female", "ind_Oita_forc_female", 
                          "ind_Miyazaki_forc_female", "ind_Kagoshima_forc_female", "ind_Okinawa_forc_female")

ind_state_forc_male = c("ind_Japan_forc_male", "ind_Hokkaido_forc_male", "ind_Aomori_forc_male", 
                        "ind_Iwate_forc_male", "ind_Miyagi_forc_male", "ind_Akita_forc_male", 
                        "ind_Yamagata_forc_male", "ind_Fukushima_forc_male", "ind_Ibaraki_forc_male", 
                        "ind_Tochigi_forc_male", "ind_Gunma_forc_male", "ind_Saitama_forc_male", 
                        "ind_Chiba_forc_male", "ind_Tokyo_forc_male", "ind_Kanagawa_forc_male",
                        "ind_Niigata_forc_male", "ind_Toyama_forc_male", "ind_Ishikawa_forc_male", 
                        "ind_Fukui_forc_male", "ind_Yamanashi_forc_male", "ind_Nagano_forc_male", 
                        "ind_Gifu_forc_male", "ind_Shizuoka_forc_male", "ind_Aichi_forc_male",
                        "ind_Mie_forc_male", "ind_Shiga_forc_male", "ind_Kyoto_forc_male", 
                        "ind_Osaka_forc_male", "ind_Hyogo_forc_male", "ind_Nara_forc_male", 
                        "ind_Wakayama_forc_male", "ind_Tottori_forc_male", "ind_Shimane_forc_male",
                        "ind_Okayama_forc_male", "ind_Hiroshima_forc_male", "ind_Yamaguchi_forc_male", 
                        "ind_Tokushima_forc_male", "ind_Kagawa_forc_male", "ind_Ehime_forc_male", 
                        "ind_Kochi_forc_male", "ind_Fukuoka_forc_male", "ind_Saga_forc_male", 
                        "ind_Nagasaki_forc_male", "ind_Kumamoto_forc_male", "ind_Oita_forc_male", 
                        "ind_Miyazaki_forc_male", "ind_Kagoshima_forc_male", "ind_Okinawa_forc_male")

ind_state_forc_total = c("ind_Japan_forc_total", "ind_Hokkaido_forc_total", "ind_Aomori_forc_total", 
                         "ind_Iwate_forc_total", "ind_Miyagi_forc_total", "ind_Akita_forc_total", 
                         "ind_Yamagata_forc_total", "ind_Fukushima_forc_total", "ind_Ibaraki_forc_total", 
                         "ind_Tochigi_forc_total", "ind_Gunma_forc_total", "ind_Saitama_forc_total", 
                         "ind_Chiba_forc_total", "ind_Tokyo_forc_total", "ind_Kanagawa_forc_total",
                         "ind_Niigata_forc_total", "ind_Toyama_forc_total", "ind_Ishikawa_forc_total", 
                         "ind_Fukui_forc_total", "ind_Yamanashi_forc_total", "ind_Nagano_forc_total", 
                         "ind_Gifu_forc_total", "ind_Shizuoka_forc_total", "ind_Aichi_forc_total",
                         "ind_Mie_forc_total", "ind_Shiga_forc_total", "ind_Kyoto_forc_total", 
                         "ind_Osaka_forc_total", "ind_Hyogo_forc_total", "ind_Nara_forc_total", 
                         "ind_Wakayama_forc_total", "ind_Tottori_forc_total", "ind_Shimane_forc_total",
                         "ind_Okayama_forc_total", "ind_Hiroshima_forc_total", "ind_Yamaguchi_forc_total", 
                         "ind_Tokushima_forc_total", "ind_Kagawa_forc_total", "ind_Ehime_forc_total", 
                         "ind_Kochi_forc_total", "ind_Fukuoka_forc_total", "ind_Saga_forc_total", 
                         "ind_Nagasaki_forc_total", "ind_Kumamoto_forc_total", "ind_Oita_forc_total", 
                         "ind_Miyazaki_forc_total", "ind_Kagoshima_forc_total", "ind_Okinawa_forc_total")

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

# Mean Errors at state level

ind_state_me_female = c("ind_Japan_me_female", "ind_Hokkaido_me_female", "ind_Aomori_me_female", 
                         "ind_Iwate_me_female", "ind_Miyagi_me_female", "ind_Akita_me_female", 
                         "ind_Yamagata_me_female", "ind_Fukushima_me_female", "ind_Ibaraki_me_female", 
                         "ind_Tochigi_me_female", "ind_Gunma_me_female", "ind_Saitama_me_female", 
                         "ind_Chiba_me_female", "ind_Tokyo_me_female", "ind_Kanagawa_me_female",
                         "ind_Niigata_me_female", "ind_Toyama_me_female", "ind_Ishikawa_me_female", 
                         "ind_Fukui_me_female", "ind_Yamanashi_me_female", "ind_Nagano_me_female", 
                         "ind_Gifu_me_female", "ind_Shizuoka_me_female", "ind_Aichi_me_female",
                         "ind_Mie_me_female", "ind_Shiga_me_female", "ind_Kyoto_me_female", 
                         "ind_Osaka_me_female", "ind_Hyogo_me_female", "ind_Nara_me_female", 
                         "ind_Wakayama_me_female", "ind_Tottori_me_female", "ind_Shimane_me_female",
                         "ind_Okayama_me_female", "ind_Hiroshima_me_female", "ind_Yamaguchi_me_female", 
                         "ind_Tokushima_me_female", "ind_Kagawa_me_female", "ind_Ehime_me_female", 
                         "ind_Kochi_me_female", "ind_Fukuoka_me_female", "ind_Saga_me_female", 
                         "ind_Nagasaki_me_female", "ind_Kumamoto_me_female", "ind_Oita_me_female", 
                         "ind_Miyazaki_me_female", "ind_Kagoshima_me_female", "ind_Okinawa_me_female")

ind_state_me_male = c("ind_Japan_me_male", "ind_Hokkaido_me_male", "ind_Aomori_me_male", 
                       "ind_Iwate_me_male", "ind_Miyagi_me_male", "ind_Akita_me_male", 
                       "ind_Yamagata_me_male", "ind_Fukushima_me_male", "ind_Ibaraki_me_male", 
                       "ind_Tochigi_me_male", "ind_Gunma_me_male", "ind_Saitama_me_male", 
                       "ind_Chiba_me_male", "ind_Tokyo_me_male", "ind_Kanagawa_me_male",
                       "ind_Niigata_me_male", "ind_Toyama_me_male", "ind_Ishikawa_me_male", 
                       "ind_Fukui_me_male", "ind_Yamanashi_me_male", "ind_Nagano_me_male", 
                       "ind_Gifu_me_male", "ind_Shizuoka_me_male", "ind_Aichi_me_male",
                       "ind_Mie_me_male", "ind_Shiga_me_male", "ind_Kyoto_me_male", 
                       "ind_Osaka_me_male", "ind_Hyogo_me_male", "ind_Nara_me_male", 
                       "ind_Wakayama_me_male", "ind_Tottori_me_male", "ind_Shimane_me_male",
                       "ind_Okayama_me_male", "ind_Hiroshima_me_male", "ind_Yamaguchi_me_male", 
                       "ind_Tokushima_me_male", "ind_Kagawa_me_male", "ind_Ehime_me_male", 
                       "ind_Kochi_me_male", "ind_Fukuoka_me_male", "ind_Saga_me_male", 
                       "ind_Nagasaki_me_male", "ind_Kumamoto_me_male", "ind_Oita_me_male", 
                       "ind_Miyazaki_me_male", "ind_Kagoshima_me_male", "ind_Okinawa_me_male")


ind_state_me_total = c("ind_Japan_me_total", "ind_Hokkaido_me_total", "ind_Aomori_me_total", 
                        "ind_Iwate_me_total", "ind_Miyagi_me_total", "ind_Akita_me_total", 
                        "ind_Yamagata_me_total", "ind_Fukushima_me_total", "ind_Ibaraki_me_total", 
                        "ind_Tochigi_me_total", "ind_Gunma_me_total", "ind_Saitama_me_total", 
                        "ind_Chiba_me_total", "ind_Tokyo_me_total", "ind_Kanagawa_me_total",
                        "ind_Niigata_me_total", "ind_Toyama_me_total", "ind_Ishikawa_me_total", 
                        "ind_Fukui_me_total", "ind_Yamanashi_me_total", "ind_Nagano_me_total", 
                        "ind_Gifu_me_total", "ind_Shizuoka_me_total", "ind_Aichi_me_total",
                        "ind_Mie_me_total", "ind_Shiga_me_total", "ind_Kyoto_me_total", 
                        "ind_Osaka_me_total", "ind_Hyogo_me_total", "ind_Nara_me_total", 
                        "ind_Wakayama_me_total", "ind_Tottori_me_total", "ind_Shimane_me_total",
                        "ind_Okayama_me_total", "ind_Hiroshima_me_total", "ind_Yamaguchi_me_total", 
                        "ind_Tokushima_me_total", "ind_Kagawa_me_total", "ind_Ehime_me_total", 
                        "ind_Kochi_me_total", "ind_Fukuoka_me_total", "ind_Saga_me_total", 
                        "ind_Nagasaki_me_total", "ind_Kumamoto_me_total", "ind_Oita_me_total", 
                        "ind_Miyazaki_me_total", "ind_Kagoshima_me_total", "ind_Okinawa_me_total")

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

ind_state_mae_female = c("ind_Japan_mae_female", "ind_Hokkaido_mae_female", "ind_Aomori_mae_female", 
                         "ind_Iwate_mae_female", "ind_Miyagi_mae_female", "ind_Akita_mae_female", 
                         "ind_Yamagata_mae_female", "ind_Fukushima_mae_female", "ind_Ibaraki_mae_female", 
                         "ind_Tochigi_mae_female", "ind_Gunma_mae_female", "ind_Saitama_mae_female", 
                         "ind_Chiba_mae_female", "ind_Tokyo_mae_female", "ind_Kanagawa_mae_female",
                         "ind_Niigata_mae_female", "ind_Toyama_mae_female", "ind_Ishikawa_mae_female", 
                         "ind_Fukui_mae_female", "ind_Yamanashi_mae_female", "ind_Nagano_mae_female", 
                         "ind_Gifu_mae_female", "ind_Shizuoka_mae_female", "ind_Aichi_mae_female",
                         "ind_Mie_mae_female", "ind_Shiga_mae_female", "ind_Kyoto_mae_female", 
                         "ind_Osaka_mae_female", "ind_Hyogo_mae_female", "ind_Nara_mae_female", 
                         "ind_Wakayama_mae_female", "ind_Tottori_mae_female", "ind_Shimane_mae_female",
                         "ind_Okayama_mae_female", "ind_Hiroshima_mae_female", "ind_Yamaguchi_mae_female", 
                         "ind_Tokushima_mae_female", "ind_Kagawa_mae_female", "ind_Ehime_mae_female", 
                         "ind_Kochi_mae_female", "ind_Fukuoka_mae_female", "ind_Saga_mae_female", 
                         "ind_Nagasaki_mae_female", "ind_Kumamoto_mae_female", "ind_Oita_mae_female", 
                         "ind_Miyazaki_mae_female", "ind_Kagoshima_mae_female", "ind_Okinawa_mae_female")

ind_state_mae_male = c("ind_Japan_mae_male", "ind_Hokkaido_mae_male", "ind_Aomori_mae_male", 
                       "ind_Iwate_mae_male", "ind_Miyagi_mae_male", "ind_Akita_mae_male", 
                       "ind_Yamagata_mae_male", "ind_Fukushima_mae_male", "ind_Ibaraki_mae_male", 
                       "ind_Tochigi_mae_male", "ind_Gunma_mae_male", "ind_Saitama_mae_male", 
                       "ind_Chiba_mae_male", "ind_Tokyo_mae_male", "ind_Kanagawa_mae_male",
                       "ind_Niigata_mae_male", "ind_Toyama_mae_male", "ind_Ishikawa_mae_male", 
                       "ind_Fukui_mae_male", "ind_Yamanashi_mae_male", "ind_Nagano_mae_male", 
                       "ind_Gifu_mae_male", "ind_Shizuoka_mae_male", "ind_Aichi_mae_male",
                       "ind_Mie_mae_male", "ind_Shiga_mae_male", "ind_Kyoto_mae_male", 
                       "ind_Osaka_mae_male", "ind_Hyogo_mae_male", "ind_Nara_mae_male", 
                       "ind_Wakayama_mae_male", "ind_Tottori_mae_male", "ind_Shimane_mae_male",
                       "ind_Okayama_mae_male", "ind_Hiroshima_mae_male", "ind_Yamaguchi_mae_male", 
                       "ind_Tokushima_mae_male", "ind_Kagawa_mae_male", "ind_Ehime_mae_male", 
                       "ind_Kochi_mae_male", "ind_Fukuoka_mae_male", "ind_Saga_mae_male", 
                       "ind_Nagasaki_mae_male", "ind_Kumamoto_mae_male", "ind_Oita_mae_male", 
                       "ind_Miyazaki_mae_male", "ind_Kagoshima_mae_male", "ind_Okinawa_mae_male")


ind_state_mae_total = c("ind_Japan_mae_total", "ind_Hokkaido_mae_total", "ind_Aomori_mae_total", 
                        "ind_Iwate_mae_total", "ind_Miyagi_mae_total", "ind_Akita_mae_total", 
                        "ind_Yamagata_mae_total", "ind_Fukushima_mae_total", "ind_Ibaraki_mae_total", 
                        "ind_Tochigi_mae_total", "ind_Gunma_mae_total", "ind_Saitama_mae_total", 
                        "ind_Chiba_mae_total", "ind_Tokyo_mae_total", "ind_Kanagawa_mae_total",
                        "ind_Niigata_mae_total", "ind_Toyama_mae_total", "ind_Ishikawa_mae_total", 
                        "ind_Fukui_mae_total", "ind_Yamanashi_mae_total", "ind_Nagano_mae_total", 
                        "ind_Gifu_mae_total", "ind_Shizuoka_mae_total", "ind_Aichi_mae_total",
                        "ind_Mie_mae_total", "ind_Shiga_mae_total", "ind_Kyoto_mae_total", 
                        "ind_Osaka_mae_total", "ind_Hyogo_mae_total", "ind_Nara_mae_total", 
                        "ind_Wakayama_mae_total", "ind_Tottori_mae_total", "ind_Shimane_mae_total",
                        "ind_Okayama_mae_total", "ind_Hiroshima_mae_total", "ind_Yamaguchi_mae_total", 
                        "ind_Tokushima_mae_total", "ind_Kagawa_mae_total", "ind_Ehime_mae_total", 
                        "ind_Kochi_mae_total", "ind_Fukuoka_mae_total", "ind_Saga_mae_total", 
                        "ind_Nagasaki_mae_total", "ind_Kumamoto_mae_total", "ind_Oita_mae_total", 
                        "ind_Miyazaki_mae_total", "ind_Kagoshima_mae_total", "ind_Okinawa_mae_total")

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

ind_state_rmse_female = c("ind_Japan_rmse_female", "ind_Hokkaido_rmse_female", "ind_Aomori_rmse_female", 
                          "ind_Iwate_rmse_female", "ind_Miyagi_rmse_female", "ind_Akita_rmse_female", 
                          "ind_Yamagata_rmse_female", "ind_Fukushima_rmse_female", "ind_Ibaraki_rmse_female", 
                          "ind_Tochigi_rmse_female", "ind_Gunma_rmse_female", "ind_Saitama_rmse_female", 
                          "ind_Chiba_rmse_female", "ind_Tokyo_rmse_female", "ind_Kanagawa_rmse_female",
                          "ind_Niigata_rmse_female", "ind_Toyama_rmse_female", "ind_Ishikawa_rmse_female", 
                          "ind_Fukui_rmse_female", "ind_Yamanashi_rmse_female", "ind_Nagano_rmse_female", 
                          "ind_Gifu_rmse_female", "ind_Shizuoka_rmse_female", "ind_Aichi_rmse_female",
                          "ind_Mie_rmse_female", "ind_Shiga_rmse_female", "ind_Kyoto_rmse_female", 
                          "ind_Osaka_rmse_female", "ind_Hyogo_rmse_female", "ind_Nara_rmse_female", 
                          "ind_Wakayama_rmse_female", "ind_Tottori_rmse_female", "ind_Shimane_rmse_female",
                          "ind_Okayama_rmse_female", "ind_Hiroshima_rmse_female", "ind_Yamaguchi_rmse_female", 
                          "ind_Tokushima_rmse_female", "ind_Kagawa_rmse_female", "ind_Ehime_rmse_female", 
                          "ind_Kochi_rmse_female", "ind_Fukuoka_rmse_female", "ind_Saga_rmse_female", 
                          "ind_Nagasaki_rmse_female", "ind_Kumamoto_rmse_female", "ind_Oita_rmse_female", 
                          "ind_Miyazaki_rmse_female", "ind_Kagoshima_rmse_female", "ind_Okinawa_rmse_female")

ind_state_rmse_male = c("ind_Japan_rmse_male", "ind_Hokkaido_rmse_male", "ind_Aomori_rmse_male", 
                        "ind_Iwate_rmse_male", "ind_Miyagi_rmse_male", "ind_Akita_rmse_male", 
                        "ind_Yamagata_rmse_male", "ind_Fukushima_rmse_male", "ind_Ibaraki_rmse_male", 
                        "ind_Tochigi_rmse_male", "ind_Gunma_rmse_male", "ind_Saitama_rmse_male", 
                        "ind_Chiba_rmse_male", "ind_Tokyo_rmse_male", "ind_Kanagawa_rmse_male",
                        "ind_Niigata_rmse_male", "ind_Toyama_rmse_male", "ind_Ishikawa_rmse_male", 
                        "ind_Fukui_rmse_male", "ind_Yamanashi_rmse_male", "ind_Nagano_rmse_male", 
                        "ind_Gifu_rmse_male", "ind_Shizuoka_rmse_male", "ind_Aichi_rmse_male",
                        "ind_Mie_rmse_male", "ind_Shiga_rmse_male", "ind_Kyoto_rmse_male", 
                        "ind_Osaka_rmse_male", "ind_Hyogo_rmse_male", "ind_Nara_rmse_male", 
                        "ind_Wakayama_rmse_male", "ind_Tottori_rmse_male", "ind_Shimane_rmse_male",
                        "ind_Okayama_rmse_male", "ind_Hiroshima_rmse_male", "ind_Yamaguchi_rmse_male", 
                        "ind_Tokushima_rmse_male", "ind_Kagawa_rmse_male", "ind_Ehime_rmse_male", 
                        "ind_Kochi_rmse_male", "ind_Fukuoka_rmse_male", "ind_Saga_rmse_male", 
                        "ind_Nagasaki_rmse_male", "ind_Kumamoto_rmse_male", "ind_Oita_rmse_male", 
                        "ind_Miyazaki_rmse_male", "ind_Kagoshima_rmse_male", "ind_Okinawa_rmse_male")

ind_state_rmse_total = c("ind_Japan_rmse_total", "ind_Hokkaido_rmse_total", "ind_Aomori_rmse_total", 
                         "ind_Iwate_rmse_total", "ind_Miyagi_rmse_total", "ind_Akita_rmse_total", 
                         "ind_Yamagata_rmse_total", "ind_Fukushima_rmse_total", "ind_Ibaraki_rmse_total", 
                         "ind_Tochigi_rmse_total", "ind_Gunma_rmse_total", "ind_Saitama_rmse_total", 
                         "ind_Chiba_rmse_total", "ind_Tokyo_rmse_total", "ind_Kanagawa_rmse_total",
                         "ind_Niigata_rmse_total", "ind_Toyama_rmse_total", "ind_Ishikawa_rmse_total", 
                         "ind_Fukui_rmse_total", "ind_Yamanashi_rmse_total", "ind_Nagano_rmse_total", 
                         "ind_Gifu_rmse_total", "ind_Shizuoka_rmse_total", "ind_Aichi_rmse_total",
                         "ind_Mie_rmse_total", "ind_Shiga_rmse_total", "ind_Kyoto_rmse_total", 
                         "ind_Osaka_rmse_total", "ind_Hyogo_rmse_total", "ind_Nara_rmse_total", 
                         "ind_Wakayama_rmse_total", "ind_Tottori_rmse_total", "ind_Shimane_rmse_total",
                         "ind_Okayama_rmse_total", "ind_Hiroshima_rmse_total", "ind_Yamaguchi_rmse_total", 
                         "ind_Tokushima_rmse_total", "ind_Kagawa_rmse_total", "ind_Ehime_rmse_total", 
                         "ind_Kochi_rmse_total", "ind_Fukuoka_rmse_total", "ind_Saga_rmse_total", 
                         "ind_Nagasaki_rmse_total", "ind_Kumamoto_rmse_total", "ind_Oita_rmse_total", 
                         "ind_Miyazaki_rmse_total", "ind_Kagoshima_rmse_total", "ind_Okinawa_rmse_total")

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

ind_region_train_residual_female = c("ind_region_R1_train_residual_female", "ind_region_R2_train_residual_female", "ind_region_R3_train_residual_female", 
                           "ind_region_R4_train_residual_female", "ind_region_R5_train_residual_female", "ind_region_R6_train_residual_female", 
                           "ind_region_R7_train_residual_female", "ind_region_R8_train_residual_female")

ind_region_train_residual_male = c("ind_region_R1_train_residual_male", "ind_region_R2_train_residual_male", "ind_region_R3_train_residual_male", 
                         "ind_region_R4_train_residual_male", "ind_region_R5_train_residual_male", "ind_region_R6_train_residual_male", 
                         "ind_region_R7_train_residual_male", "ind_region_R8_train_residual_male")

ind_region_train_residual_total = c("ind_region_R1_train_residual_total", "ind_region_R2_train_residual_total", "ind_region_R3_train_residual_total", 
                          "ind_region_R4_train_residual_total", "ind_region_R5_train_residual_total", "ind_region_R6_train_residual_total", 
                          "ind_region_R7_train_residual_total", "ind_region_R8_train_residual_total")

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

ind_region_forc_female = c("ind_region_R1_forc_female", "ind_region_R2_forc_female", "ind_region_R3_forc_female", 
                           "ind_region_R4_forc_female", "ind_region_R5_forc_female", "ind_region_R6_forc_female", 
                           "ind_region_R7_forc_female", "ind_region_R8_forc_female")

ind_region_forc_male = c("ind_region_R1_forc_male", "ind_region_R2_forc_male", "ind_region_R3_forc_male", 
                         "ind_region_R4_forc_male", "ind_region_R5_forc_male", "ind_region_R6_forc_male", 
                         "ind_region_R7_forc_male", "ind_region_R8_forc_male")

ind_region_forc_total = c("ind_region_R1_forc_total", "ind_region_R2_forc_total", "ind_region_R3_forc_total", 
                          "ind_region_R4_forc_total", "ind_region_R5_forc_total", "ind_region_R6_forc_total", 
                          "ind_region_R7_forc_total", "ind_region_R8_forc_total")

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

ind_region_me_female = c("ind_region_R1_me_female", "ind_region_R2_me_female", "ind_region_R3_me_female", 
                          "ind_region_R4_me_female", "ind_region_R5_me_female", "ind_region_R6_me_female", 
                          "ind_region_R7_me_female", "ind_region_R8_me_female")

ind_region_me_male = c("ind_region_R1_me_male", "ind_region_R2_me_male", "ind_region_R3_me_male", 
                        "ind_region_R4_me_male", "ind_region_R5_me_male", "ind_region_R6_me_male", 
                        "ind_region_R7_me_male", "ind_region_R8_me_male")

ind_region_me_total = c("ind_region_R1_me_total", "ind_region_R2_me_total", "ind_region_R3_me_total", 
                         "ind_region_R4_me_total", "ind_region_R5_me_total", "ind_region_R6_me_total", 
                         "ind_region_R7_me_total", "ind_region_R8_me_total")

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

ind_region_mae_female = c("ind_region_R1_mae_female", "ind_region_R2_mae_female", "ind_region_R3_mae_female", 
                          "ind_region_R4_mae_female", "ind_region_R5_mae_female", "ind_region_R6_mae_female", 
                          "ind_region_R7_mae_female", "ind_region_R8_mae_female")

ind_region_mae_male = c("ind_region_R1_mae_male", "ind_region_R2_mae_male", "ind_region_R3_mae_male", 
                        "ind_region_R4_mae_male", "ind_region_R5_mae_male", "ind_region_R6_mae_male", 
                        "ind_region_R7_mae_male", "ind_region_R8_mae_male")

ind_region_mae_total = c("ind_region_R1_mae_total", "ind_region_R2_mae_total", "ind_region_R3_mae_total", 
                         "ind_region_R4_mae_total", "ind_region_R5_mae_total", "ind_region_R6_mae_total", 
                         "ind_region_R7_mae_total", "ind_region_R8_mae_total")

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

ind_region_rmse_female = c("ind_region_R1_rmse_female", "ind_region_R2_rmse_female", "ind_region_R3_rmse_female", 
                           "ind_region_R4_rmse_female", "ind_region_R5_rmse_female", "ind_region_R6_rmse_female", 
                           "ind_region_R7_rmse_female", "ind_region_R8_rmse_female")

ind_region_rmse_male = c("ind_region_R1_rmse_male", "ind_region_R2_rmse_male", "ind_region_R3_rmse_male", 
                         "ind_region_R4_rmse_male", "ind_region_R5_rmse_male", "ind_region_R6_rmse_male", 
                         "ind_region_R7_rmse_male", "ind_region_R8_rmse_male")

ind_region_rmse_total = c("ind_region_R1_rmse_total", "ind_region_R2_rmse_total", "ind_region_R3_rmse_total", 
                          "ind_region_R4_rmse_total", "ind_region_R5_rmse_total", "ind_region_R6_rmse_total", 
                          "ind_region_R7_rmse_total", "ind_region_R8_rmse_total")

ind_dynamic_region_rmse_female = c("ind_dynamic_region_R1_rmse_female", "ind_dynamic_region_R2_rmse_female", "ind_dynamic_region_R3_rmse_female", 
                           "ind_dynamic_region_R4_rmse_female", "ind_dynamic_region_R5_rmse_female", "ind_dynamic_region_R6_rmse_female", 
                           "ind_dynamic_region_R7_rmse_female", "ind_dynamic_region_R8_rmse_female")

ind_dynamic_region_rmse_male = c("ind_dynamic_region_R1_rmse_male", "ind_dynamic_region_R2_rmse_male", "ind_dynamic_region_R3_rmse_male", 
                         "ind_dynamic_region_R4_rmse_male", "ind_dynamic_region_R5_rmse_male", "ind_dynamic_region_R6_rmse_male", 
                         "ind_dynamic_region_R7_rmse_male", "ind_dynamic_region_R8_rmse_male")

ind_dynamic_region_rmse_total = c("ind_dynamic_region_R1_rmse_total", "ind_dynamic_region_R2_rmse_total", "ind_dynamic_region_R3_rmse_total", 
                          "ind_dynamic_region_R4_rmse_total", "ind_dynamic_region_R5_rmse_total", "ind_dynamic_region_R6_rmse_total", 
                          "ind_dynamic_region_R7_rmse_total", "ind_dynamic_region_R8_rmse_total")

########################
# clustering by regions
########################

# R1

ind_R1 = Hokkaido
ind_R1_smooth = Hokkaido_smooth

# R2

ind_R2_female_count = Aomori$pop$female + Iwate$pop$female + Miyagi$pop$female + Akita$pop$female + 
  Yamagata$pop$female + Fukushima$pop$female
ind_R2_female_rate = (Aomori_count_female + Iwate_count_female + Miyagi_count_female + Akita_count_female + 
                        Yamagata_count_female + Fukushima_count_female)/ind_R2_female_count

ind_R2_male_count = Aomori$pop$male + Iwate$pop$male + Miyagi$pop$male + Akita$pop$male + 
  Yamagata$pop$male + Fukushima$pop$male
ind_R2_male_rate = (Aomori_count_male + Iwate_count_male + Miyagi_count_male + Akita_count_male + 
                      Yamagata_count_male + Fukushima_count_male)/ind_R2_male_count

ind_R2_total_count = Aomori$pop$total + Iwate$pop$total + Miyagi$pop$total + Akita$pop$total + 
  Yamagata$pop$total + Fukushima$pop$total
ind_R2_total_rate = (Aomori_count_total + Iwate_count_total + Miyagi_count_total + Akita_count_total + 
                       Yamagata_count_total + Fukushima_count_total)/ind_R2_total_count


ind_R2_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R2_female_rate), as.numeric(ind_R2_male_rate), as.numeric(ind_R2_total_rate))
colnames(ind_R2_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R2_rate_summary, "ind_R2_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R2_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R2_female_count), as.numeric(ind_R2_male_count), as.numeric(ind_R2_total_count))
colnames(ind_R2_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R2_count_summary, "ind_R2_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R2 = read.demogdata("ind_R2_rate.txt", "ind_R2_count.txt", type="mortality", label = "R2", skip=0)
ind_R2_smooth = smooth.demogdata(ind_R2)

# R3

ind_R3_female_count = Ibaraki$pop$female + Tochigi$pop$female + Gunma$pop$female + Saitama$pop$female + 
  Chiba$pop$female + Tokyo$pop$female + Kanagawa$pop$female
ind_R3_female_rate = (Ibaraki_count_female + Tochigi_count_female + Gunma_count_female + Saitama_count_female + 
                        Chiba_count_female + Tokyo_count_female + Kanagawa_count_female)/ind_R3_female_count

ind_R3_male_count = Ibaraki$pop$male + Tochigi$pop$male + Gunma$pop$male + Saitama$pop$male + 
  Chiba$pop$male + Tokyo$pop$male + Kanagawa$pop$male
ind_R3_male_rate = (Ibaraki_count_male + Tochigi_count_male + Gunma_count_male + Saitama_count_male + 
                      Chiba_count_male + Tokyo_count_male + Kanagawa_count_male)/ind_R3_male_count

ind_R3_total_count = Ibaraki$pop$total + Tochigi$pop$total + Gunma$pop$total + Saitama$pop$total + 
  Chiba$pop$total + Tokyo$pop$total + Kanagawa$pop$total
ind_R3_total_rate = (Ibaraki_count_total + Tochigi_count_total + Gunma_count_total + Saitama_count_total + 
                       Chiba_count_total + Tokyo_count_total + Kanagawa_count_total)/ind_R3_total_count

ind_R3_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R3_female_rate), as.numeric(ind_R3_male_rate), as.numeric(ind_R3_total_rate))
colnames(ind_R3_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R3_rate_summary, "ind_R3_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R3_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R3_female_count), as.numeric(ind_R3_male_count), as.numeric(ind_R3_total_count))
colnames(ind_R3_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R3_count_summary, "ind_R3_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R3 = read.demogdata("ind_R3_rate.txt", "ind_R3_count.txt", type="mortality", label = "R3", skip=0)
ind_R3_smooth = smooth.demogdata(ind_R3)

# R4

ind_R4_female_count = Niigata$pop$female + Toyama$pop$female + Ishikawa$pop$female + Fukui$pop$female + 
  Yamanashi$pop$female + Nagano$pop$female + Gifu$pop$female + Shizuoka$pop$female + Aichi$pop$female
ind_R4_female_rate = (Niigata_count_female + Toyama_count_female + Ishikawa_count_female + Fukui_count_female + 
                        Yamanashi_count_female + Nagano_count_female + Gifu_count_female + Shizuoka_count_female + Aichi_count_female)/ind_R4_female_count

ind_R4_male_count = Niigata$pop$male + Toyama$pop$male + Ishikawa$pop$male + Fukui$pop$male + 
  Yamanashi$pop$male + Nagano$pop$male + Gifu$pop$male + Shizuoka$pop$male + Aichi$pop$male
ind_R4_male_rate = (Niigata_count_male + Toyama_count_male + Ishikawa_count_male + Fukui_count_male + 
                      Yamanashi_count_male + Nagano_count_male + Gifu_count_male + Shizuoka_count_male + Aichi_count_male)/ind_R4_male_count

ind_R4_total_count = Niigata$pop$total + Toyama$pop$total + Ishikawa$pop$total + Fukui$pop$total + 
  Yamanashi$pop$total + Nagano$pop$total + Gifu$pop$total + Shizuoka$pop$total + Aichi$pop$total
ind_R4_total_rate = (Niigata_count_total + Toyama_count_total + Ishikawa_count_total + Fukui_count_total + 
                       Yamanashi_count_total + Nagano_count_total + Gifu_count_total + Shizuoka_count_total + Aichi_count_total)/ind_R4_total_count

ind_R4_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R4_female_rate), as.numeric(ind_R4_male_rate), as.numeric(ind_R4_total_rate))
colnames(ind_R4_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R4_rate_summary, "ind_R4_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R4_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R4_female_count), as.numeric(ind_R4_male_count), as.numeric(ind_R4_total_count))
colnames(ind_R4_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R4_count_summary, "ind_R4_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R4 = read.demogdata("ind_R4_rate.txt", "ind_R4_count.txt", type="mortality", label = "R4", skip=0)
ind_R4_smooth = smooth.demogdata(ind_R4)

# R5

ind_R5_female_count = Mie$pop$female + Shiga$pop$female + Kyoto$pop$female + Osaka$pop$female + 
  Hyogo$pop$female + Nara$pop$female + Wakayama$pop$female
ind_R5_female_rate = (Mie_count_female + Shiga_count_female + Kyoto_count_female + Osaka_count_female +
                        Hyogo_count_female + Nara_count_female + Wakayama_count_female)/ind_R5_female_count

ind_R5_male_count = Mie$pop$male + Shiga$pop$male + Kyoto$pop$male + Osaka$pop$male + 
  Hyogo$pop$male + Nara$pop$male + Wakayama$pop$male
ind_R5_male_rate = (Mie_count_male + Shiga_count_male + Kyoto_count_male + Osaka_count_male +
                      Hyogo_count_male + Nara_count_male + Wakayama_count_male)/ind_R5_male_count

ind_R5_total_count = Mie$pop$total + Shiga$pop$total + Kyoto$pop$total + Osaka$pop$total + 
  Hyogo$pop$total + Nara$pop$total + Wakayama$pop$total
ind_R5_total_rate = (Mie_count_total + Shiga_count_total + Kyoto_count_total + Osaka_count_total +
                       Hyogo_count_total + Nara_count_total + Wakayama_count_total)/ind_R5_total_count

ind_R5_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R5_female_rate), as.numeric(ind_R5_male_rate), as.numeric(ind_R5_total_rate))
colnames(ind_R5_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R5_rate_summary, "ind_R5_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R5_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R5_female_count), as.numeric(ind_R5_male_count), as.numeric(ind_R5_total_count))
colnames(ind_R5_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R5_count_summary, "ind_R5_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R5 = read.demogdata("ind_R5_rate.txt", "ind_R5_count.txt", type="mortality", label = "R5", skip=0)
ind_R5_smooth = smooth.demogdata(ind_R5)

# R6 

ind_R6_female_count = Tottori$pop$female + Shimane$pop$female + Okayama$pop$female + Hiroshima$pop$female + Yamaguchi$pop$female
ind_R6_female_rate = (Tottori_count_female + Shimane_count_female + Okayama_count_female + Hiroshima_count_female + Yamaguchi_count_female)/ind_R6_female_count

ind_R6_male_count = Tottori$pop$male + Shimane$pop$male + Okayama$pop$male + Hiroshima$pop$male + Yamaguchi$pop$male
ind_R6_male_rate  = (Tottori_count_male + Shimane_count_male + Okayama_count_male + Hiroshima_count_male + Yamaguchi_count_male)/ind_R6_male_count

ind_R6_total_count = Tottori$pop$total + Shimane$pop$total + Okayama$pop$total + Hiroshima$pop$total + Yamaguchi$pop$total
ind_R6_total_rate  = (Tottori_count_total + Shimane_count_total + Okayama_count_total + Hiroshima_count_total + Yamaguchi_count_total)/ind_R6_total_count

ind_R6_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R6_female_rate), as.numeric(ind_R6_male_rate), as.numeric(ind_R6_total_rate))
colnames(ind_R6_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R6_rate_summary, "ind_R6_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R6_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R6_female_count), as.numeric(ind_R6_male_count), as.numeric(ind_R6_total_count))
colnames(ind_R6_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R6_count_summary, "ind_R6_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R6 = read.demogdata("ind_R6_rate.txt", "ind_R6_count.txt", type="mortality", label = "R6", skip=0)
ind_R6_smooth = smooth.demogdata(ind_R6)

# R7

ind_R7_female_count = Tokushima$pop$female + Kagawa$pop$female + Ehime$pop$female + Kochi$pop$female
ind_R7_female_rate  = (Tokushima_count_female + Kagawa_count_female + Ehime_count_female + Kochi_count_female)/ind_R7_female_count

ind_R7_male_count = Tokushima$pop$male + Kagawa$pop$male + Ehime$pop$male + Kochi$pop$male
ind_R7_male_rate  = (Tokushima_count_male + Kagawa_count_male + Ehime_count_male + Kochi_count_male)/ind_R7_male_count

ind_R7_total_count = Tokushima$pop$total + Kagawa$pop$total + Ehime$pop$total + Kochi$pop$total
ind_R7_total_rate  = (Tokushima_count_total + Kagawa_count_total + Ehime_count_total + Kochi_count_total)/ind_R7_total_count

ind_R7_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R7_female_rate), as.numeric(ind_R7_male_rate), as.numeric(ind_R7_total_rate))
colnames(ind_R7_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R7_rate_summary, "ind_R7_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R7_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R7_female_count), as.numeric(ind_R7_male_count), as.numeric(ind_R7_total_count))
colnames(ind_R7_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R7_count_summary, "ind_R7_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R7 = read.demogdata("ind_R7_rate.txt", "ind_R7_count.txt", type="mortality", label = "R7", skip=0)
ind_R7_smooth = smooth.demogdata(ind_R7)

# R8

ind_R8_female_count = Fukuoka$pop$female + Saga$pop$female + Nagasaki$pop$female + Kumamoto$pop$female + 
  Oita$pop$female + Miyazaki$pop$female + Kagoshima$pop$female + Okinawa$pop$female
ind_R8_female_rate = (Fukuoka_count_female + Saga_count_female + Nagasaki_count_female + Kumamoto_count_female + 
                        Oita_count_female + Miyazaki_count_female + Kagoshima_count_female + Okinawa_count_female)/ind_R8_female_count

ind_R8_male_count = Fukuoka$pop$male + Saga$pop$male + Nagasaki$pop$male + Kumamoto$pop$male + 
  Oita$pop$male + Miyazaki$pop$male + Kagoshima$pop$male + Okinawa$pop$male
ind_R8_male_rate = (Fukuoka_count_male + Saga_count_male + Nagasaki_count_male + Kumamoto_count_male + 
                      Oita_count_male + Miyazaki_count_male + Kagoshima_count_male + Okinawa_count_male)/ind_R8_male_count

ind_R8_total_count = Fukuoka$pop$total + Saga$pop$total + Nagasaki$pop$total + Kumamoto$pop$total + 
  Oita$pop$total + Miyazaki$pop$total + Kagoshima$pop$total + Okinawa$pop$total
ind_R8_total_rate = (Fukuoka_count_total + Saga_count_total + Nagasaki_count_total + Kumamoto_count_total + 
                       Oita_count_total + Miyazaki_count_total + Kagoshima_count_total + Okinawa_count_total)/ind_R8_total_count

ind_R8_rate_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R8_female_rate), as.numeric(ind_R8_male_rate), as.numeric(ind_R8_total_rate))
colnames(ind_R8_rate_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R8_rate_summary, "ind_R8_rate.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R8_count_summary = cbind(rep(1975:2016,101), rep(0:100,42), as.numeric(ind_R8_female_count), as.numeric(ind_R8_male_count), as.numeric(ind_R8_total_count))
colnames(ind_R8_count_summary) = c("Year","Age","Female","Male","Total")
write.table(ind_R8_count_summary, "ind_R8_count.txt", quote = FALSE, row.names=FALSE, col.names=TRUE)

ind_R8 = read.demogdata("ind_R8_rate.txt", "ind_R8_count.txt", type="mortality", label = "R8", skip=0)
ind_R8_smooth = smooth.demogdata(ind_R8)

region = c("ind_R1", "ind_R2", "ind_R3", "ind_R4", "ind_R5", "ind_R6", "ind_R7", "ind_R8")
region_smooth = c("ind_R1_smooth", "ind_R2_smooth", "ind_R3_smooth", "ind_R4_smooth", "ind_R5_smooth", 
                  "ind_R6_smooth", "ind_R7_smooth", "ind_R8_smooth")

# Errors

me   = ftsa:::me
mae  = ftsa:::mae
rmse = ftsa:::rmse


# iw: region index
# year_horizon: forecast horizon
# fmethod: "classical" or "M" (standard vs robust functional principal component analysis)
# alpha: 1 - nominal coverage probability

# function for region forecast

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
      eigen_decomp_female = eigen(C_0_female$BT_FT_fix_C0)
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
      eigen_decomp_male = eigen(C_0_male$BT_FT_fix_C0)
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
      eigen_decomp_total = eigen(C_0_total$BT_FT_fix_C0)
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

# function for prefecture forecast

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
      eigen_decomp_female = eigen(C_0_female$BT_FT_fix_C0)
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
      eigen_decomp_male = eigen(C_0_male$BT_FT_fix_C0)
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
      eigen_decomp_total = eigen(C_0_total$BT_FT_fix_C0)
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

