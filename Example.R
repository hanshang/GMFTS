########################################
# Example of computating point forecast
########################################

library(demography)

state = c("Japan", "Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki",  "Tochigi", "Gunma", "Saitama",  
          "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka", "Aichi",    
          "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", "Shimane", "Okayama", "Hiroshima", "Yamaguchi",
          "Tokushima", "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa") 

state_all = c("Japan_all", "Hokkaido_all", "Aomori_all", "Iwate_all", "Miyagi_all", "Akita_all", "Yamagata_all", "Fukushima_all",
              "Ibaraki_all", "Tochigi_all", "Gunma_all", "Saitama_all", "Chiba_all", "Tokyo_all", "Kanagawa_all", "Niigata_all",
              "Toyama_all", "Ishikawa_all", "Fukui_all", "Yamanashi_all", "Nagano_all", "Gifu_all", "Shizuoka_all", "Aichi_all",
              "Mie_all", "Shiga_all", "Kyoto_all", "Osaka_all", "Hyogo_all", "Nara_all", "Wakayama_all", "Tottori_all", "Shimane_all",
              "Okayama_all", "Hiroshima_all", "Yamaguchi_all", "Tokushima_all", "Kagawa_all", "Ehime_all", "Kochi_all",
              "Fukuoka_all", "Saga_all", "Nagasaki_all", "Kumamoto_all", "Oita_all", "Miyazaki_all", "Kagoshima_all", "Okinawa_all")

state_smooth = c("Japan_smooth", "Hokkaido_smooth", "Aomori_smooth", "Iwate_smooth", "Miyagi_smooth", "Akita_smooth", "Yamagata_smooth", "Fukushima_smooth", "Ibaraki_smooth",  "Tochigi_smooth", "Gunma_smooth", "Saitama_smooth",  
                 "Chiba_smooth", "Tokyo_smooth", "Kanagawa_smooth", "Niigata_smooth", "Toyama_smooth", "Ishikawa_smooth", "Fukui_smooth", "Yamanashi_smooth", "Nagano_smooth", "Gifu_smooth", "Shizuoka_smooth", "Aichi_smooth",    
                 "Mie_smooth", "Shiga_smooth", "Kyoto_smooth", "Osaka_smooth", "Hyogo_smooth", "Nara_smooth", "Wakayama_smooth", "Tottori_smooth", "Shimane_smooth", "Okayama_smooth", "Hiroshima_smooth", "Yamaguchi_smooth",
                 "Tokushima_smooth", "Kagawa_smooth", "Ehime_smooth", "Kochi_smooth", "Fukuoka_smooth", "Saga_smooth", "Nagasaki_smooth", "Kumamoto_smooth", "Oita_smooth", "Miyazaki_smooth", "Kagoshima_smooth", "Okinawa_smooth") 


#############################################
# read data from Japanese Mortality Database
#############################################

read.jpn <- function (region,  label = region) 
{
	path <- paste("http://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/",   "Mx_1x1.txt", sep = "")
	txt <- RCurl::getURL(path)
  	con <- textConnection(txt)
  	mx <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."), 
            TRUE)
  	close(con)
  	if (class(mx) == "try-error") 
    	stop("Connection error at www.mortality.org. Please check username, password and country label.")
  	path <- paste("http://www.ipss.go.jp/p-toukei/JMD/", region, "/STATS/",   "Exposures_1x1.txt", sep = "")
	txt <- RCurl::getURL(path)
  	con <- textConnection(txt)
  	pop <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."), 
             TRUE)
  	close(con)
  	if (class(pop) == "try-error") 
    	stop("Exposures file not found at www.mortality.org")
  	obj <- list(type = "mortality", label = label, lambda = 0)
  	obj$year <- sort(unique(mx[, 1]))
  	n <- length(obj$year)
  	m <- length(unique(mx[, 2]))
  	obj$age <- mx[1:m, 2]
  	mnames <- names(mx)[-c(1, 2)]
  	n.mort <- length(mnames)
  	obj$rate <- obj$pop <- list()
  	for (i in 1:n.mort) 
  	{
    	obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
	    obj$rate[[i]][obj$rate[[i]] < 0] <- NA
    	obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
	    obj$pop[[i]][obj$pop[[i]] < 0] <- NA
    	dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, obj$year)
  	}
  	names(obj$pop) = (names(obj$rate) <- tolower(mnames))
  	obj$age <- as.numeric(as.character(obj$age))
  	if (is.na(obj$age[m])) 
    	obj$age[m] <- 2 * obj$age[m - 1] - obj$age[m - 2]
  	return(structure(obj, class = "demogdata"))
}

# mortality rates before smoothing, 0 to 100

Japan     = extract.years(extract.ages(read.jpn("00", "Japan"), 0:100), 1975:2016)
Hokkaido  = extract.years(extract.ages(read.jpn("01", "Hokkaido"), 0:100), 1975:2016)
Aomori    = extract.years(extract.ages(read.jpn("02", "Aomori"), 0:100), 1975:2016)
Iwate     = extract.years(extract.ages(read.jpn("03", "Iwate"), 0:100), 1975:2016)
Miyagi    = extract.years(extract.ages(read.jpn("04", "Miyagi"), 0:100), 1975:2016)
Akita     = extract.years(extract.ages(read.jpn("05", "Akita"), 0:100), 1975:2016)
Yamagata  = extract.years(extract.ages(read.jpn("06", "Yamagata"), 0:100), 1975:2016)
Fukushima = extract.years(extract.ages(read.jpn("07", "Fukushima"), 0:100), 1975:2016)
Ibaraki   = extract.years(extract.ages(read.jpn("08", "Ibaraki"), 0:100), 1975:2016)
Tochigi   = extract.years(extract.ages(read.jpn("09", "Tochigi"), 0:100), 1975:2016)
Gunma     = extract.years(extract.ages(read.jpn("10", "Gunma"), 0:100), 1975:2016)
Saitama   = extract.years(extract.ages(read.jpn("11", "Saitama"), 0:100), 1975:2016)
Chiba     = extract.years(extract.ages(read.jpn("12", "Chiba"), 0:100), 1975:2016)
Tokyo     = extract.years(extract.ages(read.jpn("13", "Tokyo"), 0:100), 1975:2016)
Kanagawa  = extract.years(extract.ages(read.jpn("14", "Kanagawa"), 0:100), 1975:2016)
Niigata   = extract.years(extract.ages(read.jpn("15", "Niigata"), 0:100), 1975:2016)
Toyama    = extract.years(extract.ages(read.jpn("16", "Toyama"), 0:100), 1975:2016)
Ishikawa  = extract.years(extract.ages(read.jpn("17", "Ishikawa"), 0:100), 1975:2016)
Fukui     = extract.years(extract.ages(read.jpn("18", "Fukui"), 0:100), 1975:2016)
Yamanashi = extract.years(extract.ages(read.jpn("19", "Yamanashi"), 0:100), 1975:2016)
Nagano    = extract.years(extract.ages(read.jpn("20", "Nagano"), 0:100), 1975:2016)
Gifu      = extract.years(extract.ages(read.jpn("21", "Gifu"), 0:100), 1975:2016)
Shizuoka  = extract.years(extract.ages(read.jpn("22", "Shizuoka"), 0:100), 1975:2016)
Aichi     = extract.years(extract.ages(read.jpn("23", "Aichi"), 0:100), 1975:2016)
Mie       = extract.years(extract.ages(read.jpn("24", "Mie"), 0:100), 1975:2016)
Shiga     = extract.years(extract.ages(read.jpn("25", "Shiga"), 0:100), 1975:2016)
Kyoto     = extract.years(extract.ages(read.jpn("26", "Kyoto"), 0:100), 1975:2016)
Osaka     = extract.years(extract.ages(read.jpn("27", "Osaka"), 0:100), 1975:2016)
Hyogo     = extract.years(extract.ages(read.jpn("28", "Hyogo"), 0:100), 1975:2016)
Nara      = extract.years(extract.ages(read.jpn("29", "Nara"), 0:100), 1975:2016)
Wakayama  = extract.years(extract.ages(read.jpn("30", "Wakayama"), 0:100), 1975:2016)
Tottori   = extract.years(extract.ages(read.jpn("31", "Tottori"), 0:100), 1975:2016)
Shimane   = extract.years(extract.ages(read.jpn("32", "Shimane"), 0:100), 1975:2016)
Okayama   = extract.years(extract.ages(read.jpn("33", "Okayama"), 0:100), 1975:2016)
Hiroshima = extract.years(extract.ages(read.jpn("34", "Hiroshima"), 0:100), 1975:2016)
Yamaguchi = extract.years(extract.ages(read.jpn("35", "Yamaguchi"), 0:100), 1975:2016)
Tokushima = extract.years(extract.ages(read.jpn("36", "Tokushima"), 0:100), 1975:2016)
Kagawa    = extract.years(extract.ages(read.jpn("37", "Kagawa"), 0:100), 1975:2016)
Ehime     = extract.years(extract.ages(read.jpn("38", "Ehime"), 0:100), 1975:2016)
Kochi     = extract.years(extract.ages(read.jpn("39", "Kochi"), 0:100), 1975:2016)
Fukuoka   = extract.years(extract.ages(read.jpn("40", "Fukuoka"), 0:100), 1975:2016)
Saga      = extract.years(extract.ages(read.jpn("41", "Saga"), 0:100), 1975:2016)
Nagasaki  = extract.years(extract.ages(read.jpn("42", "Nagasaki"), 0:100), 1975:2016)
Kumamoto  = extract.years(extract.ages(read.jpn("43", "Kumamoto"), 0:100), 1975:2016)
Oita      = extract.years(extract.ages(read.jpn("44", "Oita"), 0:100), 1975:2016)
Miyazaki  = extract.years(extract.ages(read.jpn("45", "Miyazaki"), 0:100), 1975:2016)
Kagoshima = extract.years(extract.ages(read.jpn("46", "Kagoshima"), 0:100), 1975:2016)
Okinawa   = extract.years(extract.ages(read.jpn("47", "Okinawa"), 0:100), 1975:2016)

# mortality rates of all ages, 1 to 110

Japan_all     = extract.years(read.jpn("00", "Japan"), 1975:2016)
Hokkaido_all  = extract.years(read.jpn("01", "Hokkaido"), 1975:2016)
Aomori_all    = extract.years(read.jpn("02", "Aomori"), 1975:2016)
Iwate_all     = extract.years(read.jpn("03", "Iwate"), 1975:2016)
Miyagi_all    = extract.years(read.jpn("04", "Miyagi"), 1975:2016)
Akita_all     = extract.years(read.jpn("05", "Akita"), 1975:2016)
Yamagata_all  = extract.years(read.jpn("06", "Yamagata"), 1975:2016)
Fukushima_all = extract.years(read.jpn("07", "Fukushima"), 1975:2016)
Ibaraki_all   = extract.years(read.jpn("08", "Ibaraki"), 1975:2016)
Tochigi_all   = extract.years(read.jpn("09", "Tochigi"), 1975:2016)
Gunma_all     = extract.years(read.jpn("10", "Gunma"), 1975:2016)
Saitama_all   = extract.years(read.jpn("11", "Saitama"), 1975:2016)
Chiba_all     = extract.years(read.jpn("12", "Chiba"), 1975:2016)
Tokyo_all     = extract.years(read.jpn("13", "Tokyo"), 1975:2016)
Kanagawa_all  = extract.years(read.jpn("14", "Kanagawa"), 1975:2016)
Niigata_all   = extract.years(read.jpn("15", "Niigata"), 1975:2016)
Toyama_all    = extract.years(read.jpn("16", "Toyama"), 1975:2016)
Ishikawa_all  = extract.years(read.jpn("17", "Ishikawa"), 1975:2016)
Fukui_all     = extract.years(read.jpn("18", "Fukui"), 1975:2016)
Yamanashi_all = extract.years(read.jpn("19", "Yamanashi"), 1975:2016)
Nagano_all    = extract.years(read.jpn("20", "Nagano"), 1975:2016)
Gifu_all      = extract.years(read.jpn("21", "Gifu"), 1975:2016)
Shizuoka_all  = extract.years(read.jpn("22", "Shizuoka"), 1975:2016)
Aichi_all     = extract.years(read.jpn("23", "Aichi"), 1975:2016)
Mie_all       = extract.years(read.jpn("24", "Mie"), 1975:2016)
Shiga_all     = extract.years(read.jpn("25", "Shiga"), 1975:2016)
Kyoto_all     = extract.years(read.jpn("26", "Kyoto"), 1975:2016)
Osaka_all     = extract.years(read.jpn("27", "Osaka"), 1975:2016)
Hyogo_all     = extract.years(read.jpn("28", "Hyogo"), 1975:2016)
Nara_all      = extract.years(read.jpn("29", "Nara"), 1975:2016)
Wakayama_all  = extract.years(read.jpn("30", "Wakayama"), 1975:2016)
Tottori_all   = extract.years(read.jpn("31", "Tottori"), 1975:2016)
Shimane_all   = extract.years(read.jpn("32", "Shimane"), 1975:2016)
Okayama_all   = extract.years(read.jpn("33", "Okayama"), 1975:2016)
Hiroshima_all = extract.years(read.jpn("34", "Hiroshima"), 1975:2016)
Yamaguchi_all = extract.years(read.jpn("35", "Yamaguchi"), 1975:2016)
Tokushima_all = extract.years(read.jpn("36", "Tokushima"), 1975:2016)
Kagawa_all    = extract.years(read.jpn("37", "Kagawa"), 1975:2016)
Ehime_all     = extract.years(read.jpn("38", "Ehime"), 1975:2016)
Kochi_all     = extract.years(read.jpn("39", "Kochi"), 1975:2016)
Fukuoka_all   = extract.years(read.jpn("40", "Fukuoka"), 1975:2016)
Saga_all      = extract.years(read.jpn("41", "Saga"), 1975:2016)
Nagasaki_all  = extract.years(read.jpn("42", "Nagasaki"), 1975:2016)
Kumamoto_all  = extract.years(read.jpn("43", "Kumamoto"), 1975:2016)
Oita_all      = extract.years(read.jpn("44", "Oita"), 1975:2016)
Miyazaki_all  = extract.years(read.jpn("45", "Miyazaki"), 1975:2016)
Kagoshima_all = extract.years(read.jpn("46", "Kagoshima"), 1975:2016)
Okinawa_all   = extract.years(read.jpn("47", "Okinawa"), 1975:2016)

# smoothed functional curves using penalized regression spline with monotonic constraint

for(i in 1:48)
{
  assign(state_smooth[i], smooth.demogdata(get(state[i])))
}


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
    eigen_decomp = eigen(C_0$BT_FT_fix_C0)
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

me   = ftsa:::me
mae  = ftsa:::mae
rmse = ftsa:::rmse

# function for prefecture forecast

mfts_back_test <- function(iw,  pcamethod = c("static", "dynamic"), fmethod = "classical", year_horizon)
{
  forecast_year = (year_horizon + 1)
  n_year = tail(get(state_smooth[1])$year,1) - forecast_year
  res_male = res_female = array(NA, dim = c(year_horizon, 101, year_horizon))
  e0_female = e0_male = matrix(NA, year_horizon, year_horizon)
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
    
    for(ik in 1:year_horizon)
    {
      e0_female[ik,ij] = demography:::lt(fun_forc$fore_res[1:101,ik], startage=0, agegroup=1, sex="female")$ex[1]
      e0_male[ik,ij]   = demography:::lt(fun_forc$fore_res[102:202,ik], startage=0, agegroup=1, sex="male")$ex[1]
    }
  }
  # MAE & RMSE
  female_me = male_me = female_mae = male_mae = female_rmse = male_rmse = 
    e0_female_me = e0_male_me = e0_female_mae = e0_male_mae = e0_female_rmse = e0_male_rmse = vector("numeric",year_horizon)
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
    
    ## e0
    
    # me 
    e0_female_me[ik] = me(e0_female[ik,1:(forecast_year-ik)], e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="female"))
    e0_male_me[ik]   = me(e0_male[ik,1:(forecast_year-ik)],   e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="male"))
    
    # mae
    e0_female_mae[ik] = mae(e0_female[ik,1:(forecast_year-ik)], e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="female"))
    e0_male_mae[ik]   = mae(e0_male[ik,1:(forecast_year-ik)],   e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="male"))
    
    # rmse
    e0_female_rmse[ik] = rmse(e0_female[ik,1:(forecast_year-ik)], e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="female"))
    e0_male_rmse[ik]   = rmse(e0_male[ik,1:(forecast_year-ik)],   e0(extract.years(get(state[iw]), years = (2001+ik):2016), series="male"))
  }
  return(list(res_female = res_female, res_male = res_male,
              train_residual_female = train_residual_female, train_residual_male = train_residual_male,
              female_me = female_me, male_me = male_me, 
              female_mae = female_mae, male_mae = male_mae,
              female_rmse = female_rmse, male_rmse = male_rmse,
              e0_female_me = e0_female_me, e0_male_me = e0_male_me, 
              e0_female_mae = e0_female_mae, e0_male_mae = e0_male_mae, 
              e0_female_rmse = e0_female_rmse, e0_male_rmse = e0_male_rmse))
}


#######################################################################
# Multivariate forecast of age-specific mortality rates by prefectures
#######################################################################

mfts_state_train_residual_female = mfts_state_train_residual_male = paste("mfts_example_train_residual", 1:5, sep = "_")
mfts_state_forc_female = mfts_state_forc_male = paste("mfts_example_forc", 1:5, sep = "_")
mfts_state_me_female = mfts_state_me_male = paste("mfts_example_me", 1:5, sep = "_")
mfts_state_mae_female = mfts_state_mae_male = paste("mfts_example_mae", 1:5, sep = "_")
mfts_state_rmse_female = mfts_state_rmse_male = paste("mfts_example_rmse", 1:5, sep = "_")

for(ik in 2:6)
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













