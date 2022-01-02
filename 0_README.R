# This file lists names of essential R files and their usage in the project.

###########################################################################
# Functions shared by both univariate and multivariate forecasting methods
###########################################################################

# 1_Importing_Japanese_mortality.R: import data from Japanese Mortality Database.
# 2_Exposure_to_risk.R: forecast exposure to risk at each level of the hierarchy of Japanese mortality rates.
# 3_Summing_matrix_S.R: construct summing matrix using forecast exposure to risk.
# 4_Deaths_count_and_multivariate_demography.R: count deaths at regional and prefecture-level and create smoothed multivariate demography data.

#################################
#################################
# Geographical factor hierarchy #
#################################
#################################

###############################################
# Univariate point forecast relevant functions
###############################################

# 5_Univariate_point_forecast.R: obtain point forecasts by the grouped univariate forecasting method without reconciliation.
# 6_Reconciled_univariate_point_forecast.R: reconcile univariate point forecasts using both bottom-up and optimal combination methods and calculate errors.

#################################################
# Multivariate point forecast relevant functions
#################################################

# 7_Multivariate_point_forecast.R: obtain point forecasts by the grouped multivariate forecasting method without reconciliation.
# 8_Reconciled_multivariate_point_forecast: reconcile multivariate point forecasts using both bottom-up and optimal combination methods and calculate errors.

###############################################
# Univariate interval score relevant functions
###############################################

# 9_Univariate_PI_function.R: define functions used to construct univariate prediction intervals.
# 10_Univariate_PI_base.R: calculate interval scores for the univariate interval predictions without reconciliation.
# 11_Univariate_reconciliation_function.R: define functions used to reconcile univariate interval predictions.
# 12_Univariate_bottom_up_reconciliation.R: calculate reconciled interval scores for the univariate forecasting method with the bottom-up method.
# 13_Univariate_optimal_combination_reconciliation.R: calculate reconciled interval scores for the univariate forecasting method by the optimal combination method.
# 14_Univariate_MinT_reconciliation.R: calculate reconciled interval scores for the univariate forecasting method by the MinT method

#################################################
# Multivariate interval score relevant functions
#################################################

# 15_Multivariate_PI_function.R: define functions used to construct multivariate prediction intervals.
# 16_Multivariate_PI_base.R: calculate interval scores for the multivariate interval predictions without reconciliation.
# 17_Multivariate_reconciliation_function.R: define functions used to reconcile multivariate interval predictions.
# 18_Multivariate_bottom_up_reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method with the bottom-up method.
# 19_Multivariate_optimal_combination_reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method with the optimal combination method.
# 20_Multivariate_MinT_reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method by the MinT method.

#############################
#############################
# Gender-specific hierarchy #
#############################
#############################

# 21_Gender_specific_base_forecasts.R: obtain point forecasts by the grouped multivariate forecasting method following the gender-specific hierarchical structure.
# 22_Gender_specific_summing_matrix.R: construct summing matrix using forecast exposure to risk following the gender-specific hierarchical structure.
# 23_Gender_specific_point_forecasts_reconciliation.R: reconcile multivariate point forecasts using the bottom-up, optimal combination, and MinT methods following the gender-specific summing matrix defined in 22. 
# 24_Gender_specific_PI_function.R: define functions used to construct multivariate prediction intervals following the gender-specific hierarchical structure.
# 25_Gender_specific_PI_base.R: calculate interval scores for the multivariate interval predictions following the gender-specific hierarchical structure.
# 26_Gender_specific_bottom_up_reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method with the bottom-up method following the gender-specific hierarchical structure.
# 27_Gender_specific_optimal_combination_reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method with the optimal combination method following the gender-specific hierarchical structure.
# 28_Gender_specific_MinT reconciliation.R: calculate reconciled interval scores for the multivariate forecasting method by the MinT method following the gender-specific hierarchical structure.


