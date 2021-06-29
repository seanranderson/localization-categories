# This R script inputs data from the manuscript into the 
# the workspace in R. It contains instructions on how to 
# replicate particular analyses in case the user is not 
# familiar with R. For additional information and code, see 
# Supplementary Appendix 2 accompanying Anderson et al. 
# (2021).
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Columns in dataset correspond to the target angle, followed 
# by an underscore, and 'mean' or 'sd' for standard deviation.
# Rows in dataset correspond to listeners arranged according 
# to the alphabet.
dat <- read.table('Anderson2021_PLOSONE_Dataset.txt', 
                  sep = ',',
                  header=T)


raw.dat <- read.table('Anderson2021_PLOSONE_forRegression.txt',
                      sep = ',',
                      header = T)

# ------------------- INSTRUCTIONS FOR USE -------------------
# - Run `HelperFunctions.R` and `LocalizationCategoryDefinitions.R` 
#   to input functions to the workspace.
#
# - Run `SimulationStudy.R` script to replicate the 
#   simulation study in Anderson et al. (2021).
#     - Note: This will take a long time to complete (e.g., one 
#       one day on the primary author's computer).
#
# - Run `LocalizationCategories.R` script to assign listeners 
#   to categories.
# 
# - Run `MiscRegressionAnalyses.R` to replicate the regression 
#   results from Anderson et al. (2021).
