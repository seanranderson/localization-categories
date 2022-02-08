# This R script inputs data from the manuscript into the 
# the workspace in R. It contains instructions on how to 
# replicate particular analyses in case the user is not 
# familiar with R. For additional information and code, see 
# Supplementary Appendix 2 accompanying Anderson et al. 
# (2022).
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Dataset from accompanying open-access publication supporting files
  # doi: xx
  # Took approximately 1 hour to complete author's laptop computer
raw.dat <- read.csv('S3_Dataset.csv',head=T)

# Load in dependencies
library(reshape2)

# Aggregate data by mean and SD
agg.mn <- aggregate(RespAz ~ SubID + TargAz, 
                    data = raw.dat, 
                    FUN = mean)
agg.sd <- aggregate(RespAz ~ SubID + TargAz, 
                    data = raw.dat, 
                    FUN = sd)
agg <- rbind(agg.mn, agg.sd)
agg$mnvssd <- c(rep(1, length( agg[,1]) / 2), # mean
                rep(2, length( agg[,1]) / 2)) # SD
agg <- with(agg,
            agg[order(SubID, TargAz, mnvssd), ])

# Cast data into new dataframe
dat <- acast(agg, SubID ~ TargAz + mnvssd, 
             value.var = 'RespAz')
dat <- dat[, c(seq(1,37,2), seq(2,38,2)) ]

# ------------------- INSTRUCTIONS FOR USE -------------------
# - Run `HelperFunctions.R` and `LocalizationCategoryDefinitions.R` 
#   to input functions to the workspace.
#
# - Run `SimulationStudy.R` script to replicate the 
#   simulation study in Anderson et al. (2022).
#     - Note: This will take a long time to complete (e.g., one 
#       one day on the primary author's laptop computer).
#
# - Run `LocalizationCategories.R` script to assign listeners 
#   to categories.
# 
# - Run `MiscRegressionAnalyses.R` to replicate the regression 
#   results from Anderson et al. (2022).
