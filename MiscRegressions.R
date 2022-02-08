# This script is written to replicate the analyses included in 
# Anderson et al. (2021). They are otherwise unrelated to the 
# localization-categories analysis. This analysis is identical
# to the `S1_File.R` file included with the supporting files 
# of the original manuscript, except that the data frame is 
# renamed `reg.dat` in this version. To obtain the data used 
# here, please see the manuscript with which the data were 
# published at doi: xx
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Load required packages
#   Note: If you do not currently have these libraries installed 
#   in R, you will first need to manually enter 
#   `install.packages('PackageName')`, replacing PackageName with
#   the name of each library below.

# Prepare workspace
rm(list=ls())
library(car)
library(lme4)
library(lmerTest)
set.seed(5219)

# Read in the dataset
reg.dat <- read.csv('S1_Dataset.csv',head=T)
dat.by.targ <- read.csv('S2_Dataset.csv',head=T)

# Regression analysis in Root-mean-square error section
  # Associated with Fig 4A
summary(lm(formula = RMS ~ OnsetGroup + TestingAge, 
           data = reg.dat))

  # Associated with Table 2
    # Compute squared target angle
dat.by.targ$targ.sq <- dat.by.targ$TargAz^2
    # Merge with original dataset
dat.by.targ <- merge(dat.by.targ,reg.dat,by='SubID')
    # Compute log-transformed RMS error
dat.by.targ$log.rms <- log(dat.by.targ$RMSByTarg)

  # Complete regression
summary(lmer(log.rms ~ OnsetGroup * targ.sq
              + (1 | SubID),
              REML = F,
              data = dat.by.targ))

# Regression analysis in Localization sensitivity analysis section
  # Associated with Fig 4B
summary(lm(formula = LSI ~ OnsetGroup + TestingAge, 
           data = reg.dat))

# Correlation between RMS error and LSI, associated with Fig 4C
summary(lm(RMS ~ LSI,
           data = reg.dat))

# Regression analysis in Four-parameter logistic functions section
  # Associated with Fig 5A
summary(lm(A.B ~ OnsetGroup + TestingAge,
           data = reg.dat))

  # Associated with Fig 5B
summary(lm(LogisticSlope ~ OnsetGroup + TestingAge,
           data = reg.dat))
