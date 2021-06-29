# This script is written to replicate the analyses included in 
# Anderson et al. (2021). They are otherwise unrelated to the 
# localization-categories analysis.
# 
# Sean R. Anderson - sean.hearing@gmail.com

# Load required packages
#   Note: If you do not currently have these libraries installed 
#   in R, you will first need to manually enter 
#   `install.packages('PackageName')`, replacing PackageName with
#   the name of each library below.
require(lme4)
require(lmerTest)

# Multiple linear regressions
  # Fig. 3A
summary(lm(RMS.Overall ~ Inter.Implantation.Delay * Onset.Group,
           data = raw.dat))
  # Fig. 3B
summary(lm(LSI ~ Inter.Implantation.Delay * Onset.Group,
           data = raw.dat))
  # Fig. 4A
summary(lm(A.minus.B ~ Inter.Implantation.Delay * Onset.Group,
           data = raw.dat))
  # Fig. 4B
summary(lm(Slope ~ Inter.Implantation.Delay * Onset.Group,
           data = raw.dat))

# Transform data with log transformation for regression
raw.dat$log.RMS <- log(raw.dat$RMS.by.Target)

# Regression from Table 2
mmod <- lmer(log.RMS ~ Inter.Implantation.Delay + Onset.Group 
             + Target + Target.Squared
             + Inter.Implantation.Delay * Onset.Group 
             + Inter.Implantation.Delay * Target 
             + Inter.Implantation.Delay * Target.Squared
             + Onset.Group * Target.Squared + (1 | SubID),
              data = raw.dat,
              REML = F)
summary(mmod)
