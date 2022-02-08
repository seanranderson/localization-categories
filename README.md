# localization-categories
Sound source localization analysis code (unsupervised learning with stochastic prototypes)

This code was developed to analyze sound source localization data from perceptual experiments. In these experiments, listeners guess the location of a sound in a semi-circle around the head, and the results can be summarized in terms of the angle (between -90 and +90) where the sound was presented and the angle where the listener perceived the sound. The software takes the results from these experiments (function relating angle of target and perceived sound) and sorts them into one of twenty pre-defined categories. It is intended to be modified to fit the specific analysis needs of the user. 

Definitions of categories are given in `LocalizationCategoryDefinitions.R`. This code should be modified by the user to change parameters for categories, and add or remove categories. Functions from `LocalizationCategoryDefinitions.R` and `HelperFunctions.R` need to be input to the user workspace in R before other scripts will work properly.

The manuscript associated with this code had been published in PLOS ONE at https://doi.org/10.1371/journal.pone.0263516 where the data are also available.
