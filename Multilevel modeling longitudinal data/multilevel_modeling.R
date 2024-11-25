library(lme4)
library(nlme)
library(haven)

df <- read_sav("Nurses.sav")
names(df)

# Check for ICC to see if multilevel modeling is required
intercept_model <- lmer(stress ~ (1|hospital), data = df)
summary(intercept_model)

"""
ICC is similar to R-squared value. 
Formula = 0.2851 / (0.2851 + 0.6863), 
ICC value = 0.29

Meaning 29% of the variance in the DV (stress level) is explained by the clustering variable
"""

mlm_model <- lmer(stress ~ age + gender + experien + (1|hospital), data = df)
summary(mlm_model)
