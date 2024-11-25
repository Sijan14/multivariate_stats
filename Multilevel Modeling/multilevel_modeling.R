library(lme4)
library(nlme)
library(haven)
library(lmerTest)

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

# multilevel modeling with Level-1 predictors
mlm_model <- lmer(stress ~ age + gender + experien + (1|hospital), data = df)
summary(mlm_model)

# multilevel modeling with Level-2 predictors
mlm_model2 <- lmer(stress ~ age + gender + experien + as.factor(hospsize) + (1|hospital), data = df)
summary(mlm_model2)

# adding random slope to the model
mlm_model3 <- lmer(stress ~ age + gender + experien + as.factor(hospsize) + (1 + age|hospital), data = df)
summary(mlm_model3)
