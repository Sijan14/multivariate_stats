library(interactions)
library(tidyverse)
library(psych)
library(car)
library(lavaan)

df <- read_csv("survey.csv")

## Part A
df$Mnegaff_cent <- df$Mnegaff - mean(df$Mnegaff, na.rm = TRUE)

model1 <- lm(Moptim ~ Mnegaff + sex, data = df)
summary(model1)

model2 <- lm(Moptim ~ Mnegaff_cent*sex, data = df)
summary(model2)

sim_slopes(model2, pred = Mnegaff_cent, modx = sex, johnson_neyman = FALSE)

## Part B
# class practice
lav_model <- '
Mposaff ~ a*age
Mlifesat ~ c*age + b*Mposaff
indirect := a*b
total := a*b + c
'

fit <- sem(lav_model, data = df, se="bootstrap", bootstrap = 5000)
summary(fit, rsq=T, standardized=T)

parameterEstimates(fit, boot.ci.type = "bca.simple")

## Part C
df$Mpstress_cent <- df$Mpstress - mean(df$Mpstress, na.rm = TRUE)
df$Mnegaff_cent <- df$Mnegaff - mean(df$Mnegaff, na.rm = TRUE)

model3 <- lm(Mslfest ~ Mpstress_cent*Mnegaff_cent, data = df)
summary(model3)

# multicollinearity
vif(model3)

# normality
hist(residuals(model3))
qqnorm(residuals(model3))

# linearity
plot(model3, 1)

# homoscedasticity
plot(model3, 3)

# interact plot
interact_plot(model3, pred = Mpstress_cent, modx = Mnegaff_cent)

describe(df$Mnegaff_cent)
