library(psych)
library(tidyverse)
library(apaTables)

df <- read_csv("survey.csv")
names(df)

## Part A
# Question 1
df$Optimism <- rowMeans(df[ , c("op1", "Rop2", "op3", "Rop4", "op5", "Rop6")], na.rm = TRUE)

# Question 2
describe(df$Optimism) # mean = 3.68, sd = 0.74

# Question 3
psych::alpha(df[, c("op1", "Rop2", "op3", "Rop4", "op5", "Rop6")]) # Cronbach's alpha = 0.8

# Question 4
table(df$age_group)

df <- df %>% 
  mutate(MiddleAged_Adults = if_else(age_group == 1, 1, 0)) %>% 
  mutate(Older_Adults = if_else(age_group == 2, 1, 0))

head(df[ , c("age_group", "MiddleAged_Adults", "Older_Adults")], 5) # checks out

# Question 5
t.test(Optimism ~ child, data = df)

df %>% 
  group_by(child) %>% 
  summarise(Mean = mean(Optimism, na.rm = TRUE), SD = sd(Optimism, na.rm = TRUE))

# Question 6
anova_model <- aov(Optimism ~ as.factor(age_group), data = df)
summary(anova_model)

TukeyHSD(anova_model)

# Question 7
con_table <- table(df$sex, df$smoke)
chisq.test(con_table)

# Question 8
table_cor <- df[ , c("sex", "age", "smoke", "Mslfest", "Mposaff", "Mnegaff", "Mpstress", "Mlifesat")]
correlation <- corr.test(table_cor)

apa.cor.table(table_cor, filename = "correlation_matrix.doc")

## Part B
# Question 9
model <- lm(Mnegaff ~ sex + child + smoke + Mpstress, data = df)
summary(model)

# Question 10, 11 & 12
step1 <- lm(Mnegaff ~ sex, data = df)
step2 <- lm(Mnegaff ~ sex + smoke, data = df)
step3 <- lm(Mnegaff ~ sex + smoke + child, data = df)
step4 <- lm(Mnegaff ~ sex + smoke + child + Mpstress, data = df)

summary(step1)
summary(step2)
summary(step3)
summary(step4)

## Part C
# Question 1&2
describe(df$Mposaff)

# Question 3&4
table(df$child)

# Question 5
t.test(df$Mposaff ~ df$child, var.equal = TRUE)

df %>% 
  group_by(child) %>% 
  summarise(SD = sd(Mposaff, na.rm = T))

# Question 6
corr.test(df$Mlifesat, df$Mpstress)

# Question 7, 8 & 9
m_model <- lm(Mslfest ~ age + smoke + Mpstress, data = df)
summary(m_model)




