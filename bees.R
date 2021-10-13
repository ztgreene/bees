rm(list = ls()) # clear memory

library(tidyverse)
library(lme4)
library(MuMIn)
library(DT)

df_bees <- read.csv(file = "Bees.csv")

# state Binomial model is good option
# fit full GLM > probably won't converge, remove 3-way interaction
# Show GLM is overdispersed WRT  Binomial distribution
# Fit GLMER with OLRE, and 2-way interactions
# Do AIC analysis on models
# get best model, analyse effects
# plot fitted (without OLRE) vs proprtion bees active
# final comments etc.


# Fit complete linear model
df_bees$percent_active <- df_bees$detected / df_bees$tags *100

fit <- lm(percent_active ~ 1 + temp*rain*wind, data = df_bees, na.action = na.fail )

df_AIC <- dredge(fit_glm, rank = "AIC")

names(df_AIC)[1] <- "1"                  # rename the first column
df_AIC$"1" <- "+"                        # make all first column "+". Note use of quotes/
df_AIC$rain[!is.na(df_AIC$rain)] <- "+"    # replace numbers with "+"
df_AIC$temp[!is.na(df_AIC$temp)] <- "+"    # replace numbers with "+"
df_AIC$wind[!is.na(df_AIC$wind)] <- "+"    # replace numbers with "+"

df_AIC$"rain:temp"[!is.na(df_AIC$"rain:temp")] <- "+"    # replace numbers with "+"
df_AIC$"rain:wind"[!is.na(df_AIC$"rain:wind")] <- "+"    # replace numbers with "+"\
df_AIC$"temp:wind"[!is.na(df_AIC$"temp:wind")] <- "+"    # replace numbers with "+"
df_AIC$"rain:temp:wind"[!is.na(df_AIC$"rain:temp:wind")] <- "+"    # replace numbers with "+"

df_AIC$logLik <- round(df_AIC$logLik, 2) # round numbers to 2dp
df_AIC$delta <- round(df_AIC$delta, 2)   # round numbers to 2dp

df_AIC$AIC<- round(df_AIC$AIC, 2)   # round numbers to 2dp
df_AIC$weight<- round(df_AIC$weight, 2)   # round numbers to 2dp

filter(df_AIC, delta < 6) # display the AIC table for delta < 6

datatable(df_AIC, rownames=FALSE, caption = "AIC analysis of Honey Bee data set.")
  
# 1 + rain + temp + rain:temp

# 1 + temp       -------------------- THis is most parsimonious

# 1 + rain + temp

# 1 + temp + wind

# 1 + rain

# This fit is way worse than lm
fit_glm <- glmer(cbind(detected, tags-detected) ~ 1 + temp + rain + (1|Obs), family = binomial(link = 'logit'), data = df_bees, na.action = na.fail )
summary(fit_glm)




# Nicest 
fit_2 <- lm(sqrt(percent_active) ~ 1 + temp, data = df_bees, na.action = na.fail )
plot(fit_2)
summary(fit_2)

# sqrt(active) is best
fit_3 <- lmer(sqrt(percent_active) ~ 1 + temp*rain*wind + (1|Obs), data = df_bees, na.action = na.fail )
plot(fit_3)

# AIC of this shows

fit_4 <- lm(sqrt(percent_active) ~ temp + rain, data = df_bees, na.action = na.fail )
plot(fit_4)
summary(fit_4)

# add observation-level random effects
df_bees$Obs <- 1:nrow(df_bees)
df_bees$Obs <- factor(df_bees$Obs)
