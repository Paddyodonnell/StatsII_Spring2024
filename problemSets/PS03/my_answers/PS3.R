#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "stargazer", "ggplot2", "tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

#View(gdp_data)

# 1)

# Change GDPWdiff from numeric to categorical, and then factor with reference.
gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff == 0, "no change", 
                            ifelse(gdp_data$GDPWdiff > 0, "positive", "negative"))

gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff, ordered = FALSE)
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")

model <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)

summary(model)
stargazer(model)


exp(cbind(OR=coef(model), confint(model)))

##########################################################
# IGNORE

#Cut off points

# Extract coefficients, the log odds.
coefficients <- coef(model)

# Define the reference category
reference_category <- "no change"

# Convert log-odds to probabilities using logistic function
probabilities <- exp(coefficients) / rowSums(exp(coefficients))

print(probabilities)

# Determine cutoff points (e.g., threshold of 0.5)
cutoff_points <- apply(probabilities, 2, function(x) {
  ifelse(x > 0.5, "positive", ifelse(x < 0.5, "negative", reference_category))
})

# View cutoff points
print(cutoff_points)
##############################################################################
# IGNORE

# Isolate coefficients
model_coef <- coef(model)

# Get cut off points
cut_off_points <- exp(model_coef)

View(cut_off_points)
stargazer(cut_off_points)

exp(model_coef[,c(1:3)])
View(cut_off_points)

#############################################################################
# 2)

# Order the factors.
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff, ordered = TRUE, levels = c("negative", "no change", "positive"))

model2 <- polr(GDPWdiff ~ REG + OIL, Hess = T, data = gdp_data)

summary(model2)
stargazer(model2)

# Table of intercepts, cutoff points




# Odds ratios

odds_ratio <- exp(cbind(OR = coef(model2), confint(model2)))

exp(cbind(OR = coef(model2), confint(model2)))

View(odds_ratio)
summary(odds_ratio)
stargazer(odds_ratio)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

View(mexico_elections)

# a)

# Run Poisson regression.
model3 <- glm(PAN.visits.06 ~ competitive.district
          + marginality.06 + PAN.governor.06,
          family = "poisson", data = mexico_elections)

summary(model3)
stargazer(model3)

# Odds Ratios

or_model3 <- exp(cbind(OR=coef(model3), confint(model3)))

print(or_model3)
stargazer(or_model3)

# Is the model zero inflated?

# Have a look at the distribution of the outcome variable.
hist(mexico_elections$PAN.visits.06)
# The data appears to be zero inflated.

# Run a zero inflated Poisson regression.

library(pscl)
zip_model3 <- zeroinfl(PAN.visits.06 ~ competitive.district
                            + marginality.06 + PAN.governor.06,
                            data = mexico_elections, dist = "poisson")

summary(zip_model3)
stargazer(zip_model3)

coef(zip_model3)

# Test the models.

library(AER)
dispersiontest(model3)
AIC (model3, zip_model3)

# The model could be somewhat inflated but the high p-value of the dispersion test
# suggests that we do not have sufficient evidence that make that claim.
# Also looking at the coefficients and p-values of the zero inflated model
# it does not appear to offer more useful insights than
# the regular Poisson model. 

# b)

# Make table of test-statistics and p-values.

#install.packages("xtable")
library(xtable)

xtable(summary(model3))

#z-value: Test statistic (z-value) calculated as the coefficient estimate
#divided by its standard error.

#Pr(>|z|): p-values corresponding to the z-values.
#These p-values represent the probability of observing 
#a test statistic as extreme as, or more extreme than, 
#the one calculated, under the null hypothesis 
#that the coefficient is equal to zero.

# c)

model3_cfs <- coef(model3)

exp(model3_cfs[1] + model3_cfs[2]*1 + model3_cfs[3]*0 + model3_cfs[4]*1)

pred <- data.frame(competitive.district = 1,
                   marginality.06 = 0,
                   PAN.governor.06 = 1)

predict(model3, pred, type = "response")

