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
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "packages"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Poisson

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# What conclusions would you draw from this analysis?

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

View(long_data)

# Wrangle

long_data <- within(long_data, {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

# Eda

str(long_data)
summary(long_data)

with(long_data,
     list(mean(art), var(art))) # Do we meet the assumptions for a poisson?

# a) Examine the distribution of response variable

hist(long_data$art)

library(ggplot2)

ggplot(long_data, aes(ment, art, color = fem)) +
  geom_jitter(alpha = 0.5)

# OLS?
mod.lm <- lm(art ~ ., data = long_data) 
summary(mod.lm)

mod2.lm <- lm(art ~ fem * ., data = long_data) # interaction effects on gender?
summary(mod2.lm)

# Do we meet assumptions?

plot(predict(mod2.lm), abs(resid(mod2.lm)), xlab = "predicted", ylab = "Absolute")

sresid <- rstandard(mod2.lm) # distribution of standardised residuals
hist(sresid, main = "")

par(mfrow = C(2, 2))
plot(mod2.lm) # R's biuilt in plots for linear resgression model assessment

# b) Poisson regression
mod.ps <- glm(art ~ ., data = long_data, family = poisson)
summary(mod.ps)

# interpreting output
cfs <- coef(mod.ps)

# Predicted no. of articles
exp(cfs[1] + cfs[2]*0 + cfs[3]*5 + cfs[4]*2 + cfs[5]*1 + cfs[6]*1)

pred <- data.frame(fem = FALSE
                   )

# check the the predict() function
predict(mod.ps, newdata = pred, type = "response")

# plot predictions vs count
ggplot(data = NULL, aes(x = mod.ps$fitted.values, y = long_data$art)) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = "blue") #+
  #geom_smooth(method = "loess", color = "red)








