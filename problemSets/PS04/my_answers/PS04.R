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

lapply(c("nnet", "MASS", "stargazer", "ggplot2", "tidyverse", "eha",
         "survival"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


########################
# Question.
########################

# Load data.
data(package = "eha")
data("child")

# View the data.
View(child)

# Fit Cox Proportional Hazard model
add_surv <- coxph(Surv(enter, exit, event) ~ m.age + sex,
      data = child)

# Check the summary.
summary(add_surv)

# Produce LaTeX table.
stargazer(add_surv)
# Some changes made in LaTeX

# Fit Cox Proportional Hazard model with interaction effect.

interact_surv <- coxph(Surv(enter, exit, event) ~ m.age * sex,
                  data = child)

# According to the p_value the interaction effect is not significant.
# The coefficient is also very small.
summary(interact_surv)

# Assessing additive model.

assessment <- 
  
drop1(add_surv, test = "Chisq")

summary(assessment)

stargazer(assessment)

# Plotting the cumulative hazard function

plot_CoxPH <- coxreg(Surv(enter, exit, event) ~ m.age + sex,
       data = child)

plot(plot_CoxPH)



