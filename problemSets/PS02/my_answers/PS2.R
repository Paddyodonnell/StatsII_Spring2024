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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
View(climateSupport)
str(climateSupport)

#Creating dummy variables.
climateSupport$choice <- ifelse(climateSupport$choice == "Supported", 1, 0)
climateSupport <- within(climateSupport, {
  countries_20 <- ifelse(countries == "20 of 192", 1, 0)
  countries_80 <- ifelse(countries == "80 of 192", 1, 0)
  countries_160 <- ifelse(countries == "160 of 192", 1, 0)
  
  sanctions_None <- ifelse(sanctions == "None", 1, 0)
  sanctions_5 <- ifelse(sanctions == "5%", 1, 0)
  sanctions_15 <- ifelse(sanctions == "15%", 1, 0)
  sanctions_20 <- ifelse(sanctions == "20%", 1, 0)
})

#Fitting the model using the glm function.
#Family = binomial(logit) tells us that the dependent variable is binary.
#Any two of the dummy variables can be omitted to act as the reference.
#(As long as one is countries, and one is sanctions).
#This avoids perfect multicollinearity, which would result in NAs.
#model below omits countries_20 and sanctions_None.
#I will use this model, as it makes sense to use the lowest values as the reference.
model <- glm(choice ~ countries_80 + countries_160 +
              sanctions_5 + sanctions_15 + sanctions_20, 
            data = climateSupport, family = binomial(logit))

summary(model)

stargazer(model)

#For testing regression equation.
countries_20 <- 0
countries_80 <- 0
countries_160 <- 0
sanctions_None <- 0
sanctions_5 <- 0
sanctions_15 <- 0
sanctions_20 <- 0

#Regression equation for model.
#Reference, countries_20 and sanctions_None
{-0.27266 + 0.33636*countries_80 + 0.64835*countries_160 +
  0.19186*sanctions_5 - 0.13325*sanctions_15 - 0.30356*sanctions_20 
}

###############################################################################
#Alternative method.

#Uses relevel function to set the reference categories.
#Results in the same coefficients as dummy variable method.

#ord.factor must be converted to unordered factors.
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)

climateSupport$countries <- relevel(climateSupport$countries, ref = "20 of 192")
climateSupport$sanctions <- relevel(climateSupport$sanctions, ref = "None")

model2 <- glm(choice ~ countries + sanctions,
              family = binomial(link = "logit"), data = climateSupport)

summary(model2)

################################################################################
#P-Value and global null hypothesis (model comparison).

#H0 = The reduced model is just as useful as the full model.
#The predictor variabeles do not have a significant effect on the outcome variable.
#All slopes = 0

#HA = The full model is more useful thn the reduced model.
#The predictor variables have a significant effect on the outcome variable
#At least one slope =/= 0

#Create model_null with no X data, for comparison with "model".
model_null <- glm(choice ~ 1, data = climateSupport, family = binomial(logit))
summary(model_null)

#Perform a likeihood ratio test.
anova(model_null, model, test = "LRT")

#P-value = 2.2e-16
#At least one predictor in the model is reliable.

################################################################################

#Q2 (a).
{-0.27266 + 0.33636*0 + 0.64835*1 +
    0.19186*1 - 0.13325*0 - 0.30356*0
}
#countries_160 = 1, sanctions_5 = 1.
#0.56755
#P(hat)=
1/(1+exp(-(0.56755)))
#0.6381977

{-0.27266 + 0.33636*0 + 0.64835*1 +
    0.19186*0 - 0.13325*1 - 0.30356*0 
}
#countries_160 = 1, sanctions_15 = 1.
#0.24244
#P(hat) = 
1/(1+exp(-(0.24244)))
#0.5603149

0.6381977 - 0.5603149
#0.0778828

#Q2 (b).
{-0.27266 + 0.33636*1 + 0.64835*0 +
    0.19186*0 - 0.13325*0 - 0.30356*0 
}
#0.0637
#P(hat)=
1/(1+exp(-(0.0637)))
#0.5159196

#########################################################

model_interaction <- glm(choice ~ countries_80 * countries_160 *
                           sanctions_5 * sanctions_15 * sanctions_20, 
             data = climateSupport, family = binomial(logit))

summary(model_interaction)
anova(model_interaction, model, test = "LRT")


#Testing with model2, the alternative method. (Same results)
model2_interaction <- glm(choice ~ countries * sanctions, data = climateSupport,
                          family = binomial(logit))

summary(model2_interaction)
anova(model2_interaction, model2, test = "LRT")

