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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

set.seed(123)
data <- (rcauchy(1000, location = 0, scale = 1))

ks_test <- function(x){
  #Use length function to find n, the size of the data vector.
  n <- length(x)
  #Code provided in the hint, used to find D the ks test statistic.
  # create empirical distribution of observed data
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(x)))
  #Function for finding p value
  #transcribed into R code from the Marsaglia formula provided in the question. 
  p_value <- function(x) {
    (sqrt(2*pi)/x)*sum(exp(-(2 * (1:floor(x)) - 1)^2 * pi^2) / (8 * x^2))
  }
  
  p_value_result <- p_value(sqrt(n) * D)
  
  return(list(test_statistic = D, p_value = p_value_result))
}

#Test built function.
ks_test(data)

#Test results against ks test built into R. 
ks.test(data, "pnorm")


#####################
# Problem 2
#####################

?optim

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

#view the data for reference.
View(data)

#Define the objective function for OLS regression.
#beta =  the coefficients of the linear regression model.
#x = independent variable.
#y =  dependent variable.
Objective_ols_function <- function(beta, x, y) {
  #Linear regression equation.
  y_hat <- beta[1] + beta[2] * x
  #Calculate residuals.
  sum((y - y_hat)^2)
}

#Initial values for coefficients, set to zero.
initial_beta <- c(0, 0)

#Use BFGS method, in the optim function, to minimize the objective function.
ols_result <- optim(par = initial_beta, fn = Objective_ols_function, x = data$x, y = data$y, method = "BFGS")

#Estimated coefficients, subset from ols_results.
ols_coefficients <- ols_result$par

#Print estimated coefficients.
print(ols_coefficients)

#Compare with lm function.
lm_result <- lm(y ~ x, data = data)
print(summary(lm_result))












