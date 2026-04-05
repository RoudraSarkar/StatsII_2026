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

lapply(c("nnet", "MASS"),  pkgTest)
lapply(c("survival"),  pkgTest)
lapply(c("eha"),  pkgTest)
lapply(c("sampleSelection"),  pkgTest)

library(survival)
library(eha)
library(sampleSelection)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")

# Inspect the data
head(child)
str(child)
summary(child)
 
# Fit the Cox Proportional Hazards model
# The child dataset uses enter/exit format for survival times and an event indicator
# Covariates: mother's age (m.age) and infant's gender (sex)
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
 
# Present the output
summary(cox_model)

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")


# Inspect the data
head(disaster_data)
str(disaster_data)
summary(disaster_data)

# Fit the Heckman Selection Model
# Selection equation:  binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged
# Outcome equation:    originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged
 
heckman_model <- selection(
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome   = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data      = disaster_data,
  method    = "ml"    # Maximum Likelihood estimation
)
 
# Present the output
summary(heckman_model)

