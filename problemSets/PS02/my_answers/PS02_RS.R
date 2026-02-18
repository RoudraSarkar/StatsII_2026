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

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# check object name
ls()
str(climateSupport)


# Prepare variables
climateSupport$countries <- factor(climateSupport$countries)
climateSupport$sanctions <- factor(climateSupport$sanctions)


# QUESTION 1 Additive Model
# Fit additive logistic regression
add_model <- glm(choice ~ countries + sanctions,
                 data = climateSupport,
                 family = binomial)

# Summary output (REPORT THIS)
summary(add_model)

# Global null hypothesis test
# H0: all slope coefficients = 0
null_model <- glm(choice ~ 1,
                  data = climateSupport,
                  family = binomial)

anova(null_model, add_model, test = "Chisq")


# QUESTION 2
# Interpretation from additive logistic regression
options(contrasts = c("contr.treatment", "contr.poly"))
# Refit additive model
add_model <- glm(choice ~ countries + sanctions,
                 data = climateSupport,
                 family = binomial)

summary(add_model)
# 2(a) Odds change: sanctions 5% -> 15%
# when 160 of 192 countries participate


new_160 <- data.frame(
  countries = "160 of 192",
  sanctions = c("5%", "15%")
)
# predicted log-odds
log_odds_160 <- predict(add_model,
                        newdata = new_160,
                        type = "link")
# odds ratio
OR_160 <- exp(log_odds_160[2] - log_odds_160[1])

OR_160



# 2(b) Odds change: sanctions 5% -> 15%
# when 20 of 192 countries participate
new_20 <- data.frame(
  countries = "20 of 192",
  sanctions = c("5%", "15%")
)

log_odds_20 <- predict(add_model,
                       newdata = new_20,
                       type = "link")

OR_20 <- exp(log_odds_20[2] - log_odds_20[1])

OR_20
# 2(c) Estimated probability
# countries = 80 of 192, sanctions = None
new_case <- data.frame(
  countries = "80 of 192",
  sanctions = "None"
)

prob_support <- predict(add_model,
                        newdata = new_case,
                        type = "response")

prob_support


# QUESTION 3 Interaction Model
# Fit interaction model
interaction_model <- glm(choice ~ countries * sanctions,
                         data = climateSupport,
                         family = binomial)

summary(interaction_model)

# Test whether interaction improves model
anova(add_model, interaction_model, test = "Chisq")


