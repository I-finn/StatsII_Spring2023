# Thu Feb 16 17:17:07 2023 ------------------------------
#Imelda Finn
# POP77004
# Problem Set 2

#####################
# clear global .envir
# set wd
# load libraries
#####################

# remove objects
rm(list=ls())
options(prompt = "# ")

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

#genl
lapply(c("ggplot2", "tidyverse", "stargazer"),  pkgTest)

#####################
# Problem 1
#####################
# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
dataPath <- "./data/ClimateSupport.RDS"
saveRDS(climateSupport, dataPath)

summary(climateSupport)
#factors
climateSupport$choice <- ifelse(climateSupport$choice=="Support", TRUE, FALSE)
climateSupport$countries <- ifelse(climateSupport$countries=="20 of 192", 0, 
                                ifelse(climateSupport$countries=="80 of 192", 1, 2))
climateSupport$sanctions <- ifelse(climateSupport$sanctions=="None", 0, 
                                ifelse(climateSupport$sanctions=="5%", 1, 
                                       ifelse(climateSupport$sanctions=="15%", 2, 3)))


mod <- glm(choice ~., family = binomial(link="logit"), 
           data = climateSupport)

stargazer(mod, type="text")

null_mod <- glm(choice ~1, family = binomial(link="logit"), 
           data = csFacs)
anova(mod, null_mod)
#Analysis of Deviance Table

#Model 1: choice ~ countries + sanctions
#Model 2: choice ~ 1
#Resid. Df Resid. Dev Df Deviance
#1      8497          0            
#2      8499      11783 -2   -11783

