# Thu Feb 16 17:17:07 2023 ------------------------------
#Imelda Finn 22334657
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
#q specific
lapply(c("lattice", "patchwork"),  pkgTest)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

#####################
# Problem 1
#####################

#we asked 8,500 individuals whether they support a given policy, and
#for each participant, we vary the 
#     (1) number of countries that participate in the international agreement
# and (2) sanctions for not following the agreement.

#Response variable:
# * choice: 
#    1 if the individual agreed with the policy; 
#    0 if the individual did not support the policy
#Explanatory variables:
# * countries: Number of participating countries 
#        [20 of 192; 80 of 192; 160 of 192]
# * sanctions: Sanctions for missing emission reduction targets 
#        [None, 5%, 15%, and 20% of the monthly household costs given 2% GDP growth]


# load data
#load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
#saveRDS(climateSupport, dataPath)
dataPath <- "./data/ClimateSupport.RDS"

climateSupport <- readRDS(dataPath)
#reads in with data factored
# choice = 1,2
# countries = 1, 2, 3
# sanctions = 1, 2, 3, 4

head(climateSupport)
summary(climateSupport)

#1. Remember, we are interested in predicting the likelihood of an 
# individual supporting a policy based on the number of countries 
# participating and the possible sanctions for non-compliance.
#  Fit an additive model. 

#
csFacs <- climateSupport
csFacs$choice <- relevel(csFacs$choice, "Supported")

summary(csFacs)

# generate likelihood model, binomial - logit
# both predictors on choice
mod <- glm(choice ~., family = binomial(link="logit"), 
           data = csFacs)
summary(mod)

#p<-plot(mod)
pairs(csFacs)

null_mod <- glm(choice ~1, family = binomial(link="logit"), 
           data = csFacs)
summary(null_mod)
anova_null <- anova(null_mod, mod)
anova_null

#Analysis of Deviance Table

#Model 1: choice ~ 1
#Model 2: choice ~ countries + sanctions
#Resid. Df Resid. Dev Df Deviance
#1      8499      11783            
#2      8494      11568  5   215.15

# Thu Feb 16 19:09:40 2023 ------------------------------
#  Provide  the summary output, 

# inspect model output
stargazer(null_mod, mod, type="text")
stargazer(anova_null, type="text")

# output results tables to latex
output_stargazer("./tables/glm.tex", mod, appendVal = FALSE, label="tab:glm")
output_stargazer("./tables/glm_null.tex", mod, null_mod, appendVal = FALSE, 
                 label="tab:glm:null")
output_stargazer("./tables/glm_null_anova.tex", mod, null_mod, anova_null, appendVal = FALSE, 
                 label="tab:null:anova")
output_stargazer("./tables/anova.tex", anova_null, appendVal = FALSE, 
                 label = "tab:anova")

# Thu Feb 16 19:10:43 2023 ------------------------------
#           the global null hypothesis,

# H0: coefficients are all 0, ie factors have no effect on outcome
Halpha = 0.05


CI_alpha <- confint(mod, level = 1-Halpha)

CI_alpha[3]
#Waiting for profiling to be done...
#2.5 %      97.5 %
#  (Intercept) -0.04872948  0.03739769
#countries.L  0.38387752  0.53323991
#countries.Q -0.08454591  0.06463850
#sanctions.L -0.36248909 -0.19029622
#sanctions.Q -0.26728222 -0.09494376
#sanctions.C  0.06403753  0.23649319

exp_ci <- exp(CI_alpha)
#Waiting for profiling to be done...
#2.5 %    97.5 %
#  (Intercept) 0.9524387 1.0381058
#countries.L 1.4679656 1.7044456
#countries.Q 0.9189295 1.0667733
#sanctions.L 0.6959419 0.8267142
#sanctions.Q 0.7654570 0.9094241
#sanctions.C 1.0661324 1.2667989

odds <- exp(coef(mod))
odds

attributes(coef_intervals)
coef_intervals <- tibble(lower=round(exp_ci[,1],4), estimate = round(odds,4),
                                  upper= round(exp_ci[,2],4))

attributes(coef_intervals)$row.names <- c("intercept", 
                                          "countries 80", "countries 160",
                                          "sanctions 5%", "sanctions 10%","sanctions 15%")
stargazer(coef_intervals, type = "text", summary = FALSE)
output_stargazer("./tables/intervals.tex", coef_intervals, appendVal = FALSE, 
                 summary = FALSE,
                 label="tab:CIs")

stargazer(exp_ci, odds, type = "text", summary = FALSE)

stargazer(odds, type = "text", summary = FALSE)

#output_stargazer("intervals.tex", odds, exp_ci, appendVal = FALSE, summary = FALSE,
#                 label="tab:CIs")

#           and p-value. 


#Please describe the results and provide a conclusion.
summary(mod)


#2. If any of the explanatory variables are significant in this model,
#then:
#  (a) For the policy in which nearly all countries participate 
#[160 of 192], how does increasing sanctions from 5% to 15% change 
#the odds that an individual will support the policy? (Interpretation of a coefficient)
#(b) What is the estimated probability that an individual will support
#a policy if there are 80 of 192 countries participating with no 
#sanctions?






#  (c) Would the answers to 2a and 2b potentially change if we included
#the interaction term in this model? Why?
#  * Perform a test to see if including an interaction is appropriate.

int_mod <- glm(choice ~countries + sanctions + countries * sanctions,
               family = binomial(link="logit"), data = csFacs)

int_odds <- exp(coef(int_mod))

anova_int <- anova(mod, int_mod)
anova_int
#Analysis of Deviance Table

#Model 1: choice ~ countries + sanctions + countries * sanctions
#Model 2: choice ~ countries + sanctions
#Resid. Df Resid. Dev Df Deviance
#1      8488      11562            
#2      8494      11568 -6  -6.2928


# inspect the results
# only the countries.Q:sanctions.L term is significant
# significant at the 10% level
# interaction is countries = 160 of 192 * sanctions 5%

stargazer(mod, null_mod, int_mod, anova_null, anova_int, type="text")
stargazer(mod, null_mod, int_mod, type="text")
stargazer(mod, int_mod, type="text")


#output latex tables
#output_stargazer("./tables/glm_null_int.tex", mod, null_mod, int_mod, appendVal = FALSE, 
#                 label="tab:glm:null:int")
output_stargazer("./tables/glm_int.tex", mod, null_mod, int_mod, appendVal = FALSE, 
                 label="tab:glm:int")
output_stargazer("./tables/anovas.tex", anova_null, anova_int, appendVal = FALSE, 
                 label="tab:anovas", title = "ANOVA", caption = "ANOVA")
output_stargazer("./tables/all.tex", mod, null_mod, int_mod, anova_null, anova_int, 
                 appendVal = FALSE, label="tab:all")



# Thu Feb 16 19:12:21 2023 ------------------------------
#compare to linear model - no coefficients significant
# residuals don't come out
csFacs$choice <- ordered(csFacs$choice)

lm_mod <- lm(choice ~., data = csFacs)
stargazer(lm_mod, type="text")

summary(lm_mod)
rm(lm_mod)


# Thu Feb 16 18:58:18 2023 ------------------------------
# convert choice to true/false
# algorithms don't converge and can't get confidence intervals
cs <- climateSupport
cs$choice <- ifelse(cs$choice=="Support", TRUE, FALSE)

csFacs <- cs

output_stargazer("./tables/logi.tex", mod, null_mod, int_mod, anova_mod, appendVal = FALSE)
