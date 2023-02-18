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
dev.off()

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
output_stargazer <- function(outputFile, appendVal=FALSE, ...) {
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

# Thanks Martyn
# 
# get a version of the dataset with the response variable coded as 
# True = supported
# False = not supported
cs <- climateSupport
cs$choice <- as.logical(as.numeric(cs$choice)-1) 

summary(cs) # check switch done correctly

#png("graphics/pairs.png")
pdf("graphics/pairs.pdf")
pairs(cs, upper.panel = NULL)
# changes from mirrored rectangle, to triangle
dev.off()

# generate likelihood model, binomial - logit
# both predictors on choice
mod <- glm(choice ~., family = binomial(link="logit"), data = cs)
mod
summary(mod)

#TODO plot 4 images in 1
#p<-plot(mod)
#png("graphics/mod.png")
#pdf("graphics/mod.pdf")
#plot(mod)
#dev.off()

png("graphics/wireframe.png")
wireframe(choice~countries+sanctions,data = cs, title = "choice ~ countries+sanctions",
          xlab="country",ylab="sanctions",zlab="supported",
          distance=0.5,zoom=0.5,
          scales=list(arrows=TRUE,
                      distance=c(3,3,3)),
          shade=TRUE)
dev.off()

#-------------------------------------------------------------------------------------
#           the global null hypothesis,
# H0: coefficients are all 0, ie factors have no effect on outcome
# (all the slopes are 0)
Halpha = 0.05
# ANOVA test - chisq test with stat = null deviance - full deviance

null_mod <- glm(choice ~1, family = binomial(link="logit"), data = cs)
null_mod
summary(null_mod)

# Run anova/chisq test to compare deviance
anova_null <- anova(null_mod, mod, test = "LRT")
anova_null

1-pchisq(215.15, df = 5)

#  Provide  the summary output, 

countries_levels <-attributes(cs$countries)$levels
paste("countries", countries_levels )

mod_labels <- c(paste("countries: ", countries_levels[2:3]), 
                "sanctions: 5\\%", "sanctions: 5\\%", "sanctions: 5\\%")

# inspect model output
stargazer(null_mod, mod, type="text", model.names = TRUE,
          covariate.labels = mod_labels)

# output results tables to latex
output_stargazer("./tables/glm.tex", mod, appendVal = FALSE, label="tab:glm",
                 covariate.labels = mod_labels,model.names = TRUE)
output_stargazer("./tables/glm_null.tex", mod, null_mod, appendVal = FALSE, 
                 label="tab:glm:null",model.names = TRUE,
                 covariate.labels = mod_labels)

output_stargazer("./tables/anova.tex", anova_null, appendVal = FALSE, 
                 label = "tab:anova")
#Analysis of Deviance Table

#Model 1: choice ~ 1
#Model 2: choice ~ countries + sanctions
#Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1      8499      11783                          
#2      8494      11568  5   215.15 < 2.2e-16 ***

stargazer(anova_null, type="text")

output_stargazer("./tables/glm_null_anova.tex",
                mod, null_mod, appendVal = FALSE, 
                label="tab:glm:null",model.names = TRUE,
                covariate.labels = mod_labels)
output_stargazer("./tables/glm_null_anova.tex", anova_null, appendVal = TRUE, 
                 label = "tab:anova", title = "ANOVA additive vs Interactive")

# Thu Feb 16 19:09:40 2023 ------------------------------

CI_alpha <- confint(mod, level = 1-Halpha)
# Extract confidence intervals around the estimates
confRaw <- data.frame(cbind(lower = CI_alpha[,1], 
                            coefs = coef(mod), 
                            upper = CI_alpha[,2]))

confMod <- data.frame(cbind(lower = exp(CI_alpha[,1]), 
                             coefs = exp(coef(mod)), 
                             upper = exp(CI_alpha[,2])))
# Plot the estimates and confidence intervals
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,8,1)) +
  labs(x = "Terms", y = "Coefficients")
ggsave("graphics/coefficients.png")

confRaw
#Waiting for profiling to be done...
#2.5 %      97.5 %
#(Intercept) -0.04872948 -0.005665297  0.03739769
#countries.L  0.38387752  0.458452482  0.53323991
#countries.Q -0.08454591 -0.009949894  0.06463850
#sanctions.L -0.36248909 -0.276332165 -0.19029622
#sanctions.Q -0.26728222 -0.181085919 -0.09494376
#sanctions.C  0.06403753  0.150206598  0.23649319

confMod
#Waiting for profiling to be done...
#lower     coefs     upper
#(Intercept) 0.9524387 0.9943507 1.0381058
#countries.L 1.4679656 1.5816245 1.7044456
#countries.Q 0.9189295 0.9900994 1.0667733
#sanctions.L 0.6959419 0.7585609 0.8267142
#sanctions.Q 0.7654570 0.8343637 0.9094241
#sanctions.C 1.0661324 1.1620743 1.2667989

stargazer(confMod, type = "text", summary = FALSE, digits=4)

#output_stargazer("./tables/intervals.tex", confMod, appendVal = FALSE, 
#                 summary = FALSE,
#                 label="tab:CIs")

#output_stargazer("intervals.tex", odds, exp_ci, appendVal = FALSE, summary = FALSE,
#                 label="tab:CIs")

#------------------------------------------------------------------
# Consider for instance if we had a model just consisting of factors:
predicted_data <- with(cs, expand.grid(countries = unique(countries),
                                       sanctions = unique(sanctions)))

predicted_data <- cbind(predicted_data, predict(mod, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

# Now we can use the code in Jeff's lecture to fill out the confidence intervals 
# and predicted probability (see lecture)
predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })

predicted_data

stargazer(predicted_data, type= "text", summary = FALSE)
output_stargazer("./tables/predicted.tex", predicted_data, appendVal = FALSE, summary= FALSE, 
                 label="tab:pred")

#           and p-value. 

#Please describe the results and provide a conclusion.

#2. If any of the explanatory variables are significant in this model,
#then:
#  (a) For the policy in which nearly all countries participate 
#[160 of 192], how does increasing sanctions from 5% to 15% change 
#the odds that an individual will support the policy? (Interpretation of a coefficient)

#160of1925%0.638   0.01210.660   0.6490.654
#160of19215%0.560   0.01310.642   0.6310.637
mod
change = exp( -0.181086)
exp(-0.005665  -0.458453 )
#0.6286894
before <- -0.005665+  1* 0.4584522 +2*(-0.009950) + 1*(-0.276332)  #    -0.181086     0.150207  
after <- -0.005665+  1* 0.4584522+ 2*(-0.009950) + 1*(-0.276332)  +2*(-0.181086 )#    0.150207  
diff <- before - after
before <- exp(before)
after <- exp(after)
exp(-diff)
exp(-0.181086)
after/before

#(b) What is the estimated probability that an individual will support
#a policy if there are 80 of 192 countries participating with no 
#sanctions?
#0.626  - jeff table of predicted values



before <- -0.005665+  1* 0.458452 +0*(-0.009950) + 0   #1*(-0.276332)  #    -0.181086     0.150207  
plogis(before)

#new probability
before*exp(-0.181086)   # 0.6264761

#  (c) Would the answers to 2a and 2b potentially change if we included
#the interaction term in this model? Why?
#  * Perform a test to see if including an interaction is appropriate.

int_mod <- glm(choice ~countries + sanctions + countries * sanctions,
               family = binomial(link="logit"), data = cs)

anova_int <- anova(mod, int_mod, test= "LRT")

int_odds <- exp(coef(int_mod))

anova_int

output_stargazer("./tables/int_model.tex",
                 mod, int_mod, appendVal = FALSE, 
                 label="tab:int",model.names = TRUE,
                 covariate.labels = mod_labels, title = "Additive vs Interactive")
output_stargazer("./tables/int_anova.tex", anova_int, appendVal =FALSE, 
                 label="tab:anova:int", title = "ANOVA additive vs Interactive")

#Analysis of Deviance Table

#Model 1: choice ~ countries + sanctions
#Model 2: choice ~ countries + sanctions + countries * sanctions
#Resid. Df Resid. Dev Df Deviance Pr(>Chi)
#1      8494      11568                     
#2      8488      11562  6   6.2928   0.3912
# inspect the results
# only the countries.Q:sanctions.L term is significant
# significant at the 10% level
# interaction is countries = 160 of 192 * sanctions 5%


cdplot(as.factor(choice) ~ countries , data = cs)
cdplot(as.factor(choice) ~ sanctions , data = cs)


stargazer(null_mod, mod, int_mod, anova_null, anova_int, type="text")
stargazer(mod, null_mod, int_mod, type="text")
stargazer(mod, int_mod, type="text")


#output latex tables
#output_stargazer("./tables/glm_null_int.tex", mod, null_mod, int_mod, appendVal = FALSE, 
#                 label="tab:glm:null:int")
output_stargazer("./tables/glm_int.tex", mod, null_mod, int_mod, appendVal = FALSE, 
                 label="tab:glm:int")
#output_stargazer("./tables/all.tex", mod, null_mod, int_mod, anova_null, anova_int,                  appendVal = FALSE, label="tab:all")


# Thu Feb 16 19:12:21 2023 ------------------------------
#compare to linear model - no coefficients significant
# residuals don't come out
cs$choice <- ordered(cs$choice)

lm_mod <- lm(choice ~., data = cs)
stargazer(lm_mod, type="text")

summary(lm_mod)
rm(lm_mod)


# Thu Feb 16 18:58:18 2023 ------------------------------
# convert choice to true/false
# algorithms don't converge and can't get confidence intervals
cs <- climateSupport
cs$choice <- ifelse(cs$choice=="Support", TRUE, FALSE)

output_stargazer("./tables/logi.tex", mod, null_mod, int_mod, anova_mod, appendVal = FALSE)

#===================================================================
ggplot(cs, aes(sanctions, choice, fill = countries)) +
  geom_tile()
ggsave("graphics/cs_tile.png")

# using lattice
levels_countries <- attributes(cs$countries)$levels
levels_sanctions <- attributes(cs$sanctions)$levels

levelplot(choice ~ countries * sanctions, cs, col.regions = terrain.colors(100))
contourplot(choice ~ countries * sanctions, cs)

library(scatterplot3d)

s3d <- with(cs, scatterplot3d(y=choice, x = countries, z = sanctions, color = as.numeric(choice)+5, pch = 19))
s3d <- with(cs, scatterplot3d(choice~ countries* sanctions, color = as.numeric(choice)+5, pch = 19))

res <- residuals(mod, type="deviance")
pdf("graphics/pred_res.pdf")
plot(log(predict(mod)), res)
abline(h=0, lty=2)
dev.off()

#qqnorm(res)
#qqline(res)

#mod_labels <- ifelse(mod_labels %in% countries_levels ,paste("countries: ", mod_labels),
#                paste("sanctions: ", mod_labels))
