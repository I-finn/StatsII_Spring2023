#####################
# Imelda Finn
# 22334657
# app stats II
clean_env <- function() {# clear environment
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

# load general packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "here",
         "broom", "gridExtra", "patchwork", "crosstable"),
       pkgTest)

# multi/poiss
lapply(c("AER", "pscl", "nnet","MASS"), pkgTest)
lapply(c("foreign", "MASS","Hmisc", "reshape2", "nnet"),  pkgTest)
lapply(c("survival", "eha", "tidyverse", "ggfortify"), pkgTest)
}
clean_env()

# setup env/options
setwd(here())
ggetwd()
options(prompt="# >", continue="# >")
theme_set(theme_minimal())

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)}
############################################################################
# We're interested in modeling the historical causes of infant
#mortality. We have data from 5641 first-born in seven Swedish
#parishes 1820-1895. Using the "infants" dataset in the eha library,
#fit a Cox Proportional Hazard model using mother's age and 
#child's gender as covariates. Present and interpret the output.

# a)
child_surv <- with(child, Surv(enter, exit, event))

crosstable(child, c(m.age, sex), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)

cox <- coxph(child_surv ~ m.age + sex, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")
output_stargazer("tables/coxph.tex",cox,  appendVal = FALSE)

# There is a 0.082 decrease in the expected log of the hazard for female babies
#compared to male, holding mothers' age constant. For a unit increase 
# of mother's age, there is a 0.008 increase in the expected log
# of the hazard, holding sex constant.

crosstable(child, c(sex, socBranch), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)

cox <- coxph(child_surv ~ sex + socBranch, data = child)

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.082215)  # exp( )beta_sex)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)
ggsave("graphics/coxph.png")
newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), m.age=mean(32)
               )
)

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * m.age, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")


# child_surv         
# ------------------------------------------------
#   sexfemale                     -0.084***         
#   (0.027)          
# 
# socBranchfarming               -0.007           
# (0.092)          
# 
# socBranchbusiness              0.335**          
#   (0.141)          
# 
# socBranchworker                 0.110           
# (0.094)          
# 
# ------------------------------------------------
#   Observations                   26,574           
# R2                              0.001           
# Max. Possible R2                0.986           
# Log Likelihood               -56,498.610        
# Wald Test                33.180*** (df = 4)     
# LR Test                  32.267*** (df = 4)     
# Score (Logrank) Test     33.279*** (df = 4)     

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

##################################
# Tutorial 7: Survival Analysis #
##################################


km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years" ) #, ylim = c(0.7, 1))
autoplot(km)

km_civst <- survfit(child_surv ~ civst, data = child)
autoplot(km_civst)


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")
cox.int$residuals %>% plot()

ggplot(aes(y=exit, colour = c(sex)), data = child)+ 
  geom_point(aes(x=m.age)) +facet_wrap(vars(sex)) #+

#ggplot(aes(y=age, colour = mother, fill = mother), data = child)+ 
ggplot(aes(y=exit, colour = c(sex)), data = child) + 
  geom_point(aes(x=m.age)) +facet_wrap(vars(sex)) #+
  #geom_boxplot() +facet_wrap(vars(sex)) #+
#  geom_histogram(binwidth = 20) +  facet_wrap(vars(m.age, sex)) +
  ggtitle(label="age at death/exit") + #coord_flip() +
  ylab("age at death/exit") +
  xlab("mothers age")  +
  labs(colour="sex", fill="sex",
       caption = "age at death ~ m.age+sex" ) +
  #  scale_fill_manual(name="group",values=c("blue","green", "red"),labels=sex)
  theme(plot.caption = element_text(size=4),
        legend.text = element_text(size = 8))

