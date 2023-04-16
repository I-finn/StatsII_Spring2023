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
lapply(c("survival", "eha", "survminer", "ggfortify"), pkgTest)
}
clean_env()
rm(list=ls())
# setup env/options
setwd(here())
getwd()
options(prompt="#> ", continue="#> ")
theme_set(theme_minimal())


# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)}
############################################################################
# We're interested in modeling the historical causes of infant
#mortality. We have data from 26754 children born in Skellefteå, Sweden, 
# 1850-1884, followed fifteen years or until death or out-migration.
#Using the "child" dataset in the eha library,
#fit a Cox Proportional Hazard model using mother's age and 
#child's gender as covariates. Present and interpret the output.

data(child)
head(child)
dim(child)
summary(child[c("enter", "exit", "event")])

crosstable(child, c(m.age, sex, event), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)

child_surv <- with(child, Surv(enter, exit, event))
child_cph <- coxph(child_surv~ m.age + sex, data = child)
child_drop1 <- drop1(child_cph, test = "Chisq")
child_drop1
summary(child_cph)

fit<- survfit(child_cph)
autoplot(fit, surv.line='dashed', censor=FALSE)

fit$surv %>% plot()
fit$cumhaz %>% plot()
output_stargazer("tables/child_drop1.tex",child_drop1,  appendVal = FALSE,
                 label="tbl:add:drop", summary=FALSE)
child_drop1 %>% stargazer(type="text", summary=FALSE)
child_drop1

# output plot of cumulative hazard
plot_CoxPH <- coxreg(child_cph , data=child)
png("graphics/coxreg.png")
plot(plot_CoxPH, xlab = "Duration (Years)", ylab="Cumulative Hazard",
     conf.int = 0.95, main = "eha::child ~ sex + m.age", col="darkgrey",
     fn="cumhaz")
dev.off()

plot(plot_CoxPH, xlab = "Duration (Years)", ylab="Survival",
     conf.int = 0.95, main = "eha::child ~ sex + m.age", col="green",
     fn="surv" )
plot(plot_CoxPH, xlab = "Duration (Years)", ylab="log",
     conf.int = 0.95, main = "eha::child ~ sex + m.age", col="grey",
     fn="log" )
plot(plot_CoxPH, xlab = "Duration (Years)", ylab="loglog",
     conf.int = 0.95, main = "eha::child ~ sex + m.age", col="purple",
     fn="loglog" )

summary(plot_CoxPH)
stargazer(plot_CoxPH, type = "text")

stargazer(child_cph, type = "text")
output_stargazer("tables/coxph.tex",child_cph,  appendVal = FALSE,
                 label="tbl:add")


# There is a 0.082 decrease in the expected log of the hazard for female babies
#compared to male, holding mothers' age constant. For a unit increase 
# of mother's age, there is a 0.008 increase in the expected log
# of the hazard, holding sex constant.

beta_m.age<-child_cph$coefficients[1]
beta_sex<-child_cph$coefficients[2]

# exponentiate parameter estimates to obtain hazard ratios
exp(beta_m.age)  # exp(beta_mothers_age)
exp(beta_sex)  # exp(beta_sex)

# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

################################################################
# interaction interaction term of 0.001, not significant

# modelling an interaction between gender and mother's age
interact_surv <- coxph(Surv(enter, exit, event) ~ m.age * sex, data = child)
summary(interact_surv)
stargazer(interact_surv, type = "text")
output_stargazer("tables/interact.tex",interact_surv,  appendVal = FALSE,
                 label="tbl:int")

drop1(interact_surv, test = "Chisq") %>% stargazer(type="text")


# Sat Apr 15 23:52:15 2023 ------------------------------

new_df <- with(child,
               data.frame(sex = c("female", "male"), 
                          m.age = rep(mean(m.age, na.rm = TRUE), 2)
               )
)
fit <- survfit(child_surv, newdata = new_df)

ggsurvplot(fit, conf.int = TRUE, palette = "Dark2", 
           censor = FALSE, data = child, ylim=c(0.75, 1))

#censor.shape = '*', censor.size = 5, censor.colour = "red" 
#, facets = TRUE, ncol = 2


# There is a 0.082 decrease in the expected log of the hazard for female babies
#compared to male, holding mothers' age constant. For a unit increase 
# of mother's age, there is a 0.008 increase in the expected log
# of the hazard, holding sex constant.

crosstable(child, c(sex, socBranch), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)

cox.sb <- coxph(child_surv ~ sex + socBranch, data = child)


# Survival curves with new data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%
#library(survminer)

plot(fit)

autoplot(fit)
ggsave("graphics/coxph.png")
newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), m.age=mean(child$m.age)
               )
)

png("graphics/surv_prop.png")
plot(survfit(child_cph, newdata = newdat),# xscale = 12,
     conf.int = T,
     ylim = c(0.7, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))

dev.off()

# Adding an interaction
cox.int <- coxph(child_surv ~ sex * m.age, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")



# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

# plotting cumulative hazard
fit.sex <- coxreg(Surv(enter, exit, event) ~ sex, data = child)
fit.mage <- coxreg(Surv(enter, exit, event) ~ m.age, data = child)
fit.sex.mage <- coxreg(Surv(enter, exit, event) ~ sex+m.age, data = child)

compHaz(fit.sex, fit.mage, printLegend=FALSE) #
legend("topleft", legend=c("sex", "m.age"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



##################################
# Tutorial 7: Survival Analysis #
##################################

km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1) ) #, ylim = c(0.7, 1))
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

#http://www.sthda.com/english/wiki/cox-proportional-hazards-model
#  t represents the survival time
#  h(t)   is the hazard function determined by a set of p covariates (x1,x2,...,xp)
#the coefficients (b1,b2,...,bp) measure the impact (i.e., the effect size) of covariates.
#  the term h0  is called the baseline hazard. It corresponds to the value 
#of the hazard if all the xi are equal to zero (the quantity exp(0) equals 1). 
#The ‘t’ in h(t) reminds us that the hazard may vary over time.

#Put another way, a hazard ratio above 1 indicates a covariate that is 
# positively associated with the event probability, and thus negatively 
# associated with the length of survival.
  
#  In summary,
#  HR = 1: No effect
#  HR < 1: Reduction in the hazard
#  HR > 1: Increase in Hazard
  data(infants)
  head(infants)
  dim(infants)
  summary(infants[c("enter", "exit", "event")])
  
  infants_surv <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
  summary(infants_surv)
  
  int_infants_surv <- coxph(Surv(enter, exit, event) ~ age * sex, data = infants)
  summary(int_infants_surv)
  
  drop1(infants_surv, test = "Chisq")
  
  drop1(int_infants_surv, test = "Chisq")
  
  plot_infants <- coxreg(Surv(enter, exit, event ) ~ age +sex , data=infants)
  plot(plot_infants)
  
  # Sat Apr 15 23:55:58 2023 ------------------------------
  

  # not relevant
  not_used <- function(){
    autoplot(aareg(Surv(exit, event) ~ sex*m.age, data = child), 
             facets = TRUE, ncol = 2)
    ?aareg
    
  }