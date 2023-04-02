# Imelda Finn
# 22334657
# App Stats II - 2023

# Replication - additional regression
rm(list=ls())
source("R/clean_env.R")
clean_env()

lapply(c("devtools","multiwayvcov","multcomp"), pkgTest)
#install_github("fair-nhh/mmbruteluck")

getwd()
setwd(here())

# c) mechanical turk - participants
# change summarize to summarise

mturk_workers <- read_csv(here("data","bl_mturkworkers.csv"))
mturk_workers <- mturk_workers %>% mutate(treatment=factor(treatment_str))
rdf_summary <- mturk_workers %>% 
  group_by(treatment) %>%
  summarise(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
            mean_female = mean(female), se_female= sd(female)/sqrt(n()),
            mean_education = mean(education), se_education = sd(education)/sqrt(n()),
            mean_political = mean(political), se_political = sd(political)/sqrt(n()),
            mean_risky = mean(risky), se_risky = sd(risky)/sqrt(n()),
            mean_green = mean(green), se_green = sd(green)/sqrt(n()),
            n = n())

rdf_totals <- mturk_workers %>% 
#  summarize(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
  summarise(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
            mean_female = mean(female), se_female= sd(female)/sqrt(n()),
            mean_education = mean(education), se_education = sd(education)/sqrt(n()),
            mean_political = mean(political), se_political = sd(political)/sqrt(n()),
            mean_risky = mean(risky, na.rm=TRUE), se_risky = sd(risky, na.rm=TRUE)/sqrt(n()),
            mean_green = mean(green, na.rm=TRUE), se_green = sd(green, na.rm=TRUE)/sqrt(n()),
            n = n())

rdf_totals <- mturk_workers %>% 
#  summarize(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
  summarise(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
            mean_female = mean(female), se_female= sd(female)/sqrt(n()),
            mean_education = mean(education), se_education = sd(education)/sqrt(n()),
            mean_political = mean(political), se_political = sd(political)/sqrt(n()),
            mean_risky = mean(risky, na.rm=TRUE), se_risky = sd(risky, na.rm=TRUE)/sqrt(n()),
            mean_green = mean(green, na.rm=TRUE), se_green = sd(green, na.rm=TRUE)/sqrt(n()),
            n = n())

rdf_summary %>% knitr::kable(digits=c(3,1,2,2,2,2,2,2,2,2,2,2,2,0))
rdf_totals %>% knitr::kable(digits=c(1,2,2,2,2,2,2,2,2,2,2,2,2,0))

mturk_workers %>% lm(age ~ treatment, data=.) %>% summary()
mturk_workers %>% lm(female ~ treatment, data=.) %>% summary() # interaction .1 base w
mturk_workers %>% lm(education ~ treatment, data=.) %>% summary()
mturk_workers %>% lm(political ~ treatment, data=.) %>% summary()

sessionInfo()



