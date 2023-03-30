# Imelda Finn
# 22334657
# App Stats II - 2023

# Replication

source("clean_env.R")

# Source of research:
#title: "Online Experiment"
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/A6KFNO
# author: "Alexander W. Cappelen, Sebastian Fest, Erik Ø. Sørensen, and Bertil Tungodden"
#https://cee.boun.edu.tr/sites/cee.boun.edu.tr/files/documents/CEE2018Conference/cprwmrc.pdf
# output: rmarkdown::html_vignette
# vignette: >
#   %\VignetteIndexEntry{online-experiment}
# %\VignetteEngine{knitr::rmarkdown}
# %\VignetteEncoding{UTF-8}
#paper
#https://wiwi.uni-mainz.de/files/2020/01/mmbruteluck_paper.pdf
#data: 
#library(tidyverse)
#library(stargazer)
#library(multcomp)
#library(multiwayvcov)
#library(here)

lapply(c("devtools","multiwayvcovr","here"), pkgTest)
lapply(c("multcomp","stargazer","tidyverse"), pkgTest)
#install_github("fair-nhh/mmbruteluck")

getwd(here())
setwd(here())

# a) online survey
#data("mtcars")
#head(mtcars,1)

# change summarize to summarise

treatmentsv = c("Base (w)", "Forced Choice (w)", "Nominal Choice (w)", 
                "Base (nw)", "Forced Choice (nw)", "Nominal Choice (nw)",
                "Forced Choice strong", "Forced Choice very strong")
treatmentsv_short = c("Base", "Forced Choice", "Nominal Choice")
df_k <- read_csv("data/bl_online.csv")  %>% 
  mutate(treatment_org = factor(treatment, levels=treatmentsv),
         treatment = fct_recode(treatment_org,
                                "Forced Choice (w)" = "Forced Choice strong",
                                "Forced Choice (w)" = "Forced Choice very strong"),
         treatment_kantar = factor(treatment_kantar),
         treatmentgroup = factor(treatmentgroup, levels=treatmentsv_short),
         treatmentgroup8 = fct_recode(treatment,
                                      "Base" = "Base (w)",
                                      "Base" = "Base (nw)",
                                      "Nominal Choice" = "Nominal Choice (w)",
                                      "Nominal Choice" = "Nominal Choice (nw)",
                                      "Forced Choice" = "Forced Choice (w)",
                                      "Forced Choice" = "Forced Choice (nw)",
                                      "Forced Choice" = "Forced Choice strong",
                                      "Forced Choice" = "Forced Choice very strong"),
         gender = factor(gender),
         education = factor(education),
         indincome = factor(indincome),
         work_temp = fct_relevel(as.factor(workp), c("TRUE", "FALSE")),
         work = fct_recode(work_temp, 
                           "Work" = "TRUE", 
                           "No Work"= "FALSE"),
         inequality = abs(8 - 2*y)/8.0,
         zero_to_worst_off = (y %in% c(0,8)),
         university = (education %in% c("Universitet/hoyskole I", "Universitet/hoyskole II")),
         high_income = (indincome %in% c("1.000.000 kroner eller mer", 
                                         "800.000 - 999.999 kroner",
                                         "700-000 - 799.999 kroner",
                                         "600.000 - 699.999 kroner",
                                         "500.000 - 599.999 kroner")),  # Median is within 400-499 group.
         female = (gender=="Kvinne"),
         choice = (treatmentgroup %in% c("Forced Choice", "Nominal Choice")),
         age_h = (age > median(age)),
         crt_h = (crt %in% c(2,3)),
         understanding2n = as.numeric(gsub("[^0-9]","", understanding2))) %>%
  filter(comp==1)

head(df_k)
# if don't specify bins, gives warning
#df_k %>% ggplot(aes(x=y, y=1* (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
#  geom_histogram() + facet_grid(work~treatmentgroup) + theme_bw() + ylab("Fraction") + 
#  xlab("Transfer from Lucky to Unlucky")
df_k %>% ggplot(aes(x=y, y=1* (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  geom_histogram(bins=30) + facet_grid(work~treatmentgroup) + theme_bw() + ylab("Fraction") + 
  xlab("Transfer from Lucky to Unlucky")
ggsave(here("graphs", "histograms_kantar_wd.pdf"))
df_k %>% ggplot(aes(x=y, y=1* (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  geom_histogram(bins=30) + facet_grid(.~treatmentgroup) + theme_bw() + ylab("Fraction") + 
  xlab("Transfer from Lucky to Unlucky")
ggsave(here("graphs", "histograms_kantar.pdf"))




#


sessionInfo()



