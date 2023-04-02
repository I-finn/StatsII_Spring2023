# Imelda Finn
# 22334657
# App Stats II - 2023

# Replication - additional regression

source("R/clean_env.R")
clean_env()

lapply(c("devtools","multiwayvcov","multcomp"), pkgTest)
#install_github("fair-nhh/mmbruteluck")

getwd()
setwd(here())

# a) lab experiment
# get data
df_l <- read_csv(here("data","bl_lab.csv")) %>%
  mutate(choice = (T %in% c(2,3)),
         inequality = abs(800 - 2*transfer)/800,
         zero_to_worst_off = (transfer %in% c(0,800)),
         female = (sex==2),
         crt_h = (cr %in% c(2,3)),
         age_h = (age >= median(age)),
         treatmentorg = fct_recode(as_factor(T),
                                   "Base" = "1",
                                   "Forced Choice" = "3",
                                   "Nominal Choice" = "2"),
         treatment = fct_relevel(treatmentorg, c("Base", "Forced Choice", "Nominal Choice")),
         leftp = !(polparty %in% c(6,7)))


########################################
# checking inline percentages & pvalues
(0.204+0.120)/0.204
(800-800*(0.204+0.120))/2

(0.204+0.164)/0.204
(800-800*(0.204+0.164))/2
252.8

# online
0.201*8
(0.201+0.166)/0.201
(8-8*(0.201+0.166))/2

(0.201+0.109)/0.201
round((8-8*(0.201+0.109))/2,2)
2.76
######
# run original models
t1ineq1_l <- df_l %>% lm(inequality ~ treatment , data=.)
t1ineq2_l <- df_l %>% lm(inequality ~ treatment + leftp + female + age_h + crt_h, data=.)
t1noth1_l <- df_l %>% lm(zero_to_worst_off ~treatment , data=.)
t1noth2_l <- df_l %>% lm(zero_to_worst_off ~treatment + leftp + female + age_h + crt_h , data=.)

#fivenum(df_l$transfer)
# scale transfer to 0..1
df_l$transfer <- df_l$transfer/800
# linear regression on amount of money transferred
t1trans1_l <- df_l %>% lm(transfer ~ treatment , data=.)
t1trans2_l <- df_l %>% lm(transfer ~ treatment + leftp + female + age_h + crt_h, data=.)

stargazer(t1ineq1_l, t1ineq2_l, t1trans1_l, t1trans2_l,
          se = list(sqrt(diag(cluster.vcov(t1ineq1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1ineq2_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1trans1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1trans2_l, cluster=1:nrow(df_l))))),
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"),
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
output_stargazer(here("tables", "transfer_lab.tex"), t1ineq1_l, t1ineq2_l, t1trans1_l, t1trans2_l,
                 se = list(sqrt(diag(cluster.vcov(t1ineq1_l, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1ineq2_l, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1trans1_l, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1trans2_l, cluster=1:nrow(df_l))))),
                 style="aer", df=FALSE, keep.stat=c("rsq","n"),
                 star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp",
                 appendVal=FALSE, label="tbl:o:trans", type="latex")

##===============================================================================

# online data
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


######################################################
# Sun Apr  2 17:26:28 2023 ------------------------------

# original linear models on inequality measure (0..1)
ot1ineq_1 <-  df_k %>% lm(inequality ~ treatmentgroup + workp , data = . )
ot1ineq_2 <-  df_k %>% lm(inequality ~ treatmentgroup + workp  +
                           leftp + female + age_h + crt_h, data=.)
ot1ineq_3 <-  df_k %>% lm(inequality ~ treatmentgroup  + workp +
                           leftp + female + age_h + crt_h +
                           university + high_income, data=.)


# scale transfer to 0..1
df_k$transfer <- df_k$y/8
# linear regression on amount of money transferred
ot1trans_1 <-  df_k %>% lm(transfer ~ treatmentgroup + workp , data = . )

ot1trans_2 <-  df_k %>% lm(transfer ~ treatmentgroup + workp  +
                            leftp + female + age_h + crt_h, data=.)
ot1trans_3 <-  df_k %>% lm(transfer ~ treatmentgroup  + workp +
                            leftp + female + age_h + crt_h +
                            university + high_income, data=.)

summary(ot1trans_3)
#```

#Now, outputting these regressions to a table.
#```{r}
stargazer(ot1ineq_1, ot1ineq_2, ot1ineq_3, ot1trans_1, ot1trans_2, #ot1trans_3,
          se = list(sqrt(diag(cluster.vcov(ot1ineq_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_3, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_3, cluster=1:nrow(df_k))))
          ),
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"),
          p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp", header=FALSE)
# ```
# 
# And to disk:
#   ```{r include=FALSE}
output_stargazer(here("tables", "transfer_online.tex"), 
                 ot1ineq_1, ot1ineq_2, ot1trans_1, ot1trans_2, ot1trans_3, #ot1trans_3,
          se = list(sqrt(diag(cluster.vcov(ot1ineq_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_3, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_3, cluster=1:nrow(df_k))))
          ),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp",
          header=FALSE, appendVal = FALSE, #title = "", caption="",
          label = "tbl:o:trans")

output_stargazer(here("tables", "transfer_online.tex"), 
                 ot1ineq_3, #
                 se = list(sqrt(diag(cluster.vcov(ot1ineq_1, cluster=1:nrow(df_k)))),
                           sqrt(diag(cluster.vcov(ot1ineq_2, cluster=1:nrow(df_k)))),
                           sqrt(diag(cluster.vcov(ot1ineq_3, cluster=1:nrow(df_k)))),
                           sqrt(diag(cluster.vcov(ot1trans_1, cluster=1:nrow(df_k)))),
                           sqrt(diag(cluster.vcov(ot1trans_2, cluster=1:nrow(df_k)))),
                           sqrt(diag(cluster.vcov(ot1trans_3, cluster=1:nrow(df_k))))),
                 style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
                 star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp",
                 header=FALSE, appendVal = TRUE
                 )

stargazer(ot1ineq_1, ot1ineq_2, ot1ineq_3, ot1trans_1, ot1trans_2, ot1trans_3,
          se = list(sqrt(diag(cluster.vcov(ot1ineq_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1ineq_3, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_1, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_2, cluster=1:nrow(df_k)))),
                    sqrt(diag(cluster.vcov(ot1trans_3, cluster=1:nrow(df_k))))),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcs",
          out = here("tables", "transfer_online.tex"), header=FALSE)


############################################################################
# 
# run binomial logit regression on zero_to_worst_off
t1noth1_lB <- df_l %>% glm(zero_to_worst_off ~treatment , data=., 
                           binomial(link = "logit"))
t1noth2_lB <- df_l %>% glm(zero_to_worst_off ~treatment + leftp + female + age_h + crt_h , data=.,
                           binomial(link = "logit"))
require(multiwayvcov)
stargazer(t1noth1_l, t1noth2_l, t1noth1_lB, t1noth2_lB,
          se = list(sqrt(diag(cluster.vcov(t1noth1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth2_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth1_lB, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth2_lB, cluster=1:nrow(df_l))))),
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"),
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
output_stargazer(here("tables", "lab_tweak.tex"), t1ineq1_l, t1ineq2_l, t1noth1_l, t1noth2_l,
                 se = list(sqrt(diag(cluster.vcov(t1noth1_l, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1noth2_l, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1noth1_lB, cluster=1:nrow(df_l)))),
                           sqrt(diag(cluster.vcov(t1noth2_lB, cluster=1:nrow(df_l))))),
                 style="aer", df=FALSE, keep.stat=c("rsq","n"),
                 star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp",
                 appendVal=FALSE, label="tbl:tweak1", type="latex")


anova(t1noth1_l, t1noth1_lB, test = "Chisq")# 0 df so no chisq
anova(t1noth2_lB, t1noth2_l, test = "Chisq") # interpretation ?

# prediction * confusion matrix/ accuracy



#----------------------------------------------------------------------------
# interaction between left-wing and female and age
tweak1_ineq <- df_l %>% lm(inequality ~ choice + leftp * female * age_h + crt_h +
                             choice*leftp + choice*female + choice*age_h + choice*crt_h, data=.)
tweak1_ineq
summary(tweak1_ineq)
summary(t2ineq6)
print(df_l$age[df_l$age_h])
med_lab_age  <- median(df_l$age)
max_lab_age  <- max(df_l$age)
print(df_l$age[!df_l$age_h])

#######################################

sessionInfo()



