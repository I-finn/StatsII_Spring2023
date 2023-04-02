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

# changed data location
# a) lab experiment
# get data
df_l <- read_csv(here("data/bl_lab.csv")) %>%
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

# Descriptive graphs

## Histograms by treatment
df_l %>% ggplot(aes(x=transfer, y=1* (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
  geom_histogram() + facet_wrap(~treatment) + theme_bw() + ylab("Fraction") + 
  xlab("Transfer from Lucky to Unlucky")
ggsave(here("graphs/histograms_lab.pdf"))

### Counting different aspects
#Counting the proportions that equalize:

dfl_equal <- df_l %>% group_by(treatment) %>% mutate(equal= (transfer==400)) 
dfl_equal %>% summarize(mean_equal=mean(equal))
ce_l <- dfl_equal %>%
  group_by(treatment) %>% summarise( yc= sum(transfer==400), n=n())
prop.test(ce_l$yc[ce_l$treatment %in% c("Base", "Nominal Choice") ], 
          ce_l$n[ce_l$treatment %in% c("Base", "Nominal Choice") ])
prop.test(ce_l$yc[ce_l$treatment %in% c("Base", "Forced Choice") ], 
          ce_l$n[ce_l$treatment %in% c("Base", "Forced Choice") ])
prop.test(ce_l$yc[ce_l$treatment %in% c("Nominal Choice", "Forced Choice") ], 
          ce_l$n[ce_l$treatment %in% c("Nominal Choice", "Forced Choice") ])

#Counting the proportions that give nothing to the unlucky participant
#```{r}
dfl_nothing <- df_l %>% group_by(treatment) %>% mutate(nothing= (transfer==0)) 
dfl_nothing %>% summarise(mean_nothing=mean(nothing))
ce_n <- dfl_nothing %>%
  group_by(treatment) %>% summarise( y0= sum(transfer==0), n=n())
prop.test(ce_n$y0[ce_n$treatment %in% c("Base", "Nominal Choice") ], 
          ce_n$n[ce_n$treatment %in% c("Base", "Nominal Choice") ])
prop.test(ce_n$y0[ce_n$treatment %in% c("Base", "Forced Choice") ], 
          ce_n$n[ce_n$treatment %in% c("Base", "Forced Choice") ])
prop.test(ce_n$y0[ce_n$treatment %in% c("Nominal Choice", "Forced Choice") ], 
          ce_n$n[ce_n$treatment %in% c("Nominal Choice", "Forced Choice") ])

#treatment      mean_nothing
#<fct>                 <dbl>
#  1 Base                  0.103
#2 Forced Choice         0.182
#3 Nominal Choice        0.193

## mean inequality and nothing to worst off by treatment (with SEM). 
#```{r fig.height=5, fig.width=7}
df_mean_ineq_nothing_lab <- df_l %>% dplyr::select(treatment, inequality, zero_to_worst_off) %>%
  gather(inequality, zero_to_worst_off, key="outcome", value="y") %>%
  group_by(treatment, outcome) %>%
  summarise(mean_y = mean(y, na.rm=TRUE), se_y = sd(y, na.rm=TRUE)/sqrt(n())) %>%
  mutate(outcome = fct_recode(outcome, 
                              "Inequality" = "inequality",
                              "Nothing to worse off" = "zero_to_worst_off"))
df_mean_ineq_nothing_lab %>%
  ggplot(aes(x=treatment, y=mean_y)) + geom_bar(stat="identity", width=0.7) +
  geom_errorbar(aes(ymax=mean_y+se_y, ymin=mean_y - se_y), width=0.2) + 
  facet_wrap(~ outcome, scales="free") + ylab("Mean \u00B1 s.e.m.") +
  theme_bw() + xlab("")
ggsave(here("graphs/mean_ineq_nothing_lab.pdf"))


### Counting different aspects
#```{r}
df_mean_ineq_nothing_lab %>% knitr::kable()
#df_mean_ineq_nothing_lab %>% stargazer(type="text", summary = FALSE)

df_l_outcomes <- df_l %>% dplyr::select(treatment, inequality, zero_to_worst_off) %>%
  gather(inequality, zero_to_worst_off, key="outcome", value="y") %>%
  group_by(treatment, outcome)
df_l_outcomes %>% filter(outcome=="inequality") %>% 
  filter(treatment %in% c("Base", "Nominal Choice")) %>% t.test(y~treatment, data=.)
df_l_outcomes %>% filter(outcome=="inequality") %>% 
  filter(treatment %in% c("Base", "Forced Choice")) %>% t.test(y~treatment, data=.)
# ```
# 
# Counting the proportions that give nothing to the worst off participant
# ```{r}
dfl_nwo <- df_l %>% group_by(treatment) %>% mutate(nwo= (transfer %in% c(0,800))) 
dfl_nwo %>% summarise(mean_nwo=mean(nwo))
ce_nwo <- dfl_nwo %>%
  group_by(treatment) %>% summarise( yc= sum(nwo), n=n())
prop.test(ce_nwo$yc[ce_nwo$treatment %in% c("Base", "Nominal Choice") ], 
          ce_nwo$n[ce_nwo$treatment %in% c("Base", "Nominal Choice") ])
prop.test(ce_nwo$yc[ce_nwo$treatment %in% c("Base", "Forced Choice") ], 
          ce_nwo$n[ce_nwo$treatment %in% c("Base", "Forced Choice") ])
prop.test(ce_nwo$yc[ce_nwo$treatment %in% c("Nominal Choice", "Forced Choice") ], 
          ce_nwo$n[ce_nwo$treatment %in% c("Nominal Choice", "Forced Choice") ])
#```

# Regressions for paper
## Main treatment effects
#```{r}
t1ineq1_l <- df_l %>% lm(inequality ~ treatment , data=.)
t1ineq2_l <- df_l %>% lm(inequality ~ treatment + leftp + female + age_h + crt_h, data=.)
t1noth1_l <- df_l %>% lm(zero_to_worst_off ~treatment , data=.)
t1noth2_l <- df_l %>% lm(zero_to_worst_off ~treatment + leftp + female + age_h + crt_h , data=.)
stargazer(t1ineq1_l, t1ineq2_l, t1noth1_l, t1noth2_l,
          se = list(sqrt(diag(cluster.vcov(t1ineq1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1ineq2_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth2_l, cluster=1:nrow(df_l))))),
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"),
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
stargazer(t1ineq1_l, t1ineq2_l, t1noth1_l, t1noth2_l,
          se = list(sqrt(diag(cluster.vcov(t1ineq1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1ineq2_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth2_l, cluster=1:nrow(df_l))))),
#          file = "../tables/lab_diag.tex",
          style="aer", df=FALSE, keep.stat=c("rsq","n"),
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
#multiwayvcov (version 1.2.3)
#Multi-Way Standard Error Clustering
#Description
#Exports two functions implementing multi-way clustering using the method suggested by Cameron, Gelbach, & Miller (2011) and cluster (or block) bootstrapping for estimating variance-covariance matrices. Normal one and two-way clustering matches the results of other common statistical packages. Missing values are handled transparently and rudimentary parallelization support is provided.
#```
stargazer(t1ineq1_l, t1ineq2_l, t1noth1_l, t1noth2_l,
          se = list(sqrt(diag(cluster.vcov(t1ineq1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1ineq2_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth1_l, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t1noth2_l, cluster=1:nrow(df_l))))),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), out=here("tables/main_lab.tex"),
          star.char=c("","",""), notes="", notes.append=FALSE, header=FALSE)

#```
## Interactions with choice, inequality
# Now for the role of interaction. Previously we focused on the political interaction only,
# currently we aim to look broader at the heterogeneity. I make one table for the paper 
# (inequality).
# ```{r}
t2ineq1 <- df_l %>% lm(inequality ~ choice + leftp + female + age_h + crt_h, 
                       data=.)
t2ineq2 <- df_l %>% lm(inequality ~ choice + leftp + female + age_h + crt_h +
                         choice*leftp , data=.)
t2ineq3 <- df_l %>% lm(inequality ~ choice + leftp + + female + age_h + crt_h + 
                         choice*female, data=.)
t2ineq4 <- df_l %>% lm(inequality ~ choice + leftp + female + age_h + crt_h +
                         choice*age_h, data=.)
t2ineq5 <- df_l %>% lm(inequality ~ choice + leftp + female + age_h + crt_h +
                         choice*crt_h, data=.)
t2ineq6 <- df_l %>% lm(inequality ~ choice + leftp + female + age_h + crt_h +
                         choice*leftp + choice*female + choice*age_h + choice*crt_h, data=.)
# ```
# 
# We want linear combinations with standard errors as rows in the table:
#   ```{r}
c2 <- glht(t2ineq2, linfct="choiceTRUE + choiceTRUE:leftpTRUE = 0", 
           vcov = cluster.vcov(t2ineq2, cluster=1:nrow(df_l)))
c3 <- glht(t2ineq3, linfct="choiceTRUE + choiceTRUE:femaleTRUE = 0", 
           vcov = cluster.vcov(t2ineq3, cluster=1:nrow(df_l)))
c4 <- glht(t2ineq4, linfct="choiceTRUE + choiceTRUE:age_hTRUE = 0", 
           vcov = cluster.vcov(t2ineq4, cluster=1:nrow(df_l)))
c5 <- glht(t2ineq5, linfct="choiceTRUE + choiceTRUE:crt_hTRUE = 0", 
           vcov = cluster.vcov(t2ineq5, cluster=1:nrow(df_l)))
r1 <- c("Linear combination"," ", 
        sprintf("%4.3f", summary(c2)$test$coefficients[1]),
        sprintf("%4.3f", summary(c3)$test$coefficients[1]),
        sprintf("%4.3f", summary(c4)$test$coefficients[1]),
        sprintf("%4.3f", summary(c5)$test$coefficients[1]),
        "")
r2 <- c("","",
        sprintf("(%4.3f)", summary(c2)$test$sigma[1]),
        sprintf("(%4.3f)", summary(c3)$test$sigma[1]),
        sprintf("(%4.3f)", summary(c4)$test$sigma[1]),
        sprintf("(%4.3f)", summary(c5)$test$sigma[1]),
        "")
r3 <- c("", "",
        sprintf("p=%4.3f", summary(c2)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(c3)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(c4)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(c5)$test$pvalues[1]),
        "")
# ```
# 
# ```{r}
stargazer(t2ineq1, t2ineq2, t2ineq3, t2ineq4, t2ineq5, t2ineq6,
          se = list(sqrt(diag(cluster.vcov(t2ineq1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq6, cluster=1:nrow(df_l))))),
          order=c("choice","choiceTRUE:leftp","choiceTRUE:female", "choiceTRUE:age_h",
                  "choiceTRUE:crt_h"),
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          add.lines= list(r1,r2,r3),
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
# ```
# 
# (And to disk, no output)
# ```{r include=FALSE}
stargazer(t2ineq1, t2ineq2, t2ineq3, t2ineq4, t2ineq5, t2ineq6,
          se = list(sqrt(diag(cluster.vcov(t2ineq1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2ineq6, cluster=1:nrow(df_l))))),
          order=c("choice","choiceTRUE:leftp","choiceTRUE:female", "choiceTRUE:age_h",
                  "choiceTRUE:crt_h"),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcs",
          add.lines=list(r1,r2),
          out=here("tables/heterogeneity1_lab.tex"),type="latex", header=FALSE)
# ```
# 
# 
# 
# ### Interactions with choice, nothing to the worst off
# We need similar interactions with our indicator for nothing to the worst off (for appendix).
# ```{r}
t2noth1 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + female + age_h + crt_h, 
                       data=.)
t2noth2 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + female + age_h + crt_h +
                         choice*leftp , data=.)
t2noth3 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + + female + age_h + crt_h + 
                         choice*female, data=.)
t2noth4 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + female + age_h + crt_h +
                         choice*age_h, data=.)
t2noth5 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + female + age_h + crt_h +
                         choice*crt_h, data=.)
t2noth6 <- df_l %>% lm(zero_to_worst_off ~ choice + leftp + female + age_h + crt_h +
                         choice*leftp + choice*female + choice*age_h + choice*crt_h, data=.)
# ```
# 
# We want linear combinations with standard errors as rows in the table:
#   ```{r}
d2 <- glht(t2noth2, linfct="choiceTRUE + choiceTRUE:leftpTRUE = 0", 
           vcov = cluster.vcov(t2noth2, cluster=1:nrow(df_l)))
d3 <- glht(t2noth3, linfct="choiceTRUE + choiceTRUE:femaleTRUE = 0", 
           vcov = cluster.vcov(t2noth3, cluster=1:nrow(df_l)))
d4 <- glht(t2noth4, linfct="choiceTRUE + choiceTRUE:age_hTRUE = 0", 
           vcov = cluster.vcov(t2noth4, cluster=1:nrow(df_l)))
d5 <- glht(t2noth5, linfct="choiceTRUE + choiceTRUE:crt_hTRUE = 0", 
           vcov = cluster.vcov(t2noth5, cluster=1:nrow(df_l)))
s1 <- c("Linear combination"," ", 
        sprintf("%4.3f", summary(d2)$test$coefficients[1]),
        sprintf("%4.3f", summary(d3)$test$coefficients[1]),
        sprintf("%4.3f", summary(d4)$test$coefficients[1]),
        sprintf("%4.3f", summary(d5)$test$coefficients[1]),
        "")
s2 <- c("","",
        sprintf("(%4.3f)", summary(d2)$test$sigma[1]),
        sprintf("(%4.3f)", summary(d3)$test$sigma[1]),
        sprintf("(%4.3f)", summary(d4)$test$sigma[1]),
        sprintf("(%4.3f)", summary(d5)$test$sigma[1]),
        "")
s3 <- c("", "",
        sprintf("p=%4.3f", summary(d2)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(d3)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(d4)$test$pvalues[1]),
        sprintf("p=%4.3f", summary(d5)$test$pvalues[1]),
        "")
# ```
# 
# Table with p-values for reference:
#   ```{r}
stargazer(t2noth1, t2noth2, t2noth3, t2noth4, t2noth5, t2noth6,
          se = list(sqrt(diag(cluster.vcov(t2noth1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth6, cluster=1:nrow(df_l))))),
          order=c("choice","choiceTRUE:leftp","choiceTRUE:female", "choiceTRUE:age_h",
                  "choiceTRUE:crt_h"),
          add.lines=list(s1,s2,s3), 
          type="text", style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp")
# ```
# (And to disk, no output)
# ```{r include=FALSE}
stargazer(t2noth1, t2noth2, t2noth3, t2noth4, t2noth5, t2noth6,
          se = list(sqrt(diag(cluster.vcov(t2noth1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(t2noth6, cluster=1:nrow(df_l))))),
          order=c("choice","choiceTRUE:leftp","choiceTRUE:female", "choiceTRUE:age_h",
                  "choiceTRUE:crt_h"),
          add.lines=list(s1,s2), 
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcs",
          out=here("tables/heterogeneity2_lab.tex"),type="latex", header=FALSE)
# ```
# 
# ## Triple interactions 
# The editor is interested in the possible triple interaction between political, left, and cognitive reflection.
# 
# 
# ```{r}
triple1 <- df_l %>% lm(inequality ~ choice + leftp + crt_h + female + age_h  , data=.)
triple2 <- df_l %>% lm(inequality ~ choice + choice*leftp + leftp + crt_h + female + age_h, data=.)
triple3 <- df_l %>% lm(inequality ~ choice + choice*crt_h + leftp + crt_h + female + age_h, data=. )
triple4 <- df_l %>% lm(inequality ~ choice + choice*leftp + choice*crt_h + leftp + crt_h +  
                         female + age_h, data=. )
triple5 <- df_l %>% lm(inequality ~ choice + choice*leftp + choice*crt_h + leftp*crt_h + 
                         leftp + crt_h +  female + age_h, data=. )
triple6 <- df_l %>% lm(inequality ~ choice + choice*leftp + choice*crt_h + leftp*crt_h + 
                         choice*leftp*crt_h + leftp + crt_h +  female + age_h, data=. )
stargazer(triple1, triple2, triple3, triple4, triple5, triple6,
          se = list(sqrt(diag(cluster.vcov(triple1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple6, cluster=1:nrow(df_l))))),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcsp", type="text")
```

(And to disk, no output)
```{r include=FALSE}
stargazer(triple1, triple2, triple3, triple4, triple5, triple6,
          se = list(sqrt(diag(cluster.vcov(triple1, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple2, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple3, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple4, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple5, cluster=1:nrow(df_l)))),
                    sqrt(diag(cluster.vcov(triple6, cluster=1:nrow(df_l))))),
          style="aer", df=FALSE, keep.stat=c("rsq","n"), p.auto=TRUE,
          star.char=c("", "",""), notes="", notes.append=FALSE, report="vcs",
          out=here("tables/triple_lab.tex"),type="latex", header=FALSE)
```

# Balance table (appendix)
```{r}
dfl_summary <- df_l %>% group_by(treatment) %>% summarize(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
                                                          mean_female = mean(female), se_female=sd(female)/sqrt(n()),
                                                          mean_crt = mean(cr), se_crt = sd(cr)/sqrt(n()),
                                                          mean_left = mean(leftp), se_leftp=sd(leftp)/sqrt(n()),
                                                          n= n())
dfl_totals <- df_l %>% summarize(mean_age = mean(age), se_age = sd(age)/sqrt(n()),
                                 mean_female = mean(female), se_female=sd(female)/sqrt(n()),
                                 mean_crt = mean(cr), se_crt = sd(cr)/sqrt(n()),
                                 mean_left = mean(leftp), se_leftp=sd(leftp)/sqrt(n()),
                                 n= n())
```
Output of balance table
```{r}
dfl_summary %>% knitr::kable(digits=c(3,1,2,2,2,2,2,2,2,0))
dfl_totals %>% knitr::kable(digits=c(1,2,2,2,2,2,2,2,0))
```

## Balance tests

```{r}
df_l %>% lm(age ~ treatment, data=.) %>% summary()
df_l %>% lm(female ~ treatment, data=.) %>% summary()
df_l %>% lm(cr  ~ treatment, data=.) %>% summary()
df_l %>% lm(leftp ~ treatment, data=.) %>% summary()
```


# sessionInfo()
```{r}
sessionInfo()

# Fri Mar 31 17:49:58 2023 ------------------------------






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



