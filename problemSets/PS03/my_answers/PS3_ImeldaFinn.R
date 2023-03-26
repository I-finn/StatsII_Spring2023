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
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", #"car",
         "broom", "gridExtra", "patchwork", "crosstable"),
       pkgTest)

# multi/poiss
lapply(c("AER", "pscl", "nnet","MASS"), pkgTest)
lapply(c("foreign", "MASS","Hmisc", "reshape2", "nnet"),  pkgTest)

# setup env/options
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#options(prompt="#", continue=" ")
theme_set(theme_minimal())

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)}
}
clean_env()
############################################################################
#Question 1
#We are interested in how governments' management of public resources impacts economic
#prosperity. Our data come from Alvarez, Cheibub, Limongi, and Przeworski (1996) and
#is labelled gdpChange.csv on GitHub. The dataset covers 135 countries observed between
#1950 or the year of independence or the first year for which data on economic growth are
#available ("entry year"), and 1990 or the last year for which data on economic growth are
#available ("exit year"). The unit of analysis is a particular country during a particular year,
#for a total > 3,500 observations.
#• Response variable:
#{ GDPWdiff: Difference in GDP between year t and t-1. Possible categories include:
#"positive", "negative", or "no change"
#• Explanatory variables:
#{ REG: 1=Democracy; 0=Non-Democracy
#{ OIL: 1=if the average ratio of fuel exports to total exports in 1984-86 exceeded
#50%; 0= otherwise}

#Please answer the following questions:

gdp <- read_csv("./data/gdpChange.csv")
gdp<- rename(gdp, indx=`...1`)
gdp$diff <- ifelse(gdp$GDPWdiff==0, "no change", 
                   ifelse(gdp$GDPWdiff>0, "positive", "negative")) 
gdp$diff2 <- relevel(factor(gdp$diff, ordered = FALSE), ref="no change")
head(gdp)
summary(gdp[5:12])
png("graphics/gdp_pairs.png")
pairs(gdp[c(6, 7,10)])
dev.off()

with(gdp, table(REG, OIL, diff))
with(gdp, do.call(rbind, tapply(GDPWdiff, OIL, 
                        function(x) c(M = mean(x), SD = sd(x)))))
with(gdp, do.call(rbind, tapply(GDPWdiff, REG, 
                                function(x) c(M = mean(x), SD = sd(x)))))

#1. Construct and interpret an unordered multinomial logit with 
#GDPWdiff as the output and "no change" as the reference category, 
#including the estimated cutoff points and coefficients.
head(gdp,1)

table(c(gdp$diff2,gdp$diff))

summary(gdp$diff2)
#require("nnet")

# output plot of GDPWdiff by REG and OIL
ggplot(aes(y=GDPWdiff, fill=as.factor(REG)), data = gdp)+ 
  geom_histogram(bins=20, position="dodge") +  coord_flip() +
  facet_grid(cols = vars(OIL), labeller=label_both) +  #OIL ~ REG, 
  ggtitle(label="Change in GDP year-on-year") +
  xlab("frequency") +
  ylab("GDP(t) - GDP(t-1)")  +
  labs(fill="REG:1=democracy", #colour="GDP diff", 
       caption = "OIL=1 : average ratio of fuel exports to total exports in 1984-86 exceeded
50%; REG=1 : democracy", tag="A") +
  theme(plot.title = element_text(size=11),
        plot.caption = element_text(size=4), 
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("graphics/gdp_hist.png")

#library(crosstable)
gdp_tab <- crosstable(gdp, GDPWdiff, by=c(REG, OIL), #, funs=c(mean, min, max)
           label=F) %>% as_flextable(compact=TRUE)
gdp_tab  # graphics/gdp_flextab.png
rm(gdp_tab)
# Tue Mar 21 15:04:02 2023 ------------------------------

multinom_model <- multinom(diff2 ~ REG + OIL, data = gdp )

## weights:  12 (6 variable)
#initial  value 4087.936326 
#iter  10 value 2340.076844
#final  value 2339.385155 
#converged

stargazer(multinom_model, type="text",report=("vcstp*"))
output_stargazer(outputFile = "tables/multinom.tex", multinom_model, 
                 title="Multinomial, unordered", 
                 label="tbl:multinom", report=("vcstp*"),
                 appendVal = FALSE)

summary(multinom_model)

#Call:
#multinom(formula = diff2 ~ REG + OIL, data = gdp)

#Coefficients:
#  (Intercept)      REG      OIL
#positive    4.533759 1.769007 4.576321
#negative    3.805370 1.379282 4.783968

#Std. Errors:
#  (Intercept)       REG      OIL
#positive   0.2692006 0.7670366 6.885097
#negative   0.2706832 0.7686958 6.885366

#Residual Deviance: 4678.77 
#AIC: 4690.77

#a change in REG from 0 to 1 increases the log-odds of Y = positive vs.
#Y = no change increase by 1.769007

#a change in REG from 0 to 1 multiplies the odds of Y = positive vs.
#Y = no change by a factor of exp(1.769007), holding OIL constant
exp(1.769007)   #5.865026

cfs <- coef(multinom_model)
cfs

exp(cfs)  #[,c(1:3)]
#(Intercept)      REG      OIL
#negative    3.805370 1.379282 4.783968
#positive    4.533759 1.769007 4.576321

#  an oil exporting country has odds of 97.15632 times the baseline, 
#  of achieving a positive change in GDP, compared to a country which 
# has OIL=0 (holding regime constant)

# for REG=1, OIL=1
#ln(P(positive)/P(no change)) = 4.533759 + 1.769007*1 + 4.576321*1

round(predict(multinom_model, newdata = data.frame(REG=0, OIL=0),
        type = "probs"),2)
predict(multinom_model, newdata = data.frame(REG=0, OIL=0),
              type = "class")

predict_data <- data.frame(REG = rep(c(0,1), each = 2), 
                           OIL= rep(c(0,1), 2))
cbind(predict_data, predict(multinom_model,
            newdata = predict_data, type = "class"))

predict_data
# store the predicted probabilities for each covariate combo
predicted_GDPdiff <- cbind(predict_data, predict(multinom_model,
                        newdata = predict_data,
                        type = "probs" , se = TRUE))

by(predicted_GDPdiff[,3:5], predicted_GDPdiff$REG, colMeans)
by(predicted_GDPdiff[,3:5], predicted_GDPdiff$OIL, colMeans)

pred_multi_tbl <- melt(cbind(predict_data, 
                       predict(multinom_model, predict_data, type="probs")),
                 id.vars=c("REG", "OIL"), 
                 variable.name="level", value.name="probability")
png("graphics/pairs_multi.png")
plot(pred_multi_tbl)
dev.off()

pred_multi_tbl
pred_multi_tbl %>% ggplot(aes(y=probability, x=1:length(probability), 
                          colour=level)) +
  geom_point() + 
  facet_grid(OIL ~ REG, labeller=label_both) +
  ggtitle("Unordered Multinomial Predictions") + 
  xlab("") + ylab("Probability") +
  labs(colour="GDP Diff") +
  theme(plot.title = element_text(size=11),
        axis.text.x = element_blank(),
        legend.position = "bottom"
  )
ggsave("graphics/pred_multi.png")               

##element_text(angle = 45, hjust = 1, vjust = 1)

sum(pred_multi_tbl$probability)

stargazer(pred_multi_tbl, type="text", summary=F, digits = 3)
output_stargazer("tables/pred_multi.tex", pred_multi_tbl, 
                 type = "latex", appendVal = FALSE,
                 label = "tbl:pred:multi",
                 table.placement = "!htbp",
                 summary = FALSE, flip = FALSE, digits = 3,
                 object.names = TRUE,
                 title = "Predicted results from unordered Multinomial"#
)

# calculate the mean probabilities within each level category

#predicted_GDPdiff$REG: 0
#no change    positive    negative 
#0.003630507 0.648439546 0.347929947 
#------------------------------------------------------ 
#  predicted_GDPdiff$REG: 1
#no change     positive     negative 
#0.0006958135 0.7328431503 0.2664610363 
#by(predicted_GDPdiff[,3:5], predicted_GDPdiff$OIL, colMeans)
#predicted_GDPdiff$OIL: 0
#no change    positive    negative 
#0.004284929 0.711100709 0.284614363 
#------------------------------------------------------ 
#  predicted_GDPdiff$OIL: 1
#no change     positive     negative 
#4.139172e-05 6.701820e-01 3.297766e-01 

# see how well our predictions were :
addmargins(table(gdp$diff2, predict(multinom_model,
                            type= "class")))
#with(gdp, table(REG, OIL))

dat <- gdp[c("indx","GDPWdiff", "diff2", "REG", "OIL")]
dat$predmulti <- predict(multinom_model)
dat %>% ggplot(aes(x=`indx`, y = diff2)) +
#  geom_point() +
  geom_jitter() +
  geom_line(aes(y=predmulti), linewidth=1, colour="red")

#=================================================================
#2. Construct and interpret an ordered multinomial logit with GDPWdiff 
#as the outcome variable, including the estimated cutoff points and
#coefficients.
#Ordinal logistic regression: If the outcome variable is truly 
#ordered and if it also satisfies the assumption of proportional
#odds, then switching to ordinal logistic regression will make the
#model more parsimonious.
#https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/

# get an ordered factor for GDP difference
gdp$ordered_diff <- ordered(gdp$diff,
                           labels=c("negative", "no change", "positive"))
ord_model <- polr(ordered_diff ~ REG +OIL, data = gdp, Hess = TRUE)

levels(gdp$ordered_diff)

summary(gdp$ordered_diff)
summary(ord_model)
stargazer(ord_model, type="text", ord.intercepts = TRUE, report=("vctp*"))
output_stargazer(outputFile = "tables/ordered.tex", ord_model, 
                 title="Multinomial Logit, ordered", 
                 label="tbl:ordered",
                 report=("vcstp*"),
                 ord.intercepts = TRUE,
                 appendVal = FALSE)

summary_table <- coef(summary(ord_model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

lo_cis<-cbind(logOdds = coef(ord_model), confint(ord_model))
#       logOdds      2.5 %     97.5 %
#REG  0.3984834  0.2516548 0.54643410
#OIL -0.1987177 -0.4237548 0.03019571
#oddsR_CIs <- exp(cbind(OR = coef(ord_model), confint(ord_model)))
oddsR_CIs <- exp(lo_cis)
#OR     2.5 %   97.5 %
#REG 1.4895639 1.2861520 1.727083
#OIL 0.8197813 0.6545844 1.030656
oddsR_CIs
#a change in REG from 0 to 1 increases the log-odds of Y = positive vs.
#Y = no change increase by 1.4895639, holding OIL constant

#a change in REG from 0 to 1 multiplies the odds of (Y = positive vs.
#Y = no change) by a factor of exp(0.3984834), holding OIL constant
exp(0.3984834)   #1.489564

coefficients(ord_model)
#ln(P(positive)/P(no change)) = 0.3984834*REG + -0.1987177*OIL

#So, for a Democratic country, the odds of having positive GDP 
# growth are 1.49 times higher compared to non-democratic country
# holding constant oil revenue status

coef(ord_model)
exp(coef(ord_model)[c(1:2)])
#REG       OIL 
#1.4895639 0.8197813 

#  an oil exporting country has odds of 0.8197813 times the baseline, 
# compared to a country which has OIL=0 (holding REG constant)

round(predict(ord_model, newdata = data.frame(REG=0, OIL=0), type = "probs"),2)
pred_ord <- predict(ord_model, newdata = data.frame(REG=0, OIL=0), type = "class")
cbind(predict_data, pred_ord)

id.vars=c("REG", "OIL")
for ( i in 1:length(unique(gdp$ordered_diff))){
  assign(paste("logit_model", i, sep=""),
         glm(ifelse(ordered_diff==unique(gdp$ordered_diff)[i],
                    1 , 0) ~ REG + OIL, data = gdp),
         envir = globalenv())
}
#output 3 binary logit models
stargazer(logit_model1, logit_model2, logit_model3,
          type="text", 
           model.names = F, ord.intercepts = T, model.numbers = F,
          column.labels = c("negative", "no change", "positive"),
          column.separate = c(1,1,1))

output_stargazer("tables/q1_parallel_lines.tex",
                 logit_model1, logit_model2, logit_model3,
                 model.names = F, ord.intercepts = T, model.numbers = F,
                 column.labels = c("negative", "no change", "positive"),
                 column.separate = c(1,1,1),
                 title="Comparison of models for GDP Diff categories",
                 label="tbl:parallel", digits = 3, appendVal = FALSE
)

# rows not consistent - +--; -+-
# suggests parallel odds assump invalid 
#- poss not ordered or wrong categories
# "reshape", "rms"

# predictions
predict_data
pred_ord <- melt(cbind(predict_data, 
                        predict(ord_model, predict_data, type="probs")),
                  id.vars=c("REG", "OIL"), 
                  variable.name="level", value.name="probability")
png("graphics/pairs_ord.png")
plot(pred_ord)
dev.off()
pred_ord

output_stargazer("tables/pred_ord.tex", pred_ord, type = "latex", 
                 appendVal = FALSE,
                 label = "tbl:pred:ord",
                 table.placement = "!htbp",
                 summary = FALSE, flip = FALSE, digits = 3,
                 object.names = TRUE,
                 title = "Predicted results from Ordered Model"#,                 covariate.labels = c(" ", "value" )
)

pred_ord %>% ggplot(aes(y=probability, x=1:length(probability), 
                          colour=level)) +
  geom_point() + 
  facet_grid(OIL ~ REG, labeller=label_both) +
  ggtitle("Ordered Multinomial Predictions") + 
  xlab("") + ylab("Probability") +
  labs(colour="GDP Diff") +
  theme(plot.title = element_text(size=11),
        axis.text.x = element_blank(),
        legend.position = "bottom"
  )
ggsave("graphics/pred_ord.png")          

# predicting all negative
addmargins(table(gdp$ordered_diff, predict(ord_model,
                                   type= "class")))

dato <- gdp[c("indx","GDPWdiff", "diff2", "REG", "OIL")]
names(dato$...1) <- "index"
dato$predord <- predict(ord_model)
dat %>% ggplot(aes(x=`...1`, y = diff2)) +
#  geom_point() +
  geom_jitter() +
  geom_line(aes(y=predmulti), size=1, colour="red")
ggsave("graphics/ord_pred.png")

# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.

#-----------------------------------------------------------------------------
# check to see if changing cutoff point gives better results
# 
fivenum(gdp_cutoff$GDPWdiff)
mean(gdp_cutoff$GDPWdiff)
#first quartile = -24, median = 111
# no change = -24, 24

sdiff <- sort(gdp_cutoff$GDPWdiff)
plot(sdiff)
length(sdiff)/3
brks <- sdiff[c(1,1240, 1240*2, length(sdiff))]
brks
hist(gdp_cutoff$GDPWdiff, brks)

gdp_cutoff <- gdp[c("OIL", "REG", "GDPWdiff")]
gdp_cutoff$diff <- ifelse(gdp_cutoff$GDPWdiff<=brks[2], "negative", 
                   ifelse(gdp_cutoff$GDPWdiff>=brks[3], "positive", "no change")) %>%
  factor(ordered = FALSE, levels = c("no change", "negative", "positive"))
levels(gdp_cutoff$diff)

gdp_cutoff$odiff <- ordered(gdp_cutoff$diff, labels=c("negative", "no change", "positive"))
levels(gdp_cutoff$odiff)

mmodel <- multinom(diff ~ REG + OIL, data = gdp_cutoff )
omodel <- polr(odiff ~ REG +OIL, data = gdp_cutoff, Hess = TRUE)

pred_m_all <- predict(mmodel, type="class")
pred_o_all <- predict(omodel, type="class")

table(predict(multinom_model, type="class"), gdp$diff2)
table(pred_m_all, gdp_cutoff$diff)

table(predict(ord_model, type="class"), gdp$ordered_diff)
table(pred_o_all, gdp_cutoff$odiff)

stargazer(mmodel, type="text")
stargazer(omodel, type="text")

omodel$deviance
anova(omodel, ord_model, test="Chisq")
anova(mmodel, multinom_model, test="Chisq")

multinom_model$deviance
##################################################################
#Week 9: Count Data - Poisson Regression

#Question 2
#Consider the data set MexicoMuniData.csv, which includes municipal-level information from
#Mexico. The outcome of interest is the number of times the winning PAN presidential can-
#didate in 2006 (PAN.visits.06) visited a district leading up to the 2009 federal elections,
#which is a count. Our main predictor of interest is whether the district was highly contested,
#or whether it was not (the PAN or their opponents have electoral security) in the previ-
#ous federal elections during 2000 (competitive.district), which is binary (1=close/swing
#district, 0="safe seat"). We also include marginality.06 (a measure of poverty) and
#PAN.governor.06 (a dummy for whether the state has a PAN-affiliated governor) as ad-
#ditional control variables.
#(a) Run a Poisson regression because the outcome is a count variable. Is there evidence
#that PAN presidential candidates visit swing districts more? Provide a test statistic
#and p-value.
#(b) Interpret the marginality.06 and PAN.governor.06 coefficients.
clean_env()
mexico <- read_csv("./data/MexicoMuniData.csv")

# facet by competitive.district, PAN.governor.06
# output plot of GDPWdiff by REG and OIL
ggplot(aes(y=PAN.visits.06, fill=as.factor(PAN.governor.06)), data = mexico)+ 
  geom_histogram(bins=10) +  coord_flip() +
  facet_grid(PAN.governor.06 ~ competitive.district, labeller=label_both) +
  ggtitle(label="PAN Candidate visits") +
  xlab("frequency") +
  ylab("count of candidate visits")  +
  labs(fill="governor", #colour="GDP diff", 
       caption = "visits; governor; competitiive", tag="A") +
  theme(plot.title = element_text(size=11),
        plot.caption = element_text(size=4), 
        legend.text = element_text(size = 8),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
#ggsave("graphics/mex_hist.png")

ggplot(aes(y=PAN.visits.06, fill=as.factor(PAN.governor.06)), data = mexico)+ 
  geom_histogram(bins=8, position="dodge") +  coord_flip() +
  facet_grid(labeller=label_both,cols = vars(competitive.district )) +
  ggtitle(label="PAN Candidate visits") +
  xlab("frequency") +
  ylab("count of candidate visits")  +
  labs(fill="governor", #colour="GDP diff", 
       caption = "visits; governor; competitiive", tag="A") +
  theme(plot.title = element_text(size=11),
        plot.caption = element_text(size=4), 
        legend.text = element_text(size = 8),
        legend.position = "bottom", strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("graphics/mex_hist.png")

mex_tab <- crosstable(mexico, PAN.visits.06,
                  by=c(competitive.district, PAN.governor.06), #, funs=c(mean, min, max)
                  label=F) %>% as_flextable(compact=TRUE)
mex_tab  # graphics/mex_flextab.png

View(crosstable(mexico, PAN.visits.06,
           by=c(competitive.district, PAN.governor.06), #, funs=c(mean, min, max)
           label=F))

length(mexico$PAN.visits.06)
summary(mexico)#"pan.vote.09", 
table(mexico[c("PAN.governor.06", "competitive.district")])
table(mexico[c("PAN.visits.06")])

mexico %>% ggplot(aes(x=marginality.06, y = PAN.visits.06,
                      colour=as.factor(competitive.district))) +
  geom_jitter() +
  facet_grid(PAN.governor.06 ~ competitive.district, labeller=label_both) +
  ggtitle(label="PAN Candidate visits") +
  #xlab("Marginality") +
  ylab("count of candidate visits")  +
  labs(colour="competitive: 1:TRUE", #fill="GDP diff", 
     caption = "visits; governor; competitiive", tag="B") +
  theme(plot.title = element_text(size=11),
        plot.caption = element_text(size=4), 
        legend.text = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("graphics/mex_jitter.png")

mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + 
            marginality.06 + PAN.governor.06, data= mexico, family =poisson)

summary(mexico_poisson)
stargazer(mexico_poisson, type="text", report=("vcstp*"))
output_stargazer("tables/poisson.tex",
                 mexico_poisson, report=("vcstp*"),
                 model.names = T, ord.intercepts = T,
                 model.numbers = F,
                 #column.labels = c("negative", "no change", "positive"),
                 #column.separate = c(1,1,1),
                 title="Poisson Model of candidate visit counts",
                 label="tbl:poisson", digits = 3,
                 appendVal = FALSE
)

cd1 <-mexico$PAN.visits.06[mexico$competitive.district==1]
cd0 <-mexico$PAN.visits.06[mexico$competitive.district==0]

poisson.test(x=c(sum(cd1), sum(cd0)), T=c(length(cd1), length(cd0)),
             alternative="greater", conf.level=0.95)

#poisson.test(x=c(sum(cd0), sum(cd1)), T=c(length(cd0), length(cd1)),alternative="greater", conf.level=0.95)

sum(cd1)/length(cd1)
sum(cd0)/length(cd0)


summary(mexico)
coeffs
coeffs <- coefficients(mexico_poisson)
xvalues <- sort(mexico$marginality.06)
means <- exp(coeffs[1] + coeffs[3] * xvalues +coeffs[2] + coeffs[4])
plot(xvalues, mexico$PAN.visits.06)
#plot(xvalues, means, lty=2, col="red", type = "l")
lines(xvalues, means, lty=2, col="red")

lo_cis<-cbind(logOdds = coef(mexico_poisson), confint(mexico_poisson))
lo_cis
e_count_cis <- exp(lo_cis)
e_count_cis
dimnames(e_count_cis)[[2]][1] <- "lambda_hat"
#(b) Interpret the marginality.06 and PAN.governor.06 coefficients.



#(c) Provide the estimated mean number of visits from the winning PAN presidential candi-
#date for a hypothetical district that was competitive (competitive.district=1), had
#an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).
coeffs <- coef(mexico_poisson)
mex_pred_data <- data.frame(competitive.district = 1, 
                            marginality.06=0,
                            PAN.governor.06=1)
pred_mex <- cbind(predict(mexico_poisson,
                          mex_pred_data,
                          type= "response", se.fit =TRUE),
                  mex_pred_data)
# create lower and upper bounds for CIs
pred_mex$lowerBound <- pred_mex$fit - 1.96 * pred_mex$se.fit
pred_mex$upperBound <- pred_mex$fit + 1.96 * pred_mex$se.fit

round(pred_mex,3)

lambdaC <- exp(coeffs[1]+coeffs[2]*1 + coeffs[3]*0 + coeffs[4]*1)
lambdaC

pred_mex
#pred_mex %>% ggplot(aes(x=competitive.district)) +
#  geom_pointrange(aes(y=pred_mex$fit,
#                      ymin=pred_mex$lowerBound,
#                      ymax=pred_mex$upperBound)) +
#  theme_minimal()+
#  ggtitle(label = "visits") +
#  ylab("visits") + xlab("contested")
#labs()

dispersiontest(mexico_poisson)
#Overdispersion test

#data:  mexico_poisson
#z = 1.1176, p-value = 0.1319
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion
#7.176712

#https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf

zeroinfl_poisson <- zeroinfl(PAN.visits.06 ~ competitive.district + 
                               marginality.06 + PAN.governor.06,
                             data=mexico,
                             dist = "poisson")
zeroinfl_poisson
stargazer(zeroinfl_poisson, type="text")

summary(mexico_poisson)
summary(zeroinfl_poisson)

stargazer(zeroinfl_poisson, mexico_poisson, zero.component = T,type="text")
output_stargazer("tables/zip_poisson.tex",
                 zeroinfl_poisson,zero.component = T,
                 model.names = T, ord.intercepts = T, model.numbers = F,
                 #column.labels = c("negative", "no change", "positive"),
                 #column.separate = c(1,1,1),
                 title="Zero-inflation model",
                 label="tbl:zip", digits = 3, appendVal = FALSE
)
output_stargazer("tables/zip_poisson.tex",
                 zeroinfl_poisson, mexico_poisson,
                 model.names = T, ord.intercepts = T, model.numbers = F,
                 #column.labels = c("negative", "no change", "positive"),
                 #column.separate = c(1,1,1),
                 title="Zero Infl Poisson vs Poisson Model",
                 label="tbl:zip:pois", digits = 3, appendVal = TRUE
)

rm(ptest)
summary(zeroinfl_poisson)

mexico_null <- glm(PAN.visits.06 ~ 1, data= mexico,
                      family =poisson )

anova(mexico_null, mexico_poisson, test = "Chisq")

stargazer(mexico, type="text")
zeroinfl_poisson[[1]][2]
# TODO outlier
#mexico %>% ggplot(aes(PAN.visits.06)) + geom_boxplot()

coeffs
outlier_df <- mexico[mexico$PAN.visits.06==35,]
cbind(predict(mexico_poisson, outlier_df, type= "response", se.fit =TRUE),
      outlier_df)
lambdaOut <- exp(coeffs[1]+coeffs[2]*outlier_df$competitive.district + 
                   coeffs[3]*outlier_df$marginality.06 + 
                   coeffs[4]*outlier_df$PAN.governor.06)
lambdaOut
m_outlier <- filter(mexico, mexico$PAN.visits.06!=35)
summary(mom<-glm(PAN.visits.06 ~ competitive.district + 
            marginality.06 + PAN.governor.06, data= m_outlier, family =poisson))



stargazer(mom, mexico_poisson,type="text")
tail(mexico)

pred_mex <- cbind(predict(mom, mex_pred_data, type= "response", se.fit =TRUE),
                  mex_pred_data)
# create lower and upper bounds for CIs
pred_mex$lowerBound <- pred_mex$fit - 1.96 * pred_mex$se.fit
pred_mex$upperBound <- pred_mex$fit + 1.96 * pred_mex$se.fit

round(pred_mex,3)

###=================================================########
predict_data <- data.frame(REG = rep(c(0,1), each = 2), 
                           OIL= rep(c(0,1), 2))

mpredict_data <- data.frame(competitive.district = rep(c(0,1), each = 6), 
                            marginality.06=rep(c(-1,0,1), each = 4),
                            PAN.governor.06= rep(c(0,1), each = 6))

mpredict_data <- data.frame(competitive.district = rep(c(0,1),6), PAN.governor.06=0,
                            marginality.06=rep(-2:3,2)   )

mpredict_data <-cbind(mpredict_data, predict(mexico_poisson,
                             newdata = mpredict_data, type = "response"))
mpredict_data <- rename(mpredict_data, predict=`predict(mexico_poisson, newdata = mpredict_data, type = "response")`)

ggplot(aes(x=marginality.06, y=predict), data = mpredict_data) +
  geom_line(aes(colour=competitive.district)) +
  facet_grid(vars(competitive.district))

anova(mexico_poisson, test="Chisq")
class(mpredict_data)
hist(mpredict_data$predict)
#colour=competitive.district

plot(mpredict_data$marginality.06, mpredict_data$predict, type="l")
#lambda(x+1) = lambda(x)e^beta1
#If beta1 > 0, then expected count increases as X increases
#If beta1 < 0, then expected count decreases as X increases

p <- mexico
p$phat <- predict(mexico_poisson, type="response")
head(p)
## order by program and then by math
#p <- p[with(p, order("competitive.district","marginality.06")), ]


## create the plot
ggplot(p, aes(x = marginality.06, y = phat, 
              colour = as.factor(competitive.district))) +
  geom_point(aes(y = PAN.visits.06), alpha=.4, shape='.', 
             position=position_jitter(h=.2)) +
  geom_line(size = 1) + ylim(-1,5.5) +
  labs(x = "marginality", y = "predicted visits", colour="Swing District" ,
       caption="plot excluding district 9000, 35 visits") +
  ggtitle(label="Predicted PAN Candidate visits") +
  theme(plot.title = element_text(size=11),
        plot.caption = element_text(size=4), 
        legend.text = element_text(size = 8),
        legend.position = "bottom", strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("graphics/mex_pred.png")


#s13 Example: parameter interpretation
#For the elephant data:
#  beta_hat0 : No inherent meaning in the context of the data since
#age= 0 is not meaningful, outside of range of possible data
#Since coefficient is positive, expected # of mates " with age
# beta_hat1 : An increase of 1 year in age increases expected number
#of elephant mates by a multiplicative factor of e^0.06859 ~ 1:07

exp(coeffs[2])

#s14 Example: Getting fitted values


#s17 Assumptions: Over-dispersion
#Assuming that model is correctly specified, assumption that
#conditional variance is equal to conditional mean should be
#checked
#There are several tests including the likelihood ratio test of
#over-dispersion parameter alpha by running same model
#using negative binomial distribution
#R package AER provides many functions for count data
#including dispersiontest for testing over-dispersion
#One common cause of over-dispersion is excess zeros, which
#in turn are generated by an additional data generating
#process
#In this situation, zero-inflated model should be considered

#s18 Zero inatied poisson: # of mates
# of mates
#Frequency
#0 2 4 6 8
#0 2 4 6 8 10 12 14
#Though predictors do seem to impact distribution of elephant mates,
#Poisson regression may not be a good fit (large # of 0s)
#  We’ll check by
#> Running an over-dispersiontest
#> Fit a zero-inflated Poisson regression

#s19
# check equal variance assumption
#require("AER")
dispersiontest(elephant_poisson)
#	Overdispersion test

#data:  elephant_poisson
#z = 0.49631, p-value = 0.3098
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion
#1.107951

?dispersiontest
#Tests the null hypothesis of equidispersion in Poisson GLMs against
#the alternative of overdispersion and/or underdispersion.


#s21 ZIP model in R
#R contributed package “pscl" contains the function zeroinfl:
# same equation for l o g i t and poisson
#require("pscl")
zeroinfl_poisson <- zeroinfl(Matings ~ Age , data=elephants,
                             dist = "poisson")
zeroinfl_poisson
stargazer(zeroinfl_poisson, type="text")

#Call:
#  zeroinfl(formula = Matings ~ Age, data = elephants, dist = "poisson")

#Count model coefficients (poisson with log link):
#  (Intercept)          Age
#-1.44877      0.06559

#Zero-inflation model coefficients (binomial with logit link):
#  (Intercept)          Age
#222.725       -8.132

#s22 Exposure Variables: Offset parameter
#Count data often have an exposure variable, which indicates # of
#times event could have happened
#This variable should be incorporated into a Poisson model
#using offset option

#s23 Ex: Food insecurity in Tanzania and Mozambique
#Survey data from households about agriculture
#Covered such things as:
# > Household features (e.g. construction materials used,
#                        number of household members)
# > Agricultural practices (e.g. water usage)
# > Assets (e.g. number and types of livestock)
# > Details about the household members
#Collected through interviews conducted between Nov. 2016 -
#  June 2017 using forms downloaded to Android Smartphones

#s24 What predicts owning more livestock?
#Outcome: Livestock count [1-5]
#Predictors:
# > # of years lived in village
# > # of people who live in household
# > Whether they’re apart of a farmer cooperative
# > Conflict with other farmers

#s25 Owning Livestock: Estimate poisson regression
# load data
#safi <- read.csv("https://raw.githubusercontent.com/ASDS=TCD/StatsII_Spring2023/main/datasets/SAFI.csv",                                           stringsAsFactors = T )
safi <- read.csv("../datasets/SAFI.csv")

# estimate poisson regression model
safi_poisson <- glm(liv_count ~ no_membrs + years_liv +
                      memb_assoc + affect_conflicts, data= safi,
                    family =poisson )

coeffs <- coefficients(safi_poisson)
xvalues <- sort(safi$years_liv)
means <- exp(coeffs[1] + coeffs[2] * xvalues)
plot(xvalues, safi$liv_count)
#plot(xvalues, means, lty=2, col="red", type = "l")
lines(xvalues, means, lty=2, col="red")
plotdf <- data.frame(x=xvalues, means=means, liv_count=safi$liv_count)

plotdf %>% ggplot(aes(x=xvalues)) + ylim(0,5) +
  geom_jitter(aes(y=liv_count)) +
  geom_line(aes(y=means, colour = "red")) +
  theme_minimal() +
  ggtitle(label="owning livestock - poisson regression curve") +
  xlab(label = "years lived in village") +
  ylab(label = "number of livestock")


safi_ex <- data.frame(no_membrs = rep(mean(safi$no_membrs), 6),
                      years_liv = seq (1, 60, 10),
                      memb_assoc = rep("no", 6),
                      affect_conflicts = rep("never", 6))
pred_safi <- cbind(predict(safi_poisson , safi_ex, type="response",
                           se.fit=TRUE), safi_ex)

#Warning message:
#In predict.lm(object, newdata, se.fit, scale = residual.scale,
#type = if (type ==  :  prediction from a rank-deficient fit may be misleading


# create lower and upper bounds for CIs
pred_safi$lowerBound <- pred_safi$fit -
  1.96 * pred_safi$se.fit
pred_safi$upperBound <- pred_safi$fit +
  1.96 * pred_safi$se.fit

pred_safi %>% ggplot(aes(x=years_liv)) +
  geom_pointrange(aes(y=pred_safi$fit,
                      ymin=pred_safi$lowerBound,
                      ymax=pred_safi$upperBound)) +
  theme_minimal()+
  ggtitle(label = "Livestock Ownership") +
  ylab("Predicted no of livestock") + xlab("Years in Village")


#s28 Owning Livestock: Over-dispersion
dispersiontest(safi_poisson)

#Overdispersion test
#data: safi_poisson
#z = -12.433, p-value = 1
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:
#  dispersion
#0.4130252
#Don’t really need a ZIP model


# Mon Mar 20 13:41:12 2023 ------------------------------

#https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
#https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

# Tue Mar 21 16:34:50 2023 ------------------------------



### replication
# 1 main figure
# 1 main table
# + 1 twist - interaction w goodness of fit; extra variable
#    alt model

ps1 <- function() {
  
  #genl
  lapply(c("ggplot2", "tidyverse", "stargazer"),  pkgTest)
  #q specific
  lapply(c("KSgeneral"),  pkgTest)
  lapply(c("lattice", "patchwork"),  pkgTest)
  
  # set wd for current folder
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # function to save output to a file that you can read in later to your docs
  output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
    output <- capture.output(stargazer(...))
    cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
  }
  
  options(prompt = "# ")
    # return results
    res <- tibble('D'= D, 'Dmax' = Dmax, 'Dmin' = Dmin,
                  'K'=K, 'pval'=1-qval)
    
  #p < 0.05 so we reject null hypothesis
  # ie evidence does not support the hypothesis that the
  # sample data is from Normal distribution
  
  #https://www.statisticshowto.com/kolmogorov-smirnov-test/
  #K-S Test P-Value Table
  # alpha = 0.05 , N > 50
  D_alpha <-1.36 / sqrt(N)
  #  0.04300698
  
  # Tue Feb  7 17:48:10 2023 ------------------------------
  
  
  summary(empiricalCDF)
  # OUTPUT  ------------------------------ ------------------------------
  # plot
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  emp_data <- sort(empiricalCDF)
  pnorm_data <- sort(pnorm(data))
  
  ks_df <- data.frame('index' = 1:N, 'Observed_CDF' = emp_data, 'Normal' = pnorm_data)
  summary(empiricalCDF)
  summary(pnorm_data)
  
  ks_df %>% gather(key, value, Observed_CDF, Normal) %>%
    ggplot(aes(x = index, y = value, colour = key)) +
    geom_line() + xlab('') +
    annotate(
      "text", label = "Observed vs Normal",
      x = 500, y = 1.01, size = 2
    )  +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  ggsave("graphics/kolmogorov_smirnov.png")
  
  #require(stargazer)
  output_stargazer("ks.tex", ks_df[,2:3],  appendVal = FALSE,
                   type = "latex", label = "tab:ks:data",
                   title="Data Summary Table", summary = TRUE,
                   rownames = FALSE, nobs=TRUE)
  
  test_notes <- paste("one sample, two sided, normal; alpha 0.05; D alpha: ", round(D_alpha,5))
  
  output_stargazer("ks.tex", ks_results, type = "latex", appendVal = TRUE,
                   label = "tab:ks:results",
                   table.placement = "!htbp",
                   summary = FALSE, flip = TRUE, digits = 5,
                   object.names = TRUE,
                   title = "Kolmogorov-Smirnov Test results",
                   notes = test_notes,
                   covariate.labels = c(" ", "value" )
  )
  
  
  # Tue Feb  7 17:06:15 2023 ------------------------------
  # check against built in KS implementations
  #require(KSgeneral)
  
  #https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
  KSgeneral::cont_ks_c_cdf(ks_results$D, N)
  KSgeneral::cont_ks_test(data, "pnorm")
  
  ks.test(data, "pnorm", )
  
  # Tue Feb  7 16:12:14 2023 ------------------------------
  #implementation of k-s pdf from PS01
  # input: observed data
  pks <- function (dat)  {
    N <- length(dat)
    ECDF <- ecdf(dat)
    empiricalCDF <- ECDF(dat)
    # generate test statistic
    D <- max(abs(empiricalCDF - pnorm(dat)))
    pval <- 0
    k = seq(1,N)
    #calculate critical value
    K <- D * sqrt(N)
    x <- K
    
    pval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * x^2)))
    pval <- pval * sqrt(2 * pi) * (1/x)
    return(pval)
  }
  1-pks(data)
  
  # input: D (test statistic), N (Number of values)
  pks_xN <- function (D,N)  {
    K <- D * sqrt(N)
    x<- K
    k = seq(1,N)
    
    qval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * x^2)))
    qval <- qval * sqrt(2 * pi) * (1/x)
    return(qval)
  }
  
  1-pks_xN(ks_results$Dval, N)
  
  # Tue Feb  7 16:20:54 2023 ------------------------------
  
  #kolmim implementation
  pkolmim <- function(x, N){
    s <- x ^ 2 * N
    ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(N) + 1.409 / N) * s))
    return(ps)
  }
  
  1-pkolmim(ks_results$D, N)
  
  # references
  #https://stackoverflow.com/questions/24697706/where-can-i-find-the-limiting-distribution-of-the-kolmogorov-smirnov-distance-in
  #https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm
  #https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjYzKjswej8AhWfQUEAHX2SBIEQFnoECAkQAQ&url=https%3A%2F%2Fwww.statology.org%2Fkolmogorov-smirnov-test-r%2F&usg=AOvVaw2oPtTJ0EQQCRTasHZKPLpQ
  #https://rweb.webapps.cla.umn.edu/R/library/stats/html/ks.test.html
  #https://www.jstatsoft.org/article/view/v039i11
  #https://www.statology.org/kolmogorov-smirnov-test-r/
  #https://stats.stackexchange.com/questions/389034/kolmogorov-smirnov-test-calculating-the-p-value-manually
  
  # Tue Feb  7 21:33:40 2023 ------------------------------
  #####################
  # Problem 2
  #####################
  #Estimate an OLS regression in R that uses the Newton-Raphson algorithm (specically BFGS,
  #which is a quasi-Newton method), and
  #show that you get the equivalent results to using lm.
  #Use the code below to create your data.
  set.seed (123)
  data2 <- data.frame(x = runif(200, 1, 10))
  data2$y <- 0 + 2.75*data2$x + rnorm(200, 0, 1.5)
  
  pd <-ggplot(data2, aes(x = y)) + geom_dotplot(aes()) + theme_minimal()
  pd
  pp <- ggplot(data2, aes(x, y)) + geom_point(size=0.8, colour = "blue") +
    plot_annotation(title = "y ~ x") & theme_minimal()
  pp
  ggsave("graphics/q2_xy.png")
  
  ph <- ggplot(data2, aes(x = y)) +
    geom_histogram(bins = 15, aes(y=..density..),colour = "black", fill = "grey") +
    geom_density(col="red", position="stack") +
    plot_annotation(title = "distribution of y") + theme_minimal()
  ph
  ggsave("graphics/q2_hist.png")
  
  #require(patchwork)
  pp  + ph + plot_layout(ncol = 1, guides = "collect") +
    theme_minimal() +
    plot_annotation(title = "Q2 Data") #&
  ggsave("graphics/q2_data.png")
  
  
  # from Jeff's notes
  # define likelihood function to be solved by optim
  linear.lik <- function(theta, y, X){
    n <- nrow(X )
    k <- ncol(X )
    beta <- theta[ 1 : k ]
    sigma2 <- theta[ k + 1 ] ^ 2
    e <- y - X%*%beta
    logl <- -.5 *n* log(2 * pi)-.5 *n* log(sigma2)-(( t(e)%*%
                                                        e)/(2 * sigma2))
    return(-logl )
  }
  
  #  use optim to get MLE estimators for parameters
  linear.MLE <- optim(fn = linear.lik, par = c(theta=1, y=1, X=1),
                      hessian =TRUE, y = data2$y, X= cbind(1, data2$x), method = "BFGS")
  
  # get se for the parameters
  linear.MLE$se <- sqrt(diag(solve(linear.MLE$hessian)))
  
  
  linear.MLE$par
  #theta          y          X
  #0.1398324  2.7265559 -1.4390716
  
  linear.MLE$se
  #     theta          y          X
  #0.25140690 0.04136606 0.07191798
  
  # Ex: Compare MLE to lm in R
  #  Ordinary least squares is equivalent to maximum likelihood
  #  for a linear model, so it makes sense that lm would give us
  #  same answers
  
  linear.lm <- lm(y ~ x, data2)
  summary(lm(y ~ x, data2))
  # summary(lm(y ~ x, data2))
  #
  #Call:
  #	lm(formula = y ~ x, data = data2)
  #
  #Residuals:
  #	Min      1Q  Median      3Q     Max
  #-3.1906 -0.9374 -0.1665  0.8931  4.8032
  #
  #Coefficients:
  #	Estimate Std. Error t value Pr(>|t|)
  #(Intercept)  0.13919    0.25276   0.551    0.582
  #x            2.72670    0.04159  65.564   <2e-16 ***
  #	---
  #	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  #Residual standard error: 1.447 on 198 degrees of freedom
  #Multiple R-squared:  0.956,	Adjusted R-squared:  0.9557
  #F-statistic:  4299 on 1 and 198 DF,  p-value: < 2.2e-16
  
  # Fri Feb 10 22:42:16 2023 ------------------------------
  
  summary(linear.MLE)
  ll.lm <-linear.lm
  ll.lm$coefficients <- linear.MLE$par
  ll.lm$fitted.values <- linear.MLE$par[1] + linear.MLE$par[2]*data2$x
  ll.lm$residuals <- data2$y - linear.MLE$fitted.values
  
  linear.MLE.coefs <- data.frame(coefficient=names(linear.MLE$par), value=as.numeric(linear.MLE$par),
                                 se=as.numeric(sqrt(diag(solve(linear.MLE$hessian)))))
  ll.lm$se<- sqrt(diag(solve(linear.MLE$hessian)))
  
  ll.lm$rank <- 0
  
  stargazer(ll.lm, linear.lm, se = list(ll.lm$se), summary=FALSE, type='latex')
  
  stargazer::stargazer(ll.lm, type="text", summary=FALSE)
  
  output_stargazer("mle_models.tex",
                   ll.lm, linear.lm,
                   title="Comparison of MLE to Linear Model - normal",
                   label="tab:mle:lm",  digits = 4, appendVal = FALSE
  )
  
  stargazer(linear.MLE.coefs, title="MLE Coefficients", summary=FALSE, type="text")  # Works, but how can I mimic what stargazer does with lm objects?
  
  stargazer(ll.lm, linear.lm, se = list(linear.MLE.coefs$se), summary=FALSE, type='latex',
            label = "tab:mle", file="q2models.tex")
  
  stargazer(ll.lm, se = list(ll.lm$se), summary=FALSE, type='text')
  
  
  #check prediction equation
  paste("hat(y) = ", round(linear.MLE$par[1],5) , " (theta)  + ",
        round(mean(data2$x),5) , 	" (mean(x)) * ",  round(linear.MLE$par[2],5),
        " (beta) = " , round(linear.MLE$par[1] + linear.MLE$par[2]  * mean(data2$x),5))
  linear.MLE$par[1] + linear.MLE$par[2]  * mean(data2$x)
  mean(data2$y)
  mean(data2$y)
  
  sd(data2$x)
  
  
  #https://colinfay.me/intro-to-r/statistical-models-in-r.html
  #https://stackoverflow.com/questions/66906859/how-to-use-optim-to-produce-coefficient-estimates-for-a-generalized-linear-mod
  
  #https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/
  #https://stackoverflow.com/questions/27671317/stargazer-model-names-instead-of-numbers
}

not_used <- function() {
  #jeff
  # faraway  - ilogit
  #lapply(c("faraway"),pkgTest)
  #GS
  #lapply(c("xtable", "reshape", "rms"), pkgTest)
  #lapply(c("Zelig", "ZeligChoice", "apsrtable"), pkgTest)
  
}

lm_test <- function() {
  lm_model <- lm(GDPWdiff ~ REG +
                   OIL , #+REG*OIL,
                 data = gdp )
  ggplot(data=gdp, aes(y= GDPWdiff ,x= REG * OIL)) +
    geom_line()
  
  coeffs
  
  coeffs <- coefficients(lm_model)
  
  plot(lm_model)
  
  coeffs <- coefficients(lm_model)
  xvalues <- seq(1,length(gdp$GDPWdiff))
  #xvalues <- sort(gdp$REG)
  means <- sort(coeffs[1] + coeffs[2] * gdp$REG + coeffs[3] * gdp$OIL)
  plot(xvalues, gdp$GDPWdiff)
  #plot(xvalues, means, lty=2, col="red", type = "l")
  lines(xvalues, means, lty=2, col="red")
  
}

diffplots <- function() {
  #unique(gdp[c('diff2', "GDPWdiff")])
  ggplot(aes(y=GDPWdiff, colour = diff2), data = gdp)+ 
  geom_bar()
  
  
    plot(gdp$GDPWdiff, col=gdp$diff2,cex=0.5)
 
  plot(sort(gdp$GDPWdiff))
  
  ggplot(aes(y=GDPWdiff, colour = diff, fill = diff), data = gdp)+ 
    #ggplot(aes(y=GDPWdiff, colour = c(OIL)), data = gdp)+ 
    #  geom_boxplot() +facet_wrap(vars(REG, OIL)) +
    geom_histogram(binwidth = 100) +  #coord_flip() +
    facet_wrap(vars(REG, OIL)) +
    ggtitle(label="Difference in GDP year-on-year") +
    ylab("REG = 1: democracy") +
    xlab("OIL = 1: country with significant oil revenue")  +
    labs(colour="GDP diff", fill="GDP diff",
         caption = "OIL=1 : the average ratio of fuel exports to total exports in 1984-86 exceeded
50%") +
    #  scale_fill_manual(name="group",values=c("blue","green", "red"),labels=diff)
    theme(plot.caption = element_text(size=4),
          legend.text = element_text(size = 8))
  
  ggplot(aes(x=COUNTRY, y=GDPWdiff), data = gdp)+ 
    geom_point() + # coord_flip() +
    facet_wrap(vars(REG, OIL)) +
    ggtitle(label="Change in GDP year-on-year") +
    ylab("REG = 1: democracy") +
    xlab("OIL = 1: country with significant oil revenue")  +
    labs(fill="GDP diff", #colour="GDP diff", 
         caption = "OIL=1 : the average ratio of fuel exports to total exports in 1984-86 exceeded
50%") +
    theme(plot.caption = element_text(size=4), 
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  
  ggplot(aes(y=GDPWdiff, colour = diff), data = gdp)+ 
    #ggplot(aes(y=GDPWdiff, colour = c(OIL)), data = gdp)+ 
    geom_boxplot() +facet_wrap(vars(REG, OIL)) +
    #geom_histogram(binwidth = 100) +  #coord_flip() +
    facet_wrap(vars(REG, OIL)) +
    ggtitle(label="Difference in GDP year-on-year") +
    ylab("REG = 1: democracy") +
    xlab("OIL = 1: country with significant oil revenue") +
    labs(caption = "OIL=1 : the average ratio of fuel exports to total exports in 1984-86 exceeded
50%") +
    theme(plot.caption = element_text(size=5), 
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ?pairs
  gdp %>% ggplot(aes(y=GDPWdiff, x=COUNTRY, colour=as.factor(OIL),pch=as.factor(REG))) +
    geom_point() +
    facet_wrap(vars(REG,OIL)) + 
    ggtitle(label = "Difference in GDP between year t and t-1")
  ggsave("graphics/gdp_ex_vars.png")
  
  gdp %>% ggplot(aes(x=COUNTRY, y = GDPWdiff, alpha=0.2)) +
    geom_point() + theme_minimal() +  coord_flip() +
    ggtitle(label="Difference in GDP between year t and t-1")
  # legend
  ggsave("graphics/gdp_diff_country.png")
 
  pred_multi %>% ggplot() +
    geom_point(aes(x=REG, y = probability, colour=as.factor(OIL))) + 
    facet_wrap(vars(level)) +
    theme_minimal() +
    ggtitle(label="predictions for ordered model")
 
  gdp_tbl <- crosstable(gdp, GDPWdiff, by=c(REG, OIL), #, funs=c(mean, min, max)
                        label=F)
  output_stargazer(outputFile = "tables/gdp_crosstab.tex", gdp_tbl, 
                   summary=F, appendVal = F, 
                   label="tbl:gdp", caption="GDP data summary")
  gdp_tbl

  # see how well our predictions were :
  mm <- data.frame(fitted=multinom_model$fitted.values, 
                   residuals=multinom_model$residuals)
  plot(multinom_model$residuals, mm$fitted)
  ggplot(data=mm, aes(y="residuals")) +# x=1:length(residuals))) +
    geom_histogram()
  #gdp$ordered_diff <- factor(gdp$diff, ordered = TRUE)
  
  output_stargazer("tables/ordered.tex", summary_table, 
                   type = "latex",
                   appendVal = TRUE,
                   table.placement = "!htbp",
                   summary = FALSE, flip = FALSE, digits = 3,
                   object.names = TRUE
  )

  # ROC for logistic regression
  library(ROCR)
  pred_m_all <- predict(multinom_model, type="probs")
  pred_o_all <- predict(ord_model, type="probs")
  pred_m <- prediction(pred_m_all, gdp$diff2)
  pred_o <- prediction(ord_model, gdp$diff)
  perf_multi <- performance(pred_m, "tpr", "fpr")
  perf_ord <- performance(pred_o, "tpr", "fpr")
  
  plot(perf_multi, colorize=TRUE)
  plot(perf_ord, colorize=TRUE)
  
  
}

