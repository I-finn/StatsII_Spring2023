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
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(here())
ggetwd()
options(prompt="#", continue="#>")
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
#infant's gender as covariates. Present and interpret the output.

data(infants)

head(infants)

#### Completed
# a)
child_surv <- with(child, Surv(enter, exit, event))
infant_surv <- with(infants, Surv(enter, exit, event))

crosstable(infants, c(mother, age, sex), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)
crosstable(infants, c(enter, exit), funs=c(mean, min, max),
           label = F) %>% as_flextable(compact=TRUE)

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


km <- survfit(infant_surv ~ 1, data = infants)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years" ) #, ylim = c(0.7, 1))
autoplot(km)

km_civst <- survfit(infant_surv ~ civst, data = infants)
autoplot(km_civst)

# b)

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
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
  ggtitle(label="age at death/exita") + #coord_flip() +
  ylab("age at death/exit") +
  xlab("mothers age")  +
  labs(colour="sex", fill="sex",
       caption = "age at death ~ m.age+sex" ) +
  #  scale_fill_manual(name="group",values=c("blue","green", "red"),labels=sex)
  theme(plot.caption = element_text(size=4),
        legend.text = element_text(size = 8))




not_used <- function(){
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

gdp_tab <- crosstable(gdp, GDPWdiff, by=c(REG, OIL), #, funs=c(mean, min, max)
           label=F) %>% as_flextable(compact=TRUE)
gdp_tab  # graphics/gdp_flextab.png
rm(gdp_tab)
# Tue Mar 21 15:04:02 2023 ------------------------------

multinom_model <- multinom(diff2 ~ REG + OIL, data = gdp )

stargazer(multinom_model, type="text",report=("vcstp*"))
output_stargazer(outputFile = "tables/multinom.tex", multinom_model, 
                 title="Multinomial, unordered", 
                 label="tbl:multinom", report=("vcstp*"),
                 appendVal = FALSE)

exp(1.769007)   #5.865026

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
                 label="tbl:parallel", digits = 3, appendVal = FALSE)

output_stargazer("tables/pred_ord.tex", pred_ord, type = "latex", 
                 appendVal = FALSE,
                 label = "tbl:pred:ord",
                 table.placement = "!htbp",
                 summary = FALSE, flip = FALSE, digits = 3,
                 object.names = TRUE,
                 title = "Predicted results from Ordered Model"#,                 covariate.labels = c(" ", "value" )
)
##################################################################
#Week 9: Count Data - Poisson Regression


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

stargazer(mexico_poisson, type="text", report=("vcstp*"))
output_stargazer("tables/poisson.tex",
                 mexico_poisson, report=("vcstp*"),
                 model.names = T, ord.intercepts = T,
                 model.numbers = F,
                 #column.labels = c("negative", "no change", "positive"),
                 #column.separate = c(1,1,1),
                 title="Poisson Model of candidate visit counts",
                 label="tbl:poisson", digits = 3,
                 appendVal = FALSE)

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

dispersiontest(mexico_poisson)
#Overdispersion test
###=================================================########

mpredict_data <- data.frame(competitive.district = rep(c(0,1), each = 6), 
                            marginality.06=rep(c(-1,0,1), each = 4),
                            PAN.governor.06= rep(c(0,1), each = 6))


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
  # faraway  - ilogit
  #lapply(c("faraway"),pkgTest)
  #GS
  #lapply(c("xtable", "reshape", "rms"), pkgTest)
  #lapply(c("Zelig", "ZeligChoice", "apsrtable"), pkgTest)
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
  }
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

