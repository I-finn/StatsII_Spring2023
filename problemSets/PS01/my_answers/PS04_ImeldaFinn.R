###############################################################
# Imelda Finn, 22334657
# POP77003 - Stats I
# clear global .envir, load libraries, set wd
###############################################################

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if(length(package.list)>0)  for(package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if(length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# load necessary packages
lapply(c("ggplot2", "stargazer", "tidyverse", "stringr", "car",
         "broom", "gridExtra"),  
       pkgTest)
#require(gridExtra)

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
###############################################################

#Problem Set 4
#Applied Stats/Quant Methods 1
#Due: December 4, 2022
#Instructions
#• Please show your work! You may lose points by simply writing in the answer. If the
#problem requires you to execute commands in R, please include the code you used to
#get your answers. Please also include the .R file that contains your code. If you are
#not sure if work needs to be shown for a particular problem, please ask.
#• Your homework should be submitted electronically on GitHub.
#• This problem set is due before 23:59 on Sunday December 4, 2022. No late assignments
#will be accepted.

#-----------------------------------------------------------------
#Question 1: Economics
#In this question, use the prestige dataset in the car library. First, run the following
#commands:
#install.packages(car)
library(car)
help(Prestige)

#education
#Average education of occupational incumbents, years, in 1971.
#income
#Average income of incumbents, dollars, in 1971.
#women
#Percentage of incumbents who are women.
#prestige
#Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.
#census
#Canadian Census occupational code.
#type
#Type of occupation. A factor with levels(note: out of order):
# bc, Blue Collar; 
# prof, professional, Managerial, and Technical; 
# wc, White Collar.

#-----------------------------------------------------------------
#We would like to study whether individuals with higher levels 
# of income have more prestigious jobs. Moreover, we would like
#to study whether professionals have more prestigious jobs than
#blue and white collar workers.

# line in tex = 87
data(Prestige)

# - explore ------------------------------------------
mean(Prestige$prestige)
fivenum(Prestige$prestige)
#14.8 35.2 43.6 59.6 87.2
fivenum(Prestige$income)
# 611.0  4075.0  5930.5  8206.0 25879.0


#(a) Create a new variable professional by recoding the variable
# type so that professionals #are coded as 1, and blue and white
#collar workers are coded as 0(Hint: ifelse).

#line in tex 102
Prestige['professional'] <- ifelse(Prestige$type == 'prof', 1, 0)

Prestige %>% select(type, professional) %>% unique()
#type professional
#gov.administrators  prof            1
#nursing.aides         bc            0
#medical.technicians   wc            0
#athletes            <NA>           NA

Prestige %>% group_by(type, professional) %>% summary()
# 44 bc, 31 prof, 23 wc, 4 na
# mean income for 

Prestige %>% filter(is.na(type))
#              education income women prestige census type
#  athletes        11.44   8206  8.13     54.1   3373 <NA>
#  newsboys         9.62    918  7.00     14.8   5143 <NA>
#  babysitters      9.46    611 96.53     25.9   6147 <NA>
#  farmers          6.84   3643  3.60     44.1   7112 <NA>

Prestige %>% group_by(type, professional) %>% 
  mutate(minc = mean(income), meduc = mean(education)) %>% 
  select(type, minc, meduc) %>% unique()

#  professional type    minc meduc
#<dbl> <fct>  <dbl> <dbl>
#1            1 prof  10559. 14.1 
#2            0 bc     5374.  8.36
#3            0 wc     5052. 11.0 
#4           NA NA     3344.  9.34
#todo set newsboys and babysitters to blue collar
#todo set athletes = prof based on income, wc based on educ
#todo set farmers = w/bc based on income, bc based on educ

# class all nas as non-professional(based on education)
Prestige2 <- Prestige
Prestige2['professional'] <- ifelse(is.na(Prestige2$type) ,0,Prestige2$professional)

# class all nas as non-professional, apart from athletes(based on income)
Prestige3 <- Prestige2
Prestige3["athletes","professional"] <- 1

pres_nas <- lm(prestige ~ income + professional + income:professional, 
                    data = Prestige2)
pres_athletes <- lm(prestige ~ income + professional + income:professional, 
               data = Prestige3)

summary(pres_nas)

summary(pres_athletes)
Prestige3['athletes',]
#png("Graphics/prestige.png" )
pairs(~prestige + income + professional, Prestige)
#dev.off() # close output

#(b) Run a linear model with prestige as an outcome and income, 
#professional, and the #interaction of the two as predictors 
#(Note: this is a continuous * dummy interaction.)

#line in tex 162-163
pres_inc_prof <- lm(prestige ~ income + professional + 
                      income:professional, data = Prestige)
summary(pres_inc_prof)

output_stargazer("Tables/prestige_model.tex", appendVal = FALSE, pres_inc_prof,
                 title="Prestige as a function of professional job  and income",
                 label="tab:pres_inc_prof",  digits = 6
)

#Call:
#  lm(formula = prestige ~ income + professional + income:professional, 
#     data = Prestige)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-14.852  -5.332  -1.272   4.658  29.932 

#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         21.1422589  2.8044261   7.539 2.93e-11 ***
#  income               0.0031709  0.0004993   6.351 7.55e-09 ***
#  professional        37.7812800  4.2482744   8.893 4.14e-14 ***
#  income:professional -0.0023257  0.0005675  -4.098 8.83e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 8.012 on 94 degrees of freedom
#(4 observations deleted due to missingness)
#Multiple R-squared:  0.7872,	Adjusted R-squared:  0.7804 
#F-statistic: 115.9 on 3 and 94 DF,  p-value: < 2.2e-16

output_stargazer("Tables/prestige_nas.tex", 
                 pres_inc_prof, pres_nas, pres_athletes,
                 title="Effect of na job types on model",
                 label="tab:pres_nas",  digits = 4, appendVal = FALSE
)


View(Prestige)

# test for parallel lines
# hyp 0: beta3 = 0
plot_coefs <- pres_inc_prof$coefficients
plot_coefs

summary(pres_inc_prof)
tstat_b3 <- plot_coefs[4] / 0.0005675
pval_b3 <- 2*pt(abs(tstat_b3), df = 102 - 4- 4, lower.tail = FALSE)

#reject null - the two lines have different slopes

inc <- 16250
bwp <- plot_coefs[1] + plot_coefs[2] * inc
pp <- plot_coefs[1] + plot_coefs[3] + (plot_coefs[2] +plot_coefs[4])* inc

#"16500wc, bc 73.46 prof 72.87"
#"16250wc, bc 72.67 prof 72.66"
paste0(inc, "wc, bc ",round(bwp, 2)," prof ", round(pp,2))

#output model
stargazer(pres_inc_prof, pres_nas, pres_athletes, type = "text")
#===============================================
#  Dependent variable:    
#  ---------------------------
#  prestige          
#-----------------------------------------------
#  income                       0.003***          
# (0.0005)          
#
#professional                 37.781***         
# (4.248)          
#
#income:professional          -0.002***         
# (0.001)          
#
#Constant                     21.142***         
# (2.804)          
#
#-----------------------------------------------
#  Observations                    98             
#R2                             0.787           
#Adjusted R2                    0.780           
#Residual Std. Error       8.012(df = 94)      
#F Statistic           115.878***(df = 3; 94)  
#===============================================
#  Note:               *p<0.1; **p<0.05; ***p<0.01

#(c) Write the prediction equation based on the result.

#ref
#https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/contint?action=AttachFile&do=get&target=int.pdf


#Prediction Equation 
# prestige = 21.142 +(0.003) * income + 37.81 * professional +
#(-0.002) * professional * income 

# ie the prestige score is 21.142 when income is 0 and type = wc/bc
#    it increases by 0.003 for each unit increase in income
#    and increases by 37.81 if job type = prof
#    and by -0.002*income if job type = prof


#(d) Interpret the coefficient for income.
#when 
#  income                       0.003***          
# (0.0005)          


#(e) Interpret the coefficient for professional.
#professional                 37.781***         
# (4.248)          


#(f) What is the effect of a $1,000 increase in income on prestige score for professional
#occupations? In other words, we are interested in the marginal effect of income when
#the variable professional takes the value of 1. Calculate the change in ^y associated
#with a $1,000 increase in income based on your answer for(c).
(21.142 + 37.81) +(0.003-0.002)*1000


#(g) What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6,000. Calculate the change in ^y
#based on your answer for(c).
# professional = 0
21.42 + 0.003*6000
#39.42
# professional = 1
(21.142 + 37.81) +(0.003-0.002)*6000
#69.952
64.952 - 39.42
37.81 -0.002*6000
beta0 <-round(pres_inc_prof$coefficients[1],6)
beta1 <-round(pres_inc_prof$coefficients[2],6)
delta1 <-round(pres_inc_prof$coefficients[3],6)
delta2 <-round(pres_inc_prof$coefficients[4],6)

#  \[\hat{y_0} =(\beta_0) +(\beta_1)* 6000 = 21.42 + 0.003*6000 = 39.42\]
#when \texttt{professional} is 1, \texttt{prestige} is:
#  \[\hat{y}_1  =(\beta_0 + \delta_1) +(\beta_1 +\delta_2)* 6000 \]
#\[=(21.142 + 37.81) +(0.003-0.002)*6000 = 64.952  \]
#\[\Delta\hat{y}  =  \delta_1  \delta_2*x = 37.81 -0.002*6000 \]
#so, $\Delta\hat{y} = hat{y}_1 - \hat{y}_0 = 64.952 - 39.42 = 25.532 $

y0 <-beta0 + beta1 * 6000
y1 <-beta0 + delta1 +(beta1+delta2) * 6000
y1-y0
delta1 + delta2*6000

png("Graphics/add_variable.png" )
avPlots(pres_inc_prof, col = carPalette()[1], pch = 1, cex=0.5, id=FALSE)
dev.off() # close output

fivenum(Prestige$income)

#------------------------------------
png("Graphics/prestige_interaction.png")
plot(Prestige$income , Prestige$prestige, type= "n" ,
       xlim=c(500 , 30000) , ylab = "prestige" , xlab = "income" )
points(Prestige[which(Prestige$professional ==1) , "income"], 
        Prestige[which(Prestige$professional ==1) , "prestige" ],
        col =2 , pch =18)
points(Prestige[which(Prestige$professional ==0) , "income"],
       Prestige[which(Prestige$professional ==0) , "prestige"],
       col =3 , pch =18)

#when prof = 1
abline( plot_coefs[1] + plot_coefs[3], plot_coefs[2] + 
          plot_coefs[4], col =2 , lwd =2)
#when prof = 0
abline( plot_coefs[1] , plot_coefs[2] , col =3 , lwd =2)
legend("bottomright", legend=c("Professional", "White/Blue Collar"),
       col=c(2, 3), lty=1, cex=0.8)

dev.off()

#-----------------------------------------------------------------------
#ref:https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r
#https://rdrr.io/github/smbjohnson/UtilityFunctions/src/R/ggAVplots.R

avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile()
  png(filename = ff)
  OUT <- car::avPlots(MODEL, ...)
  dev.off()
  unlink(ff)
  OUT }
#Produce matrix of added variable plots
#library(gridExtra)

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL)
#  AVPLOTS <- avPlots(MODEL)
  K       <- length(AVPLOTS)
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K)
  for(i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]])
    # doesn't like aes_string - deprecated 
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black', size = 0.5) + 
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n(', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n(',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')) }
  
  #Return output object
  GGPLOTS }


gg_avs <- ggAVPLOTS(pres_inc_prof)

grid.arrange(gg_avs[[1]], gg_avs[[2]],gg_avs[[3]],  nrow = 3, ncol=1)
ggsave("./Graphics/av_ggplot.png")# only saves 2nd plot
# export manually to ./Graphics/av_gg_manual.png

#================================================================
#-----------------------------------------------------------------
#Question 2: Political Science

#Researchers are interested in learning the effect of all of 
#those yard signs on voting preferences.
#Working with a campaign in Fairfax County, Virginia, 131 precincts
#were randomly divided into a treatment and control group. 
#In 30 precincts, signs were posted around the precinct that 
#read: 
#"For Sale: Terry McAuliffe. Don't Sellout Virgina on November 5."
#Below is the result of a regression with two variables and a
#constant. The dependent variable is the proportion of the vote
#that went to McAuliffe's opponent Ken Cuccinelli. The
#first variable indicates whether a precinct was randomly
#assigned to have the sign against #McAuliffe posted. The second
#variable indicates a precinct that was adjacent to a precinct
#in the treatment group(since people in those precincts might 
#be exposed to the signs).
#Impact of lawn signs on vote share

#Precinct assigned lawn signs(n=30) 0.042(0.016)
#Precinct adjacent to lawn signs(n=76) 0.042(0.013)
#Constant 0.302(0.011)
#Notes: R2=0.094, N=131

# in LaTeX 415 - 418
n <- 131
est_coeffs <- 3
df <- n-est_coeffs
R2 <- 0.094

#(a) Use the results from a? linear regression to determine 
#whether having these yard signs in a precinct affects vote 
#share(e.g., conduct a hypothesis test with alpha= .05).
#Donald P. Green, Jonathan S. Krasno, Alexander Coppock, Benjamin D. Farrer, Brandon Lenoir, Joshua
#N. Zingher. 2016. 
#"The effects of lawn signs on vote outcomes: 
#Results from four randomized field experiments." 
#Electoral Studies 41: 143-150.

# in LaTeX 430 - 435
beta1 <- 0.042
n1 <- 30
se1 <- 0.016
# get t-test value and pvalue
t_precinct <- beta1 / se1
pval_precinct <- 2*pt(abs(t_precinct), df , lower.tail = FALSE)

pval_precinct # 0.00972002
paste0(round(pval_precinct*100,2), "%")


#(b) Use the results to determine whether being next to 
#precincts with these yard signs affects vote share 
#(e.g., conduct a hypothesis test with alpha = .05).

# in LaTeX 446 - 451
n2 <- 76
beta2 <- 0.042
se2 <- 0.013

t_adj <- beta2 / se2
pval_adj <- 2*pt(abs(t_adj), df, lower.tail = FALSE)
pval_adj # 0.00156946
paste0(round(pval_adj*100,2), "%")

pval_adj
#(c) Interpret the coefficient for the constant term substantively.
# if not in a precinct with yard signs and not in an adjacent 
# precinct, Cuccinelli gets 30.2% of the vote

# constant value
# in LaTeX 462 - 468
beta0 <- 0.302
se0 <- 0.011

tscore <- qt(0.975, df) # get tscore for df 128,

CI0_L <- beta0 - tscore*se0
CI0_U <- beta0 + tscore*se0
#At the 95% confidence level, the vote for C in a precinct with
# no signs and not adjacent to a precinct with signs, is between
paste0("CI: ",round(CI0_L*100,2), "% to ",round(CI0_U*100,2),"%." )

#(d) Evaluate the model fit for this regression. 
#What does this tell us about the importance of yard signs
#versus other factors that are not modeled?

#pvalues are < 5% - reject null hypothesis that no effect
#increase in vote in both cases(signs in precinct or adjacent) = 4.2%
#  could be enough in a close election

CI1_L <- beta1 - tscore*se1
CI1_U <- beta1 + tscore*se1

CI2_L <- beta2 - tscore*se2
CI2_U <- beta2 + tscore*se2

paste0("CI: ",round(CI1_L*100,2), "% to ",round(CI1_U*100,2),"%." )
paste0("CI: ",round(CI2_L*100,2), "% to ",round(CI2_U*100,2),"%." )


#but R2 is only 0.094(ie R is =/- 0.3065942)
# ie linear model only explains less than 10% of the variance in the dep var
# poss different model would explain more

multicolin <- 1/(1-R2)
# can't get R^2 for indi vars

# assumes predictors are independent - ie signs are the effect on adjacent 
# assumes precincts chosen so can only be in/adjacent/neither
# assumes precincts heterogeneous - party affiliation, voter turnout, 
#   through traffic, size
# assumes 
n = 131
k = 3

F.test <-(R2/(k-1))/((1-R2)/(n-k))
#F test statistic F = 6.640 with 2 and 129 degrees of freedom
df1 <- k - 1
df2 <- n-k

F.pvalue <-df(F.test, df1, df2)
# 0.001634304
# at 5% reject null hyp that betas are all 0

#partial F-test
#hyp 0: beta1 = beta2 = 0
#hyp a: at least 1 non-zero
# don't have enough info to do



