#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

#genl
lapply(c("ggplot2", "tidyverse", "stargazer"),  pkgTest)
#specific
#lapply(c("olsrr"),  pkgTest)
lapply(c("KSgeneral"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# function to save output to a file that you can read in later to your docs
output_stargazer <- function(outputFile, appendVal=TRUE, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=appendVal)
}

#options(scipen = -10)
options(prompt = "# ")
#####################
# Problem 1
#####################

#The Kolmogorov-Smirnov test uses cumulative distribution statistics test the similarity of the
#empirical distribution of some observed data and a specified PDF, and serves as a goodness
#of fit test. The test statistic is created by:
#  D = max_i=1:n{i/n - F_(i); F_(i-1) - (i - 1)/n}

#where F is the theoretical cumulative distribution of the distribution being tested and F(i) is
#the ith ordered value. Intuitively, the statistic takes the largest absolute difference between
#the two distribution functions across all x values. Large values indicate dissimilarity and the
#rejection of the hypothesis that the empirical distribution matches the queried theoretical
#distribution. The p-value is calculated from the Kolmogorov- Smirnoff CDF:

#  p(D <= x) = sqrt((2 * pi) / x) Sum_k = 1:1000{e^(-*2k-1)^2 * pf^2/(8x^2)}

#which generally requires approximation methods (see Marsaglia, Tsang, and Wang 2003).
#This so-called non-parametric test (this label comes from the fact that the distribution of
#the test statistic does not depend on the distribution of the data being tested) performs
#poorly in small samples, but works well in a simulation environment. 
#Write an R function that implements this test where the reference distribution is normal.
#Using R generate 1,000 Cauchy random variables (rcauchy(1000, location = 0, scale = 1)) and 
#perform the test (remember, use the same seed, something like set.seed(123), whenever you're 
#generating your own data).

set.seed(123)
N <- 1000
# create empirical distribution of observed data
data <- rcauchy(n=N, location = 0, scale = 1)
summary(data)
hist(data)

View(data)
write.csv(data, "ks.csv")

#H0 data follows normal distribution
#Ha data doesn't follow normal distribution
#alpha = 5%

# to latex
kolmogorov_smirnov <- function (dat)  {
  # get number of elements in data
  N <- length(dat)
  # convert data to ECDF
  ECDF <- ecdf(dat)
  empiricalCDF <- ECDF(dat)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(dat)))

  # get probability of calculated test statistic
  #create sequence of N numbers
  k = seq(1,N)
  #calculate critical value
  K <- D * sqrt(N)

  #calculate q value for P(K)
  qval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * K^2)))
  qval <- qval * sqrt(2 * pi) * (1/K)
  
  # return results
  res <- tibble('Dval'= D, 'Kval'=K, 'pval'=1-qval)
  return(res)
}

ks_results <-kolmogorov_smirnov(data)
print(ks_results)

#https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
KSgeneral::cont_ks_c_cdf(ks_results$Dval, N)
ksg<-KSgeneral::cont_ks_test(data, "pnorm")
ksg


# Tue Feb  7 16:12:14 2023 ------------------------------

#implementation of k-s pdf from PS01
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

nPlot <- 100
d <- 1:nPlot / (nPlot/2)
k<- sqrt(nPlot) * d

pPlot <- 1-pks_xN(k, nPlot)
plot(k, pPlot)

nData <- rnorm(n = 1000, mean = 0, sd = 1)
summary(nData)
hist(nData)
# check results comparing two normal samples
kN<-kolmogorov_smirnov(nData)
kN$pval



ksg$p.value
1-.Call(stats:::C_pKolmogorov2x,  STATISTIC = kN$statistic, n = N)
1-.Call(stats:::C_pKolmogorov2x,  STATISTIC = ks_results$Kval, n = N)
# 6.106227e-15


#https://stackoverflow.com/questions/24697706/where-can-i-find-the-limiting-distribution-of-the-kolmogorov-smirnov-distance-in
datN <- rnorm(n = N, mean = 0, sd = 1)
print(kolmogorov_smirnov(datN))
kN<-ks.test(datN, "pnorm")
1-.Call(stats:::C_pKolmogorov2x,  STATISTIC = kN$statistic, n = N)
#> alpha, so can't reject null OK



ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
View(abs(empiricalCDF - pnorm(data)))

D <- max(abs(empiricalCDF - pnorm(data)))

1-psmirnov(D, sizes=c(1000, 1000), two.sided = TRUE)
# 2.321597e-08
# <alpha so reject null
#--------------------------------------------------------------

#kolmim implementation
n <- 100
x <- 1:100 / 500

pkolmim <- function(x, N){
  s <- x ^ 2 * N
  ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(N) + 1.409 / N) * s))
  return(ps)
}

pkolmim(D, N)

n <- N
x<- 1:n / (n/2)
#s <- x ^ 2 * n
#ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(n) + 1.409 / n) * s))
#ps_pdf <- ps
ps <- 1-pks_xN(x, n) 
for (i in 2:N) {
  ps_pdf[i] <- ps[i]-ps[i-1] 
}
plot.new()
#lines(x, ps)

plot(ps_pdf)
plot(empiricalCDF)
#plot(x, sapply(x, function (x) pkolmim(x, n)), type='l')
#plot(x, sapply(x, function (x) pks_xN(x, n)), type='l')

df <- data.frame(x, ps)
ggplot(data = df, aes(x = x, y=ps)) + 
  geom_point()
#=----------------------------------------------

#plot(x, sapply(x, function (x) pkolmim(x, n)), type='l')
s <- x ^ 2 * n
ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(n) + 1.409 / n) * s))
lines(x, ps, lty=2)

plot(x, ps)
#=----------------------------------------------

y1 = rnorm(n, mean = 0, sd = 1)
ks.test(datN, y1, alternative="less")

# plots
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
emp_data <- sort(empiricalCDF)
pnorm_data <- sort(pnorm(data))

df <- data.frame('index' = 1:N, 'Observed_CDF' = emp_data, 'Normal' = pnorm_data)
summary(empiricalCDF)
summary(pnorm_data)

df %>% gather(key, value, Observed_CDF, Normal) %>%
  ggplot(aes(x = index, y = value, colour = key)) +
  geom_line() + xlab('')
ggsave("graphics/kolmogorov_smirnov.png")

hist(pnorm(data))
plot(pnorm(data))

ggplot(data <- df, aes()) +
  geom_line(aes(x = 1:N, y=emp_data),col = "green") +
  geom_line(aes(x = 1:N, y=pnorm_data), col="blue") 

ggplot(data = df, aes(index)) +
  geom_line(aes(y=Observed_CDF),col = "green") +
  geom_line(aes(y=Normal), col="blue") 

ggplot(data = df) + 
  geom_line(aes(x=index,y=Observed_CDF,color=Observed_CDF), group = 1) + 
  geom_line(aes(x=index,y=Normal,color=Normal), group = 2) + 
  ylab('Values')+xlab('')


#	Asymptotic one-sample Kolmogorov-Smirnov test

#data:  data
#D = 0.13573, p-value = 2.22e-16
#alternative hypothesis: two-sided


#p < 0.05 so we reject null hypothesis
# ie evidence does not support the hypothesis that the 
# sample data is from 

z <- ks_results$Dval 

pval <- 1
for ( i in 1:1000) {
  pval <- pval - 2 * sum((-1)^(i-1)) * exp((-2)*(i^2)*(z^2))
}

#https://www.itl.nist.gov/div898/handbook/eda/section3/eda35g.htm
#https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwjYzKjswej8AhWfQUEAHX2SBIEQFnoECAkQAQ&url=https%3A%2F%2Fwww.statology.org%2Fkolmogorov-smirnov-test-r%2F&usg=AOvVaw2oPtTJ0EQQCRTasHZKPLpQ
#https://rweb.webapps.cla.umn.edu/R/library/stats/html/ks.test.html
#https://www.jstatsoft.org/article/view/v039i11
#https://www.statology.org/kolmogorov-smirnov-test-r/
#https://stats.stackexchange.com/questions/389034/kolmogorov-smirnov-test-calculating-the-p-value-manually


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

hist(data2$y, breaks = 10,probability = T ,
     main = "Histogram of y Variable")
lines(density(data2$y), col="red", lwd=2)

ggplot(data2, aes(x = y)) + geom_dotplot()


ggplot(data2, aes(x, y)) + geom_point()

#https://stackoverflow.com/questions/66906859/how-to-use-optim-to-produce-coefficient-estimates-for-a-generalized-linear-mod

ols <- glm(y~x, data = data2, family = "gaussian")
coef(ols)
#(Intercept)           x 
#1.6159588   0.1820778 



#dnorm(x, mean = 0, sd = 1, log = FALSE)
neg.LL <- function(p=c(0,0)) with(data2, {
  eta <- p[1] + p[2]*x 
  sd_val <- sqrt(sum((x-eta)^2)/length(x))
  - sum(dnorm(y, mean = eta, sd=sd_val, log = FALSE))
})
opt<- optim(c(0,0), neg.LL, method="BFGS")
opt
# compare to linear model
fm <- lm(y~x, data = data2)

#opt<- optim(coef(fm), neg.LL, method="BFGS")
opt
coef(fm)

ols$model
library(bbmle)
m<-mle2(neg.LL,start=list(),data=data2)
m

stargazer::stargazer(fm, type="text")
stargazer::stargazer(ols, type="text")
output_stargazer(opt)

# coef(fm)
#(Intercept)           x 
#0.1391874   2.7266985 

#-----------------------------------
f <- function(x) sum((x-1:length(x))^2)

ols <- nlm(f,  p=c(200,200) )
ols
summary(ols)

# NO below - Fisher scoring
ols <- glm(y~x, data = data, family = quasi )
summary(ols)
#library
#ols2 <- ols(y~x, data = data, family = quasi )

#quasibinomial doesn't work because y>1
ols <- glm(y~x, data = data, family = quasipoisson , method = "")
summary(ols)

#ols <- glm(y~x, data = data, family = binomial(logit) )
#summary(ols)


# library(olsrr)
olsr <- ols_regress(y~x, data = data2, family = "gaussian")
olsr


#https://rpubs.com/Koba/MLE-Normal
#library(bbmle)
mu.bb = mean(data2$y)
sd.bb = sd(data2$y)

MLE <- optim(c(0,0), fn = neg.LL, control=(list(fnscale = -1)),
             hessian = T)

#https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/
#hist(Y$Count, breaks = 50,probability = T ,main = "Histogram of Count Variable")
#lines(density(Y$Count), col="red", lwd=2)

library(caTools)


##
kolmogorov_smirnov <- function (dat)  {
  N <- length(dat)
  ECDF <- ecdf(dat)
  empiricalCDF <- ECDF(dat)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(dat)))
  x <- D
  pval <- 0
  #create sequence of N numbers
  k = seq(1,N)
  #calculate p value for P(D )
  pval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * x^2)))
  pval <- pval * sqrt(2 * pi) * (1/x)
  
  s <- x ^ 2 * N
  ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(N) + 1.409 / N) * s))
  
  K <- D * sqrt(N)
  res <- tibble('Dval'= D, 'Kval'=K, 'pval'=pval, 'pkolmim'=ps)
  return(res)
}
