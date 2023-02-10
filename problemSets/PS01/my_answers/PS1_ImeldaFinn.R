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

# create empirical distribution of observed data
set.seed(123)
N <- 1000
data <- rcauchy(n=N, location = 0, scale = 1)
summary(data)
hist(data)

View(data)

#H0 data follows normal distribution
#Ha data doesn't follow normal distribution
#alpha = 5%

# to latex
# function to return values for kolmogorov-smirnov test
#   - one-sample, comparison to normal PDF
# input: empirical data
# output: D-statistic, D+, D-, K-statistic (=D*sqrt(N)), P(K)
kolmogorov_smirnov <- function (dat)  {
  # get number of elements in data
  N <- length(dat)
  # convert data to ECDF
  ECDF <- ecdf(dat)
  empiricalCDF <- ECDF(dat)
  # calculate test statistic (D)
  D <- max(abs(empiricalCDF - pnorm(dat)))
  Dmin <- min(empiricalCDF - pnorm(dat))
  Dmax <- max(empiricalCDF - pnorm(dat))

  #calculate critical value (K-statistic)
  K <- D * sqrt(N)

  # get probability of calculated test statistic
  k = seq(1,N)

  #calculate q value for P(K)
  qval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * K^2)))
  qval <- qval * sqrt(2 * pi) * (1/K)

  # return results
  res <- tibble('D'= D, 'Dmax' = Dmax, 'Dmin' = Dmin,
    'K'=K, 'pval'=1-qval)
  return(res)
}

ks_results <-kolmogorov_smirnov(data)
print(ks_results)

#p < 0.05 so we reject null hypothesis
# ie evidence does not support the hypothesis that the
# sample data is from

#https://www.statisticshowto.com/kolmogorov-smirnov-test/
#K-S Test P-Value Table
# alpha = 0.05 , N > 50
D_alpha <-1.36 / sqrt(N)
#  0.04300698

# Tue Feb  7 17:48:10 2023 ------------------------------

# output
summary(empiricalCDF)

#require(stargazer)
output_stargazer("ks.tex", df[,2:3],  appendVal = FALSE,
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

# Tue Feb  7 16:50:37 2023 ------------------------------
# plot
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
emp_data <- sort(empiricalCDF)
pnorm_data <- sort(pnorm(data))

df <- data.frame('index' = 1:N, 'Observed_CDF' = emp_data, 'Normal' = pnorm_data)
summary(empiricalCDF)
summary(pnorm_data)

df %>% gather(key, value, Observed_CDF, Normal) %>%
  ggplot(aes(x = index, y = value, colour = key)) +
  geom_line() + xlab('') +
	annotate(
		"text", label = "Observed vs Normal",
		x = 500, y = 1.01, size = 2
	)  +
	theme_minimal() +
	theme(legend.position = "bottom", legend.title = element_blank())

ggsave("graphics/kolmogorov_smirnov.png")

# Tue Feb  7 17:06:15 2023 ------------------------------
# check against built in KS implementations
#require(KSgeneral)

#https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
KSgeneral::cont_ks_c_cdf(ks_results$Dval, N)
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
pp <- ggplot(data2, aes(x, y)) + geom_point(size=1) +
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
# define function to be solved for maximum by optim
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



#----------------------------------------------------------------------

#https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/


#png(filename = "graphics/q2_hist.png")
#hist(data2$y, breaks = 15,probability = T ,
#     main = "Histogram of y Variable")
#lines(density(data2$y), col="red", lwd=2)
#dev.off()


#  Ex: Normal MLE
#  This function, at different values of β and σ, creates a surface
surface <- list()
k <- 0
for(beta in seq(0, 5, 0.1)) {
	for(sigma in seq(0.1, 5, 0.1)) {
		k <- k + 1
		logL <- linear.lik(theta= c(0, beta, sigma), y = data2$y,
											 X = cbind(1, data2$x))
		surface[[k]] <- data.frame(beta = beta, sigma = sigma ,
															 logL = -logL )
	}
}

surface <- do.call(rbind, surface )
#require(lattice)

# doesn't work as png
pdf("./graphics/wireframe.pdf", width = 9 )
wireframe(logL ~ beta * sigma, surface, shade = TRUE )
dev.off()

# Fri Feb 10 22:02:49 2023 ------------------------------

x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,
			 1.8369, 1.8610, 1.8839)
y <- c( 6, 13, 18, 28, 52, 53, 61, 60)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)

fn <- function(p) {
	sum(-(y*(p[1]+p[2]*x ) - n*log(1+exp(p[1]+p[2]*x))+ log(choose(n,y))))
}

out <- nlm(fn, p=c(-50,20), hessian = TRUE)

out$code
out$estimate

out <- nlm(f=linear.lik, p=c(1,1,1), hessian = TRUE,
					 y = data2$y, X= cbind(1, data2$x))
out

neg.LL <- function(p=c(0,0)) with(data2, {
	eta <- p[1] + p[2]*x
	sd_val <- sqrt(sum((x-eta)^2)/length(x))
	- sum(dnorm(y, mean = eta, sd=sd_val, log = FALSE))
})
opt<- optim(c(0,0), neg.LL, method="BFGS")
opt
# compare to linear model
fm <- lm(y~x, data = data2)
fm
#opt<- optim(coef(fm), neg.LL, method="BFGS")
opt
coef(fm)


# Fri Feb 10 22:28:26 2023 ------------------------------

#beta <- 2.7
#sigma <- sqrt(1.3 )
#ex_data <- data.frame(x = runif(200, 1, 10))
#ex_data$y <- 0 + beta * ex_data$x + rnorm(200, 0, sigma )

#data2 %>% ggplot(aes(x=x, y=y)) + geom_point()
#ggsave("graphics/mle_data.png")

#pdf("./graphics/normal_mle_ex.pdf ", width = 9 )
#plot(data2$x, data2$y, ylab = 'Y', xlab = 'X')
#dev.off()

# Fri Feb 10 22:29:14 2023 ------------------------------
4# Newton-Raphson solution + Fisher scoring
#library
# gives more information on models
require(olsrr)
ols2 <- olsrr(y~x, data = data, family = quasi )


MLE <- optim(c(0,0), fn = neg.LL, control=(list(fnscale = -1)),
						 hessian = T)

# Fri Feb 10 22:31:49 2023 ------------------------------


library(bbmle)
m<-mle2(neg.LL,start=list(),data=data2)
stargazer::stargazer(fm, type="text")
stargazer::stargazer(ols, type="text")
output_stargazer(opt)

# coef(fm)
#(Intercept)           x
#0.1391874   2.7266985
