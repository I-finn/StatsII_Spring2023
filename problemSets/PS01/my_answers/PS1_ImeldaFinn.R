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
kolmogorov_smirnov <- function (dat)  {
  # get number of elements in data
  N <- length(dat)
  # convert data to ECDF
  ECDF <- ecdf(dat)
  empiricalCDF <- ECDF(dat)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(dat)))
  Dmin <- min(empiricalCDF - pnorm(dat))
  Dmax <- max(empiricalCDF - pnorm(dat))

  #calculate critical value
  K <- D * sqrt(N)

  # get probability of calculated test statistic
  k = seq(1,N)

  #calculate q value for P(K)
  qval <- sum(exp(-1*((2*k - 1)^2)*(pi^2)/(8 * K^2)))
  qval <- qval * sqrt(2 * pi) * (1/K)

  # return results
  res <- tibble('Dval'= D, 'Dmax' = Dmax, 'Dmin' = Dmin,
    'Kval'=K, 'pval'=1-qval)
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
K_alpha <-1.36 / sqrt(N)
#  0.04300698

# Tue Feb  7 17:48:10 2023 ------------------------------

# output

summary(empiricalCDF)

require(stargazer)
stargazer(df[,2:3], type = "latex", label = "tab:ks:data", out = "ks_data.tex",
          title="Data Summary Table", summary = TRUE,
          rownames = FALSE, nobs=TRUE)

ks_output <- ks_results
ks_output$k_alpha <- K_alpha
stargazer(ks_output, type = "latex", out="ks_results.tex", label = "tab:ks:results",
          title="Kolmogorov-Smirnov Test results - one sample",
          summary = FALSE, flip = TRUE, digits = 5,
          object.names = TRUE,
          dep.var.caption = "Kolmogorov-Smirnov Test results - one sample",
          add.lines = list(c("alpha 0.05", K_alpha)),
          model.numbers = FALSE,
          dep.var.labels = "compared to normal distribution"  ,
          column.labels = c("D Value", "D +", "D -", "Test Statistic (Z)", "P-value" ),
          covariate.labels = c("D Value", "D +", "D -", "Test Statistic (Z)", "P-value" )
)


#stargazer(ks_results, type = "text", title="table", flip=TRUE,
#          covariate.labels = c("difference", "statistic", "pvalue"))

# Tue Feb  7 16:50:37 2023 ------------------------------

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
  geom_line() + xlab('') +
	annotate(
		"text", label = "Observed vs Normal",
		x = 500, y = 1.01, size = 2
	) +
	theme(legend.position = "bottom", legend.title = element_blank())

ggsave("graphics/kolmogorov_smirnov.png")
# Tue Feb  7 17:06:15 2023 ------------------------------

# check against built in KS implementations
require(KSgeneral)

#https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
KSgeneral::cont_ks_c_cdf(ks_results$Dval, N)
ksg<-KSgeneral::cont_ks_test(data, "pnorm")
ksg

ks.test(data, "pnorm", )

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

#https://stackoverflow.com/questions/24697706/where-can-i-find-the-limiting-distribution-of-the-kolmogorov-smirnov-distance-in


#kolmim implementation
#n <- 100
#x <- 1:100 / 500

pkolmim <- function(x, N){
  s <- x ^ 2 * N
  ps <- pmax(0, 1 - 2 * exp(-(2.000071 + .331 / sqrt(N) + 1.409 / N) * s))
  return(ps)
}

1-pkolmim(ks_results$Dval, N)



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

png(filename = "graphics/q2_hist.png")
hist(data2$y, breaks = 15,probability = T ,
     main = "Histogram of y Variable")
lines(density(data2$y), col="red", lwd=2)
dev.off()

pd <- ggplot(data2, aes(x = y)) + geom_dotplot()
pp <- ggplot(data2, aes(x, y)) + geom_point()
pp + plot_annotation(title = "y ~ x") & theme_minimal()
ggsave("graphics/q2_xy.png")

#ph <-

ph <- ggplot(data2, aes(x = y)) +
	geom_histogram(bins = 15, aes(y=..density..),colour = "black", fill = "grey") +
	geom_density(col="red", position="stack") + theme_minimal()
ph
ggsave("graphics/q2_hist.png")

pp  + ph +
	plot_annotation(title = "y ~ x") & theme_minimal()
ggsave("graphics/q2_xy.png")

require(patchwork)

pd + pp + plot_layout(ncol = 2, guides = "collect") +
	plot_annotation(title = "y values") & theme_minimal()


#beta <- 2.7
#sigma <- sqrt(1.3 )
#ex_data <- data.frame(x = runif(200, 1, 10))
#ex_data$y <- 0 + beta * ex_data$x + rnorm(200, 0, sigma )

#data2 %>% ggplot(aes(x=x, y=y)) + geom_point()
#ggsave("graphics/mle_data.png")

#pdf("./graphics/normal_mle_ex.pdf ", width = 9 )
#plot(data2$x, data2$y, ylab = 'Y', xlab = 'X')
#dev.off()

# from Jeff's notes
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
require(lattice)

# doesn't work as png
pdf("./graphics/wireframe.pdf", width = 9 )
wireframe(logL ~ beta * sigma, surface, shade = TRUE )
dev.off()

#  Ex: Normal MLE in R
#  We can find parameters that specify this point with R's
#  built-in optimization commands

linear.MLE <- optim(fn = linear.lik, par = c(1, 1, 1), hessian =TRUE,
										y = data2$y, X= cbind(1, data2$x), method = "BFGS")

linear.MLE$par
#[1]  0.1398324  2.7265559 -1.4390716


# Ex: Compare MLE to lm in R
#  Ordinary least squares is equivalent to maximum likelihood
#  for a linear model, so it makes sense that lm would give us
#  same answers
#  I Note that σ2 is used in determining the standard errors

linear.lm <- lm(y ~ x, data2)
summary(lm(y ~ x, data2))
#  Coefficients:
#    Estimate Std. Error t value Pr(>|t|)
#  (Intercept) -0.01307 0.17228 -0.076 0.94
#  x 2.70436 0.02819 95.950 <2e-16 ***

# summary(lm(y ~ x, data2))

#Call:
#	lm(formula = y ~ x, data = data2)

#Residuals:
#	Min      1Q  Median      3Q     Max
#-3.1906 -0.9374 -0.1665  0.8931  4.8032

#Coefficients:
#	Estimate Std. Error t value Pr(>|t|)
#(Intercept)  0.13919    0.25276   0.551    0.582
#x            2.72670    0.04159  65.564   <2e-16 ***
#	---
#	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.447 on 198 degrees of freedom
#Multiple R-squared:  0.956,	Adjusted R-squared:  0.9557
#F-statistic:  4299 on 1 and 198 DF,  p-value: < 2.2e-16

starg.MLE <- glm(y~x, data = data2, family = quasi )
starg.MLE$coefficients <- linear.MLE

stargazer::stargazer(starg.MLE, type="text", summary=FALSE)
stargazer::stargazer(linear.lm, type="text")

print(linear.MLE)
str(linear.MLE)
str(linear.lm)


output_stargazer("mle_models.tex",
								 linear.MLE, linear.lm,
								 title="Comparison of MLE to Linear Model - normal",
								 label="tab:mle:lm",  digits = 4, appendVal = FALSE
)
stargazer(type="text",
								 starg.MLE, linear.lm,
								 title="Comparison of MLE to Linear Model - normal",
								 label="tab:mle:lm",  digits = 4, appendVal = FALSE
)



N <- 200
df <- data2
plot(df$x, df$y)

model1 <- lm(y ~ x, data=df)
stargazer(model1, title="A Model")  # I'd like to produce a similar table for the model below

ll <- function(params) {
	## Log likelihood for y ~ x + student's t errors
	params <- as.list(params)
	return(sum(dt((df$y - params$const - params$beta*df$x) / params$scale, df=params$degrees.freedom, log=TRUE) -
						 	log(params$scale)))
}

model2 <- optim(par=c(const=5, beta=1, scale=3, degrees.freedom=5), lower=c(-Inf, -Inf, 0.1, 0.1),
								fn=ll, method="L-BFGS-B", control=list(fnscale=-1), hessian=TRUE)
model2.coefs <- data.frame(coefficient=names(model2$par), value=as.numeric(model2$par),
													 se=as.numeric(sqrt(diag(solve(-model2$hessian)))))

#stargazer(model2.coefs, title="Another Model", summary=FALSE, type="text")  # Works, but how can I mimic what stargazer does with lm objects?



model2.lm = lm(y ~ ., data.frame(y=runif(5), beta=runif(5), scale=runif(5), degrees.freedom=runif(5)))
model2.lm$coefficients <- model2$par
model2.lm$fitted.values <- model2$par["const"] + model2$par["beta"]*df$x
model2.lm$residuals <- df$y - model2.lm$fitted.values


stargazer(model2.lm, model1, se = list(model2.coefs$se), summary=FALSE, type='text')



#ll.lm = lm(y ~ ., data.frame(y=runif(5), beta=runif(5), scale=runif(5), degrees.freedom=runif(5)))
ll.lm$coefficients <- linear.MLE$par
ll.lm$fitted.values <- linear.MLE$par["const"] + linear.MLE$par["beta"]*data2$x
ll.lm$residuals <- data2$y - linear.MLE$fitted.values
stargazer(ll.lm, se = list(linear.MLE.coefs$se), summary=FALSE, type='text')




# NO
stargazer(linear.MLE,
	coef = list(linear.MLE$par),
	se = list(),
	omit = c(sequence),
	covariate.labels = c("intercept", "beta1",  "beta2" ),
	dep.var.labels.include = FALSE,
	notes.append=FALSE, type = "text")   #, file=""

#https://stackoverflow.com/questions/66906859/how-to-use-optim-to-produce-coefficient-estimates-for-a-generalized-linear-mod


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

# Newton-Raphson solution + Fisher scoring
ols <- glm(y~x, data = data2, family = quasi )
summary(ols)
#library
#ols2 <- ols(y~x, data = data, family = quasi )


MLE <- optim(c(0,0), fn = neg.LL, control=(list(fnscale = -1)),
             hessian = T)

#https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/
#hist(Y$Count, breaks = 50,probability = T ,main = "Histogram of Count Variable")
#lines(density(Y$Count), col="red", lwd=2)

library(caTools)

