#https://stackoverflow.com/questions/68793266/how-to-specify-a-particular-optimization-method-in-glm
## constrained (probability, (0,1)) space
lfun <- function(beta) {
  -sum(dbinom(Y, size=1, 
              prob = plogis(as.matrix(X) %*% invlink(beta)),
              log=TRUE))
}
npar <- ncol(X)

#Using mle2:
  
  ## extra setup for using mle2 with a score function that takes a vector
  ## parameter
pnm <- paste0("beta", seq(npar))
parnames(lfun) <- pnm

m1 <- mle2(lfun, start = setNames(rep(0, npar-1), pnm[-1]), vecpar = TRUE)
invlink(coef(m1))
##  [1] 4.897617e-01 2.221793e-01 1.274830e-05 2.198596e-05 7.541545e-02
##  [6] 1.175966e-01 1.590407e-05 4.662563e-02 4.825376e-02 1.170077e-04

#or

opt <- optim(par = rep(0, npar-1), fn = lfun, method = "BFGS")
invlink(opt$par)


#=======================================================
#https://federicovegetti.github.io/teaching/ecpr_glm/lab/Day2_lab.html

p <- 0.6
gov.app <- rbinom(n = 10, size = 1, prob = p)
table(gov.app)

mean(gov.app)

#if likelihood  fn = prod{p^y*i-p)^(1-y)}
p.lik <- function(data, p){
  lik <- 1
  # We need to loop through all observations to multiply them
  for (i in 1:length(data)){ 
    y <- data[i]
    lik <- lik*(p^y*(1-p)^(1-y)) # This is the distribution function
  }
  return(lik) # Returns the final product
}

#options(scipen=999)  # This command kills the scientific notation
p.lik(gov.app, 0.5)

p.lik(gov.app, 0.6)

p.lik(gov.app, 0.4)

p.seq <- seq(0, 1, by = 0.001)
plot(x = p.seq, y = p.lik(gov.app, p.seq), type = "n", 
     xlab = "p", ylab = "likelihood")
lines(x = p.seq, y = p.lik(gov.app, p.seq))

#ML estimations are usually obtained iteratively: the algorithm
#tries some possible values of the parameter(s) to be estimated, 
#and it keeps updating them given the values of the first and 
#second derivative, until it reaches the maximum. R has a base 
#function called “optimize” that automatically returns the maximum 
#of a given function:
opt <- optimize(f = p.lik,         # The function to be optimized
                data = gov.app,    # The data
                interval = c(0,1), # The interval of possible values
                maximum = T)       # The default is minimize, we ask to maximize
opt

#log likelihood fn for binomial(N)
# L(N) = sum(y * log(p) + (1-y)*log(1-p))
p.log.lik <- function(data,p){
  l.lik <- 0
  for (i in 1:length(data)){ 
      y <- data[i]
      l.lik <- l.lik + y * log(p) + (1-y)*log(1-p)
  }
  return(l.lik) 
}
l.p = 0.5
l.N = 1000
l.data = rbinom(n=1:l.N, size=l.N, prob = p)

p.log.lik(l.data, 0.2)
p.log.lik(l.data, 0.3)
p.log.lik(l.data, 0.4)
p.log.lik(l.data, 0.5)
p.log.lik(l.data, 0.8)

hist(l.data)

p.seq <- seq(0, 1, by = 0.001)
plot(x = p.seq, y = p.log.lik(l.data, p.seq), type = "l", 
     xlab = "p", ylab = "likelihood")
lines(x = p.seq, y = p.lik(l.data, p.seq))

l.interval <- c(p.log.lik(l.data, 0.00001)-1000, p.log.lik(l.data, 1-0.00001)+1000)
l.opt <- optimize(f = p.log.lik, data = l.data, 
                  interval = l.interval, maximum = T)
warnings()
l.opt
