setwd("labs/Ex10")

Geissler <- read.delim("~/Desktop/UniTrento/Courses/Statistical methods/labs/Ex10/Geissler.txt", header=FALSE, comment.char="#")

names(Geissler) <- c("number", "frequency")

likel <- function(p){
  val <- 1
  n = sum(Geissler$frequency)
  for (k in Geissler$number){
    res <- choose(n, k) * p^k * (1-p)^(n-k)
    val <- val * res
  }
  return(val)
}
x <- seq(0.01,.99,.0001)
y <- likel(x)
plot(x,y)

geisL <- function(p) {
  n_event <- 12
  val <- 1
  for (i in 1:13) {
    val <- val * dbinom(Geissler$number[i], n_event, p)^Geissler$frequency[i]
  }
  return(val)}

geisL(0.3)

geisLL <- function(p) {
  n_event <- 12
  val <- 0
  for (i in 1:13) {
    val <- val + Geissler$frequency[i] * dbinom(Geissler$number[i], n_event, p, log = TRUE)
  }
  
  return(val)}

geisLL(.418)

x <- seq(0.01,.99,.0001)
y <- geisLL(x)
plot(x,y, 
     type = "l",
     main = "log-likelihood",
     col = "red")

x[which.max(y)] #find value which maximizes log-likelihood, this is your MLE of p

#numerical methods
optimize(geisLL, c(.35,.5), maximum = TRUE)

#compute the negative log likelihood
minGeissLL <- function(p){
  return(-1*geisLL(p))
}

#MLE for p
p_hat <- bbmle::mle2(minuslogl = minGeissLL, start = list(p = .4))

confint(p_hat)
plot(profile(p_hat))

#plot BETA distribution
beta_dist <- dbeta(x, 34,31)
plot(beta_dist)

set.seed(124)
beta_x <- rbeta(6115, 34, 31)
random_binom <- rbinom(6115, 12, beta_x)

library(dplyr)
tab <- tibble(random_binom)

barplot(random_binom)
library(ggplot2)

ggplot(tab, aes(x = random_binom)) +
  geom_bar()

barplot(Geissler$frequency)

#we see that the method distribution generated randomly is close to the original one
#but it looks somehow mirrored about the mean


# exercise 4 --------------------------------------------------------------

#log likelihood of shifted gamma

shift_loglik <- function(x, alpha, lambda, sigma){
  val <- 0
  n = length(x)
  
  for (i in x){
    res <- (alpha - 1)*log(i-lambda) - (i - lambda)/sigma - alpha*log(sigma) - log(lgamma(alpha))
    val <- val + res
  }
  return(val)
}

neg_shift_loglik <- function(x, alpha, lambda, sigma){
  return(-1*shift_loglik(x, alpha, lambda, sigma))
}

# Define the log-likelihood function for the shifted Gamma distribution
shifted_gamma_loglikelihood <- function(params, data) {
  # Extract the parameters
  alpha <- params[1]
  lambda <- params[2]  
  sigma <- params[3]     
  
  # Ensure all data points satisfy x > c
  if (any(data <= c)) {
    return(-Inf) # Log-likelihood is undefined if x <= c for any data point
  }
  
  # Compute the log-likelihood
  log_likelihood <- sum(
    (alpha - 1) * log(data - c) - 
      (data - c) / lambda - 
      alpha * log(lambda) - 
      lgamma(alpha)
  )
  
  return(log_likelihood)
}

Gfr <- read.table("~/Desktop/UniTrento/Courses/Statistical methods/labs/Ex10/Gfr.txt", quote="\"", comment.char="")

x <- seq(.01,99,.001)
y <- shift_loglik(Gfr$V1, 6, 21, x)

plot(x,y)

x[which.max(y)] #proof that this function worked as well

loggamma_mod <- function(sc){
  res <- -sum(log(dgamma(Gfr$V1-21, scale = sc, shape = 6)))
  return(res)
}

loggamma_mod(5)
start <-list(sc=4)

res_mle <- mle2(loggamma_mod, start)
summary(res_mle)

sc <- coef(res_mle)[1]

hist(Gfr$V1, breaks = 25, xlim = c(20,110),
     col = "yellow", freq = FALSE, main = "gfr data")

asc <- (0:500)*.2
lambda <- 21
sh <- 6

lines(asc, dgamma(asc-lambda, scale = sc, shape = sh),
      col = "red",
      lwd = 2)

legend(60, 0.03, "MLE shifted gamma fit", col = "red", lwd = rep(2,2))

(p0 <- profile(res_mle))

confint(res_mle) # equal to confint(p0)
