# Mock Exam ---------------------------------------------------------------

#compute P(X<10) when X is exponential with lambda = .1155
pexp(10, rate = .1155)

#when variance of normal dist.n is higher the curve is flatter and has larger tails (more probability to be farther away from the mean)
set.seed(124)
plot(density(rnorm(1000, mean = 1, sd = 1)))
lines(density(rnorm(1000, mean = 1, sd = sqrt(2))), col = "red", lty = 1)

pgamma(2,3,3)

pgamma(6,100,20) - pgamma(4,100,20)
pnorm(6,5,sqrt(1/4)) - pnorm(4,5,sqrt(1/4))

1-pbinom(49, 100, .3678)
1-pnorm(49.5, 36.78, sqrt(23.25))

#demonstration that linear combinations of normal RVs are still normal
set.seed(124)
plot(density(rnorm(1000,2,4)),
     main = "RV~N remains N after L.C.")
abline(v = 2, lty = 2)
lines(density(rnorm(1000,2,4)*2+2), col = "red", lty=2)
abline(v = 6, col = "red", lty = 3)
# Other exercises ---------------------------------------------------------

dbinom(2, 12, 8/30)
pbinom(6,20,.3)
1-dbinom(0,6,2/5)

dbinom(12, 12, 7/10)
dbinom(0, 12, 7/10)

dbinom(3, 8, .48 ) * dbinom(1, 8, .06) * dbinom(4, 8, .46)

1 - ppois(4, 2.25)

1 - ppois(3, 3)

# SECOND PART -------------------------------------------------------------

# Problem 5 ---------------------------------------------------------------
#1) use the Poisson approximation due to rarity of event
n <- 100
p <- .03

dpois(2,n*p)
dbinom(2,n,p)

#2) use the normal approximation
n <- 197
p <- .3

#I want P(X<=50), I need to use continuity correction
pnorm(50+.5,n*p,sqrt(n*p*(1-p))) #0.09059912
pbinom(50,n,p) #0.08911225

# Problem 6 ---------------------------------------------------------------


