# Mock Exam ---------------------------------------------------------------
setwd("~/Desktop/UniTrento/Courses/Statistical methods/labs/Mock exam")

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
library(readr)
library(tidyverse)
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
#1) load data
data_mock_exam <- read_delim("data_mock_exam.tsv", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)

str(data_mock_exam)

#2) define dataset for analysis
mydf <- data_mock_exam[which(data_mock_exam$dataset == 6),]
#alternatively using dplyr
mydf <- data_mock_exam %>%
  filter(dataset == 6)

#3) replace missing values with mean of variables
mydf <- mydf %>%
  mutate(x = replace_na(x, mean(x, na.rm = T))) %>%
  mutate(y = replace_na(y, mean(y, na.rm = T)))

sum(is.na(mydf)) #check there are no more missing values


mydf %>%
  select(x,y) %>%
  summarise(mean_x = mean(x),
            mean_y = mean(y),
            sd_x = sd(x),
            sd_y = sd(y),
            first_quartile = quantile(x,.25)) # too long...

#4) compute required stats
library(psych)
mydf %>%
  select(x,y) %>%
  psych::describe(quant = c(0,.25,.5,.75,1)) %>%
  select(mean, sd, Q0, Q0.25, Q0.5, Q0.75, Q1)

#5) 
mu <- mean(mydf$x)
sd <- sd(mydf$x)

set.seed(124)
sample <- rnorm(200,mu,sd)

desc_samp <- describe(sample, quant = c(0,.25,.5,.75,1)) %>%
  select(-vars, -n, -trimmed, -mad, -min, -max, -range, -skew, -kurtosis, -se)

desc_x <- describe(mydf$x, quant = c(0,.25,.5,.75,1)) %>%
  select(-vars, -n, -trimmed, -mad, -min, -max, -range, -skew, -kurtosis, -se)

hist(mydf$x, freq = F, 
     ylim = c(0,.035),
     xlim = c(0, 101))
lines(density(sample)) #density is an estimate, so if you want to plot the actual curve you use the density computed by the normal dist.n

xvals <- seq(min(mydf$x), max(mydf$x), .01) #define x values for which densities are to be computed
yvals <- dnorm(sort(sample), mean(mydf$x), sd(mydf$x)) #compute densities for those values

hist(mydf$x, freq = F, 
     ylim = c(0,.035),
     xlim = c(0, 101))
lines(sort(sample), yvals) #use the densities produced above to show how the norm. dist.n compares to the dist.n of x

desc_samp-desc_x #by looking at the differences we can see that despite differences in mean, sd, and median are negligible, 
#x seems to exhibit bimodality and therefore cannot be normally distributed!