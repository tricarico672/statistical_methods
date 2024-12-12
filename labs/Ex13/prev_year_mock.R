rm(list = ls())
setwd("labs/Ex13")

library(emdbook)
library(bbmle)


# ex3 ---------------------------------------------------------------------

old <- c(45,38,52,48,25,39,51,46,55,46)
young <- c(34,22,15,27,37,41,24,19,26,36)

mold <- mean(old)
myoung <- mean(young)

pooled_sd <- (sd(old) + sd(young))/2

diff_mean <- mold-myoung

(t <- diff_mean/pooled_sd)

t.test(old, young)

qt(.975, 18) #18 degrees of freedom

#yes there is a significant difference between the two groups

#it would be a type I error since we reject at any significance level, we cannot commit a type II error (accept H0 when false)



# ex4 ---------------------------------------------------------------------

df <- FirDBHFec_sum

summary(df)

df_cleaned <- df[complete.cases(df),]

hist(df_cleaned$DBH)

log_lik <- function(alpha, sigma){
  res <- -sum(dgamma(df_cleaned$DBH, shape = alpha, scale = sigma, log = TRUE))
  return(res)
}

alpha <- seq(4,17,.1)
sigma <- seq(4,17,.1)

log_lik(alpha, sigma)

my_mle <- mle2(log_lik, start = list(alpha = 1, sigma = 1))
coef(my_mle)["alpha"]

hist(df_cleaned$DBH, freq = F)
x <- seq(min(df_cleaned$DBH)-5, max(df_cleaned$DBH)+5, .1)
y <- dgamma(x, shape = coef(my_mle)["alpha"], scale = coef(my_mle)["sigma"])
lines(x,y)

plot(profile(my_mle))


# ex5 ---------------------------------------------------------------------

mpg <- read.csv("mpg.csv")

str(mpg)

mpg$origin <- as.factor(mpg$origin)

mod1 <- lm(mpg~weight, data = mpg)
summary(mod1) #add comments...

plot(mpg~weight, data = mpg)
abline(mod1, col = "red", lwd = 3)

par(mfrow=c(2,2))
plot(mod1) #comment res plot...

mod2 <- lm(mpg~poly(weight,2), data = mpg)
summary(mod2) #better, explain why based on adj. r squared
AIC(mod1,mod2) #better, explain why based on AIC

coef(mod2)["(Intercept)"]

plot(mpg~weight, data = mpg)
abline(mod1, col = "red", lwd = 3)
#lines(mpg$weight,coef(mod2)["(Intercept)"] + coef(mod2)["poly(weight, 2)1"] * mpg$weight + mpg$weight^2 * coef(mod2)["poly(weight, 2)2"], col = "blue", lwd = 3)
#

# Fit the second-order polynomial regression
mod2 <- lm(mpg ~ poly(weight, 2), data = mpg)

# Plot the original data
plot(mpg ~ weight, data = mpg, main = "Polynomial Regression Overlay")

# Generate predicted values for a smooth curve
weight_seq <- seq(min(mpg$weight), max(mpg$weight), length.out = 100)
predicted_mpg <- predict(mod2, newdata = data.frame(weight = weight_seq))

# Overlay the polynomial regression line
lines(weight_seq, predicted_mpg, col = "blue", lwd = 3)
abline(mod1, col = "red", lwd = 3)

mod3 <- lm(mpg~weight+origin, data = mpg)
summary(mod3)
