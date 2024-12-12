setwd("~/Desktop/UniTrento/Courses/Statistical methods/labs/Ex11")
library(fpp3)

# exercise 1 --------------------------------------------------------------


df <- tsibble(year = 1981:2017,
       temp = c(9.301,8.788,9.173,8.824,8.799,8.985,9.141,9.345,9.076,9.378,
                9.336,8.974,9.008,9.175,9.484,9.168,9.326,9.66,9.406,9.332,
                9.542,9.695,9.649,9.451,9.829,9.662,9.876,9.581,9.657,9.828,
                9.65,9.635,9.753,9.714,9.962,10.16,10.049),
       index = year)

autoplot(df)

mean(df$temp)
var(df$temp)
sd(df$temp)
sd(df$temp)/sqrt(nrow(df))

qt(.99, 36)
pt(11.15, 36, lower.tail = FALSE)
t.test(df$temp-8.79, alternative = "greater")

# exercise 2 --------------------------------------------------------------

small_trains <- read.csv("~/Desktop/UniTrento/Courses/Statistical methods/labs/Ex11/small_trains.csv")

national <- small_trains$service == "National"
national_trains <- small_trains[national,]

international <- small_trains$service == "International"
international_trains <- small_trains[international,]

(var_equal <- var(small_trains$avg_delay_all_arriving))
(sd_equal <- sd(small_trains$avg_delay_all_arriving))

t.test(national_trains$avg_delay_all_arriving,
       international_trains$avg_delay_all_arriving)

hist(small_trains$avg_delay_all_arriving) #does not look like a normal distn

out <- 1.5*IQR(small_trains$avg_delay_all_arriving)

cond <- small_trains$avg_delay_all_arriving >= -out & small_trains$avg_delay_all_arriving <= out

trains_no_out <- small_trains[cond,]

int_no_out <- trains_no_out[trains_no_out$service == "International",]
nat_no_out <- trains_no_out[trains_no_out$service == "National",]

boxplot(trains_no_out$avg_delay_all_arriving)

hist(trains_no_out$avg_delay_all_arriving) #closer to normal distn

set.seed(124)
shapiro.test(sample(trains_no_out$avg_delay_all_arriving, 5000)) #looking at the p-value of the test we cannot say the distn is normal

t.test(int_no_out$avg_delay_all_arriving, nat_no_out$avg_delay_all_arriving) #without outliers the test performs better as it is more powerful and produces a larger t stats

# exercise 5 prev year ----------------------------------------------------

Mosquitos <- read.csv("Mosquitos.csv")

str(Mosquitos)

Mosquitos$trt.mosq <- as.factor(Mosquitos$trt.mosq)

boxplot(Mosquitos$y.mosq ~ Mosquitos$trt.mosq,
        main = "Mosquito landing rate by treatment",
        xlab = "Treatment type",
        ylab = "landing rate")

mod <- lm(y.mosq ~ trt.mosq, data = Mosquitos)
summary(mod) #the baseline is mosquito 1 so all other contrasts have to be interpreted accordingly
#the only treatment which appears to be significantly different from treatment 1 is treatment 4, and it seems to be having a lower mean compared to that of group 1

df <- Mosquitos

df$y.mosq <- round(df$y.mosq)

#df$trt.mosq <- as.numeric(df$trt.mosq)

df <- as.data.frame(table(df$y.mosq))

names(df) <- c("n_bites", "freq")

df$n_bites <- as.numeric(df$n_bites)

df$rel_freq <- df$freq/sum(df$freq)

####Alternative way of creating dataset using aggregate
aggregate(y.mosq~round(y.mosq), data = df, FUN = length)

#Poisson likelihood
'''
likel <- function(lambda){
  val <- 1
  for (bite in df$n_bites){
    res <- dpois(as.numeric(bite), lambda)
    val <- val * res
  }
  return(val)
}

lam <- seq(0,18,.01)
y <- likel(lam)
'''

log_likel_rel_freq <- function(lambda){
  bites <- as.numeric(df$n_bites)
  subjects <- df$rel_freq
  
  -sum(dpois(bites, lambda, log = TRUE) * subjects)
}

log_likel <- function(lambda){
  bites <- as.numeric(df$n_bites)
  subjects <- df$freq
  
  -sum(dpois(bites, lambda, log = TRUE) * subjects)
}

library(bbmle)
my_mle <- mle2(log_likel_rel_freq, start = list(lambda = 1))
my_mle_freq <- mle2(log_likel, start = list(lambda = 1))
#notice how the maximizing value of the likelihood does not change whether or not I use the relative frequency or the standard frequency
#this is a result that follows from the fact that no matter by which multiplicative value I scale my function, the maximum will still be attained at the same point

summary(my_mle)
summary(my_mle_freq)

confint(my_mle)
plot(profile(my_mle))

lambda <- coef(my_mle)

estimated_prob <- dpois(as.numeric(df$n_bites), lambda)

df$estimated_cases <- estimated_prob*df$freq

plot(df$n_bites, df$rel_freq)
lines(dpois(df$n_bites, lambda))

# exercise 6 prev year ----------------------------------------------------

brain <- read.csv("brain.txt", sep="")

str(brain)

brain$Gender <- as.factor(brain$Gender)

mod <- lm(Brain_weight ~ Head_size, data = brain)

plot(brain$Head_size, brain$Brain_weight)
abline(mod, col = "red")

summary(mod)
#the model shows that head size is a significant predictor fo brain weight.
#from the coefficient estimated moreover, we also know that it is a positive relationship

mod2 <- lm(Brain_weight ~ Head_size + Gender, data = brain)

summary(mod2)
AIC(mod,mod2) #given that lower AIC scores are better, the second model should be chosen over the first one

mod3 <- lm(Head_size ~ Gender + Age, data = brain)

summary(aov(mod3))
summary(mod3)
