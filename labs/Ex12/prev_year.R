rm(list = ls())
setwd("labs/Ex12")


# ex1 ---------------------------------------------------------------------


df <- airquality

summary(df)

df <- df[complete.cases(df),]

str(df)

df$Month <- as.factor(df$Month)
df$Day <- as.factor((df$Day))

lm1 <- lm(Ozone ~ Wind + as.numeric(Month), data = df)
lm2 <- lm(Ozone ~ Wind + Month, data = df)

summary(lm1) #month not significant in this reg
summary(lm2) #month is significant when takena as a grouping factor

#from the output we see that month 7 and 8 have significantly higher means compared to month 5
#to make a comparison between all pairs of months we can do an ANOVA following by different t.tests (planned contrasts)

t.test(Ozone ~ Month, data = df[df$Month == 5 | df$Month == 6,])
t.test(Ozone ~ Month, data = df[df$Month == 5 | df$Month == 7,])
t.test(Ozone ~ Month, data = df[df$Month == 5 | df$Month == 8,])
t.test(Ozone ~ Month, data = df[df$Month == 5 | df$Month == 9,])

t.test(Ozone ~ Month, data = df[df$Month == 6 | df$Month == 7,])
t.test(Ozone ~ Month, data = df[df$Month == 6 | df$Month == 8,])
t.test(Ozone ~ Month, data = df[df$Month == 6 | df$Month == 9,])

t.test(Ozone ~ Month, data = df[df$Month == 7 | df$Month == 8,])
t.test(Ozone ~ Month, data = df[df$Month == 7 | df$Month == 9,])

t.test(Ozone ~ Month, data = df[df$Month == 8 | df$Month == 9,])

df$summer <- ifelse(df$Month == 7 | df$Month == 8, 1, 0)

lm3 <- lm(Ozone ~ Wind + summer, data = df)
lm4 <- lm(Ozone ~ Wind + Temp, data = df)

summary(lm3)
summary(lm4)
AIC(lm3, lm4) #based on AIC we see that lm4 should be preferred to lm3

par(mfrow = c(2,2))
plot(lm4)

#the diagnostics of the residual for the model show that there is some heteroscedasticity as there appears to be.a pattern in the residuals which changes based on the values of the independent variable
#also they do not appear to be very much normally distributed

plot(Ozone ~ Wind, data = df) #better approximated by a hyperbole
plot(Ozone ~ Temp, data = df) #better approximated by a quadratic parabola
plot(log(Ozone) ~ Temp, data = df)
plot(log(Ozone) ~ Wind, data = df)

lm5 <- lm(log(Ozone) ~ Wind + Temp, data = df)

AIC(lm4, lm5)
summary(lm5)


# ex2 ---------------------------------------------------------------------

#a linear model with response variable Y and two predictors, one quantitative and the other qualitative
x1 <- 1:12
x2 <- factor(c("1","2","3"))
x2 <- rep(x2,4)
y <- rnorm(12)

rand_df <- data.frame(y,x1,x2)

lm6 <- lm(y~x1*x2)
summary(lm6)

#...

# ex3 ---------------------------------------------------------------------

#from the way the plots look we can see how there are patterns in the residuals meaning that the model could incorporate some more systematic changes to be improved.
#the second plot plots the quantiles of the residuals along with the theoretical quantiles of a normal distribution. This also reveals that residuals do not appear to be normally distributed
#the last plot allows us to check whether the variance of errors is the same for each value of x (homoscedasticity assumption).
library(MASS)
library(datasets)
datasets::mtcars

reg2 <- lm(mpg~poly(wt,2), data = mtcars)
summary(reg2) #the summary shows significance for the second order term
#generally the t-tests in the summary test the individual significance of each predictor. The H0 is that the predictor is = 0 and the further away the estimate coefficient is the more 
#contradiction we gather against this hypothesis, and the lower the p-value which brings us to reject H0

#reg2 <- lm(mpg~wt+, data = mtcars)

#the AIC stands for akaike information criterion and it is used to compare how well two or more models fit the data. The lower the value of the AIC the better the model fit to the data. 
#Also, this criterion takes into account the complexity of the model and penalizes models that include many variable that do not improve the fit of the model to the data
AIC(reg2) #compare AIC of two models
BIC(reg2) #compare BIC of two models
summary(reg2) #compare adjusted r squared of two models



# ex4 ---------------------------------------------------------------------

df_peng <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv")

str(df_peng)

unique(df_peng$species)

for (i in names(df_peng)){
  if (is.numeric(df_peng[,i])){
    if (sum(is.na(df_peng[,i] != 0))){
      df_peng[,i][is.na(df_peng[,i])] <- ifelse(df_penguin$species == "Adelie", mean(df_peng[,i][df_peng$species=="Adelie"], na.rm = T),
                                                ifelse(df_penguin$species == "Gentoo", mean(df_peng[,i][df_peng$species=="Gentoo"], na.rm = T), df_penguin$species == "Chinstrap", mean(df_peng[,i][df_peng$species=="Chinstrap"], na.rm = T)))
    }
  }
}

lm_p <- lm(flipper_length_mm ~ body_mass_g, data = df_peng)
summary(lm_p)
#starting from the very first line we have an overview of the formula that has been used to fit the model to the data
#on the next line we have a 5-number summary of the distribution of the residuals, this helps us determine whether those are normally distributed (as per assumption).
#in the coefficients table we have the estimated coefficients for the intercept and the B1 for the body mass predictor. The standard error of the estimate is used to get the t-value (estimate / std.error) from which we can then compute the p-value of each individuald t-test for significance
#significance codes are reported below so that we can interpret the significance of each p-value and reject the H0 for the intercept and the estimated B1 accompanying the predictor at the 0.001 level of significance 
#the residual standard error measures the average value of the residuals in the model
#the multiple r-squared measures the fit of our model to the data and here we can say that our model explains 75.9% of the total variance in flipper length. The adjusted r squared can be used for comparison with models employing more predictors.
#in the last line we have the test of overall significance of the model which tells us whether we can say that our model is useful at all in describing the relationship between dependent and independent variables. In this case from the p-value of the test we know that this model is significant.

par(mfrow=c(1,1))
plot(flipper_length_mm ~ body_mass_g, data = df_peng)
abline(lm_p, col = "red", lwd = 3)

par(mfrow=c(2,2))
plot(lm_p)

#from here we learn that there are minimal patterns in how residuals change along with the fitted values, so this is good
#also the distribution of the residuals seems to be pretty close to a normal one (points are close to the diagonal line)
#we also see that the residuals' variance does not change much for different x values

# ex5 ---------------------------------------------------------------------

tooth <- read.csv("tooth.csv")

summary(tooth)
sd(tooth$areainfl)

boxplot(areainfl ~ smoke, data = tooth)

t.test(areainfl~smoke, data = tooth) #significant difference in means between conditions

boxplot(areainfl ~ il1b, data = tooth)

anov1 <- aov(areainfl ~ il1b, data = tooth)
summary(anov1) #the summary shows that there is no significant difference in the means of the three groups
#in the first column there are the degrees of freedom associated to the treatment (between groups) and the df associated to error variation (within groups)
#sum of squares shows the sum of squares of each source of variation (due to treatment / between groups and due to error / within groups)

hist(tooth$areainfl, freq = F)
#x <- sort(tooth$areainfl)
#or
x <- seq(min(tooth$areainfl)-5,max(tooth$areainfl)+5,0.1) #adds some more polish datapoints to draw the tails better
y <- dnorm(x, mean = mean(tooth$areainfl), sd = sd(tooth$areainfl))
lines(x,y)
#the parameters mean and sd have been chosen directly from the original data by computing them through the dedicated mean and sd functions
#the approximation is good enough although it does not include the small left skew that the distribution presents