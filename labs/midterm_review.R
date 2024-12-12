library(MASS)
library(dplyr)

df <- survey
summary(df)
# Load dplyr

names(df)

is.numeric(df[,"Sex"]) #same as...
is.numeric(df$Sex)

# replacing NAs only for numeric cols with a single NA --------------------

for (colname in names(df)){
  n_nas <- sum(is.na(df[,colname]))
  
  if (n_nas == 1 & !is.factor(df[,colname])){
    df[which(is.na(df[,colname])),] <- mean(df[,colname], na.rm = T)
  }
}

#check
sum(is.na(df$Wr.Hnd))
sum(is.na(df$NW.Hnd))
sum(is.na(df$W.Hnd))


# plots -------------------------------------------------------------------

boxplot(df$Age)

hist(df$Age, freq = F, xlim = c(15,80))
x <- seq(min(df$Age), max(df$Age), .1)
y <- dexp(x, rate = 1/50)
lines(x,y, col = "red")
#trying to replace it with a gamma distribution
y <- dgamma(x, shape = length(df$Age), rate = 1/median(df$Age))
lines(x,y, col = "blue")

#replace it with a beta
y <- dbeta(x, length(df$Age), length(df$Age))#, ncp = mean(df$Age))
lines(x,y, col = "green")
