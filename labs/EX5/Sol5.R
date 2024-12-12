library(readr)
library(ggplot2)

setwd("/Users/anthony/Desktop/UniTrento/Courses/Statistical methods/labs/EX5")

# Exercise 5 --------------------------------------------------------------

muscle <- read_table("muscle.txt")
Metro_Interstate_Traffic_Volume <- read_csv("Metro_Interstate_Traffic_Volume.csv")
AsthmaLOS <- read_table("AsthmaLOS.txt")

#muscle is a .txt file and we can use read_table to read it into R
#metro is a comma-separated value file and we can use read_csv to import it into R
#asthma is a .txt file and we can use read_table to read it

head(muscle)
head(Metro_Interstate_Traffic_Volume)
head(AsthmaLOS)

str(muscle) #we should convert type col to factor
str(Metro_Interstate_Traffic_Volume) #holiday and weather should be factors
str(AsthmaLOS) #many cols should be converted into factors

# Exercise 6 --------------------------------------------------------------
#writes iris dataset to file "iris.csv"
write_csv(iris, "iris.csv")

#reads iris dataset from file
df <- read_csv("iris.csv")

#shows the structure of the dataframe
str(df)

#computes required stats only for numeric columns
sapply(df[,sapply(df, is.numeric)], FUN = mean, na.rm = T)
sapply(df[,sapply(df, is.numeric)], FUN = sd, na.rm = T)
sapply(df[,sapply(df, is.numeric)], FUN = IQR, na.rm = T)

#boxplot of Sepal.Lenght split by Species
boxplot(formula = Sepal.Length ~ as.factor(Species), data = df,
        main = "boxplot by species")

#scatterplot of Sepal.Width as a function of Sepal.Length
ggplot(df, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

#plots histogram of Sepal.Width with normal distribution
hist(df$Sepal.Width, freq = F)
x <- seq(min(df$Sepal.Width), max(df$Sepal.Width),.001)
f <- dnorm(x, mean(df$Sepal.Width), sd(df$Sepal.Width))
lines(x,f)

