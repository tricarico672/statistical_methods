rm(list = ls()) #clean the environment
setwd("labs/Lectures")

library(MASS)

df_peng <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv")

str(df_peng) #in this version 

summary(df_peng) #use this to count NAs and get summary of dataset overall

df_peng[is.na(df_peng$bill_length_mm),] #check how the rows that have missing data look like

df_peng1 <- na.omit(df_peng)

df_peng1$species <- as.factor(df_peng1$species)
df_peng1$island <- as.factor(df_peng1$island)
df_peng1$sex <- as.factor(df_peng1$sex)

summary(df_peng1)

par(mfrow = c(2,2), mar = c(2,2,1,1))
boxplot(df_peng1$bill_length_mm, main = "bill length")
boxplot(df_peng1$bill_depth_mm, main = "bill depth")
boxplot(df_peng1$flipper_length_mm, main = "flipper length")
boxplot(df_peng1$body_mass_g, main = "body mass")

par(mfrow = c(1,1))
hist(df_peng1$body_mass_g)

boxplot(body_mass_g ~ sex, data = df_peng1)

par(mfrow = c(1,2))
hist(df_peng1$body_mass_g[df_peng1$sex == "male"],
     main = "Male",
     breaks = 5)
hist(df_peng1$body_mass_g[df_peng1$sex == "female"],
     main = "Female",
     breaks = 5)

t.test(body_mass_g ~ sex, data = df_peng1)

par(mfrow = c(1,2))
hist(df_peng1$body_mass_g[df_peng1$sex == "male"],
     main = "Male",
     breaks = 5,
     freq = F)
hist(df_peng1$body_mass_g[df_peng1$sex == "female"],
     main = "Female",
     breaks = 5,
     freq = F)

unique(df_peng1$island)
unique(df_peng1$species)

par(mfrow=c(1,1))
boxplot(body_mass_g ~ species, data = df_peng1)
boxplot(body_mass_g ~ species + sex, data = df_peng1)

t.test(body_mass_g ~ sex, data = df_peng1[df_peng1$species == "Adelie",])
t.test(body_mass_g ~ sex, data = df_peng1[df_peng1$species == "Gentoo",])
t.test(body_mass_g ~ sex, data = df_peng1[df_peng1$species == "Chinstrap",])

t.test(body_mass_g ~ species, data = df_peng1) #get an error since I am comparing the mean of more than 2 groups!!

# linear regression analysis ----------------------------------------------
plot(bill_length_mm ~ body_mass_g, data = df_peng1)
plot(df_peng1$body_mass_g, df_peng1$bill_length_mm) #same plot as above

my_lm1 <- lm(bill_length_mm ~ body_mass_g, data = df_peng1)
summary(my_lm1)

plot(bill_length_mm ~ body_mass_g, data = df_peng1)
abline(my_lm1, col = "red", lwd = 3, lty = "dashed")

my_lm2 <- lm(bill_length_mm ~ body_mass_g + sex, data = df_peng1)
summary(my_lm2)

my_lm3 <- lm(bill_length_mm ~ body_mass_g + species, data = df_peng1)
summary(my_lm3)

plot(bill_length_mm ~ body_mass_g, data = df_peng1,
     col = df_peng1$species)

dataA <- df_peng1[df_peng1$species == "Adelie",]
dataC <- df_peng1[df_peng1$species == "Chinstrap",]
dataG <- df_peng1[df_peng1$species == "Gentoo",]

data_A_y <- predict(my_lm3, dataA)
data_C_y <- predict(my_lm3, dataC)
data_G_y <- predict(my_lm3, dataG)

plot(bill_length_mm ~ body_mass_g, data = df_peng1,
     col = df_peng1$species)
lines(dataA$body_mass_g, data_A_y)
lines(dataC$body_mass_g, data_C_y, col = "red")
lines(dataG$body_mass_g, data_G_y, col = "green")

my_lm4 <- lm(bill_length_mm ~ body_mass_g + flipper_length_mm, data = df_peng1)
summary(my_lm4)

cor(df_peng1$bill_length_mm, df_peng1$body_mass_g)
cor(df_peng1$bill_length_mm, df_peng1$flipper_length_mm)
cor(df_peng1[,3:6]) #you can use this to check which variables are more correlated with bill_length


# prediction --------------------------------------------------------------

new_data_for_pred <- df_peng1[1:3,]
new_data_for_pred$species <- c("Gentoo", "Adelie", "Chinstrap")
new_data_for_pred$island <- rep("Torgersen", 3)
new_data_for_pred$bill_length_mm <- c(60, 50, 42)
new_data_for_pred$bill_depth_mm <- rep(12,3)
new_data_for_pred$flipper_length_mm <- rep(179, 3)
new_data_for_pred$body_mass_g <- c(4000, 3300, 4500)

# model residuals diagnostics ---------------------------------------------

predict(my_lm3, new_data_for_pred)
rbind(predict(my_lm3, new_data_for_pred), new_data_for_pred$bill_length_mm) #see predicted vs actual values

# Train and test the models on the same dataset
model_train_out4 <- predict(my_lm4, df_peng1)

# Plot the predicted values against the actual bill length
plot(model_train_out4, df_peng1$bill_length_mm)
abline(0,1, col = "red", lwd = 3)

# Compare the predictions of two models
original_par <- par(mar = c(5,4,4,2)+.1)
par(original_par)

model_train_out3 <- predict(my_lm3, df_peng1)
plot(model_train_out3, df_peng1$bill_length_mm,
     xlab = "actual values", 
     ylab = "fitted_values",
     main = "evaluating fitness to data")
abline(0,1, col = "red", lwd = 3)

# Side-by-side plots comparing model 3 and model 4 predictions
par(mfrow = c(1,2))
plot(model_train_out3, df_peng1$bill_length_mm, main = "Model 3")
abline(0,1, col = "red", lwd = 3)
plot(model_train_out4, df_peng1$bill_length_mm, main = "Model 4")
abline(0,1, col = "red", lwd = 3)

# Calculate and display the R-squared values for the models
cor(model_train_out3, df_peng1$bill_length_mm)^2
cor(model_train_out4, df_peng1$bill_length_mm)^2

par(mfrow = c(2,2), mar = c(2,2,2,2))
plot(my_lm3)

# Plot the regression residuals
par(mfrow = c(3,2), mar = c(2,2,1,1))
plot(my_lm3$residuals)
plot(my_lm4$residuals)
hist(my_lm3$residuals)
hist(my_lm4$residuals)
qqnorm(my_lm3$residuals)
qqline(my_lm3$residuals)
qqnorm(my_lm4$residuals)
qqline(my_lm4$residuals)

my_lm_all <- lm(bill_length_mm ~ ., data = df_peng1)
summary(my_lm_all)

confint(my_lm3)
confint(my_lm_all, level = 0.99)

# model selection ---------------------------------------------------------

# Selection criteria (Higher is better)
summary(my_lm_all)$r.squared
summary(my_lm3)$r.squared

#AIC: smaller is better
AIC(my_lm3)
AIC(my_lm_all)
AIC(my_lm_all, my_lm3)

# STEP
my_ml_step <- step(my_lm_all)
summary(my_ml_step)

