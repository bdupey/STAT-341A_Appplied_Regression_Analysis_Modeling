library(readr)
library(ggplot2)
# Reading in dataframe
data <- read.csv('C:\\Users\\mason\\OneDrive\\Documents\\GitHub\\STAT-341A_Appplied_Regression_Analysis_Modeling\\chp_2_dataset.csv')
# Building linear model
model<-lm(Strength~Age, data=data)
# Summarizing model
summary(model)
beta_1=-37.15
# Generating ANOVA table
print(anova(model))

# Calculating sum of squares total
options(digits = 10)
sum_y_squared <- 0.0
decimal_places <- 5
# Loop through each observation and add the squared value to the running total
for (i in 1:length(data$Strength)) {
  squared_value <- (data$Strength[i])^2
  sum_y_squared <- sum_y_squared + squared_value
}
sum_y=sum(data$Strength)
print(sum_y_squared)
n=nrow(data)
SS_T=sum_y_squared-((sum_y)^2/n)
print(SS_T)

# Calculating the sum of cross-products(covariance between x and y)
mean_x <- mean(data$Age)
mean_y <- mean(data$Strength)

# Calculate Sxy
Sxy <- sum((data$Age - mean_x) * (data$Strength - mean_y))
print(Sxy)

# Calculating SSres
SSres=SS_T-beta_1*Sxy
print(SSres)

# Calculating sigma_sq
sigma_sq=SSres/(n-2)
print(sigma_sq)

