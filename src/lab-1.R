# Robert Cocker
# DATA-3442-001
# Dr. Bian
# Nawaraj Adhikari
# Lab 1
# 1/24/2025

# Public University Tuition Lab
# Question 10.38 Public University Tuition: 2008 vs 2014
cat("Lab 1: See comments in script for further analyses and inference\n")
cat("Question 10.38:\n")
cat("Data:\n")
# Load necessary libraries
library(ggplot2)

# Load data (replace 'file path ~/' with your file's path)
data <- read.csv("~/uta/data-3442-lab/data/EX10-41TUIT.csv")

# View the data structure
str(data)
head(data)

# Part a.)
# Scatter plot for 2008 vs 2014 tuition

ggplot(data, aes(x = Y2008, y = Y2014)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Public University Tuition: Y2008 vs Y2014",
       x = "Tuition in Y2008",
       y = "Tuition in Y2014") +
  theme_minimal()

# Inference: There tuition rates are fairly spread out. There are a few outliers 
# observed at tuition rates of about ~$3,000 USD and approximately ~$17,500 USD. There 
# is enough information and data to confirm that a linear relationship does exist. 

cat("--------------------------------------------------------------------------")
# Part b.)
# Fit the linear regression model
model <- lm(Y2014 ~ Y2008, data = data)

# View the regression summary
summary(model)

# Extract coefficients
coefficients(model)

# Plot the data points
plot(data$Y2008, data$Y2014, main = "Scatter Plot with Regression Line",
     xlab = "Y2008", ylab = "Y2014")

# Add the regression line
abline(model, col = "red")

# Add a legend for the regression line
legend("topleft", legend = "Regression Line", col = "red", lty = 1)

# Print the equation of the line
sprintf("Regression Line: Y2014 = %.2f + %.2f * Y2008", coef(model)[1], coef(model)[2])


# Part c.)
# Calculate residuals
data$residuals <- residuals(model)

# Plot residuals vs. 2008 tuition
ggplot(data, aes(x = Y2008, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Y2008 Tuition",
       x = "Tuition in Y2008",
       y = "Residuals") +
  theme_minimal()

# Inference: The data points look fairly evenly spead out.

# Part d.)
# Histogram of residuals
ggplot(data, aes(x = residuals)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Q-Q plot
qqnorm(data$residuals)
qqline(data$residuals, col = "red")

# Shapiro-Wilk test for normality:
shapiro.test(data$residuals)

# Inference: The plots make it difficult to make an inference that the
# residuals are normally distributed. The Shapiro-Wilk test confirms however, since the 
# p-value 0.003016 is less than 0.05, you would reject the null hypothesis of the 
# test or assumption, which asks if the residuals are normally distributed. Further testing and 
# analysis can be done to further confirm. Currently, with the given plots, summaries, and information, 
# this suggests that the errors (residuals) do not follow a normal distribution..

# Part e.)
# Remove California schools
data_no_CA <- data[-c(1, 5, 12, 19, 28), ]

# Refit the regression model
model_no_CA <- lm(Y2014 ~ Y2008, data = data_no_CA)

# View the new regression summary
summary(model_no_CA)

# Inference: the range of residuals eg Min, 1Q, Median, 3Q, Max, indicate variability, 
# with some large residuals (max residual = 3128.9). This suggests potential issues with 
# normality or outliers affecting the regression model. The data points do not follow a 
# normal distribution, as evidenced by the Shapiro-Wilk test (p-value = 0.003016). This 
# could impact the assumptions of the linear regression model, potentially affecting the 
# validity of hypothesis tests.

# Part f.)
# Inference: Removing California schools reduced the model's adjusted R-squared 0.5652 vs. 0.6441 
# and increased the residual standard error 1838 vs 1712. While the slope and intercept changed 
# slightly, the exclusion of California schools resulted in a model with less predictive power and 
# greater variability in residuals (erors).

cat("--------------------------------------------------------------------------\n")

# Question 10.39 More on Public University Tuition
cat("Question 10.39:")
# Part a.)
# Null and Alternative Hypothesis:
# Null Hypothesis (H0): There is no linear relationship between Y2008 and Y2014 tuition amounts 
# (B1=0).

# Alternative Hypothesis (Ha): There is a linear relationship between Y2008 and Y2014 tuition amounts 
# (B1/=0).

# Part b.)
# T-test and p-value:
# View the summary of the model
summary(model)

# Extract t-statistic and p-value
t_stat <- summary(model)$coefficients["Y2008", "t value"]
p_value <- summary(model)$coefficients["Y2008", "Pr(>|t|)"]

# Print results
cat("T-statistic:", t_stat, "\nP-value:", p_value)
cat("\n")
cat("\n")

# Inference: Given the very low p-value 1.176e-08 - ~0.00000001176 and large t-statistic ~7.68, 
# we reject the null hypothesis and conclude that Y2008 has a significant effect on Y2014.

# Part c.)
# 95% Confidence Interval for Slope
# Extract slope and standard error
slope <- summary(model)$coefficients["Y2008", "Estimate"]
se <- summary(model)$coefficients["Y2008", "Std. Error"]

# Compute confidence interval
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df = df.residual(model))
ci_lower <- slope - t_critical * se
ci_upper <- slope + t_critical * se

cat("95% Confidence Interval for the slope:", ci_lower, "to", ci_upper)
cat("\n")

# Inference: The 95% confidence interval indicates that the slope is 
# statistically significant, positive, and provides the range for the 
# true effect of Y2008 on Y2014. There is a significant linear relationship 
# between the two variables.

# Part d.)
# Variability
# Extract R-squared
r_squared <- summary(model)$r.squared
cat(sprintf("R-squared: %.2f%%\n", r_squared * 100))

# Inference: The R-squared value demonstrates that the regression model is 
# useful and meaningful for understanding the relationship between Y2008 and 
# Y2014 with an R^2 of ~66%. The unexplained variability of 34% suggests the 
# need for further investigation or additional predictors to refine the model 
# and provide a more comprehensive explanation.

# Part e.)

# Inference: The slope intercept (B0) represents the predicted Y2014 tuition when the 
# Y2008 tuition is $0. This scenario is unrealistic in the context of this problem, 
# as no university has $0 tuition. Thus, B0 is not meaningful for inference here.

cat("--------------------------------------------------------------------------\n")

# Question 10.40 Even More on Public University Tuition
cat("Question 10.40:\n")
# Part a.)
# Define given 2008 tuition for Skinflint U
tuition_2008_skinflint <- 8800

# Predict Y2014 tuition
tuition_2014_skinflint <- predict(model, newdata = data.frame(Y2008 = tuition_2008_skinflint))
cat(sprintf("Predicted Y2014 Tuition for Skinflint U: $%.2f", tuition_2014_skinflint))
cat("\n")

# Inference: Predicted tuition at Skinflint U in Y2014: ~ $12,700 USD

# Part b.)
# Define given 2008 tuition
tuition_2008_iou <- 15700

# Predict 2014 tuition
tuition_2014_iou <- predict(model, newdata = data.frame(Y2008 = tuition_2008_iou))
cat(sprintf("Predicted Y2014 Tuition for I.O.U.: $%.2f", tuition_2014_iou))
cat("\n")

# Inference: The fitted regression equation is appropriate for Skinflint U because its 
# 2008 tuition is within the range of the data. The prediction involves interpolation.
# The equation is less appropriate for I.O.U. Its 2008 tuition is outside of the input 
# data range, making the prediction based on extrapolation

# Part c.)
# Inference: The linear regression model is appropriate for predicting Y2014 tuition for universities 
# with Y2008 tuition values within the range in the input data. However, for extreme values like I.O.U.'s 
# Y2008 tuition of $15,700, the prediction involves extrapolation, which reduces reliability for inference. 
# The model's accuracy is contingent on meeting linear regression assumptions eg linearity, normality of 
# residuals, and homoscedasticity. While the model can provide reasonable predictions for most universities, 
# caution should be taken when applying it to outliers or values outside of the data's range.

cat("--------------------------------------------------------------------------\n")
# Question 10.41 Predicting Public University Tuition 2000 vs 2014:
cat("Question 10.41:")

# Part a.)
# Fit model using Y2000 tuition
model_2000 <- lm(Y2014 ~ Y2000, data = data)

# View the regression summary
summary(model_2000)

# Extract the least-squares regression equation
intercept <- coef(model_2000)[1]
slope <- coef(model_2000)[2]
cat("Least-Squares Regression Line: Y2014 =", intercept, "+", slope, "* Y2000")
cat("\n")

# Part b.)

# Inference: Model inference of linearity, homoscedasticity, and normality of residuals are generally conclusive 
# based on the plots and information given regression tests and analyses. The moderate R^2 value suggests that additional 
# factors beyond Y2000 tuition may contribute to the variability in Y2014 tuition. The model can be used for prediction, 
# but its limitations should be noted.

# Part c.)

# Conclusion: The Y2008 tuition model is the better choice due to its higher explanatory power R^2 relevance of data - is closer 
# in time to Y2014, and likely has better predictive performance. While Y2000 model is valid, it is less reliable for 
# making inferences about Y2014 tuition due to the longer time gap and lower predictive strength.


# References:
# xAI - Grok 2
# OpenAI - ChatGPT
# Google - Google.com
