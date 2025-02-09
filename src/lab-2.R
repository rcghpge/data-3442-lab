# Robert Cocker
# DATA 3442 - DS Stats 2
# Dr. Bian
# 2/8/2025
# Lab 2


# Load required libraries
library(readxl)
library(ggplot2)
library(GGally)   
library(dplyr)
library(corrplot)
library(car)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(psych)   
library(pastecs)
library(stats)
library(base)
library(openxlsx)

# Set file path for GPA input data
file_path <- "C:/Users/Robert/Desktop/GPA2.xlsx"

# Load data
gpa_data <- read_excel(file_path)

# View first few rows
head(gpa_data)

# Descriptive Statistics
desc_stats <- summary(gpa_data)
desc_psych <- describe(gpa_data)
desc_pastecs <- stat.desc(gpa_data)

# Compute correlations
cor_matrix <- cor(gpa_data[,-1])

# Save correlation plot to PDF
pdf("correlation_plot.pdf")
corrplot(cor_matrix, method = "color", tl.cex = 0.8)
dev.off()

# Save scatterplot matrix to PDF
pdf("scatterplot_matrix.pdf", width = 10, height = 10)

# Store the scatterplot matrix in a variable
scatterplot_matrix <- ggpairs(gpa_data, lower = list(continuous = wrap("smooth", color = "blue")))

# Print it inside the PDF device
print(scatterplot_matrix)
dev.off()


# Linear regression models
hs_model <- lm(gpa ~ hsm + hss + hse, data = gpa_data)
sat_model <- lm(gpa ~ satm + satv, data = gpa_data)
full_model <- lm(gpa ~ hsm + hss + hse + satm + satv, data = gpa_data)
final_model <- lm(gpa ~ hsm + hse, data = gpa_data)
model_test <- lm(gpa ~ hsm + hse + satv + satm, data = gpa_data) # Stepwise Test Model
  
# Compare models using ANOVA (General Linear Test)
anova_hs_full <- anova(hs_model, full_model)
anova_sat_full <- anova(sat_model, full_model)
anova_final <- anova(final_model, model_test)

# Save diagnostic plots to PDF
pdf("diagnostic_plots.pdf")
par(mfrow = c(2, 2))
plot(full_model)  # Residual plots for diagnostic checks
dev.off()

# Check Variance Inflation Factor (VIF) for multicollinearity
vif_values <- vif(full_model)

# Model selection using stepwise regression (AIC-based)
stepwise_model <- step(full_model, direction = "both")

# Save regression plots to PDF
pdf("regression_plots.pdf")

# GPA vs HSM Model
plot_hsm <- ggplot(gpa_data, aes(x = hsm, y = gpa)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  ggtitle("GPA vs High School Math (HSM)")
print(plot_hsm)

# GPA vs HSS Model
plot_hss <- ggplot(gpa_data, aes(x = hss, y = gpa)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  ggtitle("GPA vs High School Science (HSS)")
print(plot_hss)

# GPA vs HSE Model
plot_hse <- ggplot(gpa_data, aes(x = hse, y = gpa)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  ggtitle("GPA vs High School English (HSE)")
print(plot_hse)

# GPA vs SATM Model
plot_satm <- ggplot(gpa_data, aes(x = satm, y = gpa)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "red") + 
  ggtitle("GPA vs SAT Math (SATM)")
print(plot_satm)

# GPA vs SATV Model
plot_satv <- ggplot(gpa_data, aes(x = satv, y = gpa)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "red") + 
  ggtitle("GPA vs SAT Verbal (SATV)")
print(plot_satv)

# Close the PDF device
dev.off()


# Extract the coefficients from the full model summary
coefficients <- summary(full_model)$coefficients

# Parse columns for subjects and variables
write.xlsx(coefficients, file = "model_results.xlsx", colNames = TRUE, rowNames = TRUE)

# Save analysis results to a TXT file
sink("analysis_results.txt")
cat("Descriptive Statistics (summary function):\n")
print(desc_stats)

cat("\n\nDescriptive Statistics (psych package):\n")
print(desc_psych)

cat("\n\nDescriptive Statistics (pastecs package):\n")
print(desc_pastecs)

cat("\n\nHigh School Model Summary:\n")
print(summary(hs_model))

cat("\n\nSAT Model Summary:\n")
print(summary(sat_model))

cat("\n\nFull Model Summary:\n")
print(summary(full_model))

cat("\n\nANOVA: High School vs Full Model:\n")
print(anova_hs_full)

cat("\n\nANOVA: SAT vs Full Model:\n")
print(anova_sat_full)

cat("\n\nANOVA: Final Model vs SAT Stepwise Test Model:\n")
print(anova_stepwise)

cat("\n\nVariance Inflation Factor (VIF):\n")
print(vif_values)

cat("\n\nStepwise Regression Summary:\n")
print(summary(stepwise_model))

cat("\n\nFinal Regression Model Summary (further modeling can be done):")
print(summary(final_model))
sink()
graphics.off()

# References
# ChatGPT
# Lecture, lab, and course materials