# Robert Cocker
# DATA 3442
# Dr. Bian
# 2/21/2025
# Lab 3

# Golf Ball Distance Analysis in R

# Load necessary libraries
library(tidyverse)
library(car)
library(stats)
library(multcomp)
library(ggpubr)

# Load the dataset
golf_data <- read.csv("golf.csv")

# Display first few rows
head(golf_data)

# Descriptive Statistics
desc_stats <- golf_data %>% group_by(brand) %>% summarise(
  count = n(),
  mean = mean(dist),
  sd = sd(dist),
  min = min(dist),
  q25 = quantile(dist, 0.25),
  median = median(dist),
  q75 = quantile(dist, 0.75),
  max = max(dist)
)
cat("\n--- Descriptive Statistics ---\n")
print(desc_stats)

# Perform One-way ANOVA
anova_result <- aov(dist ~ brand, data = golf_data)
summary(anova_result)
cat("\n--- One-way ANOVA Results ---\n")
print(summary(anova_result))


# Extract F-statistic and p-value from ANOVA
anova_summary <- summary(anova_result)
f_statistic <- anova_summary[[1]]$`F value`[1]
p_value <- anova_summary[[1]]$`Pr(>F)`[1]

# Print F-statistic and p-value
cat("\n--- F-Test Statistic ---\n")
cat("F-statistic:", f_statistic, "\n")
cat("P-value:", p_value, "\n")

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
cat("\n========== Tukey's HSD Test Results ==========\n")
print(tukey_result)

# Convert Tukey results to a dataframe
tukey_df <- as.data.frame(tukey_result$brand)
cat("\n--- Tukey's HSD Test Pairwise Comparisons ---\n")
print(tukey_df)

# Create Q-Q plots for normality check
cat("\n========== Q-Q Plots ==========\n\n")
cat("See Lab-3 Anaylsis Report PDF")
ggqqplot(golf_data, x = "dist", facet.by = "brand", title = "Q-Q Plots for Golf Ball Brands")
