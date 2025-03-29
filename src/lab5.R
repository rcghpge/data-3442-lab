# ============================================================
# Robert Cocker
# DATA 3442
# Dr. Bian
# 3/28/2025
# Lab 5 - High School Education Study: Logistic Regression
# ============================================================

# ============================================================
# Load Libraries
# ============================================================
library(tidyverse)

# ============================================================
# Load Data
# ============================================================
# Adjust file path for data as needed here
data <- read.csv("C:/Users/rcock/Desktop/lab5/EX14-047GPAHI.csv")
head(data)

# ============================================================
# Define Function for Model Fitting and Output
# ============================================================
fit_and_output_model <- function(formula, data, model_name) {
  sink(paste0(model_name, ".txt"), split = TRUE)
  cat("\n============================================================\n")
  cat(paste("Model:", model_name, "\n"))
  cat("============================================================\n\n")
  model <- glm(formula, data = data, family = binomial)
  print(summary(model))
  sink()
}

# ============================================================
# Model 1: Predicting HIGPA using High School Grades
# ============================================================
fit_and_output_model(HIGPA ~ HSM + HSS + HSE, data, "model1")

# ============================================================
# Model 2: Predicting HIGPA using SAT Scores
# ============================================================
fit_and_output_model(HIGPA ~ SATM + SATCR, data, "model2")

# ============================================================
# Model 3: Predicting HIGPA using Both High School Grades and SAT Scores
# ============================================================
fit_and_output_model(HIGPA ~ HSM + HSS + HSE + SATM + SATCR, data, "model3")

# ============================================================
# Model 4a: Assessing the Effect of Gender on HIGPA (sex only)
# ============================================================
fit_and_output_model(HIGPA ~ sex, data, "model4a")

# ============================================================
# Model 4b: Assessing the Effect of Gender on HIGPA (sex, SATM, SATCR)
# ============================================================
fit_and_output_model(HIGPA ~ sex + SATM + SATCR, data, "model4b")

# ============================================================
# Instructions to Run the Models and Generate Outputs
# ============================================================

# ➤ Prerequisites:
#    - R and RStudio installed
#    - The following libraries installed:
#         install.packages("tidyverse")
#
# ➤ Files required in the same folder:
#    - lab5-script.R         (this script)
#    - EX14-047GPAHI.csv     (dataset)

# ➤ How to run this script:
#    1. Open this script in RStudio.
#    2. Set working directory to this folder:
#         setwd("C:/Users/rcock/Desktop/lab5/")
#    3. Run the script line-by-line or use:
#         source("lab5-script.R")
#
# Note: Be sure that your file paths are correct e.g. data and R scripts
# are referenced correctly in the code.

# ➤ What this script does:
#    - Loads the GPAHI dataset
#    - Defines a reusable model function
#    - Fits five logistic regression models:
#         Model 1  -> HSM, HSS, HSE
#         Model 2  -> SATM, SATCR
#         Model 3  -> All academic variables
#         Model 4a -> sex only
#         Model 4b -> sex, SATM, SATCR
#    - Saves each model's output to:
#         model1.txt, model2.txt, ..., model4b.txt


# ============================================================
# References
# ============================================================
# 1. Google
#    Google is a multinational technology company specializing in internet-related services and products.
#    [https://www.google.com](https://www.google.com)
#
# 2. ChatGPT
#    ChatGPT is an AI language model developed by OpenAI, designed to assist with a variety of tasks through natural language processing.
#    [https://openai.com/chatgpt](https://openai.com/chatgpt)
# 3. **University of Texas at Arlington Course Materials**
#    Course materials provided by the University of Texas at Arlington, including lectures, labs, and other resources, have been instrumental in this work.
#    [https://www.uta.edu/academics/courses-and-schedules](https://www.uta.edu/academics/courses-and-schedules)

# ============================================================
# End of Lab 5
# ============================================================
