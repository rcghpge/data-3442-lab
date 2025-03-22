# ============================================================
# Robert Cocker
# DATA 3442 - Lab 4: Two-Way ANOVA
# Dr. Bian
# 3/21/2025
# Lab 4
# ============================================================

# ============================================================
# Lab 4: Plant Species - Two-Way ANOVA in R
# ============================================================

# Load necessary libraries
library(tidyverse)
library(readxl)
library(car)
library(ggpubr)
library(emmeans)

# ============================================================
# Exercise 13.47 - Analysis of PLANTS1 Data
# ============================================================

plants1 <- read.csv("C:/Users/Robert/Desktop/lab-4/plants1.csv")
plants1$species <- factor(plants1$species, labels = c("Leucaena leucocephala", "Acacia saligna", "Prosopis juliflora", "Eucalyptus citriodora"))
plants1$water <- as.factor(plants1$water)

means_plants1 <- plants1 %>%
  group_by(species, water) %>%
  summarise(mean_pctnit = mean(pctnit, na.rm = TRUE), .groups = 'drop')

plot1 <- ggplot(means_plants1, aes(x = as.numeric(as.character(water)), y = mean_pctnit, color = species)) +
  geom_line() + geom_point() +
  labs(title = "Mean Percent Nitrogen by Water Level and Species", x = "Water Level (mm)", y = "Mean Percent Nitrogen") +
  theme_minimal()
ggsave("mean-pctnit-water-species.pdf", plot = plot1, width = 8, height = 6)

sds_plants1 <- plants1 %>%
  group_by(species, water) %>%
  summarise(sd_pctnit = sd(pctnit, na.rm = TRUE), .groups = 'drop')

anova_plants1 <- aov(pctnit ~ species * water, data = plants1)
anova_summary_plants1 <- summary(anova_plants1)
print(anova_summary_plants1)

# ============================================================
# Exercise 13.48 - Residuals for PLANTS1
# ============================================================

pdf("diagnostic-plots-plants1.pdf")
par(mfrow = c(2, 2))
plot(anova_plants1)
dev.off()

# ============================================================
# Exercise 13.49 - One-Way ANOVAs by Water Level
# ============================================================

anova_by_water <- plants1 %>%
  nest(data = -water) %>%
  mutate(anova = map(data, ~ aov(pctnit ~ species, data = .x)),
         summary = map(anova, summary),
         tukey = map(anova, TukeyHSD))

anova_summaries_by_water <- anova_by_water$summary
tukey_by_water <- anova_by_water$tukey

# ============================================================
# Exercise 13.50 - One-Way ANOVAs and Regression by Species
# ============================================================

analysis_by_species <- plants1 %>%
  nest(data = -species) %>%
  mutate(anova = map(data, ~ aov(pctnit ~ water, data = .x)),
         anova_summary = map(anova, summary),
         regression = map(data, ~ lm(pctnit ~ as.numeric(as.character(water)), data = .x)),
         regression_summary = map(regression, summary))

anova_summaries_by_species <- analysis_by_species$anova_summary
regression_summaries_by_species <- analysis_by_species$regression_summary

# ============================================================
# Exercise 13.51 - Analysis of PLANTS2 Data
# ============================================================

plants2 <- read_excel("C:/Users/Robert/Desktop/lab-4/plants2.xls")
plants2$species <- factor(plants2$species, labels = c("Leucaena leucocephala", "Acacia saligna", "Prosopis juliflora", "Eucalyptus citriodora"))
plants2$water <- as.factor(plants2$water)

# ============================================================
# Exercise 13.52 - ANOVA for Fresh and Dry Biomass
# ============================================================

anova_fbiomass <- aov(fbiomass ~ species * water, data = plants2)
anova_summary_fbiomass <- summary(anova_fbiomass)
print(anova_summary_fbiomass)

anova_dbiomass <- aov(dbiomass ~ species * water, data = plants2)
anova_summary_dbiomass <- summary(anova_dbiomass)
print(anova_summary_dbiomass)

# ============================================================
# Exercise 13.53 - Residual Diagnostics for PLANTS2
# ============================================================

pdf("diagnostic-plots-plants2.pdf")
par(mfrow = c(2, 2))
plot(anova_fbiomass, main = "Fresh Biomass")
plot(anova_dbiomass, main = "Dry Biomass")
dev.off()

# ============================================================
# Exercise 13.54 - One-Way ANOVAs and Regression (PLANTS2)
# ============================================================

analysis_fbiomass_by_species <- plants2 %>%
  nest(data = -species) %>%
  mutate(anova = map(data, ~ aov(fbiomass ~ water, data = .x)),
         anova_summary = map(anova, summary),
         regression = map(data, ~ lm(fbiomass ~ as.numeric(as.character(water)), data = .x)),
         regression_summary = map(regression, summary))

anova_fbiomass_summaries_by_species <- analysis_fbiomass_by_species$anova_summary
regression_fbiomass_summaries_by_species <- analysis_fbiomass_by_species$regression_summary

# ============================================================
# Save Summaries to File and Print to Console
# ============================================================

sink("lab4-anova-summaries.txt")
cat("=== PLANTS1 Two-Way ANOVA ===\n")
print(anova_summary_plants1)

cat("\n=== PLANTS1 One-Way ANOVAs by Water Level ===\n")
for (i in seq_along(anova_summaries_by_water)) {
  cat("\n-- Water Level", anova_by_water$water[i], "--\n")
  print(anova_summaries_by_water[[i]])
}

cat("\n=== PLANTS1 One-Way ANOVAs by Species ===\n")
for (i in seq_along(anova_summaries_by_species)) {
  cat("\n-- Species", analysis_by_species$species[i], "--\n")
  print(anova_summaries_by_species[[i]])
  print(regression_summaries_by_species[[i]])
}

cat("\n=== PLANTS2 Two-Way ANOVA for Fresh Biomass ===\n")
print(anova_summary_fbiomass)

cat("\n=== PLANTS2 Two-Way ANOVA for Dry Biomass ===\n")
print(anova_summary_dbiomass)

cat("\n=== PLANTS2 One-Way ANOVAs for Fresh Biomass by Species ===\n")
for (i in seq_along(anova_fbiomass_summaries_by_species)) {
  cat("\n-- Species", analysis_fbiomass_by_species$species[i], "--\n")
  print(anova_fbiomass_summaries_by_species[[i]])
  print(regression_fbiomass_summaries_by_species[[i]])
}
sink()

# Note: See files for plots and summary analyses

# References
# - Lecture, lab, course materials
# - Google
# - ChatGPT
# - R Documentation:
# - https://www.rdocumentation.org/
# - https://www.r-project.org/other-docs.html

