# Fly Climbing Assay Analyzer - R analysis template
# Install packages once if needed:
# install.packages(c("tidyverse"))

library(tidyverse)

# 1. Import exported CSV
# Replace with your file name if needed.
data <- read_csv("examples/example_fly_assay_results.csv")

# 2. Inspect data
print(head(data))
print(glimpse(data))

# 3. Convert grouping variables to factors
data <- data %>%
  mutate(
    Genotype = factor(Genotype),
    Sex = factor(Sex),
    Treatment = factor(Treatment),
    Trial = factor(Trial),
    Vial = factor(Vial)
  )

# 4. Summary statistics
summary_by_group <- data %>%
  group_by(Genotype, Sex, Treatment) %>%
  summarize(
    n = n(),
    mean_percent = mean(PercentClimbed, na.rm = TRUE),
    sd_percent = sd(PercentClimbed, na.rm = TRUE),
    se_percent = sd_percent / sqrt(n),
    .groups = "drop"
  )

print(summary_by_group)

# 5. Basic visualization
p <- ggplot(data, aes(x = Treatment, y = PercentClimbed)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.7) +
  ylab("Percent climbed") +
  xlab("Treatment") +
  theme_classic()

print(p)

ggsave("fly_climbing_percent_climbed_plot.png", p, width = 5, height = 4, dpi = 300)

# 6. Example statistical test for two groups
# Use only when assumptions and experimental design are appropriate.
print(t.test(PercentClimbed ~ Treatment, data = data))
