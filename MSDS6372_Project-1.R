library(ggplot2)
hospital <- read.csv("HospitalDurations.csv")
hospital

# Checking for relationships as apart of the EDA 
numeric_vars <- hospital[, c("Lgth.of.Sty",
                             "Age",
                             "Inf.Risk",
                             "R.Cul.Rat",
                             "R.CX.ray.Rat",
                             "N.Beds",
                             "Avg.Pat",
                             "Avg.Nur",
                             "Pct.Ser.Fac")]

cor(numeric_vars)

# Plot 1: Highest correlation scatterplot: Inf risk to Length of Stay
ggplot(hospital, aes(x = Inf.Risk, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Infection Risk vs Length of Stay",
    x = "Infection Risk",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 2: Interaction term: region plot
ggplot(hospital,
       aes(x = Inf.Risk,
           y = Lgth.of.Sty,
           color = factor(Region))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Infection Risk vs Length of Stay by Region",
    x = "Infection Risk",
    y = "Length of Stay",
    color = "Region"
  ) +
  theme_minimal()
