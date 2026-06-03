library(ggplot2)
hospital <- read.csv("HospitalDurations.csv")
names(hospital)
summary(hospital)

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

# Plot 3: Interaction term: average patients plot
hospital$PatientGroup <- ifelse(
  hospital$Avg.Pat >= median(hospital$Avg.Pat),
  "High Average Patients",
  "Low Average Patients"
)

ggplot(hospital,
       aes(x = Inf.Risk,
           y = Lgth.of.Sty,
           color = PatientGroup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Infection Risk vs Length of Stay by Average Patients",
    x = "Infection Risk",
    y = "Length of Stay",
    color = "Average Patients"
  ) +
  theme_minimal()

# Plot 4: Average patients to Length of Stay
ggplot(hospital, aes(x = Avg.Pat, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Patients vs Length of Stay",
    x = "Average Patients",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 5: Average nurses to Length of Stay
ggplot(hospital, aes(x = Avg.Nur, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Nurses vs Length of Stay",
    x = "Average Nurses",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 6: Number of beds to Length of Stay
ggplot(hospital, aes(x = N.Beds, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Number of Beds vs Length of Stay",
    x = "Number of Beds",
    y = "Length of Stay"
  ) +
  theme_minimal()


# Fitting the first regression model. All variables are included.
full_model <- lm(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    N.Beds +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac,
  data = hospital
)

summary(full_model)
confint(full_model)
library(car)

vif(full_model)
par(mfrow = c(2,2))
plot(full_model)


# Objective 2 

# MLR model 



