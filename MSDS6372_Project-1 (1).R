# Dependencies
install.packages("caret")
install.packages("randomForest")
install.packages("car")
library(ggplot2)
library(caret)
library(randomForest)
library(car)

# Importing the data
hospital <-  read.csv("HospitalDurations.csv")

# Converting the categorical variables to factors
hospital$Region <- factor(hospital$Region)
hospital$Med.Sc.Aff <- factor(hospital$Med.Sc.Aff)

names(hospital)
summary(hospital)

#EDA

# Correlation matrix for numerical variables
numeric_vars <- hospital[, c(
  "Lgth.of.Sty",
  "Age",
  "Inf.Risk",
  "R.Cul.Rat",
  "R.CX.ray.Rat",
  "N.Beds",
  "Avg.Pat",
  "Avg.Nur",
  "Pct.Ser.Fac"
)]

cor(numeric_vars)

# Plot 1: Infection Risk vs Length of Stay
ggplot(hospital, aes(x = Inf.Risk, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Infection Risk vs Length of Stay",
    x = "Infection Risk",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 2: Infection Risk vs Length of Stay by Region
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

# Create patient groups for interaction exploration
hospital$PatientGroup <- ifelse(
  hospital$Avg.Pat >= median(hospital$Avg.Pat),
  "High Average Patients",
  "Low Average Patients"
)

# Plot 3: Infection Risk vs Length of Stay by Average Patients
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

# Plot 4: Average Patients vs Length of Stay
ggplot(hospital, aes(x = Avg.Pat, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Patients vs Length of Stay",
    x = "Average Patients",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 5: Average Nurses vs Length of Stay
ggplot(hospital, aes(x = Avg.Nur, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Nurses vs Length of Stay",
    x = "Average Nurses",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Plot 6: Number of Beds vs Length of Stay
ggplot(hospital, aes(x = N.Beds, y = Lgth.of.Sty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Number of Beds vs Length of Stay",
    x = "Number of Beds",
    y = "Length of Stay"
  ) +
  theme_minimal()

# Objective 1
# MLR Model

# Assessing the model
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
vif(full_model)

# The final model for Objective 1 
# N.Beds was removed because of severe multicollinearity with Avg.Pat (high correlation and VIF)

mlr_model <- lm(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac,
  data = hospital
)

summary(mlr_model)
confint(mlr_model)
vif(mlr_model)

# Generating the diagnostic plots
par(mfrow = c(2,2))
plot(mlr_model)

# Determining if a reduced model would be better 
reduced_model <- lm(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    Region +
    Avg.Pat +
    Avg.Nur,
  data = hospital
)

summary(reduced_model)
AIC(mlr_model, reduced_model)

# The reduced model had a slightly lower AIC, but the improvement was small.
# The final MLR model above this one was kept because it includes hospital factors that are relevant to Objective 1.

# Objective 2
# The Complex MLR Model
complex_mlr <- lm(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac +
    Inf.Risk:Avg.Pat,
  data = hospital
)

summary(complex_mlr)
confint(complex_mlr)

# The Predicted vs Actual Plot 
hospital$complex_pred <- predict(complex_mlr, newdata = hospital)

ggplot(hospital, aes(x = complex_pred, y = Lgth.of.Sty)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Length of Stay",
    x = "Predicted Length of Stay",
    y = "Actual Length of Stay"
  ) +
  theme_minimal()

# The Nonparametric Model - Random Forest
set.seed(1234)

ctrl <- trainControl(
  method = "cv",
  number = 10
)

rf_model <- train(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac,
  data = hospital,
  method = "rf",
  trControl = ctrl,
  tuneLength = 10
)

rf_model
rf_model$results
varImp(rf_model)
plot(varImp(rf_model))


# The model comparison 
set.seed(1234)

mlr_cv <- train(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac,
  data = hospital,
  method = "lm",
  trControl = ctrl
)

complex_cv <- train(
  Lgth.of.Sty ~ Age +
    Inf.Risk +
    R.Cul.Rat +
    R.CX.ray.Rat +
    Med.Sc.Aff +
    Region +
    Avg.Pat +
    Avg.Nur +
    Pct.Ser.Fac +
    Inf.Risk:Avg.Pat,
  data = hospital,
  method = "lm",
  trControl = ctrl
)

results <- resamples(
  list(
    MLR = mlr_cv,
    Complex_MLR = complex_cv,
    Random_Forest = rf_model
  )
)

summary(results)

