# ## STA302H1 - Project Code

# Required libraries
install.packages("car"); library(car)
library(MASS)

# Set seed and read the data file
set.seed(302)
data <- read.csv("~/STA302/Project/diabetes.csv", sep=",")

# Split data into 70% training and 30% testing sets
sample <- sample(c(TRUE, FALSE), size=nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample,]
test <- data[!sample,]

# ## Pre-processing and exploratory analysis on training set

# Remove missing values from training data (values of 0 for these variables are not possible)
train <- train[train$Glucose != 0 & train$BloodPressure != 0 & train$SkinThickness != 0 
               & train$Insulin != 0 & train$BMI != 0,] # There are no missing values for age

# Plot histograms of all numerical predictors and response variable
par(mfrow=c(2,3))
hist(train$Glucose, xlab="Blood glucose levels (mg/dL)", main="")
hist(train$SkinThickness, xlab="Skin thickness (mm)", main="")
hist(train$Insulin, xlab="Blood insulin levels (μU/mL)", main="")
hist(train$BMI, xlab="Body mass index", main="")
hist(train$Age, xlab="Age", main="")
hist(train$BloodPressure, xlab="Blood pressure (mm Hg)", main="")
par(mfrow=c(1,1)) # Resets plot parameters for later plots

# Plot scatter plots of all numerical predictors against response variable
plot(train$Glucose, train$BloodPressure, xlab="Blood glucose levels (mg/dL)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs blood glucose levels")
plot(train$SkinThickness, train$BloodPressure, xlab="Skin thickness (mm)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs skin thickness")
plot(train$Insulin, train$BloodPressure, xlab="Blood insulin levels (μU/mL)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs blood insulin levels")
plot(train$BMI, train$BloodPressure, xlab="Body mass index", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs body mass index")
plot(train$Age, train$BloodPressure, xlab="Age", ylab="Blood pressure (mm Hg)", 
     main="Blood pressure vs age")

# Summaries of all numerical predictors and response variable
summary(train$Glucose)
summary(train$SkinThickness)
summary(train$Insulin)
summary(train$BMI)
summary(train$Age)
summary(train$BloodPressure)

# Pair plot numerical predictors and response variable
pairs(data.frame(train$Glucose, train$SkinThickness, train$Insulin, train$BMI, 
                 train$Age, train$BloodPressure), 
      main="Pair plot of numerical predictors and blood pressure")

# Try log transform since blood pressure vs insulin doesn't look linear
train$LogInsulin <- log(train$Insulin)
plot(train$LogInsulin, train$BloodPressure, xlab="Log of blood insulin levels (unitless)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs log of blood insulin levels")

# ## Full model

# Fitting the full model
model_1 <- lm(BloodPressure ~ Glucose + SkinThickness + LogInsulin + BMI + Age, data=train)
summary(model_1); AIC(model_1); BIC(model_1) # Model metrics for later comparison

# Checking assumptions for the full model with residuals plots and detecting any outliers
plot(train$Glucose, model_1$residuals, xlab="Blood glucose levels (mg/dL)", 
     ylab="Residuals", main="Model 1 residuals vs blood glucose levels") # Glucose vs residuals plot
plot(train$SkinThickness, model_1$residuals, xlab="Skin thickness (mm)", 
     ylab="Residuals", main="Model 1 residuals vs skin thickness") # Skin thickness vs residuals plot
plot(train$LogInsulin, model_1$residuals, xlab="Log of blood insulin levels (unitless)", 
     ylab="Residuals", main="Model 1 residuals vs log of blood insulin levels") # Log of insulin vs residuals plot
plot(train$BMI, model_1$residuals, xlab="Body mass index", 
     ylab="Residuals", main="Model 1 residuals vs body mass index") # BMI vs residuals plot
plot(train$Age, model_1$residuals, xlab="Age", 
     ylab="Residuals", main="Model 1 residuals vs age") # Age vs residuals plot
plot(model_1$fitted.values, model_1$residuals, xlab="Fitted values", 
     ylab="Residuals", main="Model 1 residuals vs fitted values") # Fitted values vs residuals plot
qqnorm(model_1$residuals); qqline(model_1$residuals) # QQ-plot

# Removing outliers using cutoff of [-2,2] for standardized residuals
standard_res <- rstandard(model_1)
combined_data <- cbind(train, standard_res)
combined_data <- subset(combined_data, standard_res <= 2 & standard_res >= -2)
train <- combined_data

# Fitting the revised full model with combined_data instead of train
model_1 <- lm(BloodPressure ~ Glucose + SkinThickness + LogInsulin + BMI + Age, data=train)
summary(model_1); AIC(model_1); BIC(model_1) # Model metrics for later comparison

# Detecting any influential observations with Cook's distance plot
plot(model_1, which=5)

# Detecting any multicollinearity with VIF
vif(model_1)

# ## Model selection

# Brainstorming and fitting other models
model_2 <- lm(BloodPressure ~ Glucose + LogInsulin + BMI + Age, data=train)
summary(model_2)
anova(model_2, model_1) # ANOVA F-test comparing current model and full model

model_3 <- lm(BloodPressure ~ Glucose + BMI + Age, data=train)
summary(model_3)
anova(model_3, model_1)

model_4 <- lm(BloodPressure ~ LogInsulin + BMI + Age, data=train)
summary(model_4)
anova(model_4, model_1)

model_5 <- lm(BloodPressure ~ BMI + Age, data=train)
summary(model_5)
anova(model_5, model_1)

# Comparing performance of all models with AIC and BIC
AIC(model_1); AIC(model_2); AIC(model_3); AIC(model_4); AIC(model_5)
BIC(model_1); BIC(model_2); BIC(model_3); BIC(model_4); BIC(model_5)

# Checking assumptions for the selected model with residuals plots
plot(train$BMI, model_5$residuals, xlab="Body mass index", 
     ylab="Residuals", main="Model 5 residuals vs body mass index") # BMI vs residuals plot
plot(train$Age, model_5$residuals, xlab="Age", 
     ylab="Residuals", main="Model 5 residuals vs age") # Age vs residuals plot
plot(model_5$fitted.values, model_5$residuals, xlab="Fitted values", 
     ylab="Residuals", main="Model 5 residuals vs fitted values") # Fitted values vs residuals plot
qqnorm(model_5$residuals); qqline(model_5$residuals) # QQ-plot


# ## Pre-processing and exploratory analysis on testing set

# Remove missing values from testing data (values of 0 for these variables are not possible)
test <- test[test$Glucose != 0 & test$BloodPressure != 0 & test$SkinThickness != 0 
               & test$Insulin != 0 & test$BMI != 0,] # There are no missing values for age

# Summaries of all numerical predictors and response variable
summary(test$Glucose)
summary(test$SkinThickness)
summary(test$Insulin)
summary(test$BMI)
summary(test$Age)
summary(test$BloodPressure)

# Plot scatter plots of all numerical predictors against response variable
plot(test$Glucose, test$BloodPressure, xlab="Blood glucose levels (mg/dL)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs blood glucose levels")
plot(test$SkinThickness, test$BloodPressure, xlab="Skin thickness (mm)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs skin thickness")
plot(test$Insulin, test$BloodPressure, xlab="Blood insulin levels (μU/mL)", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs blood insulin levels")
plot(test$BMI, test$BloodPressure, xlab="Body mass index", 
     ylab="Blood pressure (mm Hg)", main="Blood pressure vs body mass index")
plot(test$Age, test$BloodPressure, xlab="Age", ylab="Blood pressure (mm Hg)", 
     main="Blood pressure vs age")

# Pair plot numerical predictors and response variable
pairs(data.frame(train$Glucose, train$SkinThickness, train$Insulin, train$BMI, 
                 train$Age, train$BloodPressure), 
      main="Pair plot of numerical predictors and blood pressure")

# Transformations on testing set
test$LogInsulin <- log(test$Insulin)

# ## Model validation

validation_model <- lm(BloodPressure ~ BMI + Age, data=test)
summary(validation_model); AIC(validation_model); BIC(validation_model) # Compare these to selected model

# Compare confidence intervals of predictors and intercept
confint(model_5, level=0.95)
confint(validation_model, level=0.95)

# Detecting any influential observations with Cook's distance plot
plot(model_5, which=5)
plot(validation_model, which=5)

# Detecting any multicollinearity with VIF
vif(model_5)
vif(validation_model)
