# Load Required Packages
# Install necessary packages
# install.packages("beepr")
# install.packages("ROSE")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("dplyr")

library(beepr)
library(ROSE)
library(caret)
library(ggplot2)
library(dplyr)

# Perform Data Cleaning and Preprocessing
summary(data)

attach(data)

data <- data %>%
  filter(BMI <= 41, BMI >= 14, HeightInMeters <= 2.0, HeightInMeters >= 1.41,
         MentalHealthDays < 10, PhysicalHealthDays <= 8, SleepHours < 11, SleepHours > 3)

outliers <- boxplot.stats(WeightInKilograms)$out
data <- data %>%
  filter(!(WeightInKilograms %in% outliers))

dim(data)
summary(data)

# Set seed for reproducibility
set.seed(4322)

# We will use a sample of the data. In this case, we will only use half of the data
num_row = nrow(data)
new_data = data[sample(num_row, num_row*0.5),]

# Logistic Regression
rf_data = new_data

# Performing as.factor() on other categorical variables
check_and_convert_categorical <- function(test_data) {
  for (col_name in names(test_data)) {
    if (!is.factor(test_data[[col_name]]) && (is.character(test_data[[col_name]]) || length(unique(test_data[[col_name]])) <= 10)) {
      test_data[[col_name]] <- as.numeric(as.factor(test_data[[col_name]]))
    }
  }
  return(test_data)
}
rf_data <- check_and_convert_categorical(new_data)

rf_data$AgeCategory = as.factor(rf_data$AgeCategory)


# Split data
n = nrow(rf_data)
p = ncol(rf_data)

set.seed(4322)

# Logistic Regression with Cross-Validation
logistic_error_table <- numeric(10)

for (i in 1:10) {
  # Split data
  train_indices <- sample(nrow(rf_data), 0.8 * nrow(rf_data))
  rf_train <- rf_data[train_indices, ]
  rf_test <- rf_data[-train_indices, ]

  # Convert HadHeartAttack to binary categorical variable (0 and 1)
  rf_train$HadHeartAttack <- as.factor(rf_train$HadHeartAttack)
  rf_test$HadHeartAttack <- as.factor(rf_test$HadHeartAttack)

  # Ensure the levels are 0 and 1
  levels(rf_train$HadHeartAttack) <- c("0", "1")
  levels(rf_test$HadHeartAttack) <- c("0", "1")

  # Logistic Regression Model
  logistic_model <- glm(HadHeartAttack ~ .,
                        data = rf_train,
                        family = binomial)

  # Predictions on Test Data
  logistic_predictions <- predict(logistic_model, newdata = rf_test, type = "response")

  # Convert probabilities to binary predictions (0 or 1)
  logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)

  # Evaluate Model Performance
  logistic_accuracy <- mean(logistic_predictions == rf_test$HadHeartAttack)
  logistic_error_table[i] <- logistic_accuracy

  # Print summary of the logistic regression model
  print(summary(logistic_model))

  # Get AIC value
  aic <- AIC(logistic_model)
  cat("AIC:", aic, "\n")
}

# Print the mean of test accuracy from 10 iterations
cat("Mean Test Accuracy (Logistic Regression):", mean(logistic_error_table), "\n")