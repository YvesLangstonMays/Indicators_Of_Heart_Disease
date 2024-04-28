library(ggplot2)
library(dplyr)
library(caret)
library(car)
library(leaps)
library(MASS)

data <- read.csv("C:/Users/mvsta/OneDrive/Documents/groupproject/heart_2022_no_nans.csv")


# Cleaning data
sum(is.na(data))

# Removing outliers
data <- data %>% 
  filter(BMI <= 41)

data <- data %>% 
  filter(BMI >= 14)

data <- data %>% 
  filter(HeightInMeters <= 2.0)

data <- data %>% 
  filter(HeightInMeters >= 1.41)

# We will use a sample of the data. In this case, we will only use half of the data
num_row = nrow(data)
set.seed(4322)
new_data = data[sample(num_row, num_row*0.5),]

# logistic Regression
log_data = new_data

# Performing as.factor() on other categorical variables
check_and_convert_categorical <- function(test_data) {
  for (col_name in names(test_data)) {
    if (!is.factor(test_data[[col_name]]) && (is.character(test_data[[col_name]]) || is.integer(test_data[[col_name]]))) {
      unique_vals <- unique(test_data[[col_name]])
      if (length(unique_vals) <= 10) {  # Adjust the threshold as needed
        test_data[[col_name]] <- as.numeric(as.factor(test_data[[col_name]]))
      }
    }
  }
  return(test_data)
}
log_data = check_and_convert_categorical(log_data)

# The as.factor() function wasn't apply to the AgeCategory column since it has 13
# levels of unique values, but we will force perform as.factor() on it anyway
log_data$AgeCategory = as.factor(log_data$AgeCategory)

# We will drop the State columns since it will have more than 53 levels and the 
# Random Forest won't be able to perform.
log_data = log_data[, -1]
str(log_data)


# Split data
n = nrow(log_data)
p = ncol(log_data)

set.seed(4322)
train = sample(n, 0.8*n) # Split train/test as 8:2


log_train = log_data[train, ]
log_test = log_data[-train, ]

log_model <- glm(HadHeartAttack ~., data = log_train)

step.model <- stepAIC(log_model, direction = "backward", trace=FALSE)
summary(step.model)

#Getting accuracies
convert_prob_to_class <- function(probs, threshold = 0.5) {
  return(ifelse(probs > threshold, 1, 0))
}
train_predictions = predict(log_model, newdata = log_train, type = "response")
train_predictions_class = convert_prob_to_class(train_predictions)
train_accuracy = mean(train_predictions_class == train_labels)
cat("Training Accuracy:", train_accuracy, "\n")

test_predictions = predict(log_model, newdata = log_test, type = "response")
test_predictions_class = convert_prob_to_class(test_predictions)
test_accuracy = mean(test_predictions_class == test_labels)
cat("Test Accuracy:", test_accuracy, "\n")

summary(log_model)

backward_log_model = step(log_model, direction="backward")
summary(backward_log_model)


convert_prob_to_class <- function(probs, threshold = 0.5) {
  return(ifelse(probs > threshold, 1, 0))
}
# Perform the train/test split and apply to the random forest model 10 times
test_error_table = vector("numeric", length = 10)
for (i in 1:10)
{
  set.seed(i)
  
  sample_index = sample(n, 0.8  * n)
  log_train = log_data[sample_index, ]
  log_test = log_data[-sample_index, ]
  
  log_model <- glm(HadHeartAttack ~ ., data = log_train)
  

  test_labels = log_test$HadHeartAttack
  test_predictions = predict(log_model, newdata = log_test, type = "response")
  test_predictions_class = convert_prob_to_class(test_predictions)
  test_accuracy = mean(test_predictions_class == test_labels)
  test_error_table[i] = test_accuracy
}

mean(test_error_table)


vif(log_model)
#WE There suggests some multicollinearity with HeighInMeters, WeightInKilograms, and BMI

#other significant values values that are not multicollinear: Sex + PhysicalHealthDays + LastCheckupTime + PhysicalActivities + 
#HadAngina + HadStroke + HadCOPD + HadKidneyDisease + HadDiabetes + DeafOrHardOfHearing + DifficultyWalking + SmokerStatus + ChestScan

new_log_model = glm(HadHeartAttack ~ Sex + PhysicalHealthDays + LastCheckupTime + PhysicalActivities + 
                    HadAngina + HadStroke + HadCOPD + HadKidneyDisease + HadDiabetes + DeafOrHardOfHearing + 
                    DifficultyWalking + SmokerStatus + ChestScan, data = log_train)


new_test_error_table = vector("numeric", length = 10)
for (i in 1:10)
{
  set.seed(i)
  sample_index = sample(n, 0.8  * n)
  rf_train = log_data[sample_index, ]
  rf_test = log_data[-sample_index, ]
  
  new_log_model = glm(HadHeartAttack ~ Sex + PhysicalHealthDays + LastCheckupTime + PhysicalActivities + 
                        HadAngina + HadStroke + HadCOPD + HadKidneyDisease + HadDiabetes + DeafOrHardOfHearing + 
                        DifficultyWalking + SmokerStatus + ChestScan, data = log_train)
  
  new_test_labels = log_test$HadHeartAttack
  new_test_predictions = predict(log_model, newdata = log_test, type = "response")
  new_test_predictions_class = convert_prob_to_class(new_test_predictions)
  new_test_accuracy = mean(new_test_predictions_class == new_test_labels)
  new_test_error_table[i] = new_test_accuracy
}

# print the mean of 10 test accuracy rate
mean(new_test_error_table)