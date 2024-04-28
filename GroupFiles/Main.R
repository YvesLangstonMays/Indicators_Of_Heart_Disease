# install.packages("beepr")
# install.packages("ROSE")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("randomForest")

library(beepr)
library(ROSE)
library(caret)
library(ggplot2)
library(dplyr)
library(randomForest)
library(tree)

data <- read.csv("./Indicators_Of_Heart_Disease/2022/heart_2022_no_nans.csv")

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

# There is a significant difference in the number of Yes/No in our response variables 
################################################################################################
# Set seed for reproducibility 
set.seed(4322)

# We will use a sample of the data. In this case, we will only use half of the data
num_row = nrow(data)
new_data = data[sample(num_row, num_row*0.5),]

# Random Forest
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

# The as.factor() function wasn't apply to the AgaCategory column since it has 13
# levels of unique values, but we will force perform as.factor() on it anyway
rf_data$AgeCategory = as.factor(rf_data$AgeCategory)

# Mapping states to regions
northeast <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", 
               "Connecticut", "New York", "New Jersey", "Pennsylvania")
midwest <- c("Ohio", "Michigan", "Indiana", "Illinois", "Wisconsin", "Minnesota", 
             "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas")
south <- c("Delaware", "Maryland", "District of Columbia", "Virginia", "West Virginia", 
           "Kentucky", "North Carolina", "South Carolina", "Tennessee", "Georgia", 
           "Florida", "Alabama", "Mississippi", "Arkansas", "Louisiana", "Texas", "Oklahoma")
west <- c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", 
          "Utah", "Nevada", "California", "Oregon", "Washington", "Alaska", "Hawaii")
territories <- c("Puerto Rico", "Guam", "Virgin Islands")

data$Region <- with(data, factor(
  ifelse(State %in% northeast, "Northeast",
         ifelse(State %in% midwest, "Midwest",
                ifelse(State %in% south, "South",
                       ifelse(State %in% west, "West", 
                              ifelse(State %in% territories, "Territories", "Other")
                       )
                )
         )
  )
))

data <- data[, !(names(data) %in% "State")]

if(any(is.na(data$Region))) {
  warning("Some states were not categorized into any region.")
}


# Split data
n = nrow(rf_data)
p = ncol(rf_data)

set.seed(4322)
train = sample(n, 0.8*n) # Split train/test as 8:2


rf_train = rf_data[train, ]
rf_test = rf_data[-train, ]

rf_model <- randomForest(HadHeartAttack ~., 
                         data = rf_train,
                         ntree = 1000, mtry =  sqrt(p),
                         importance = TRUE)

train_predictions = predict(rf_model, newdata = rf_train)
train_accuracy = mean(train_predictions == rf_train$HadHeartAttack)
cat("Training Accuracy:", train_accuracy, "\n")

test_predictions = predict(rf_model, newdata = rf_test)
test_accuracy = mean(test_predictions == rf_test$HadHeartAttack)
cat("Training Accuracy:", test_accuracy, "\n")

plot(rf_model)
# We can see that error rate does not improved (or rather stay the same as we increase
# the number of tree). So in this case, we will let ntree = 500 when we perform the model
# ten times in order to reduce the time.

# Perform the train/test split and apply to the random forest model 10 times
test_error_table <- numeric(10)

for (i in 1:10)
{
  set.seed(4322)
  sample_index = sample(n, 0.8  * n)
  rf_train = rf_data[train, ]
  rf_test = rf_data[-train, ]
  
  rf_model = randomForest(HadHeartAttack ~., 
                          rf_train, 
                          ntree = 500, mtry = sqrt(p),
                          importance = TRUE)
  
  rf_predict = predict(rf_model, newdata = rf_test)
  test_accuracy = mean(rf_predict == rf_test$HadHeartAttack)
  test_error_table[i] = test_accuracy
}

# Print the mean of the test accuracy
mean(test_error_table)

# Analyzing the importance of each predictors on the response
# We can use the lasted trained model 
varImpPlot(rf_model)

# Train the Random Forest with the predictor that are more important to the response variable.
# So from the importance plot, we can pick out some variable to improve our model, such as:
# HadAngina, HeightInMeters, WeightInKilograms, AgeCategory, BMI, Sex, SleepHours.

# We will perform 1000 trees to see if there any improvement if we increase number of trees
new_rf_model = randomForest(HadHeartAttack ~ HadAngina + HeightInMeters + WeightInKilograms + AgeCategory + BMI + Sex + SleepHours,
                            rf_train,
                            ntree = 1000, mtry = sqrt(p), 
                            importance = TRUE)

plot(new_rf_model)
# As see from the plot, it seems like increase number of tree does not hepl much with reducing the test error rate. So 
# again, we will just perform 500 tree so the sake of time save.

# We will train the new model 10 times as well
new_test_error_table <- numeric(10)
for (i in 1:10)
{
  set.seed(4322)
  sample_index = sample(n, 0.8  * n)
  rf_train = rf_data[train, ]
  rf_test = rf_data[-train, ]
  
  new_rf_model = randomForest(HadHeartAttack ~ HadAngina + HeightInMeters + 
                                                WeightInKilograms + AgeCategory + 
                                                BMI + Sex + SleepHours,
                              rf_train,
                              ntree = 500, mtry = sqrt(p), 
                              importance = TRUE)
  
  new_rf_predict = predict(new_rf_model, newdata = rf_test)
  new_test_accuracy = mean(new_rf_predict == rf_test$HadHeartAttack)
  new_test_error_table[i] = new_test_accuracy
}

# print the mean of 10 test accuracy rate
mean(new_test_error_table)

