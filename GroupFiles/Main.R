install.packages("caret")
install.packages("keras")
install.packages("remotes")
install.packages("tensorflow")

library(ggplot2)
library(dplyr)
library(caret)
library(neuralnet)
library(randomForest)
library(keras)
library(tensorflow)
library(magrittr)

data <- read.csv("./GroupFiles/Indicators_Of_Heart_Disease/2022/heart_2022_no_nans.csv")

detach(data)
attach(data)

# Cleaning data
sum(is.na(data))

# There are no na values

# Function that will change the variables Yes and No to numerical 1 or 0
# convert_to_binary_auto <- function(data) {
#   cols <- sapply(data, function(x) all(c("Yes", "No") %in% unique(x)))
#   cols_to_convert <- names(cols[cols == TRUE])
#   
#   for (col in cols_to_convert) {
#     data[[col]] <- ifelse(data[[col]] == "Yes", 1, 0)
#   }
#   
#   return(data)
# }
# Transforming the data using the function
# data <- convert_to_binary_auto(data)


# Checking for outliers
ggplot(data, aes(y = BMI)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
  coord_flip()

ggplot(data, aes(y = HeightInMeters)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
  coord_flip()

# Removing outliers
data <- data %>% 
  filter(BMI <= 41)

data <- data %>% 
  filter(BMI >= 14)

data <- data %>% 
  filter(HeightInMeters <= 2.0)

data <- data %>% 
  filter(HeightInMeters >= 1.41)

# Checking Data
ggplot(data, aes(y = BMI)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE, ) +
  coord_flip() 

# Checking Data
ggplot(data, aes(y = HeightInMeters)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
  coord_flip()


# Neural Network

nn_data = data

# Function that will change the variables Yes and No to numerical 1 or 0
convert_to_binary_auto <- function(data) {
  cols <- sapply(data, function(x) all(c("Yes", "No") %in% unique(x)))
  cols_to_convert <- names(cols[cols == TRUE])

  for (col in cols_to_convert) {
    data[[col]] <- ifelse(data[[col]] == "Yes", 1, 0)
  }

  return(data)
}
# Transforming the data using the function
nn_data <- convert_to_binary_auto(nn_data)

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
nn_data = check_and_convert_categorical(nn_data)

# The as.factor() function wasn't apply to the AgaCategory column since it has 13
# levels of unique values, but we will force perform as.factor() on it anyway
nn_data$AgeCategory = as.numeric(as.factor(nn_data$AgeCategory))
summary(nn_data)

# Find highly correlated numeric variables
numeric_data <- nn_data[, sapply(nn_data, is.numeric)]
corr_matrix <- cor(numeric_data)
highly_correlated_num <- findCorrelation(corr_matrix, cutoff = 0.8)
drop_col = colnames(numeric_data)[24]
nn_data = nn_data[, !colnames(nn_data) %in% drop_col]

summary(nn_data)
# So we dropped the BMI column since it highly correlated to WeightInKilograms column

# Split data
n = nrow(nn_data)

set.seed(100)
train = sample(n, 0.8*n)

nn_train = nn_data[train, -1]
nn_test = nn_data[-train, -1]

str(nn_train)

nn_X_train = nn_train[, -which(names(nn_train) == "HadHeartAttack")]
nn_y_train = nn_train$HadHeartAttack

nn_X_test = nn_test[, -which(names(nn_test) == "HadHeartAttack")]
nn_y_test = nn_test$HadHeartAttack
  
# Perform neural network on training data
library(keras)

# Define your neural network model
model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = 'relu', input_shape = 38) %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')


