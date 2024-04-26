install.packages("caret")

library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

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

# There is a significant difference in the number of Yes/No in our response variables
plot(data$HadHeartAttack)

# We will use a sample of the data. In this case, we will only use half of the data
num_row = nrow(data)
set.seed(4322)
new_data = data[sample(num_row, num_row*0.5),]

# Random Forest

rf_data = new_data

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
rf_data = check_and_convert_categorical(rf_data)

# The as.factor() function wasn't apply to the AgaCategory column since it has 13
# levels of unique values, but we will force perform as.factor() on it anyway
rf_data$AgeCategory = as.factor(rf_data$AgeCategory)

# We will drop the State columns since it will have more than 53 levels and the 
# Random Forest won't be able to perform.
rf_data = rf_data[, -1]
str(rf_data)


# Split data
n = nrow(rf_data)
p = ncol(rf_data)

set.seed(4322)
train = sample(n, 0.8*n) # Split train/test as 8:2

rf_test = rf_data[-train, ]
rf_X_test = rf_test[, -9]
rf_y_test = rf_test$HadHeartAttack

rf_model <- randomForest(HadHeartAttack ~., 
                         data = rf_data,  subset = train,
                         xtest = rf_X_test, ytest = rf_y_test,
                         ntree = 1000, mtry =  sqrt(p),
                         importance = TRUE)
rf_model
varImpPlot(rf_model)
