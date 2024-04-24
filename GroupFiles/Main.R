library(ggplot2)
library(dplyr)

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

# Function that will look for categorical variable columns and convert them using as,factor()
check_and_convert_categorical <- function(test_data) {
  for (col_name in names(test_data)) {
    if (!is.factor(test_data[[col_name]]) && (is.character(test_data[[col_name]]) || is.integer(test_data[[col_name]]))) {
      unique_vals <- unique(test_data[[col_name]])
      if (length(unique_vals) <= 10) {  # Adjust the threshold as needed
        test_data[[col_name]] <- as.factor(test_data[[col_name]])
        cat(paste("Converted", col_name, "to factor\n"))
      }
    }
  }
  return(test_data)
}
data = check_and_convert_categorical(data)
summary(data)

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
