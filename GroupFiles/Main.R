library(ggplot2)
library(dplyr)

data <- read.csv("./Indicators_Of_Heart_Disease/2022/heart_2022_no_nans.csv")

detach(data)
attach(data)

# Cleaning data
sum(is.na(data))

# There are no na values

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
data <- convert_to_binary_auto(data)
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


