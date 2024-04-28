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
# ggplot(data, aes(y = BMI)) +
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
#   coord_flip()
# 
# ggplot(data, aes(y = HeightInMeters)) +
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
#   coord_flip()

# Removing outliers
data <- data %>% 
  filter(BMI <= 41)

data <- data %>% 
  filter(BMI >= 14)

data <- data %>% 
  filter(HeightInMeters <= 2.0)

data <- data %>% 
  filter(HeightInMeters >= 1.41)

# # Checking Data
# ggplot(data, aes(y = BMI)) +
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE, ) +
#   coord_flip() 
# 
# # Checking Data
# ggplot(data, aes(y = HeightInMeters)) +
#   geom_boxplot(outlier.colour = "red", outlier.shape = 1, notch = TRUE) +
#   coord_flip()

# summary(data)

 data = data[, sapply(data, is.numeric)]
 dim(data)
 summary(data)
 
 # Scaling data, explanation below
 # 
 scaled_data = scale(data)
 
 pca_result = prcomp(scaled_data, center = TRUE, scale. = TRUE)
 summary(pca_result)

 loadings <- pca_result$rotation
loadings
pca_result$x
 # Define a threshold for "high" loading
 threshold <- 0.6

 # Find loading greater than the threshold
 high_loadings <- apply(loadings, 2, function(x) which(abs(x) > threshold))

 # Print the variable names with high loading for each principal component
 lapply(seq_along(high_loadings), function(i) {
   cat("Principal Component", i, ":\n")
   print(names(high_loadings[[i]]))
   cat("\n")
 })
 plot(pca_result, type = "b")

library(tree)
set.seed(100)
sample = sample(1:nrow(data), nrow(data) / 2)
train_data = data[sample,]
test_data = data[-sample,]


 

PC4.tree <- tree(HadHeartAttack ~ PhysicalHealthDays + PhysicalActivities +
                   HadCOPD + HadArthritis + DifficultyConcentrating + 
                   DifficultyWalking + DifficultyDressingBathing +
                   DifficultyErrands + ChestScan, data = train_data)
summary(PC4.tree)
plot(PC4.tree)
text(PC4.tree, pretty = 0)

test_predictions = predict(PC4.tree, test_data)
test_mse = mean((test_predictions - test_data$HadHeartAttack))
test_mse

cv.PC4 = cv.tree(PC4.tree, FUN = prune.tree)
plot(cv.PC4$size, cv.PC4$dev, type = "b")

pruned = prune.tree(PC4.tree, best = 3)
plot(pruned)
text(pruned, pretty = 0)

