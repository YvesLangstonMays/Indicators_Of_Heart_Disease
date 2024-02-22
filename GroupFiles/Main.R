data <- read.csv("./Indicators_Of_Heart_Disease/2022/heart_2022_no_nans.csv")

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

# View the data
View(data)

