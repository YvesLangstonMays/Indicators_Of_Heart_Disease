data <- read.csv("./Indicators_Of_Heart_Disease/2020/heart_2020_cleaned.csv")

data <- data[1:20]




heart.lm <- lm(HeartDisease ~ GenHealth, data = data)
summary(heart.lm)
