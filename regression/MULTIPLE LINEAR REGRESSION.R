#Multiple Linear Regression

#built in data
data(mtcars)
head(mtcars)
str(mtcars)

#model the MLR
model <- lm(mpg ~ hp + wt + cyl, data = mtcars)
#invesitigate the properties of the model
summary(model)

data(mtcars)
#split data into train and test sets
data.train<- mtcars[1:22,]
data.test<- mtcars[23:32,]
#modelling
relation <-lm(mpg ~ hp +wt+cyl, data = data.train)
summary(relation)
# Prediction
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2)) 
mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

#CLASS ACTIVITY
Ozone <- c(11,11,11,12,12,13,13,13,13,14)
Solar.R <- c(290,44,320,149,120,137,112,27,238,274)
Wind <- c(9.2,9.7,16.6,12.6,11.5,10.3,11.5,10.3,12.6,10.9)
Temp <- c(66,62,73,74,73,76,71,76,64,68)
data3 <- data.frame(Ozone, Solar.R, Wind, Temp)
View(data3)

train_data <- data3[1:7,]
test_data <- data3[8:10,]

model1 <- lm(Ozone ~ Solar.R + Wind + Temp, data = train_data)
summary(model1)

#Predict
predicted_ozone <- predict(model1, newdata = test_data)
View(predicted_ozone)

#MAPE performance measurement 
mape1 <- mean(abs((test_data$Ozone - predicted_ozone) / test_data$Ozone) * 100)
paste("The error - MAPE is:", round(mape1, digits = 2), "%")

# Create table of actual vs predicted values
ozone_actuals_preds <- data.frame(cbind(ozone_actuals = test_data$Ozone, ozone_predicted = predicted_ozone))
View(ozone_actuals_preds)

# Correlation accuracy
correlation_accuracy1 <- cor(ozone_actuals_preds)
View(correlation_accuracy1)

# Calculate MAPE again using dataframe
mape1 <- mean(abs(ozone_actuals_preds$ozone_actuals - ozone_actuals_preds$ozone_predicted) / ozone_actuals_preds$ozone_actuals) * 100
paste("The error - MAPE is:", round(mape1, digits = 2), "%")





