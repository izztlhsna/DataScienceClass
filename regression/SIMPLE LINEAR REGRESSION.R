#SIMPLE LINEAR REGRESSION

#data of height
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
#data of weight
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)
# Apply the lm() function
relation <- lm(y~x)
print(relation)

# Find weight of a person with height 189
x_test <- data.frame(x = 189)
result <- predict(relation,x_test)
print(round(result, digit=2))

#3. Plot the chart, make a prediction for forecasting
plot(x,y,col = "blue",main = "Height & Weight Regression",
     abline(lm(y~x)),pch = 16,xlab = "Height in cm", ylab = "Weight in Kg")

#4.training
data1= data.frame(x,y)
#splitting data into training and testing
data1_train<-data1[1:7,]
data1_test<-data1[8:10,]
# Apply the lm() function
relation <- lm(y~x, data1_train)
print(relation)

#test
x_text <- data.frame(x= data1_test$x)
result <- predict(relation,x_text)
print(result)

#MAPE (Mean Absolute Percentage Error)
mape <- mean(abs((data1_test$y -result)/data1_test$y)*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")

actuals_preds <- data.frame(cbind(actuals=data1_test$y,
                                  predicteds=result))
mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicteds )/
               actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")


#HAPPINESS INCOME
df <- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DATASCIENCECLASS/income_happiness.csv")
#Split data into training (80%) and testing (20%) sets
#Randomly select row indices for training
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]
# Apply the lm() function
relation <- lm(happiness~income, data=train_data)
print(relation)

#CLASS ACTIVITY
#a
Experience_Years <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 
Monthly_Salary <- c(2500, 2700, 3000, 3400, 3900, 4400, 5000, 5600, 6200, 6900)

salary_data <- data.frame(Experience_Years, Monthly_Salary)

salary_data_train<-data1[1:7,]
salary_data_test<-data1[8:10,]

relation <- lm(Monthly_Salary ~ Experience_Years, salary_data_train)
summary(relation)

Experience_Years_text <- data.frame(Experience_Years= salary_data_test$Experience_Years)
result <- predict(relation,Experience_Years_text)
print(result)

#b
# Scatter plot
plot(salary_data$Experience_Years,
     salary_data$Monthly_Salary,
     main="Salary vs Experience",
     xlab="Years of Experience",
     ylab="Monthly Salary",
     pch=19,
     col="blue")

abline(relation, col="red", lwd=2)

