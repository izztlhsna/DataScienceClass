#INTRODUCTION
student <- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DataScienceClass4.3/student_data.csv")
install.packages("dplyr")
library(dplyr)
library(readr)
View(student)
summary(student)
head(student) #first 6 rows , if want to look at specific 10 rows head(student,10)
tail(student) #last 6 rows

#FILTER
Student_fail<- student %>% filter(final_exam_mark< 40) # use %>% (pipe)
View(Student_fail)

#ARRANGE
mydata<- student%>% filter(final_exam_mark > 40) %>% arrange(final_exam_mark)
View(mydata)
mydata1<- student %>% filter(final_exam_mark > 40) %>%arrange(desc(final_exam_mark)) #arrange in increasing order
View(mydata1)

#SELECT
mydata <- student%>% select(student_id,coursework_mark, final_exam_mark) #just select column yg nak tengok je
View(mydata)
glimpse(mydata) #it tells row,column,str ; bagitau datatype

#MUTATE (create new variable)
mydata = student %>% mutate(Total_Mark=(coursework_mark + final_exam_mark/200*100))
View(mydata)

#DESCRIPTIE ANALYTICS (dont do anything; just describe what is on the data)
data <- iris #predefined data that already have in r
View(data)
str(data)
summary(data)

A <- c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort(A) #descending order
quantile(A, 0.25)
quantile(A, 0.75)
IQR(A) # IQR = 1st quantile - 3rd quantile

#histogram untuk identify range value , frequently each sepal occur
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length",
     col = "lightblue",
     border = "black")

#boxplot is to detect outlier; compare median/IQR of each species; use to compare group
boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length",
        col = c("lightpink", "lightgreen", "lightblue"))

# Scatter Plot is to detect correlation(if correlated akan mix up), identify cluster or pattern
plot(iris$Sepal.Length, iris$Petal.Length,
     main = "Sepal Length vs Petal Length",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple",
     pch = 19)

player <- read.csv("C:/Users/HP/Desktop/algorithm-data-structure/data_science/DataScienceClass4.3/players.csv")
View(player)
is.na(player) #FALSE = no missing value ; TRUE = has missing value
sum(is.na(player))

#MEDIAN
median_age <- median(player$Age, na.rm =TRUE) #replace missing value with median value

#HANDLE OUTLIERS
data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")

first_q<-quantile(data,0.25) 
third_q<-quantile(data,0.75) 

data_new<-data
data <- data_new
iqr<-IQR(data)
le<-first_q - 1.5 * iqr 
ue<-third_q + 1.5 * iqr

data_new<-data
data_new <- data_new[!data_new<le]
data_new <- data_new[!data_new>ue]
data_new

data_new <- data
avg <- round(mean(data_new)) #for the purpose of example we round up value
data_new[data_new<le] <- avg
data_new[data_new>ue] <- avg
data_new

data_new <- data
data_new[data_new<le] <- le
data_new[data_new>ue] <- ue
data_new

boxplot(data_new, main = "Boxplot")




