# import libraries
library(tidyverse)

# load data
data <-read.csv("C:\\Users\\pc\\Desktop\\DAN\\student-mat.csv")
view(data)

# Subsetting columns of interest

data_students <-data[,c("sex","G3")]
view(data_students)

#check missing values and total number of rows
dim

#check null values
sum(is.na(data_students))

#
table(students$sex)

boxplot(G3 ~ sex, data = students, main="Final Grade by Gender", ylab="G3")


shapiro.test(students$G3[students$sex == "M"])
shapiro.test(students$G3[students$sex == 
                           

library(car)
leveneTest(G3 ~ sex, data = students)


t.test(G3 ~ sex, data = students, var.equal = TRUE)   # Student's t-test
t.test(G3 ~ sex, data = students)                     # Welch’s t-test (default)

