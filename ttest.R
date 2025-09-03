# import libraries
library(tidyverse)

### Independent t test
# load data
data <-read.csv("C:\\Users\\pc\\Desktop\\DAN\\student-mat.csv")
view(data)

# Subsetting columns of interest

data_students <-data[,c("sex","G3")]
view(data_students)

#check missing values and total number of rows
dim(data_students)

#check null values
sum(is.na(data_students))

#Check for the distribution ogf the two genders
table(data_students$sex)

# Check for outliers in the two genders
boxplot(G3 ~ sex, data = data_students, main="Final Grade by Gender", ylab="G3")

# Testing for normal distibution 

shapiro_M <- shapiro.test(data_students$G3[data_students$sex == "M"])
shapiro_F <- shapiro.test(data_students$G3[data_students$sex == "F"])

format(shapiro_M$p.value,scientific = FALSE)
format(shapiro_F$p.value,scientific = FALSE)
                           
# Checking for equality in data variance

library(car)
leveneTest(G3 ~ sex, data = data_students)

# Ran the t test
t.test(G3 ~ sex, data = data_students, var.equal = TRUE)   # Student's t-test
                  
# paired t test

data2 <- read.csv("C:\\Users\\pc\\Desktop\\DAN\\student-mat.csv")
view(data2)

# subset needed columns 
exam_scoresdf <- data2[,c("G1","G3")]
view(exam_scoresdf)

# Find the difference btwn the two
exam_scoresdf$diff_marks <- exam_scoresdf$G3 - exam_scoresdf$G1
view(exam_scoresdf)

# Check for data normality
shapiro_test_diff <- shapiro.test(exam_scoresdf$diff_marks)

format(shapiro_test_diff$p.value,scientific = FALSE)

# Run the paired t test 
wilcox.test(exam_scoresdf$G3, exam_scoresdf$G1, paired = TRUE)


# One sample t test 

data3 <-read.csv("C:\\Users\\pc\\Desktop\\DAN\\student-mat.csv")
dim(data3)


#Check Normality

shapiro_G3 <- shapiro.test(data3$G3)

format(shapiro_G3$p.value, scientific = FALSE)


#run t test
t.test(data3$G3, mu = 10)

