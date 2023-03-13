library(tidyverse)

# We will be using the TitanicSurvival dataset from Vincent Arel Bundock dataset list.
# Load TitanicSurvival.csv dataset
titanic <- read.csv("TitanicSurvival.csv")

# Examine structure of dataset
str(titanic)

# Replace missing Age values with the median age
sum(is.na(titanic$age))
median_age <- median(titanic$age, na.rm = TRUE)
titanic$age[is.na(titanic$age)] <- median_age

#As we can see, the dataset contains 1309 observations of 5 variables. The variables are:
#X: name of the passenger  
#passengerClass: the ticket class (1st, 2nd, 3rd)
#sex: the passenger's sex (female or male)
#age: the passenger's age
#survived: whether the passenger survived (yes or no)

# Convert sex, survived and passengerClass variables to factors
titanic$sex <- as.factor(titanic$sex)
titanic$survived <- as.factor(titanic$survived)
titanic$passengerClass <- as.factor(titanic$passengerClass)

#1#Basic Visualization without any package
# Create a table of survival counts by sex
survival_table <- table(titanic$sex, titanic$survived)
survival_table
# Create a bar chart of survival by sex
barplot(survival_table, beside = TRUE, col = c("blue", "red"), 
        legend = c("female", "male"), xlab = "Sex", ylab = "Count")


#2#Visualization using lattice package
# Load the lattice package
library(lattice)

# Create a grouped bar chart of survival counts by sex and passenger class
survival_table_2 <- table(titanic$sex, titanic$survived, titanic$passengerClass)
survival_table_2
barchart(survival_table_2, col = c("blue", "red",'yellow'), 
         xlab = "Passenger Class", ylab = "Count",
         main = "Survival Counts by Sex and Passenger Class",
         key = list(space = "right", 
                    text = list(c("1st Class", "2nd Class", "3rd Class")), 
                    points = list(col = c("blue", "red", "yellow"), pch = 15)))

#3#Visualization using ggplot2 package
# Load the ggplot2 package
library(ggplot2)

# Create a data frame of survival counts by sex and passenger class
survival_df <- data.frame(table(titanic$sex, titanic$survived, titanic$passengerClass))
names(survival_df) <- c("Sex", "Survived", "PassengerClass", "Count")

# Create a grouped bar chart of survival counts by sex and passenger class
ggplot(survival_df, aes(x = PassengerClass, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Sex, nrow = 1) +
  labs(title = "Survival Counts by Sex and Passenger Class", x = "Passenger Class", y = "Count", fill = "Survived") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Not Survived", "Survived"))

