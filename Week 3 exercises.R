# Venla Johansson 11.11.2020, week 3 exercises, Paulo Cortez, University of Minho, GuimarÃ£es, Portugal, http://www3.dsi.uminho.pt/pcortez 
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"
url_math <- paste(url, "student-mat.csv", sep = "/")
url_por <- paste(url, "student-por.csv", sep = "/")
math <- read.table(url_math, sep = ";", header = TRUE)
por <- read.table(url_por, sep = ";", header = TRUE)
str(math)
dim(math)
str(por)
dim(por)
install.packages("dplyr")
library("dplyr")
student_identifiers <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")
math_por <- inner_join(math, por, by = student_identifiers)
str(math_por)
dim(math_por)
colnames(math_por)
alc <- select(math_por, one_of(student_identifiers))
notjoined_columns <- colnames(math)[!colnames(math) %in% student_identifiers]
notjoined_columns

for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}
glimpse(alc)
#382 observations, 33 columns

# Average of the answers related to weekday and weekend alcohol consumption, new columns "alc_use" and "high_use"

library(dplyr)

install.packages("ggplot2")

alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

alc <- mutate(alc, high_use = alc_use > 2)

glimpse(alc)

write.csv(alc, "alc.csv")

# Data analysis

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets"
url_math <- paste(url, "student-mat.csv", sep = "/")
url_por <- paste(url, "student-por.csv", sep = "/")
math <- read.table(url_math, sep = ";", header = TRUE)
por <- read.table(url_por, sep = ";", header = TRUE)
colnames(math)
colnames(por)
# This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. 

#choose 4 interesting variables in the data and for each of them, present your personal hypothesis about their relationships with alocol consumption
# Hypotheses
#1. High consumption of alcohol is associated with higher amount of absences
#2. High alcohol consumption is not dependent on the age
#3. High alcohol consumption is associated with lower health status
#4. High alcohol consumption is associated with higher amount of failures

# 1. hypothesis, exploration with simple regression model, graphic plot() and regression line (method = lm)

library(readr)
alc <- read_csv("data/alc.csv")
glimpse(alc)

first_hypothesis <- lm(high_use ~ absences, data = alc)
summary(first_hypothesis)

plot(alc$high_use, alc$absences)
plot(first_hypothesis)
install.packages("ggplot2")
install.packages("colorspace")

library(ggplot2)
install.packages("c:/path/to/downloaded/zip/file/colorspace_1.4-1.tgz")
library(ggplot2)
plot_firsthypothesis <- ggplot(alc, aes(x = high_use, y = absences))
plot_firsthypothesis2 <- plot_firsthypothesis + geom_point()
plot_firsthypothesis2
plot_firsthypothesis3 <- plot_firsthypothesis2 + geom_smooth(method = "lm")
plot_firsthypothesis3

#2. High alcohol consumption is not dependent on the age

second_hypothesis <- lm(high_use ~ age, data = alc)
summary(second_hypothesis)

#High alcohol consumption is associated with lower health status

third_hypothesis <- lm(high_use ~ health, data = alc)
summary(third_hypothesis)

#4. High alcohol consumption is associated with higher amount of failures

fourth_hypothesis <- lm(high_use ~ failures, data = alc)
summary(fourth_hypothesis)

# logistic regression, using alc_use and high_use as target variable and the chosen variables
h1 <- glm(alc_use ~ absences + age + health + failures, data = alc)
h1
summary(h1)
h2 <- lm(alc_use ~ absences + age + health + failures, data = alc)
summary(h2)
OR <- coef(h2) %>% exp
CI <- confint(h2) %>% exp
cbind(OR, CI)

h3 <- glm(high_use ~ absences + age + health + failures, data = alc)
summary(h3)
OR2 <- coef(h3) %>% exp
CI2 <- confint(h3) %>% exp
cbind(OR2, CI2)

probabilities <- predict(h3, type = "response")
alc <- mutate(alc, probability = probabilities)

alc <- mutate(alc, prediction = probability > 0.5)

table(high_use = alc$high_use, prediction = alc$prediction)
#trying to solve a push problem