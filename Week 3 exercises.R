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
#382 observations, 33 columns, so two columns are missing?

write.csv(alc, "alc.csv")
