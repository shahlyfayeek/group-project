# Load necessary libraries
library(tidyverse)
library(ggplot2)
install.packages("corrplot")
library(corrplot)
library(readr)

# Load your dataset
df <- read.csv("StudentPerformanceFactors.csv")
summary(df)

# Check for missing values
colSums(is.na(df))

# Fill missing values with the mean of the column
df <- df %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Verify that there are no more missing values
colSums(is.na(df))

# Converting categorical variables to factors
df$Access_to_Resources <- as.factor(df$Access_to_Resources)
df$Parental_Involvement <- as.factor(df$Parental_Involvement)
df$Extracurricular_Activities <- as.factor(df$Extracurricular_Activities)
df$Internet_Access <- as.factor(df$Internet_Access)
df$School_Type <- as.factor(df$School_Type)
df$Gender <- as.factor(df$Gender)

# One-hot encoding (if necessary)
df <- cbind(df, model.matrix(~ Access_to_Resources - 1, data = df))
df <- cbind(df, model.matrix(~ Parental_Involvement - 1, data = df))
df <- cbind(df, model.matrix(~ Extracurricular_Activities - 1, data = df))
df <- cbind(df, model.matrix(~ Internet_Access - 1, data = df))
df <- cbind(df, model.matrix(~ School_Type - 1, data = df))
df <- cbind(df, model.matrix(~ Gender - 1, data = df))

# Rename columns to ensure uniqueness
names(df) <- make.names(names(df), unique = TRUE)

# Remove original categorical columns to avoid duplication
df <- df %>% select(-Access_to_Resources, -Parental_Involvement, -Extracurricular_Activities, -Internet_Access, -School_Type, -Gender)

# Ensure all columns are numeric
df <- df %>% mutate(across(everything(), as.numeric))

# Plotting the distribution of Exam Scores
ggplot(df, aes(x = Exam_Score)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Distribution of Exam Scores', x = 'Exam Score', y = 'Frequency')

# Calculate correlation matrix
correlation_matrix <- cor(df)

# Select top 5 categorical variables with the highest correlation with Exam_Score
top_5_categorical_vars <- correlation_matrix['Exam_Score', ] %>%
  abs() %>%
  sort(decreasing = TRUE) %>%
  head(6) %>%
  names() %>%
  .[2:6]

# Plotting the correlation matrix for top 5 categorical variables and Exam_Score
numeric_df <- df %>% select(c('Exam_Score', top_5_categorical_vars))
correlation_matrix_top_5 <- cor(numeric_df)
corrplot(correlation_matrix_top_5, method = 'color', type = 'upper', tl.col = 'black', tl.srt = 45)

# Scatter plot of Hours Studied vs Exam Score
ggplot(df, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(color = 'blue', alpha = 0.6) +
  labs(title = 'Hours Studied vs Exam Score', x = 'Hours Studied', y = 'Exam Score')

# Identify the correct column name for Gender after one-hot encoding
gender_col <- grep("Gender", names(df), value = TRUE)[1]

# Box plot of Exam Scores by Gender
ggplot(df, aes_string(x = gender_col, y = "Exam_Score")) +
  geom_boxplot(fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Exam Scores by Gender', x = 'Gender (Male = 1, Female = 0)', y = 'Exam Score')

# Identify the correct column name for School Type after one-hot encoding
school_type_col <- grep("School_Type", names(df), value = TRUE)[1]

# Bar plot of Exam Scores by School Type
ggplot(df, aes_string(x = school_type_col, y = "Exam_Score")) +
  geom_bar(stat = 'summary', fun = 'mean', fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Exam Scores by School Type', x = 'School Type (Private = 1, Public = 0)', y = 'Average Exam Score')

# Fit the model
model <- lm(Exam_Score ~ Hours_Studied + Sleep_Hours + Attendance + Access_to_ResourcesHigh + Access_to_ResourcesMedium + Parental_InvolvementHigh + Parental_InvolvementMedium + Extracurricular_ActivitiesYes + Internet_AccessYes + School_TypePrivate + GenderMale, data = df)

# Summary of the model to view coefficients and other stats
summary(model)

# Save cleaned dataset to a new CSV file
write.csv(df, 'Cleaned_StudentPerformanceFactors.csv', row.names = FALSE)

print("Dataset cleaned, visualizations created, and multiple linear regression model fitted. Cleaned dataset saved as 'Cleaned_StudentPerformanceFactors.csv'.")