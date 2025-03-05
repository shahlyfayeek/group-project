library(caret)
data <- read.csv("Cleaned_StudentPerformanceFactors.csv")
View(data)

# Eliminate columns with missing values
df_cleaned <- df %>% select(-which(colSums(is.na(df)) > 0))

# Verify that there are no more missing values
colSums(is.na(df_cleaned))
view(df_cleaned)

RanNum <- runif(6607)
length(RanNum)

Index <- order(RanNum)
Index

Train <- df_cleaned[Index[1:4625],]
Test <- df_cleaned[Index[4625:6607],]

# Model building
Model3 <- lm(Exam_Score ~ ., data = Train)
summary(Model3)

# Define the null model (intercept only)
Model4 <- lm(Exam_Score ~ 1, data = Train)

# Perform forward selection
forward_model <- step(Model4, 
                      scope = list(lower = Model4, upper = Model3), 
                      direction = "forward")
summary(forward_model)

# Perform backward elimination
backward_model <- step(Model3, direction = "backward")
summary(backward_model)

# Stepwise regression
stepwise_model <- step(Model4, 
                       scope = list(lower = Model4, upper = Model3), 
                       direction = "both")
summary(stepwise_model)

# Final model based on stepwise regression
final_model <- lm(Exam_Score ~ Hours_Studied + Sleep_Hours + Attendance + Access_to_ResourcesHigh + Access_to_ResourcesMedium + Parental_InvolvementHigh + Parental_InvolvementMedium + Extracurricular_ActivitiesYes + Internet_AccessYes + School_TypePrivate + GenderMale, data = Train)
summary(final_model)

# Predict on the test set
predict <- predict(final_model, newdata = Test)
Test$PREDICTED_Exam_Score <- predict
view(predict)
# Show the final exam score column along with predictions
Test <- Test %>% select(Exam_Score, PREDICTED_Exam_Score)
print(Test)
view(Test)


# Plot the final model
plot(final_model)

# Save cleaned dataset to a new CSV file
write.csv(df_cleaned, 'Cleaned_StudentPerformanceFactors.csv', row.names = FALSE)

print("Model built and summary generated. Cleaned dataset saved as 'Cleaned_StudentPerformanceFactors.csv'.")