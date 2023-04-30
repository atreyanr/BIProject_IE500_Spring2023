# Importing libraries

library(caret)
library(caTools)
library(Metrics)
library(MLmetrics)
library(ModelMetrics)
library(pROC)


# Importing data

Invistico_Airline <- read.csv("~/Library/CloudStorage/Box-Box/UB/Spring 2023/IE500 BI/Project/Invistico_Airline.csv")

# Checking unique values of each column to modify the character columns with equivalent binary numerical entries for convenience

for (col in names(Invistico_Airline)) {
  cat("Unique entries in", col, "column:\n")
  print(unique(Invistico_Airline[[col]]))
}
# Gender, Type.of.Travel, Customer.Type, satisfaction are all having binary character classification
# Therefore, they are modified to equivalent 1's and 0's

Invistico_Airline$Gender <- ifelse(Invistico_Airline$Gender == "Male", 1, 0) #1 for male, 0 for female
Invistico_Airline$Type.of.Travel <- ifelse(Invistico_Airline$Type.of.Travel == "Personal Travel", 1, 0) #1 for Personal Travel, 0 for Business Travel
Invistico_Airline$Customer.Type <- ifelse(Invistico_Airline$Customer.Type == "Loyal Customer", 1, 0) #1 for Loyal Customer, 0 for disloyal Customer
Invistico_Airline$satisfaction <- ifelse(Invistico_Airline$satisfaction == "satisfied", 1, 0) #1 for satisfied, 0 for dissatisfied


# Cleaning the data and completing the empty rows so there is no mismatch.

df_air <- na.omit(Invistico_Airline)
df_air_complete <- df_air[complete.cases(df_air), ]

# Splitting data into test and train with split ratio of 0.75

set.seed(123)

train_control <- trainControl(method="cv", number=10)
split <- caret::createDataPartition(df_air_complete$satisfaction, p=0.75, list=FALSE)
train_df_air <- df_air_complete[split,]
test_df_air <- df_air_complete[-split,]

# Fitting data with logistic regression model **before** performing PCA,
# first on train data and then on test data

# For training dataset
log_reg_train <- glm(satisfaction ~ ., data = train_df_air, family = binomial(link = "logit"))

summary(log_reg_train)

pred_train <- predict(log_reg_train, newdata = train_df_air, type = "response")

train_preds <- ifelse(pred_train > 0.5, "satisfied","dissatisfied")
train_actuals <- ifelse(train_df_air$satisfaction == 1, "satisfied","dissatisfied")

train_actuals <- factor(train_actuals, levels = c("dissatisfied", "satisfied"))
train_preds <- factor(train_preds, levels = c("dissatisfied", "satisfied"))


# Compute confusion matrix

train_cm <- confusionMatrix(train_preds, train_actuals, positive = "satisfied")
train_cm

train_cm_table <- table(train_actuals, train_preds)

if ("satisfied" %in% rownames(train_cm_table) && "satisfied" %in% colnames(train_cm_table)) {
  
  # Calculate recall
  train_recall <- caret::recall(train_cm_table, "satisfied")
  
  # Calculate precision
  train_precision <- caret::precision(train_cm_table, "satisfied")
  
} else {
  train_recall <- NA
  train_precision <- NA
  cat("No positive cases in actuals or predicted values.")
}

# Compute F1 score
train_f1 <- F1_Score(train_preds, train_actuals, positive = "satisfied")

# Convert actuals to numeric
train_actuals_numeric <- as.numeric(train_actuals) - 1
train_preds_numeric <- as.numeric(train_preds) - 1

# Compute AUC-ROC
train_auc <- AUC(train_preds,train_actuals_numeric)

# Compute Brier score
train_brier <- brier(train_preds, train_actuals)

cat("Train Recall: ", train_recall, "\n")
cat("Train Precision: ", train_precision, "\n")
cat("Train F1 Score:", train_f1, "\n")
cat("Train AUC-ROC:", train_auc, "\n")
cat("Train Brier Score:", train_brier, "\n")

# For testing dataset
log_reg_test <- glm(satisfaction ~ ., data = test_df_air, family = binomial(link = "logit"))

summary(log_reg_test)

pred_test <- predict(log_reg_test, type = "response")
test_preds <- ifelse(pred_test > 0.5, "satisfied","dissatisfied")
test_actuals <- ifelse(test_df_air$satisfaction == 1, "satisfied","dissatisfied")


test_actuals <- factor(test_actuals, levels = c("dissatisfied", "satisfied"))
test_preds <- factor(test_preds, levels = c("dissatisfied", "satisfied"))


# Compute confusion matrix

test_cm <- confusionMatrix(test_preds, test_actuals, positive = "satisfied")
test_cm


test_cm_table <- table(test_actuals, test_preds)

if ("satisfied" %in% rownames(test_cm_table) && "satisfied" %in% colnames(test_cm_table)) {
  
  # Calculate recall
  test_recall <- caret::recall(test_cm_table, "satisfied")
  
  # Calculate precision
  test_precision <- caret::precision(test_cm_table, "satisfied")
  
} else {
  test_recall <- NA
  test_precision <- NA
  cat("No positive cases in actuals or predicted values.")
}

# Compute F1 score
test_f1 <- F1_Score(test_preds, test_actuals, positive = "satisfied")

# Convert actuals to numeric
test_actuals_numeric <- as.numeric(test_actuals) - 1
test_preds_numeric <- as.numeric(test_preds) - 1

# Compute AUC-ROC
test_auc <- AUC(test_preds,test_actuals_numeric)


# Compute Brier score
test_brier <- brier(test_preds, test_actuals)


cat("Test Recall: ", test_recall, "\n")
cat("Test Precision: ", test_precision, "\n")
cat("Test F1 Score:", test_f1, "\n")
cat("Test AUC-ROC:", test_auc, "\n")
cat("Test Brier Score:", test_brier, "\n")

