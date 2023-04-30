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

sample <- sample.split(df_air_complete$satisfaction, SplitRatio = 0.75)
train_df_air <- subset(df_air_complete, sample == T)
test_df_air <- subset(df_air_complete,sample == F)

# Fitting data with logistic regression model **before** performing PCA,
# first on train data and then on test data

# For training dataset
log_reg_train <- glm(satisfaction ~ ., data = train_df_air, family = binomial(link = "logit"))

pred_train <- predict(log_reg_train, type = "response")
train_preds <- ifelse(pred_train > 0.5, "satisfied","dissatisfied")
train_actuals <- ifelse(train_df_air$satisfaction == 1, "satisfied","dissatisfied")

# Compute confusion matrix
train_cm <- confusionMatrix(train_preds, train_actuals, positive = "satisfied")


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
train_f1 <- f1(train_cm_table, posPredValue = "satisfied", negPredValue = "dissatisfied")

# Compute AUC-ROC
train_auc <- caret::roc(train_actuals, pred_train)$auc

# Compute Brier score
train_brier <- caret::BrierScore(train_preds, train_actuals, positive = "satisfied")

cat("Train Recall: ", train_recall, "\n")
cat("Train Precision: ", train_precision, "\n")
cat("Train F1 Score:", train_f1, "\n")
cat("Train AUC-ROC:", train_auc, "\n")
cat("Train Brier Score:", train_brier, "\n")

# For testing dataset
log_reg_test <- glm(satisfaction ~ ., data = test_df_air, family = binomial(link = "logit"))

pred_test <- predict(log_reg_test, type = "response")
test_preds <- ifelse(pred_test > 0.5, "satisfied","dissatisfied")
test_actuals <- ifelse(test_df_air$satisfaction == 1, "satisfied","dissatisfied")

# Compute confusion matrix
test_cm <- confusionMatrix(test_preds, test_actuals, positive = "satisfied")


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

cat("Test Recall: ", test_recall, "\n")
cat("Test Precision: ", test_precision, "\n")





# Compute F1-score, AUC-ROC, and Brier score
test_summary <- twoClassSummary(test_preds, test_actuals, positive = "satisfied")

test_f1 <- test_summary["F1"]
test_auc <- test_summary["ROC"]
test_brier <- test_summary["Brier"]