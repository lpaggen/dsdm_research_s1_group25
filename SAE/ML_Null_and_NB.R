library(pROC)
library(haven)
library(data.table)
library(randomForest)
library(caret)
library(ROSE)
library(boot)
library(dplyr)
library(ipred) 
library(rpart)
library(e1071)
library(caTools)
library(xgboost)

# Data Preparation --------------------------------------------------------


# Directory of the data
directory <- "//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/"

# Read the health survey data
gezo <- read_sav(file = paste0(directory, "GEZO_2022_voor_DSC.SAV"))

# Transform 
gezo <- as_factor(gezo)
gezo <- data.table(gezo)

# Remove NA variables for the response PUBRookStatusRoker (only responders from 13 and above)
gezo <- gezo[as.numeric(gezo$HHBLeeftijdOP) > 12, ]
gezo <- gezo[!is.na(gezo$PUBRookstatusRoker), ]  # Remove rows where PUBRookstatusRoker is NA

# Select all relevant variables for analysis
gezo_29_var <- gezo[, .(
  PUBRookstatusRoker,         # Smoking status
  HHBGeslachtOP,              # Gender
  HHBLeeftijdOP = as.numeric(as.character(HHBLeeftijdOP)),  # Age
  AFLBurgerlijkeStaatOP,      # Marital status
  AFLGeboortelandEnHerkomst,  # Country of birth and parents' origin
  AFLGeboortelandDriedeling,  # Country of birth (3 categories)
  AFLMigratieachtergrond,     # Migration background
  AFLGeneratie,               # Migration generation
  AFLHHBHHKern,               # Household composition
  AFLPositieInHuishoudenOP,   # Position in household
  HHBAantalPersonen = as.numeric(as.character(HHBAantalPersonen)), # Household size
  AFLOpleidingsniveauGevolgd, # Education level followed
  AFLOpleidingsniveauVoltooid, # Education level completed
  AFLOplNivVoltooidva25jr,    # Education level completed (25+)
  AFLInkomensKwintiel = as.numeric(AFLInkomensKwintiel),        # Income quintile
  AFLVermogensKwintiel = as.numeric(AFLVermogensKwintiel),       # Wealth quintile
  AFLWelvaartsKwintiel = as.numeric(AFLWelvaartsKwintiel),       # Welfare quintile
  AFLWerken,                  # Employment status
  AFLUrenWerk,                # Hours worked
  VRGMaatschappelijkePositie, # Social position
  REGGGD,                     # Regional health authority
  REGStedgem,                 # Urban area
  REGGemeentecode             # Municipality
)]

# Modify REGGemeentecode to fit 53 levels (limit for RF)
gezo_29_var[, REGGemeentecode := fifelse(REGGGD == "GGD Zuid-Limburg", 
                                         as.character(REGGemeentecode), "0")]
gezo_29_var[, REGGemeentecode := ifelse(REGGemeentecode == "0" & !is.na(REGGGD), 
                                        as.character(REGGGD), as.character(REGGemeentecode))]

gezo_29_var[, REGGGD := NULL]
gezo_29_var[, REGGemeentecode := as.factor(REGGemeentecode)]

# Check for missing data, no missing data
sum(is.na(gezo_29_var))

# Factorize the response variable (PUBRookstatusRoker)
gezo_29_var$PUBRookstatusRoker <- as.factor(gezo_29_var$PUBRookstatusRoker)




# Null model --------------------------------------------------------------


set.seed(123)
train_indices <- createDataPartition(
  y = gezo_29_var$PUBRookstatusRoker, 
  p = 0.7, 
  list = FALSE
)
train_data <- gezo_29_var[train_indices, ]
test_data <- gezo_29_var[-train_indices, ]


majority_class <- names(which.max(table(train_data$PUBRookstatusRoker)))
null_predictions <- rep(majority_class, nrow(test_data))

# Evaluation
# Confusion Matrix
conf_matrix_null <- confusionMatrix(
  factor(null_predictions, levels = levels(test_data$PUBRookstatusRoker)), 
  test_data$PUBRookstatusRoker
)

# Print accuracy
cat("Accuracy of Null Model:", conf_matrix_null$overall["Accuracy"], "\n")

# Calculate F1 Score
conf_table <- conf_matrix_null$table
true_positives <- conf_table[majority_class, majority_class]
false_positives <- sum(conf_table[, majority_class]) - true_positives
false_negatives <- sum(conf_table[majority_class, ]) - true_positives

precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print results
cat("Precision of Null Model:", precision, "\n")
cat("Recall of Null Model:", recall, "\n")
cat("F1 Score of Null Model:", f1_score, "\n")

# Confusion matrix
conf_table







# naiv bayes model --------------------------------------------------------


# Split data into train/test sets
set.seed(1)
split <- sample.split(gezo_29_var$PUBRookstatusRoker, SplitRatio = 0.7)
train_data <- gezo_29_var[split == TRUE]
test_data <- gezo_29_var[split == FALSE]

# Train Naïve Bayes Model
nb_model <- naiveBayes(
  PUBRookstatusRoker ~ .,
  data = train_data
)

# Predict on test set
predictions <- predict(nb_model, test_data)

# Evaluation
# Convert the true labels to a factor
true_labels <- factor(test_data$PUBRookstatusRoker)

# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, true_labels)

# Accuracy
accuracy <- conf_matrix$overall["Accuracy"]

# F1 Score
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- ifelse(is.na(precision + recall), 0, 2 * (precision * recall) / (precision + recall))

# Output results
cat("Accuracy:", accuracy, "\n")
cat("F1 Score:", f1_score, "\n")

predictions_nb <- predict(nb_model, test_data)
confMatrix_nb <- confusionMatrix(predictions_nb, test_data$PUBRookstatusRoker) # On test data
print(confMatrix_nb)




# XGBoost -----------------------------------------------------------------


library(xgboost)
library(caTools)

# 1. Set seed for reproducibility
set.seed(1)

# 2. Create a logical vector for splitting
split <- sample.split(gezo_29_var$PUBRookstatusRoker, SplitRatio = 0.7)

# 3. Use the logical vector to create train and test datasets
train_data <- gezo_29_var[split == TRUE, ]
test_data <- gezo_29_var[split == FALSE, ]


# 1. Separate features and target variable (make sure it's done correctly)
train_label <- train_data$PUBRookstatusRoker
train_features <- train_data[, !names(train_data) %in% "PUBRookstatusRoker"]

test_label <- test_data$PUBRookstatusRoker
test_features <- test_data[, !names(test_data) %in% "PUBRookstatusRoker"]

# Check the structure of the features
str(train_features)
str(test_features)

# 3. Ensure that categorical columns are factors
categorical_cols <- sapply(train_features, is.factor)

# Check for logical columns and convert them to factors (if any)
logical_cols <- sapply(train_features, is.logical)
if (any(logical_cols)) {
  train_features[logical_cols] <- lapply(train_features[logical_cols], as.factor)
  test_features[logical_cols] <- lapply(test_features[logical_cols], as.factor)
}

# 4. Create DMatrix with `enable_categorical = TRUE`
dtrain <- xgb.DMatrix(data = train_features, label = train_label, enable_categorical = TRUE)
dtest <- xgb.DMatrix(data = test_features, label = test_label, enable_categorical = TRUE)

# 5. Define parameters for the XGBoost model
params <- list(
  objective = "multi:softmax",          # Use softmax for classification
  num_class = length(unique(train_label)), # Number of unique classes
  max_depth = 6,                       # Maximum depth of trees
  eta = 0.3,                           # Learning rate
  eval_metric = "mlogloss",            # Log loss for multi-class classification
  tree_method = "hist",                # Optimized histogram-based algorithm
  enable_categorical = TRUE            # Enable categorical variable handling
)

# 6. Train the XGBoost model
xgb_model <- xgboost(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 1
)

# 7. Make predictions on the test set
preds <- predict(xgb_model, dtest)

# 8. Evaluate the model’s accuracy
accuracy <- mean(preds == test_label)
cat("Accuracy:", accuracy, "\n")
