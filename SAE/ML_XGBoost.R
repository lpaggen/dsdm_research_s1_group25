# Load necessary libraries
#install.packages("haven")
library(haven)
library(xgboost)
#install.packages("caTools")
library(caTools)
#install.packages("e1071")
library(e1071)
#install.packages("caret")
library(caret)
library(doParallel)

set.seed(1)

# Set working directory
setwd("//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data")

# Load datasets
gezo <- read_sav("Data/GEZO_2022_voor_DSC.SAV")

### Data Preparation ###
data_gezo <- as.data.frame(gezo)

# Normalize and filter data
data_gezo$PUBRookstatusRoker <- data_gezo$PUBRookstatusRoker / 100
data_gezo <- data_gezo[!is.na(data_gezo$PUBRookstatusRoker) & data_gezo$HHBLeeftijdOP > 12, ]

### Updated Subset of Relevant Variables ###
subset_gezo <- data_gezo[, c(
  "PUBRookstatusRoker",         # Smoking status
  "HHBGeslachtOP",              # Gender
  "HHBLeeftijdOP",              # Age
  "AFLLeeftijdsklassenFijn",    # Breakdown of population by age (fine)
  "AFLLeeftijdsklassenGrof",    # Breakdown of population by age (coarse)
  "AFLGeboorteJaarOP",          # Year of birth
  "AFLGeboorteMaandOP",         # Month of birth
  "AFLLeeftijdJongsteKind",     # Age of youngest child
  "AFLLeeftijdOudsteKind",      # Age of oldest child
  "AFLBurgerlijkeStaatOP",      # Marital status
  "AFLGeboortelandEnHerkomst",  # Country of birth and parents' origin
  "AFLGeboortelandDriedeling",  # Country of birth (3 categories)
  "AFLMigratieachtergrond",     # Migration background
  "AFLGeneratie",               # Migration generation
  "AFLHHBHHKern",               # Household composition
  "AFLPositieInHuishoudenOP",   # Position in household
  "HHBAantalPersonen",          # Household size
  "AFLOpleidingsniveauGevolgd", # Education level followed
  "AFLOpleidingsniveauVoltooid",# Education level completed
  "AFLOplNivVoltooidva25jr",    # Education level completed (25+)
  "AFLOpleidingsniveauVoltooidva25jr3kl", # Education level (3 grades, 25+)
  "AFLInkomensKwintiel",        # Income quintile
  "AFLVermogensKwintiel",       # Wealth quintile
  "AFLWelvaartsKwintiel",       # Welfare quintile
  "AFLWerken",                  # Employment status
  "AFLUrenWerk",                # Hours worked
  "VRGMaatschappelijkePositie", # Social position
  "REGGGD",                     # Regional health authority
  "REGGemeentecode"             # Municipal code
)]

# Convert columns to numeric where needed
numeric_columns <- c("HHBLeeftijdOP", "AFLGeboorteJaarOP", "AFLGeboorteMaandOP", 
                     "AFLLeeftijdJongsteKind", "AFLLeeftijdOudsteKind", "HHBAantalPersonen")
subset_gezo[numeric_columns] <- lapply(subset_gezo[numeric_columns], function(x) as.numeric(as.character(x)))

# Remove rows with missing values
subset_gezo <- subset_gezo[complete.cases(subset_gezo), ]

# Split into train/test sets
split <- sample.split(subset_gezo$PUBRookstatusRoker, SplitRatio = 0.7)
gezo_train <- subset(subset_gezo, split == TRUE)
gezo_test <- subset(subset_gezo, split == FALSE)

### Recursive Feature Elimination ###
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10, allowParallel = TRUE)
rfe_result <- rfe(
  x = as.matrix(gezo_train[, -which(names(gezo_train) == "PUBRookstatusRoker")]),
  y = gezo_train$PUBRookstatusRoker,
  sizes = c(1:10),
  rfeControl = ctrl
)
stopCluster(cl)

print(rfe_result)

### XGBoost Model ###
gezo_train_x <- as.matrix(gezo_train[, -which(names(gezo_train) == "PUBRookstatusRoker")])
gezo_train_y <- gezo_train$PUBRookstatusRoker
train_matrix_gezo <- xgb.DMatrix(data = gezo_train_x, label = gezo_train_y)

gezo_test_x <- as.matrix(gezo_test[, -which(names(gezo_test) == "PUBRookstatusRoker")])
gezo_test_y <- gezo_test$PUBRookstatusRoker
test_matrix_gezo <- xgb.DMatrix(data = gezo_test_x, label = gezo_test_y)

params_list <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  booster = "gbtree",
  max_depth = 6,
  eta = 0.3,
  nthread = 2
)

model_xgb <- xgb.train(params = params_list, data = train_matrix_gezo, nrounds = 100, verbose = 1)

# Predictions
predictions_xgb <- predict(model_xgb, newdata = test_matrix_gezo)
predicted_labels <- ifelse(predictions_xgb > 0.5, 1, 0)

# Evaluation metrics
accuracy <- mean(predicted_labels == gezo_test_y)
conf_matrix <- confusionMatrix(factor(predicted_labels), factor(gezo_test_y))
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- ifelse(is.na(precision + recall), 0, 2 * (precision * recall) / (precision + recall))

cat("Accuracy:", accuracy, "\n")
cat("F1 Score:", f1_score, "\n")

# Feature importance
importance_matrix <- xgb.importance(model = model_xgb)
print(head(importance_matrix, 10))
