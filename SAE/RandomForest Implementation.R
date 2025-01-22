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
library(survey)


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


# Data Split for ML -------------------------------------------------------


# Split the data into training, validation, and test sets

# Note: The dataset is unbalanced
table(gezo_29_var$PUBRookstatusRoker)

set.seed(321)
totalRows <- nrow(gezo_29_var)

trainIndex <- sample(1:totalRows, 0.6 * totalRows)  # 60% for training
remainingIndex <- setdiff(1:totalRows, trainIndex)
validationIndex <- sample(remainingIndex, 0.5 * length(remainingIndex))  # 20% for validation
testIndex <- setdiff(remainingIndex, validationIndex)  # 20% for testing

trainData <- gezo_29_var[trainIndex, ]
validationData <- gezo_29_var[validationIndex, ]
testData <- gezo_29_var[testIndex, ]


# Initial RF --------------------------------------------------------------


rf_model <- randomForest(PUBRookstatusRoker ~ ., data = trainData, importance = TRUE, ntree = 500, mtry = 7)

# Predictions and confusion matrix on validation data
predictions_rf_val <- predict(rf_model, validationData)
confMatrix_rf_val <- confusionMatrix(predictions_rf_val, validationData$PUBRookstatusRoker) # On validation data
print(confMatrix_rf_val) #Poor performance

# Plot OOB error rate
plot(rf_model$err.rate[, 3], type = "l", xlab = "Number of Trees", ylab = "OOB Error Rate", main = "OOB Error vs. Number of Trees")
# Errors increasing in the number of trees, needs to be inverted

# AUC for the Default Model

# Predicted probabilities for the validation data
probabilities_rf_val <- predict(rf_model, validationData, type = "prob")
roc_curve_val <- roc(validationData$PUBRookstatusRoker, probabilities_rf_val[, 2]) # Assuming positive class is the second column
auc_val <- auc(roc_curve_val)
print(paste("Validation AUC (Default Model):", auc_val))

plot(roc_curve_val, main = "ROC Curve for Default Random Forest Model (Validation Data)")

# MSE for the Default Model

predictions_rf_val_numeric <- as.numeric(as.factor(predictions_rf_val)) - 1
actual_val_numeric <- as.numeric(as.factor(validationData$PUBRookstatusRoker)) - 1
mse_val <- mean((predictions_rf_val_numeric - actual_val_numeric)^2)
print(paste("Validation MSE (Default Model):", mse_val))


# Training set rebalancing ------------------------------------------------


# Apply ROSE for rebalancing the training set
trainData <- as.data.frame(trainData)
rose_data <- ovun.sample(
  formula = PUBRookstatusRoker ~ ., 
  data = trainData                 
)$data

rose_data$HHBLeeftijdOP = as.integer(rose_data$HHBLeeftijdOP)
rose_data$HHBAantalPersonen = as.integer(rose_data$HHBAantalPersonen)
rose_data$AFLInkomensKwintiel = as.integer(rose_data$AFLInkomensKwintiel)


table(rose_data$PUBRookstatusRoker)

# Train Random Forest model on rebalanced dataset
rf_model_rebalanced <- randomForest(PUBRookstatusRoker ~ . , data = rose_data, importance = TRUE, ntree = 500, mtry = 7)

# Testing
predictions_rf_rebalanced <- predict(rf_model_rebalanced, testData)
confMatrix_rf_rebalanced <- confusionMatrix(predictions_rf_rebalanced, testData$PUBRookstatusRoker) # On test data
print(confMatrix_rf_rebalanced)
# There is little improvement

probabilities_rf_rebalanced_val <- predict(rf_model_rebalanced, validationData, type = "prob")

roc_curve_val_rebalanced <- roc(validationData$PUBRookstatusRoker, probabilities_rf_rebalanced_val[, 2]) # Assuming positive class is in the second column
auc_val_rebalanced <- auc(roc_curve_val_rebalanced)
print(paste("Validation AUC (Rebalanced Model):", auc_val_rebalanced)) 

# Plot the ROC curve for validation data
plot(roc_curve_val_rebalanced, main = "ROC Curve for Rebalanced Random Forest Model (Validation Data)")
# But AUC is decreasing..

predictions_rf_val_rebalanced <- predict(rf_model_rebalanced, validationData)
predictions_rf_val_numeric_rebalanced <- as.numeric(as.factor(predictions_rf_val_rebalanced)) - 1
actual_val_numeric_rebalanced <- as.numeric(as.factor(validationData$PUBRookstatusRoker)) - 1

mse_val_rebalanced <- mean((predictions_rf_val_numeric_rebalanced - actual_val_numeric_rebalanced)^2)
print(paste("Validation MSE (Rebalanced Model):", mse_val_rebalanced))



# Plot OOB error rate for the rebalanced model
plot(rf_model_rebalanced$err.rate[, 3], type = "l", xlab = "Number of Trees", ylab = "OOB Error Rate", main = "OOB Error vs. Number of Trees")
# Now increasing trees makes sense


# RF fine tuning ----------------------------------------------------------


# Train Random Forest model with tuned parameters
rf_model_tuning <- randomForest(PUBRookstatusRoker ~ .  , data = rose_data, importance = TRUE, ntree = 300, mtry = 11, nodesize= 40)


# Predictions and confusion matrix on validation data for tuned model
predictions_rf_val_tuned <- predict(rf_model_tuning, validationData)
confMatrix_rf_val_tuned <- confusionMatrix(predictions_rf_val_tuned, validationData$PUBRookstatusRoker) # On validation data
print(confMatrix_rf_val_tuned)

# Predicts very well non smokers, struggles at predicting smokers


# Overfitting analysis ----------------------------------------------------


# Performance on training set (with rebalanced data)
train_predictions <- predict(rf_model_tuning, newdata = rose_data)

# Confusion matrix on the training set
confMatrix_rf_train <- confusionMatrix(train_predictions, rose_data$PUBRookstatusRoker)
print(confMatrix_rf_train)













########################################################
# From survey to population: population register mapping ------------------
########################################################


# Data uploading ----------------------------------------------------------


population =  load("//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/populationdata_limburg_2020.RData")
population_original = population

population <- as_factor(dt)
population_original <- as_factor(dt)
population <- data.table(dt)
population_original <- data.table(dt)


# Matching columns with GEZO ----------------------------------------------


population <- population %>%
  select(
    HHBGeslachtOP = VRLGBAGESLACHT,                    # Gender
    HHBLeeftijdOP = LFT,                               # Age
    AFLBurgerlijkeStaatOP = BURKLASSE4,                # Marital status
    #AFLGeboortelandEnHerkomst = ETNGROEPKORT1,         # Country of birth and parents' origin
    AFLHHBHHKern = type_hh,                            # Household composition
    HHBAantalPersonen = AANTALBEWONERS,                # Household size
    AFLOpleidingsniveauGevolgd = OPLNIVHB2,            # Education level followed
    AFLInkomensKwintiel = inkomen,                     # Income quintile
    REGGemeentecode = GEMJJJJ                          # Municipality
  )

head(population)


# Gender
population$HHBGeslachtOP <- population$HHBGeslachtOP %>%
  recode(
    "Mannen" = "Man",
    "Vrouwen" = "Vrouw"
  )

# Consider same age target
population <- population[as.numeric(population$HHBLeeftijdOP) > 12, ]

# Marital Status
population$AFLBurgerlijkeStaatOP <- recode(
  population$AFLBurgerlijkeStaatOP,
  "Gehuwd" = "gehuwd, inclusief geregistreerd partnerschap",
  "Gescheiden" = "gescheiden",
  "Verweduwd" = "weduwe/weduwnaar",
  "Nooit gehuwd geweest" = "nooit gehuwd geweest"
)

# Household composition
population$AFLHHBHHKern <- recode(
  population$AFLHHBHHKern,
  "Eenpersoons" = "eenpersoonshuishouden",
  "Paar zonder kinderen" = "paar",
  "Paar met kinderen" = "paar met kinderen",
  "Eenouderhuishouden" = "alleenstaande ouder met kinderen",  # or "alleenstaande ouder met kinderen en anderen"
  "Overig" = "alleenstaande met anderen"  # Or map to other categories from "Overig"
)

# HH size already matches

# Education
population$AFLOpleidingsniveauGevolgd <- recode(
  population$AFLOpleidingsniveauGevolgd,
  "Basisonderwijs" = "Basisonderwijs",
  "Vmbo, mbo1, avo onderbouw" = "Vmbo, havo-, vwo-onderbouw, mbo 1",
  "Havo, vwo, mbo" = "Havo, vwo, mbo",
  "Hbo-, wo-bachelor" = "Hbo-, wo-bachelor",
  "Hbo-, wo-master, doctor" = "Hbo-, wo-master, doctor",
  "niet van toepassing" = "Weet niet of onbekend",
  "onbekend" = "Weet niet of onbekend"
)

# Income quintile 
population$AFLInkomensKwintiel <- recode(
  population$AFLInkomensKwintiel,
  "1e 20% groep" = "1", 
  "2e 20% groep" = "2", 
  "3e 20% groep" = "3", 
  "4e 20% groep" = "4", 
  "5e 20% groep" = "5", 
  "onbekend" = "6"
)


# City
city_map <- c(
  "888" = "Beek",
  "1954" = "Beekdaelen",
  "899" = "Brunssum",
  "1903" = "Eijsden-Margraten",
  "1729" = "Gulpen-Wittem",
  "917" = "Heerlen",
  "928" = "Kerkrade",
  "882" = "Landgraaf",
  "935" = "Maastricht",
  "938" = "Meerssen",
  "965" = "Simpelveld",
  "1883" = "Sittard-Geleen",
  "971" = "Stein",
  "981" = "Vaals",
  "994" = "Valkenburg aan de Geul",
  "986" = "Voerendaal"
)

population$REGGemeentecode <- gsub("^0+", "", population$REGGemeentecode)

# Recode the city codes to match the city names and replace unmatched codes with NA
population$REGGemeentecode <- recode(population$REGGemeentecode, 
                                     !!!city_map, 
                                     .default = NA_character_)

population <- subset(population, !is.na(REGGemeentecode))

table(population$REGGemeentecode)

# Adapt data types
population$AFLInkomensKwintiel = as.integer(population$AFLInkomensKwintiel)
population$REGGemeentecode = as.factor(population$REGGemeentecode)






#######################################
# Random Forest on population register ------------------------------------
#######################################


# 1) Training on the matched dataset

# Consider the matching variables between Population Register and GEZO
gezo_matched <- gezo_29_var[, .(
  PUBRookstatusRoker,               # Smoking status
  HHBGeslachtOP,                    # Gender
  HHBLeeftijdOP = as.numeric(as.character(HHBLeeftijdOP)),  # Convert to numeric inline
  AFLBurgerlijkeStaatOP,            # Marital status
  AFLHHBHHKern,                     # Household composition
  HHBAantalPersonen = as.numeric(as.character(HHBAantalPersonen)),  # Household size
  AFLOpleidingsniveauGevolgd,       # Education level followed, dropped because of missing values in the population
  AFLInkomensKwintiel = as.numeric(AFLInkomensKwintiel),  # Income quintile
  REGGemeentecode
)]

# Verify format compatibility
gezo_matched$PUBRookstatusRoker <- as.factor(gezo_matched$PUBRookstatusRoker)
gezo_matched$AFLBurgerlijkeStaatOP <- as.factor(gezo_matched$AFLBurgerlijkeStaatOP)
gezo_matched$AFLOpleidingsniveauGevolgd <- as.factor(gezo_matched$AFLOpleidingsniveauGevolgd)
gezo_matched$AFLHHBHHKern <- as.factor(gezo_matched$AFLHHBHHKern)

gezo_matched$HHBLeeftijdOP <- as.numeric(gezo_matched$HHBLeeftijdOP)
gezo_matched$HHBAantalPersonen <- as.numeric(gezo_matched$HHBAantalPersonen)
gezo_matched$AFLInkomensKwintiel <- as.numeric(gezo_matched$AFLInkomensKwintiel)


# Labeled data split
set.seed(421)
totalRows <- nrow(gezo_matched)
trainIndex <- sample(1:totalRows, 0.6 * totalRows)
remainingIndex <- setdiff(1:totalRows, trainIndex)
validationIndex <- sample(remainingIndex, 0.5 * length(remainingIndex)) 
testIndex <- setdiff(remainingIndex, validationIndex)

trainData <- gezo_matched[trainIndex, ]
validationData <- gezo_matched[validationIndex, ]
testData <- gezo_matched[testIndex, ]

trainData <- as.data.frame(trainData)
validationData <- as.data.frame(validationData)
testData <- as.data.frame(testData)


# Rebalancing
rose_data_matched <- ovun.sample(
  formula = PUBRookstatusRoker ~ .,
  p = 0.4,
  data = trainData                
)$data 

# Ensure matching
rose_data_matched$HHBLeeftijdOP = as.integer(rose_data$HHBLeeftijdOP)
rose_data_matched$HHBAantalPersonen = as.integer(rose_data$HHBAantalPersonen)
rose_data_matched$AFLInkomensKwintiel = as.integer(rose_data$AFLInkomensKwintiel)
rose_data_matched$AFLHHBHHKern = as.ordered(rose_data_matched$AFLHHBHHKern)

trainData$HHBLeeftijdOP = as.integer(trainData$HHBLeeftijdOP)
trainData$HHBAantalPersonen = as.integer(trainData$HHBAantalPersonen)
trainData$AFLInkomensKwintiel = as.integer(trainData$AFLInkomensKwintiel)
trainData$AFLHHBHHKern = as.ordered(trainData$AFLHHBHHKern)


testData$AFLHHBHHKern = as.ordered(testData$AFLHHBHHKern)
validationData$AFLHHBHHKern = as.ordered(validationData$AFLHHBHHKern)

rose_data_matched = as.data.frame(rose_data_matched)

# Train the random forest model using the rebalanced training data
rf_model_matched <- randomForest(PUBRookstatusRoker ~ ., 
                                 data = rose_data_matched, 
                                 importance = TRUE, 
                                 ntree = 300, 
                                 mtry = 3, 
                                 nodesize = 50)

predictions_rf_matched <- predict(rf_model_matched, testData)
predicted_prob_matched = predict(rf_model_matched, testData, type = "prob")

# Confusion matrix on the test data
confMatrix_rf_matched <- confusionMatrix(predictions_rf_matched, testData$PUBRookstatusRoker)

# Print the confusion matrix
print(confMatrix_rf_matched)


# Since specificity is low, we try to change the threshold
# Since we have an unbalanced training set and resampling seems to be not enough on the Population register,
# we change the threshold to be considered smoker from standard 0.5 to 0.8.

cutoff = 0.6

predicted_labels_matched <- ifelse(predicted_prob_matched[, 2] >= cutoff, "Ja", "Nee")
table(predicted_labels_matched)

# Convert predicted labels to a factor with the same levels as the true labels
predicted_labels_matched <- factor(predicted_labels_matched, levels = levels(testData$PUBRookstatusRoker))

# Confusion Matrix
conf_matrix_rf_matched <- confusionMatrix(predicted_labels_matched, testData$PUBRookstatusRoker)

# Print the confusion matrix
print(conf_matrix_rf_matched)


rf_model_matched$importance



# 2) Predictions on the Population Register




# List of factor variables to align (e.g., categorical variables in population)
categorical_vars <- c("HHBGeslachtOP","AFLBurgerlijkeStaatOP","AFLHHBHHKern","AFLOpleidingsniveauGevolgd","REGGemeentecode")

# Apply factor level alignment to all specified categorical variables
for (var in categorical_vars) {
  population[[var]] <- factor(population[[var]], 
                              levels = levels(rose_data[[var]]))
}


# Get predicted probabilities on the entire population dataset
predicted_prob_population = predict(rf_model_matched, population, type = "prob")

# Apply the new threshold of 0.8 to classify as "Ja" or "Nee"
cutoff = 0.8
predicted_labels_population <- ifelse(predicted_prob_population[, 2] >= cutoff, "Ja", "Nee")

# Print the table of predicted labels

table(predicted_labels_population)





# Final comparisons -------------------------------------------------------


# 1) Direct estimates from the GEZO survey
survey_data <- data.frame(
  SmokerStatus = gezo$PUBRookstatusRoker,
  city = gezo$REGGemeentecode,
  weight = gezo$GEWCorrectieGewicht
)

survey_design <- svydesign(
  ids = ~1,
  data = survey_data,
  weights = ~weight
)

city_smoker_proportions <- svyby(
  ~SmokerStatus,
  ~city,
  design = survey_design,
  svymean
)

direct_table = as.data.frame(city_smoker_proportions[,c(1,3)])

# 2) Random Forest

data <- data.frame(City = population$REGGemeentecode, Label = predicted_labels_population)

ja_nee_counts <- data %>%
  group_by(City, Label) %>%
  tally(wt = Label == "Ja" | Label == "Nee")

proportions <- ja_nee_counts %>%
  spread(Label, n, fill = 0) %>%
  mutate(Proportion = Ja / (Ja + Nee))

rf_table_df <- proportions %>%
  select(City, Proportion)



# 3) Statistical 

table_statistics_glmm <- table(population$REGGemeentecode, predicted_values)

glmm_df <- as.data.frame(table_statistics_glmm)

proportions_glmm <- glmm_df %>%
  group_by(Var1) %>%
  spread(predicted_values, Freq, fill = 0) %>%
  mutate(Proportion = `1` / (`1` + `0`)) %>%
  select(Var1, Proportion) %>%
  rename(City = Var1)

# Merging
direct_table$City <- as.character(direct_table$city)
rf_table_df$City <- as.character(rf_table_df$City)
proportions_glmm$City <- as.character(proportions_glmm$City)

direct_table$SmokerStatusJa <- as.numeric(direct_table$SmokerStatusJa)
rf_table_df$Proportion <- as.numeric(rf_table_df$Proportion)
proportions_glmm$Proportion <- as.numeric(proportions_glmm$Proportion)

merged_df <- Reduce(function(x, y) {
  merge(x, y, by = "City", all = TRUE)
}, list(direct_table, proportions_glmm, rf_table_df))

colnames(merged_df) <- c("City",
                         "City",
                         "Direct Estimates",
                         "Statistical Estimates",
                         "Random Forest Estimates")

merged_df <- merged_df[order(merged_df$City), ]

city_map <- c(
  "Beek",
  "Beekdaelen",
  "Brunssum",
  "Eijsden-Margraten",
  "Gulpen-Wittem",
  "Heerlen",
  "Kerkrade",
  "Landgraaf",
  "Maastricht",
  "Meerssen",
  "Simpelveld",
  "Sittard-Geleen",
  "Stein",
  "Vaals",
  "Valkenburg aan de Geul",
  "Voerendaal"
)

filtered_df <- merged_df[merged_df$City %in% city_map, c(1,3,4,5)]

# Plot
filtered_df$City <- factor(filtered_df$City, levels = filtered_df$City[order(-filtered_df$`Direct Estimates`)])

filtered_df_long <- filtered_df %>%
  gather(key = "Method", value = "Estimate", -City)

ggplot(filtered_df_long, aes(x = City, y = Estimate, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Estimates by Method for Selected Cities",
    x = "City",
    y = "Estimate",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
