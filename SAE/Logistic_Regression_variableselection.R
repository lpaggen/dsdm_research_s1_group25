###This code script is about logistic regression:

# Importing Required Libraries --------------------------------------------
library(haven)
library(data.table)
library(lmtest)
library(car)
library(ResourceSelection)
library(Deducer)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(ggcorrplot)
#Step 0: Data uploading ----------------------------------------------------------

# Directory of the data
directory <- "//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/"

# Read the health survey
gezo <- read_sav(file = paste0(directory, "GEZO_2022_voor_DSC.SAV"))

# Transform the categorical variables into factors
gezo <- as_factor(gezo)

# Turn it into a data.table (faster with computations)
gezo <- data.table(gezo)

# Remove NA variables for the response PUBRookStatusRoker (only responders from 13 on)
gezo=gezo[as.numeric(gezo$HHBLeeftijdOP) > 12,]
gezo <- gezo[!is.na(gezo$PUBRookstatusRoker), ] 

# Selected 'green' variables (11 variables including RookStatus and REGGGD)
gezo_green_var <- gezo[, .(
  AFLWelvaartsKwintiel,       # Welfare quintile
  AFLWerken,                  # Employment status
  PUBRookstatusRoker,         # Smoking status
  HHBGeslachtOP,              # Gender
  HHBLeeftijdOP = as.numeric(HHBLeeftijdOP),  # Correct conversion to numeric
  AFLGeboortelandDriedeling,  # Country of birth (3 categories)
  AFLInkomensKwintiel,        # Income quintile
  AFLLeeftijdsklassenFijn,    # Breakdown of population by age (fine)
  AFLPositieInHuishoudenOP,   # Position in household
  AFLOpleidingsniveauGevolgd, # Education level followed
  REGGGD                      # Regional health authority
)]


# Selected Variables and their meanings -----------------------------------

# All Jeldrik selected variables
gezo_29_var <- gezo[, .(
  PUBRookstatusRoker,         # Smoking status
  HHBGeslachtOP,              # Gender
  HHBLeeftijdOP = as.numeric(as.character(HHBLeeftijdOP)),  # Convert to numeric inline
  AFLLeeftijdsklassenFijn,    # Breakdown of population by age ( by two classes )
  AFLLeeftijdsklassenGrof,    # Breakdown of population by age (by more classes)
  AFLGeboorteJaarOP = as.numeric(as.character(AFLGeboorteJaarOP)),          # Year of birth
  AFLGeboorteMaandOP = as.numeric(as.character(AFLGeboorteMaandOP)),         # Month of birth
  AFLLeeftijdJongsteKind = as.numeric(as.character(AFLLeeftijdJongsteKind)),     # Age of youngest child
  AFLLeeftijdOudsteKind = as.numeric(as.character(AFLLeeftijdOudsteKind)),      # Age of oldest child
  AFLBurgerlijkeStaatOP,      # Marital status
  AFLGeboortelandEnHerkomst,  # Country of birth and parents' origin
  AFLGeboortelandDriedeling,  # Country of birth (3 categories)
  AFLMigratieachtergrond,     # Migration background
  AFLGeneratie,               # Migration generation
  AFLHHBHHKern,               # Household composition
  AFLPositieInHuishoudenOP,   # Position in household
  HHBAantalPersonen = as.numeric(as.character(HHBAantalPersonen)),          # Household size
  AFLOpleidingsniveauGevolgd, # Education level followed
  AFLOpleidingsniveauVoltooid,# Education level completed
  AFLOplNivVoltooidva25jr,    # Education level completed (25+)
  AFLOpleidingsniveauVoltooidva25jr3kl, # Education level (3 grades, 25+)
  AFLInkomensKwintiel,        # Income quintile
  AFLVermogensKwintiel,       # Wealth quintile
  AFLWelvaartsKwintiel,       # Welfare quintile
  AFLWerken,                  # Employment status
  AFLUrenWerk,                # Hours worked
  VRGMaatschappelijkePositie, # Social position
  REGGGD,                     # Regional health authority
  REGGemeentecode             # Municipal code
)]

#Check how many missing values there are in the dataset
colSums(is.na(gezo_29_var))
#This is the result:
#AFLGeboorteJaarOP 7 
#AFLGeboorteMaandOP 7 
#AFLLeeftijdJongsteKind 4972 
#AFLLeeftijdOudsteKind 4971 


#Step 1: Univariate Analysis -----------------------------------------------------
#Our first key step would be to use Univariate Analysis to explore the unadjusted association between variables and the outcome.
#This means that each of the variables will be included in a logistic regression model, one for each time
#
#This is our response variable:
response_variable=gezo$PUBRookstatusRoker

#Define the dependent variables
selected_columns <- c(2:27)
print(selected_columns)

#I want to ensure the data as dataframe
gezo_29_var=as.data.frame(gezo_29_var)

# Loop through each column in the dataset
for (col_name in colnames(gezo_29_var[,c(2:27)])) {
  if (col_name == "PUBRookstatusRoker") {
    next
  }
  model <- glm(gezo_29_var$PUBRookstatusRoker ~ gezo_29_var[[col_name]], 
               family = binomial, data = gezo_29_var)
  cat("\nModel Summary for predictor:", col_name, "\n")
  print(summary(model))
  if (is.factor(gezo_29_var[[col_name]]) || is.character(gezo_29_var[[col_name]])) {
    cat("\nLevels or modalities of predictor:", col_name, ":\n")
    print(unique(gezo_29_var[[col_name]]))
  }
}


# Comments on the Univariate Analysis Variables -----------------------------------------------
#These are comments based on common knowledge and intuition, prior to see the significance of pvalues, and without following the true methodology.
#--
#HHBGeslachtOP : I keep this variable in the model because is highly significative. Does it make sense? yes, the gender makes sense to keep it
#--
#HHBLeeftijdOP : I keep this variable in the model because is highly significative. Does it make sense? yes, the age makes sense to be kept in the model
#AFLLeeftijdsklassenFijn: I keep this variable in the model because all the modalities are highly significative. Does it make sense? yes, the classes of the age make sense to keep in the model 
#AFLLeeftijdsklassenGrof: I keep this variable in the model because all the modalities are highly significative. Does it make sense? yes, the classes of the age make sense to keep in the model
#(NOTE: IT IS BETTER TO CHOOSE BETWEEN ONE OF THESE THREE VARIABLES, BECAUSE THEY ARE ALL REGARDING AGE)
#--
#AFLGeboorteJaarOP:I keep this variable in the model because all the modalities are highly significative. Does it make sense? yes, the born years make sense to keep in the model
#AFLGeboorteMaandOP: I eliminate this variable in the model because all the modalities are not highly significative. Does it make sense? yes, knowing the month is not so useful to keep in the model
#--
#AFLLeeftijdJongsteKind: I eliminate this variable in the model because the modality is not highly significative. Does it make sense? yes, knowing the age of the oldest child is not so useful to keep in the model
#AFLLeeftijdOudsteKind: I eliminate this variable in the model because the modality is not highly significative. Does it make sense? yes, knowing the age of the youngest child is not so useful to keep in the model
#--
#AFLBurgerlijkeStaatOP: I keep this variable in the model because this variable about marital status is useful to know. in practice, if a person is married, it is significative; if someone is widow, is not significative. It is interesting
#--
#AFLGeboortelandEnHerkomst: I keep this variable in the model because quite all the modalities are significative, except Geboren in Nederland, minimaal 1 ouder geboren in buitenland, herkomst Europa. It seems that in a family where minimum one parent is born outside of Netherlands and comes from Europe, is not significative for smoking  
#AFLGeboortelandDriedeling: Is all significative, but should I keep this? because is about the same as the prior variable, but is about only three categories. they are all significative
#--
#AFLMigratieachtergrond: I keep this variable. It is all significative and it makes sense to know about if the migration background is significative to smoking
#AFLGeneratie: is all significative. But should I keep this? i don t see many useful reasons to put a variable about the migration generation
#--
#AFLHHBHHKern: I keep this variable because is interesting to see how the composition of the family is related to smoking, which makes sense. Not all the modalities are significative. Only a couple, and couple with kids seems to be significative to smoking. The rest is not significative
#AFLPositieInHuishoudenOP: This variable which is all significative , having just two exceptions in 13 modalities, is about the household composition. But, if I keep the prior one, should I keep this? I dont think so
#HHBAantalPersonen: is about the houshold size, and is significative. This variable is higly significative, but is numeric. Should I keep it as factor or not, in order to see the different modalities of the houshold size
#--
#AFLOpleidingsniveauGevolgd: is about the educational level followed. Just few of them are significative, and the interpretation makes sense: the people doing hbo, master, and doct are significative to smoking. the rest is not. I can keep this variable
#FLOpleidingsniveauVoltooid: is about educational level followed. two out of the five modalities are not significative; the results are little weird. I dont know if it useful to keep it
#AFLOplNivVoltooidva25jr: is about educational level followed (25+). is all significative, but I don-t see if it is useful to keep it. However the results of the modalities ake sense. Vmbo, mbo1, avo are not significative, while the rest is significative
#AFLOpleidingsniveauVoltooidva25jr3kl: is about educational level followed (25+) in three categories. I would keep this one because the results makes sense
#(NOTE: is better if we choose one of these variables. The latter one maybe better)
#--
#AFLInkomensKwintiel: is about income quantile. All the modalities are significative. it seems to make sense to put it in the model
#AFLVermogensKwintiel: is about Wealth quintile. All the modalities are significative. it seems to make sense to put it in the model. But how do I interpret the health quantile with smoking?
#AFLWelvaartsKwintiel: is about Welfare quintile. All the modalities are significative. it seems to make sense to put it in the model. But how do I interpret the health quantile with smoking?
#AFLWerken: is about Employment status. The two modalities are significative, which makes sense and I can keep it in the model
#AFLUrenWerk: is about Hours worked. two modalities out of four are not significative. This makes sense, and maybe I could create a dummy having in zero the two non significative cluster, and having one in the two significative clusters.
#VRGMaatschappelijkePositie: is about the Social position. Is all significative except scholostic or student and the modality "others". I should keep this because it makes sense and it is interesting related to smoking 
#--

#Step to do After: 
#--
#Variables that do not contribute to the model (like the ones with P value greater than traditional significance level) should be eliminated and a new smaller model fits.
#Then, these two models are compared to make sure that the parsimonious model fits as well as the original model.
#
model1=glm(gezo_29_var$PUBRookstatusRoker~., family = binomial, data = gezo_29_var)
anova(model1,test ="Chisq")

##I try to do a model that eliminates the unnecessary variables. I look at the highest p-values every time and I run all the model again
#I do all manually: 

#first variable eliminated is "REGGementecode", which can make also sense

model1=glm(PUBRookstatusRoker ~ . - REGGemeentecode, family = binomial, data = gezo_29_var)
anova(model1,test ="Chisq")

vif(model1)
#Now, I eliminate as second variable the "REGGGD"
#
model2=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD, family = binomial, data = gezo_29_var)
anova(model2,test ="Chisq")

# I Compare the fit of the model1 with this model 2:
#I use partial likelihood ratio test

lrtest(model1,model2)
anova(model1,model2,test = "Chisq")
#The two model are not statistically significantly different in their fits for data.
#In other words, model2 is as good as model1 in fitting data
#I choose model2 for the principal model of parsimony.
#
##
#Now I eliminate the variable "AFLGeboorteMaandOP"
#
model3=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP, family = binomial, data = gezo_29_var)

lrtest(model2,model3)
anova(model2,model3,test = "Chisq")
#Okay, I can eliminate the variable, and I keep going with model 3

anova(model3,test ="Chisq")
##
#Now I eliminate the variable "AFLGeneratie"
#
model4=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie, family = binomial, data = gezo_29_var)
lrtest(model3,model4)
anova(model3,model4,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 4 
##
anova(model4,test ="Chisq")

#Now I eliminate the variable "AFLLeeftijdOudsteKind"
#
model5=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind, family = binomial, data = gezo_29_var)
lrtest(model4,model5)
anova(model4,model5,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 5 
##
anova(model5,test ="Chisq")

#Now I eliminate the variable "HHBAantalPersonen"
#
model6=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen, family = binomial, data = gezo_29_var)
lrtest(model5,model6)
anova(model5,model6,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 6 
##
anova(model6,test ="Chisq")

#Now I eliminate the variable "AFLInkomensKwintiel"
#
model7=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel, family = binomial, data = gezo_29_var)
lrtest(model6,model7)
anova(model6,model7,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 7 
##
anova(model7,test ="Chisq")

#Now I eliminate the variable "AFLWelvaartsKwintiel"
#
model8=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel, family = binomial, data = gezo_29_var)
lrtest(model7,model8)
anova(model7,model8,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 8 
##
anova(model8,test ="Chisq")

#Now I eliminate the variable "HHBLeeftijdOP"
#
model9=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP, family = binomial, data = gezo_29_var)
lrtest(model8,model9)
anova(model8,model9,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 9 
##
anova(model9,test ="Chisq")

#Now I eliminate the variable "AFLOplNivVoltooidva25jr"
#
model10=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr, family = binomial, data = gezo_29_var)
lrtest(model9,model10)
anova(model9,model10,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 10 
##
anova(model10,test ="Chisq")

#Now I eliminate the variable "AFLOpleidingsniveauVoltooidva25jr3kl"
#which is very very high
model11=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl, family = binomial, data = gezo_29_var)
lrtest(model10,model11)
anova(model10,model11,test = "Chisq")
#okay, I can eliminate it, and I keep going with model 11 
##
anova(model11,test ="Chisq")

#Now I eliminate the variable "AFLUrenWerk"
#which is very very high
model12=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk, family = binomial, data = gezo_29_var)
lrtest(model11,model12)
anova(model11,model12,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.11
#I try to eliminate it and see what happens then
#
anova(model12,test ="Chisq")
#

#Now I eliminate the variable "AFLMigratieachtergrond"
#which is very very high
model13=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond, family = binomial, data = gezo_29_var)
lrtest(model12,model13)
anova(model12,model13,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.04
#So, I do not eliminate it 
#
anova(model13,test ="Chisq")
#

##Result: I think the model that I have to use is the model 12

#The model 12 has the following variables included:

# Study of the remained variables -----------------------------------------

#HHBGeslachtOP               7.444e-05 ***  (makes sense to remain)
#AFLLeeftijdsklassenFijn     < 2.2e-16 ***  (makes sense to remain just one of this 1)
#AFLLeeftijdsklassenGrof     0.0009222 ***  (makes sense to remain just one of this 1)
#AFLGeboorteJaarOP           0.1021206      (makes sense to remain)
#AFLLeeftijdJongsteKind      0.0210588 *    (makes sense to eliminate this variable)
#AFLBurgerlijkeStaatOP       3.163e-07 ***  (makes sense to remain)
#AFLGeboortelandEnHerkomst   0.0800558 .    (makes sense to remain)  
#AFLGeboortelandDriedeling                  (makes sense to eliminate this variable)
#AFLHHBHHKern                0.0341137 *    (makes sense to remain)  
#AFLPositieInHuishoudenOP    0.0006922 ***  (makes sense to think to eliminate, because is about similar thing to the prior one)
#AFLOpleidingsniveauGevolgd  < 2.2e-16 ***  (makes sense to remain, because is about education followed)
#AFLOpleidingsniveauVoltooid 0.0099554 **   (makes sense to remain, because is about education completed. However, if it is necessary, I can eliminate it)
#AFLVermogensKwintiel        0.0013178 **   (makes sense to remain)
#AFLWerken                   0.0075584 **   (makes sense to remain)
#VRGMaatschappelijkePositie  0.0080956 **   (makes sense to remain)

#At this point, I can try what happens if I eliminate the ones that are not important to my study.
#
#I eliminate the variable "AFLGeboortelandDriedeling"
model13=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling, family = binomial, data = gezo_29_var)
lrtest(model12,model13)
anova(model12,model13,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.047
#So, I it is a borderline case, but I still decide to eliminate it 
#
anova(model13,test ="Chisq")

##I continue the process of eliminating
#I eliminate the variable "AFLLeeftijdJongsteKind"
model14=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind, family = binomial, data = gezo_29_var)
lrtest(model13,model14)
anova(model13,model14,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.03
#So, it is a borderline case, but I still decide to eliminate it 
#
anova(model14,test ="Chisq")

##From the anova, I still see that a variable has become non significative anymore
#This variable is "AFLPositieInHuishoudenOP", which was the one that I wanted to eliminate
#So, I eliminate the variable "AFLPositieInHuishoudenOP"
model15=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP, family = binomial, data = gezo_29_var)
lrtest(model14,model15)
anova(model14,model15,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.62
#So, I decide to eliminate it
#
anova(model15,test ="Chisq")

#
#The result at this point is:
#  HHBGeslachtOP               7.444e-05 ***
#  AFLLeeftijdsklassenFijn     < 2.2e-16 ***
#  AFLLeeftijdsklassenGrof     0.0009222 ***
#  AFLGeboorteJaarOP           0.1021206    
#  AFLBurgerlijkeStaatOP       8.362e-08 ***
#  AFLGeboortelandEnHerkomst   0.0831579 .  
#  AFLHHBHHKern                0.0255759 *  
#  AFLOpleidingsniveauGevolgd  < 2.2e-16 ***
#  AFLOpleidingsniveauVoltooid 0.0162318 *  
#  AFLVermogensKwintiel        0.0003685 ***
#  AFLWerken                   0.0055461 ** 
#  VRGMaatschappelijkePositie  0.0067237 ** 


##At this point, there is the last thing to check, which is to remain only one variable between:
#AFLLeeftijdsklassenFijn     < 2.2e-16 ***
#AFLLeeftijdsklassenGrof     0.0009222 ***
# 

#I try eliminating "AFLLeeftijdsklassenGrof":

model16=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenGrof, family = binomial, data = gezo_29_var)
lrtest(model15,model16)
anova(model15,model16,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.0001
#So, I think is better to maintain it.
anova(model16,test ="Chisq")

##At this point, I try to check what happens if I eliminate the other one
#I try eliminating "AFLLeeftijdsklassenFijn":

model17=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn, family = binomial, data = gezo_29_var)
lrtest(model15,model17)
anova(model15,model17,test = "Chisq")
#okay,  if I eliminate it, I obtain a pvalue of 0.4044
#So, I think I can eliminate it.
anova(model17,test ="Chisq")
--
#By the end, the model seems to be this one:
#                               Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                         3765     3225.5              
#  HHBGeslachtOP                1   15.694      3764     3209.8 7.444e-05 ***
#  AFLLeeftijdsklassenGrof      1   65.939      3763     3143.9 4.652e-16 ***
#  AFLGeboorteJaarOP            1   25.497      3762     3118.4 4.431e-07 ***
#  AFLBurgerlijkeStaatOP        3   37.008      3759     3081.4 4.584e-08 ***
#  AFLGeboortelandEnHerkomst    4    7.752      3755     3073.6  0.101088    
#  AFLHHBHHKern                 1    5.526      3754     3068.1  0.018737 *  
#  AFLOpleidingsniveauGevolgd   5   87.902      3749     2980.2 < 2.2e-16 ***
#  AFLOpleidingsniveauVoltooid  4   13.603      3745     2966.6  0.008675 ** 
#  AFLVermogensKwintiel         5   26.392      3740     2940.2 7.489e-05 ***
#  AFLWerken                    2   30.267      3738     2909.9 2.677e-07 ***
#  VRGMaatschappelijkePositie   6   20.424      3732     2889.5  0.002327 ** 

#Deviance:2888.515
deviance(model17)

#AIC:2957.515
AIC(model17)

#Goodness of fit R squared: 0.10417
1-(model17$deviance /model17$null.deviance)


#Deviance residuals
dev_residuals_model=residuals(model17,type = "deviance")


#Plot deviance residuals
plot(dev_residuals_model, main="Deviance Residuals", ylab="Residuals",xlab="Index")
abline(h=0,col="red")


qqnorm(dev_residuals_model,main = "QQ Plot of Deviance Residuals")
qqline(dev_residuals_model,col="red")

##Another Residual diagnostic
cooks_distance=cooks.distance(model17)
plot(cooks_distance, main="Cook's Distance")

#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)
#In general, if most points are very low, it suggest that the majority of the observations have little influence on the model's coefficients
#If the values are relatively small and few points are extremely influential, the model appears stable and not overly sensitive to individual data points
#

#ROC Curve: 0.72
library(Deducer)
rocplot(model17)
#
#Hoslem Test: Goodness of fit test
hoslem.test(model17$y,fitted(model17))
#The p-value is 0.569, indicating that there is no significant difference between observed and predicted values.
#
#
#
#
#
# Step 3: Linearity Assumption --------------------------------------------
#In this step, continuous variables are checked for their linearity in relation to the logit of the outcome
#I can use smoother scatterplot to analyze it.
#For the scatterplot I want that smoking is numeric
gezo_29_var$PUBRookstatusRoker=as.numeric(gezo_29_var$PUBRookstatusRoker)


#But, in this case it has values 1 and 2. So I have to transform it in order to have 0 and 1
gezo_29_var$PUBRookstatusRoker=ifelse(gezo_29_var$PUBRookstatusRoker==2,1,0)
print(gezo_29_var$PUBRookstatusRoker)

length(gezo_29_var$PUBRookstatusRoker)
#I do the scatterplots, but before I check which are the numeric variables
numeric_vars=sapply(gezo_29_var,is.numeric)
names(gezo_29_var)[numeric_vars]

##the numerical variables in the dataset are:
#"PUBRookstatusRoook",HHBLeeftijdOP"         
#"AFLGeboorteJaarOP","AFLGeboorteMaandOP"    
# "AFLLeeftijdJongsteKind" "AFLLeeftijdOudsteKind" 
# "HHBAantalPersonen"  

#So, in my model that I ve obtained, I have only one numerical variable
#The variable is "AFLGeboorteJaarOP"

#I check if some variables are NA
sum(is.na(gezo_29_var$AFLGeboorteJaarOP))
sum(is.na(gezo_29_var$PUBRookstatusRoker))

#Obtain the scatterplot and look at the distribution
scatter.smooth(na.omit(gezo_29_var$AFLGeboorteJaarOP,log(gezo_29_var$PUBRookstatusRoker)/(1-gezo_29_var$PUBRookstatusRoker)),cex=0.5, col="black", lpars = list(col="red", lwd=2.4))
#
#The scatterplot is showing the relationship between variable of interest with smoking outcome in logit scale


##Anyways, in order to do exploratory research, I do the scatterplot of the other variables that are not included in the model
#Relation is linear
scatter.smooth(na.omit(gezo_29_var$HHBLeeftijdOP,log(gezo_29_var$PUBRookstatusRoker)/(1-gezo_29_var$PUBRookstatusRoker)),cex=0.5, col="black", lpars = list(col="red", lwd=2.4))
#Relation weird
scatter.smooth(na.omit(gezo_29_var$AFLGeboorteMaandOP,log(gezo_29_var$PUBRookstatusRoker)/(1-gezo_29_var$PUBRookstatusRoker)),cex=0.5, col="black", lpars = list(col="red", lwd=2.4))
#Relation is linear
scatter.smooth(na.omit(gezo_29_var$AFLLeeftijdJongsteKind,log(gezo_29_var$PUBRookstatusRoker)/(1-gezo_29_var$PUBRookstatusRoker)),cex=0.5, col="black", lpars = list(col="red", lwd=2.4))
#Relation is linear
scatter.smooth(na.omit(gezo_29_var$AFLLeeftijdOudsteKind,log(gezo_29_var$PUBRookstatusRoker)/(1-gezo_29_var$PUBRookstatusRoker)),cex=0.5, col="black", lpars = list(col="red", lwd=2.4))

--

# Step 4: Interaction among covariates ------------------------------------
#I check for potential interactions betwen covariates
#This is the model that I have until this point:

#                               Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                         3765     3225.5              
#  HHBGeslachtOP                1   15.694      3764     3209.8 7.444e-05 ***  (gender) is okay
#  AFLLeeftijdsklassenGrof      1   65.939      3763     3143.9 4.652e-16 ***  (age) ?
#  AFLGeboorteJaarOP            1   25.497      3762     3118.4 4.431e-07 ***  (year of birth)?
#  AFLBurgerlijkeStaatOP        3   37.008      3759     3081.4 4.584e-08 ***  (marital status) ?
#  AFLGeboortelandEnHerkomst    4    7.752      3755     3073.6  0.101088     (country of birth and heritage) makes sense to put it. r squared remains the same
#  AFLHHBHHKern                 1    5.526      3754     3068.1  0.018737 *   (household core structure)
#  AFLOpleidingsniveauGevolgd   5   87.902      3749     2980.2 < 2.2e-16 ***  (educational level followed)
#  AFLOpleidingsniveauVoltooid  4   13.603      3745     2966.6  0.008675 **   (educational level completed)
#  AFLVermogensKwintiel         5   26.392      3740     2940.2 7.489e-05 ***  (wealth quantile) is okay
#  AFLWerken                    2   30.267      3738     2909.9 2.677e-07 ***  (employment status) is okay, but penalized r squared
#  VRGMaatschappelijkePositie   6   20.424      3732     2889.5  0.002327 **  (socioeconomic position) do not put

#Before, I just look how the dependent variable might be in correspondance with the covariates
table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLGeboortelandEnHerkomst)
table( gezo_29_var$AFLHHBHHKern)
table(gezo_29_var$AFLBurgerlijkeStaatOP)
table(gezo_29_var$AFLGeboorteJaarOP)

#Substitue the unknown modality of the variables that I'm interested:
gezo_29_var$AFLVermogensKwintiel[gezo_29_var$AFLVermogensKwintiel == "onbekend"] <- "derde kwintiel"
gezo_29_var$AFLWerken[gezo_29_var$AFLWerken == "N.v.t. (< 15 jaar)"] <- "Persoon is niet werkzaam"



#I present the relation of the outcome and the variables of the model:
table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLLeeftijdsklassenGrof)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLGeboorteJaarOP)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLBurgerlijkeStaatOP)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLHHBHHKern)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLOpleidingsniveauGevolgd)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLOpleidingsniveauVoltooid)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLVermogensKwintiel)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$AFLWerken)

table(gezo_29_var$PUBRookstatusRoker, gezo_29_var$VRGMaatschappelijkePositie)

##All makes sense, but I am thinking that I could eliminate one of the two variables about school education; this is because they are very similar.
## Let s check what happens if I eliminate the variable "AFLOpleidingsniveauVoltooid"
model18=glm(PUBRookstatusRoker ~ .  - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn - AFLOpleidingsniveauVoltooid, family = binomial, data = gezo_29_var)

#I do the fit checks
anova(model18)
lrtest(model17,model18)

#AIC
AIC(model18)

#Okay, it says that the models are statistically different.
##Still need to decide if I keep it or not.

##---
##Let's start by defining potential interactions
anova(model17)



# Interactions ------------------------------------------------------------

##The first that comes to my mind is that age and gender can have interaction for smoking.
#So, I try HHBGeslachtOP*AFLLeeftijdsklassenGrof
model17_interactions=glm(PUBRookstatusRoker ~ .  - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn + HHBGeslachtOP:AFLLeeftijdsklassenGrof, family = binomial, data = gezo_29_var)
anova(model17_interactions)

lrtest(model17,model17_interactions)

#For every model that I obtain, I do the fit checks

#Deviance:2880.924
deviance(model17_interactions)

#AIC:2950.924
AIC(model17_interactions)

#Goodness of fit R squared: 0.106
1-(model17_interactions$deviance /model17_interactions$null.deviance)


#Deviance residuals
dev_residuals_model=residuals(model17_interactions,type = "deviance")


#Plot deviance residuals
plot(dev_residuals_model, main="Deviance Residuals", ylab="Residuals",xlab="Index")
abline(h=0,col="red")


qqnorm(dev_residuals_model,main = "QQ Plot of Deviance Residuals")
qqline(dev_residuals_model,col="red")

##Another Residual diagnostic
cooks_distance=cooks.distance(model17_interactions)
plot(cooks_distance, main="Cook's Distance")

#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)
#In my case, most points are very low, which suggest that the majority of the observations have little influence on the model's coefficients
#If the values are relatively small and few points are extremely influential, the model appears stable and not overly sensitive to individual data points
#

#ROC Curve: 0.7269
library(Deducer)
rocplot(model17_interactions)
#
#Hoslem Test: Goodness of fit test
hoslem.test(model17_interactions$y,fitted(model17_interactions))
#The p-value is 0.1679, indicating that there is no significant difference between observed and predicted values.


#In fact the interaction term is higly significative, and this makes sense.
#I keep this interaction in the model


##Second interaction: Another interaction is from the educational level and Socioeconomic position
#I try the interaction AFLOpleidingsniveauGevolgd*VRGMaatschappelijkePositie

model18_interactions=glm(PUBRookstatusRoker ~ .  - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn + HHBGeslachtOP:AFLLeeftijdsklassenGrof + AFLOpleidingsniveauGevolgd:VRGMaatschappelijkePositie, family = binomial, data = gezo_29_var)
anova(model18_interactions)

lrtest(model17,model18_interactions)

##The interaction is not significative, and also the lr test suggests that it makes no sense to put it 

##Another interaction is from the employment status and gender
#I try the interaction AFLWerken  * HHBGeslachtOP

model18_interactions=glm(PUBRookstatusRoker ~ .  - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn + HHBGeslachtOP:AFLLeeftijdsklassenGrof + AFLWerken  * HHBGeslachtOP, family = binomial, data = gezo_29_var)
anova(model18_interactions)

lrtest(model17_interactions,model18_interactions)
lrtest(model17,model18_interactions)
###For the test it makes sense to introduce this interaction.
#I keep the interaction in the model

#For every model that I obtain, I do the fit checks

#Deviance:2874.788
deviance(model18_interactions)

#AIC:2948.788
AIC(model18_interactions)

#Goodness of fit R squared: 0.109
1-(model18_interactions$deviance /model18_interactions$null.deviance)


#Deviance residuals
dev_residuals_model=residuals(model18_interactions,type = "deviance")


#Plot deviance residuals
plot(dev_residuals_model, main="Deviance Residuals", ylab="Residuals",xlab="Index")
abline(h=0,col="red")


qqnorm(dev_residuals_model,main = "QQ Plot of Deviance Residuals")
qqline(dev_residuals_model,col="red")

##Another Residual diagnostic
cooks_distance=cooks.distance(model18_interactions)
plot(cooks_distance, main="Cook's Distance")

#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)
#In my case, most points are very low, which suggest that the majority of the observations have little influence on the model's coefficients
#If the values are relatively small and few points are extremely influential, the model appears stable and not overly sensitive to individual data points
#

#ROC Curve: 0.72
library(Deducer)
rocplot(model18_interactions)
#
#Hoslem Test: Goodness of fit test
hoslem.test(model18_interactions$y,fitted(model18_interactions))
#The p-value is 0.2248, indicating that there is no significant difference between observed and predicted values.




##Another interaction is from the Age and wealth quantile
#I try the interaction AFLLeeftijdsklassenGrof  * AFLVermogensKwintiel

model19_interactions=glm(PUBRookstatusRoker ~ .  - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn + HHBGeslachtOP:AFLLeeftijdsklassenGrof + AFLWerken  : HHBGeslachtOP + AFLLeeftijdsklassenGrof  : AFLVermogensKwintiel, family = binomial, data = gezo_29_var)
anova(model19_interactions)

exp(coef(model19_interactions))

#anova(model19_interactions$coefficients)

lrtest(model18_interactions,model19_interactions)
lrtest(model17,model19_interactions)
##Also for this case, I can keep this interaction.

#--Fit check for the model with interactions
#As a fit check, I look at the AIC: 2947.368 for the interaction model
AIC(model17,model19_interactions)


##Plot the ROC Curve:0.7308
rocplot(model19_interactions)

#For every model that I obtain, I do the fit checks

#Deviance:2863.368
deviance(model19_interactions)

#AIC:2947.368
AIC(model19_interactions)

#Goodness of fit R squared: 0.113
1-(model19_interactions$deviance /model19_interactions$null.deviance)


#Deviance residuals
dev_residuals_model=residuals(model19_interactions,type = "deviance")


#Plot deviance residuals
plot(dev_residuals_model, main="Deviance Residuals", ylab="Residuals",xlab="Index")
abline(h=0,col="red")


qqnorm(dev_residuals_model,main = "QQ Plot of Deviance Residuals")
qqline(dev_residuals_model,col="red")

##Another Residual diagnostic
cooks_distance=cooks.distance(model19_interactions)
plot(cooks_distance, main="Cook's Distance")

#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)
#In my case, most points are very low, which suggest that the majority of the observations have little influence on the model's coefficients
#If the values are relatively small and few points are extremely influential, the model appears stable and not overly sensitive to individual data points
#

#ROC Curve: 0.7308
library(Deducer)
rocplot(model19_interactions)
#
#Hoslem Test: Goodness of fit test
hoslem.test(model19_interactions$y,fitted(model19_interactions))
#The p-value is 0.3979, indicating that there is no significant difference between observed and predicted values.


#In general, I cans see that inserting the interactions does not drastically improves the model.
#Yes, the model has really slightly slightly improved, however in this case the interactions do not seem to be as much the best tool for improving the fit of the model.
#
#So, we decide to keep working with the model without interactions, in order to keep it "simpler" and avoid overcomplications.

##Multicollinearity check -------------------------------------------------
#Multicollinearity can be checked using the Variance Inflation Factor, which reports how much of the variance in a coefficient is inflated due to multicollinearity.
#Any value above 5 or 10 is considered problematic
#
#This is the model that I have without interactions:
model17=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn - AFLGeboorteJaarOP - AFLOpleidingsniveauVoltooid, family = binomial, data = gezo_29_var)

#                               Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                         3765     3225.5              
#  HHBGeslachtOP                1   15.694      3764     3209.8 7.444e-05 ***  (gender)
#  AFLLeeftijdsklassenGrof      1   65.939      3763     3143.9 4.652e-16 ***  (age)
#  AFLGeboorteJaarOP            1   25.497      3762     3118.4 4.431e-07 ***  (year of birth)????
#  AFLBurgerlijkeStaatOP        3   37.008      3759     3081.4 4.584e-08 ***  (marital status)
#  AFLGeboortelandEnHerkomst    4    7.752      3755     3073.6  0.101088     (country of birth and heritage)
#  AFLHHBHHKern                 1    5.526      3754     3068.1  0.018737 *   (household core structure)
#  AFLOpleidingsniveauGevolgd   5   87.902      3749     2980.2 < 2.2e-16 ***  (educational level followed)
#  AFLOpleidingsniveauVoltooid  4   13.603      3745     2966.6  0.008675 **   (educational level completed)???
#  AFLVermogensKwintiel         5   26.392      3740     2940.2 7.489e-05 ***  (wealth quantile)
#  AFLWerken                    2   30.267      3738     2909.9 2.677e-07 ***  (employment status)
#  VRGMaatschappelijkePositie   6   20.424      3732     2889.5  0.002327 **  (socioeconomic position)????


table(gezo_29_var$AFLWerken,gezo_29_var$VRGMaatschappelijkePositie)

#I do again the anova test:
anova(model17,test = "Chisq")

#From the vif check, it results that some independent variables are highly correlated. So, we decide to build again (starting from the obtained model, such as the model17) the model step by step in order to know which variables are the one correlated.
#In practice, we checked which variable did not handle the hypotesys of not being correlated between them; by consequence, we decide to "drop" them.
vif(model17)

#This is the model obtained, where the variables are not correlated:
model_try=glm(PUBRookstatusRoker ~  HHBGeslachtOP+AFLLeeftijdsklassenGrof+AFLGeboorteJaarOP+AFLBurgerlijkeStaatOP+AFLGeboortelandEnHerkomst+AFLHHBHHKern+AFLOpleidingsniveauGevolgd+AFLVermogensKwintiel+AFLWerken, family = binomial, data = gezo_29_var)
anova(model_try)

#                             Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                        8737     7690.0              
#  HHBGeslachtOP               1   45.266      8736     7644.8 1.720e-11 ***
#  AFLLeeftijdsklassenGrof     1   70.561      8735     7574.2 < 2.2e-16 ***
#  AFLGeboorteJaarOP           1  183.769      8734     7390.4 < 2.2e-16 ***
#  AFLBurgerlijkeStaatOP       3  113.343      8731     7277.1 < 2.2e-16 ***
#  AFLGeboortelandEnHerkomst   4   19.370      8727     7257.7 0.0006648 ***
#  AFLHHBHHKern                7   30.379      8720     7227.3 8.089e-05 ***
#  AFLOpleidingsniveauGevolgd  5  134.074      8715     7093.3 < 2.2e-16 ***
#  AFLVermogensKwintiel        4  109.263      8711     6984.0 < 2.2e-16 ***
#  AFLWerken                   2   37.268      8709     6946.7 8.079e-09 ***




#I ve taken out GeboorteJaarOP; this is because we have another variable with the same meaning, but with different modalities.
model_try_1=glm(PUBRookstatusRoker ~  HHBGeslachtOP+AFLLeeftijdsklassenGrof+AFLBurgerlijkeStaatOP+AFLGeboortelandEnHerkomst+AFLHHBHHKern+AFLOpleidingsniveauGevolgd+AFLVermogensKwintiel+AFLWerken, family = binomial, data = gezo_29_var)
anova(model_try_1)
#Response: PUBRookstatusRoker

#                             Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                        8744     7692.5              
#  HHBGeslachtOP               1   45.307      8743     7647.2 1.684e-11 ***
#  AFLLeeftijdsklassenGrof     1   71.897      8742     7575.3 < 2.2e-16 ***
#  AFLBurgerlijkeStaatOP       3  229.067      8739     7346.2 < 2.2e-16 ***
#  AFLGeboortelandEnHerkomst   4   26.428      8735     7319.8 2.593e-05 ***
#  AFLHHBHHKern                7   26.168      8728     7293.6 0.0004701 ***
#  AFLOpleidingsniveauGevolgd  5  103.480      8723     7190.1 < 2.2e-16 ***
#  AFLVermogensKwintiel        4  142.878      8719     7047.3 < 2.2e-16 ***
#  AFLWerken                   2   44.073      8717     7003.2 2.689e-10 ***

vif(model_try_1)

#                               GVIF Df GVIF^(1/(2*Df))
#HHBGeslachtOP              1.038581  1        1.019108
#AFLLeeftijdsklassenGrof    1.280064  1        1.131399
#AFLBurgerlijkeStaatOP      2.123472  3        1.133725
#AFLGeboortelandEnHerkomst  1.240994  4        1.027357
#AFLHHBHHKern               2.174028  7        1.057037
#AFLOpleidingsniveauGevolgd 1.381112  5        1.032816
#AFLVermogensKwintiel       1.259704  4        1.029280
#AFLWerken                  1.508454  2        1.108238


vif(model_try)

#                                GVIF Df GVIF^(1/(2*Df))
#HHBGeslachtOP              1.045595  1        1.022543
#AFLLeeftijdsklassenGrof    1.368695  1        1.169912
#AFLGeboorteJaarOP          3.199276  1        1.788652
#AFLBurgerlijkeStaatOP      3.467587  3        1.230282
#AFLGeboortelandEnHerkomst  1.254880  4        1.028787
#AFLHHBHHKern               2.504899  7        1.067788
#AFLOpleidingsniveauGevolgd 1.440965  5        1.037207
#AFLVermogensKwintiel       1.301823  4        1.033520
#AFLWerken                  1.685977  2        1.139496

#Diagnostics of the model
library(pscl)
pR2(model_try)


library(pscl)
pR2(model_try_1)


#I do anyways the fit checks of the model_try:


#Goodness of fit R squared: 0.0966
1-(model_try$deviance /model_try$null.deviance)

#Goodness of fit R squared: 0.0896
1-(model_try_1$deviance /model_try_1$null.deviance)


#Deviance residuals
dev_residuals_model=residuals(model_try,type = "deviance")

fitted_values=model_try$fitted.values

ggplot(data.frame(Fitted=fitted_values, Residuals=dev_residuals_model),aes(x=Fitted, y=Residuals))+
  geom_point(alpha=0.7)+
  geom_hline(yintercept = 0,color="red",linetype="dashed")+
  labs(title="Residuals vs Fitted Values",
       x="Fitted Values",
       y="Deviance Residuals")+
  theme_minimal()


#Comparison with normal dstr.
ggplot(data.frame(Residuals=dev_residuals_model),aes(x=Residuals))+
  geom_density(color="blue",fill="blue", alpha=0.3)+
  stat_function(fun=dnorm,args=list(mean=mean(dev_residuals_model), sd=sd(dev_residuals_model)),
                color="red",linetype="dashed")+
  labs(title="Density of Deviance Residuals vs Normal Distribution",
       x="Residuals",
       y="Density")+
  theme_minimal()



##These plot are for the model_try_1 (the model without the variable GeboorteJaarOP):

#Deviance residuals
dev_residuals_model_try_1=residuals(model_try_1,type = "deviance")

fitted_values_model_try_1=model_try_1$fitted.values

ggplot(data.frame(Fitted=fitted_values_model_try_1, Residuals=dev_residuals_model),aes(x=Fitted, y=Residuals))+
  geom_point(alpha=0.7)+
  geom_hline(yintercept = 0,color="red",linetype="dashed")+
  labs(title="Residuals vs Fitted Values",
       x="Fitted Values",
       y="Deviance Residuals")+
  theme_minimal()



ggplot(data.frame(Residuals=dev_residuals_model_try_1),aes(x=Residuals))+
  geom_density(fill="blue",alpha=0.3)+
  labs(title="Density Plot of Deviance Residuals",
       x="residuals",
       y="Density")+
  theme_minimal()


qqnorm(dev_residuals_model_try_1,main="QQ Plot of Deviance Residuals")
qqline(dev_residuals_model_try_1)


##Another Residual diagnostic
cooks_distance=cooks.distance(model_try)
plot(cooks_distance, main="Cook's Distance")
#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)


##Another Residual diagnostic for model_try_1
cooks_distance=cooks.distance(model_try_1)
plot(cooks_distance, main="Cook's Distance")
#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)



#Roc Curve:0.7194
rocplot(model_try)

#ROC Curve modeltry1: 0.7119
rocplot(model_try_1)

#Hoslem Test: Goodness of fit test
hoslem.test(model_try$y,fitted(model_try))
#The p-value is 0.0371, very close to the borderline.


#Hoslem Test for model_try_1: Goodness of fit test
hoslem.test(model_try_1$y,fitted(model_try_1))
#The p-value is 0.1693, indicating that there is not significant difference between observed and predicted values.


#--Transormations: Interactions in the model without multicollinearity:
#This is the model that I have:
model_try=glm(PUBRookstatusRoker ~  HHBGeslachtOP+AFLLeeftijdsklassenGrof+AFLGeboorteJaarOP+AFLBurgerlijkeStaatOP+AFLGeboortelandEnHerkomst+AFLHHBHHKern+AFLOpleidingsniveauGevolgd+AFLVermogensKwintiel+AFLWerken, family = binomial, data = gezo_29_var)
anova(model_try)

#I could drop the variable: AFLLeeftijdsklassenGrof

#                              Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                        8737     7690.0              
#  HHBGeslachtOP               1   45.266      8736     7644.8 1.720e-11 *** linear relation
#  AFLLeeftijdsklassenGrof     1   70.561      8735     7574.2 < 2.2e-16 *** linear relation
#  AFLGeboorteJaarOP           1  183.769      8734     7390.4 < 2.2e-16 ***
#  AFLBurgerlijkeStaatOP       3  113.343      8731     7277.1 < 2.2e-16 *** not linear
#  AFLGeboortelandEnHerkomst   4   19.370      8727     7257.7 0.0006648 *** not linear
#  AFLHHBHHKern                7   30.379      8720     7227.3 8.089e-05 *** not linear
#  AFLOpleidingsniveauGevolgd  5  134.074      8715     7093.3 < 2.2e-16 *** not so linear
#  AFLVermogensKwintiel        5  111.605      8710     6981.7 < 2.2e-16 *** linear relation
#  AFLWerken                   2   37.111      8708     6944.6 8.737e-09 *** linear relation


#Now, I will check manually the relation of every variable beween our varaibale of interest
#As a result, the variables

#Smoking and Gender
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$HHBGeslachtOP)

ggplot(gezo_29_var,aes(x=HHBGeslachtOP,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()

model_try_inte=glm(PUBRookstatusRoker~HHBGeslachtOP, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#Relation in decreasing. Men tend more to smoke compared to women

--#Smoking and years (in classes)--
#There are two classes
library(vcd)
library(effects)
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLLeeftijdsklassenGrof)

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLLeeftijdsklassenGrof+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#

model_try_inte=glm(PUBRookstatusRoker~AFLLeeftijdsklassenGrof, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#
ggplot(gezo_29_var,aes(x=AFLLeeftijdsklassenGrof,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()

#I can also do a chi squared test, in order to measure the association between a categorical predictor and a binary response
chisq.test(table(gezo_29_var$AFLLeeftijdsklassenGrof, gezo_29_var$PUBRookstatusRoker))

#relation is linear but is increasing. persons between 12 and 17 years old tend to smoke less compared to the ones of age 18 or older.
--


--#Smoking and years (continuous variable)--
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLGeboorteJaarOP)


model_try_inte=glm(PUBRookstatusRoker~AFLGeboorteJaarOP, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLGeboorteJaarOP+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)


ggplot(gezo_29_var,aes(x=AFLGeboorteJaarOP,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()



--#Smoking and marital Status--  (drop from here the ones that are less than 18)
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLBurgerlijkeStaatOP)

model_try_inte=glm(PUBRookstatusRoker~AFLBurgerlijkeStaatOP, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLBurgerlijkeStaatOP+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLBurgerlijkeStaatOP,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()

##The relation is not linear . Up down up.   is low in married, then grows for divorced; then, decreased for wedow; then increases for people of never been married


--#Smoking and born country and where family comes from--
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLGeboortelandEnHerkomst)

model_try_inte=glm(PUBRookstatusRoker~AFLGeboortelandEnHerkomst, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLGeboortelandEnHerkomst+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLGeboortelandEnHerkomst,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()


#The relation is quite linear. Just for the last modality it decreases slightly


#--Smoking and household structure--
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLHHBHHKern)

model_try_inte=glm(PUBRookstatusRoker~AFLHHBHHKern, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLHHBHHKern+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLHHBHHKern,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()


#This relation is not linear


#--Smoking and education level followed--
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLOpleidingsniveauGevolgd)

model_try_inte=glm(PUBRookstatusRoker~AFLOpleidingsniveauGevolgd, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLOpleidingsniveauGevolgd+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLOpleidingsniveauGevolgd,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()

ggplot(gezo_29_var,aes(x=AFLOpleidingsniveauGevolgd,fill = as.factor(PUBRookstatusRoker)))+
  geom_point(position=position_jitter(width = 0.1,height = 0.02),alpha=0.5)+
  geom_smooth(method = "loess",se=FALSE,color="blue")+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()



##The relation doesn-t seems so linear


#--Smoking and wealth quintile--
#Substitue the unknown in quintiles
gezo_29_var$AFLVermogensKwintiel[gezo_29_var$AFLVermogensKwintiel == "onbekend"] <- "derde kwintiel"


table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLVermogensKwintiel)

model_try_inte=glm(PUBRookstatusRoker~AFLVermogensKwintiel, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLVermogensKwintiel+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLVermogensKwintiel,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()


#The relation is decreasingly linear


#--Smoking and working status--
table(gezo_29_var$PUBRookstatusRoker,gezo_29_var$AFLWerken)

model_try_inte=glm(PUBRookstatusRoker~AFLWerken, data = gezo_29_var, family = binomial)
plot(allEffects(model_try_inte))

#This visualizes the contingency table between the response variable and my independent variable
mosaic(~AFLWerken+PUBRookstatusRoker, data=gezo_29_var, shade=TRUE)

#
ggplot(gezo_29_var,aes(x=AFLWerken,fill = as.factor(PUBRookstatusRoker)))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(y="Proportion", fill="Smoking")+
  theme_minimal()



#--The relation is linear.
##
#
#
#
##For curiosity, let's see what happens if I drop the variables that do not have a linear relation with the outcome:
#Probably, I would expect that there is more relation with the oucome.
#This is the model with just only supposed linear relations:

# Fit the model without non-linear variables
model_linear_only <- glm(PUBRookstatusRoker ~ HHBGeslachtOP + AFLLeeftijdsklassenGrof + 
                           AFLGeboorteJaarOP + AFLVermogensKwintiel + AFLWerken, 
                         data = gezo_29_var, family = binomial())

# Summary of the new model
summary(model_linear_only)

#Call:
#  glm(formula = PUBRookstatusRoker ~ HHBGeslachtOP + AFLLeeftijdsklassenGrof + 
#        AFLGeboorteJaarOP + AFLVermogensKwintiel + AFLWerken, family = binomial(), 
#      data = gezo_29_var)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                             -38.912124   4.080065  -9.537  < 2e-16 ***
#  HHBGeslachtOPVrouw                                       -0.458786   0.060959  -7.526 5.23e-14 ***
#  AFLLeeftijdsklassenGrof18 jaar of ouder                   1.188041   0.184521   6.439 1.21e-10 ***
#  AFLGeboorteJaarOP                                         0.018752   0.002019   9.286  < 2e-16 ***
#  AFLVermogensKwintieltweede kwintiel                      -0.166991   0.097044  -1.721   0.0853 .  
#  AFLVermogensKwintielderde kwintiel                       -0.641062   0.092263  -6.948 3.70e-12 ***
#  AFLVermogensKwintielvierde kwintiel                      -1.057432   0.100222 -10.551  < 2e-16 ***
#  AFLVermogensKwintielvijfde kwintiel (hoogste vermogens)  -1.158896   0.100174 -11.569  < 2e-16 ***
#  AFLWerkenPersoon is niet werkzaam                         0.097840   0.077368   1.265   0.2060    
#  AFLWerkenN.v.t. (< 15 jaar)                              -2.352947   0.531373  -4.428 9.51e-06 ***


anova(model_linear_only)

#                         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#  NULL                                     8737     7690.0              
#  HHBGeslachtOP            1   45.266      8736     7644.8  1.72e-11 ***
#  AFLLeeftijdsklassenGrof  1   70.561      8735     7574.2 < 2.2e-16 ***
#  AFLGeboorteJaarOP        1  183.769      8734     7390.4 < 2.2e-16 ***
#  AFLVermogensKwintiel     4  222.965      8730     7167.5 < 2.2e-16 ***
#  AFLWerken                2   36.348      8728     7131.1  1.28e-08 ***
#  ---


vif(model_linear_only)
#                            GVIF Df GVIF^(1/(2*Df))
#HHBGeslachtOP           1.012539  1        1.006250
#AFLLeeftijdsklassenGrof 1.270506  1        1.127167
#AFLGeboorteJaarOP       1.619189  1        1.272474
#AFLVermogensKwintiel    1.088928  4        1.010706
#AFLWerken               1.570638  2        1.119487


#Diagnostics of the model
library(pscl)
pR2(model_linear_only)

#AIC:7151.126
AIC(model_linear_only)

#Goodness of fit R squared: 0.09
1-(model_linear_only$deviance /model_linear_only$null.deviance)

#Deviance residuals
dev_residuals_model=residuals(model_linear_only,type = "deviance")
dev_residuals_model

#Plot deviance residuals
plot(dev_residuals_model, main="Deviance Residuals", ylab="Residuals",xlab="Index")
abline(h=0,col="red")


#Plot the residuals plot
predicted=predict(model_linear_only)

plot(x=predicted,
y=model_linear_only$residuals)



##Another Residual diagnostic
cooks_distance=cooks.distance(model_linear_only)
plot(cooks_distance, main="Cook's Distance")
#I add a reference line for a threshold
abline(h=4/length(cooks_distance),col="red",lty=2)

#Roc Curve:0.6929
rocplot(model_linear_only)

#Hoslem Test: Goodness of fit test
hoslem.test(model_linear_only$y,fitted(model_linear_only))

#Hosmer and Lemeshow goodness of fit (GOF) test

#data:  model_linear_only$y, fitted(model_linear_only)
#X-squared = 13.432, df = 8, p-value = 0.09782

#The p-value is 0.09782, indicating that there is no significant difference between observed and predicted values.


##
#At this point, I want to look better at the interaction terms (even trying to merge some modalities together in order to have all linear, whithout dropping the variables).
#(THIS IS SOME FURTHER WORK TO DO)

##Change and put together some modalities needed.

#Change for gebortelandEnHerkomst
table(droplevels(factor(x=gezo_29_var$AFLGeboortelandEnHerkomst, 
                        levels = levels(gezo_29_var$AFLGeboortelandEnHerkomst), labels = c("1","2","3","4","4","6"))))
#Change for other variables:




#As an exploratory example.
#Then, do the test in order to see if the model works better
m0=glm(PUBRookstatusRoker ~  1, family = binomial, data = gezo_29_var)
m1=glm(PUBRookstatusRoker ~  AFLLeeftijdsklassenGrof, family = binomial, data = gezo_29_var)
m2=glm(PUBRookstatusRoker ~  HHBGeslachtOP, family = binomial, data = gezo_29_var)
m3=glm(PUBRookstatusRoker ~  HHBGeslachtOP+AFLLeeftijdsklassenGrof, family = binomial, data = gezo_29_var)
m4=glm(PUBRookstatusRoker ~  HHBGeslachtOP*AFLLeeftijdsklassenGrof, family = binomial, data = gezo_29_var)

anova(m3, m4)
#Hig significative.indicates that including the interaction term significantly improves the model's fit to the data.
#analysis of Deviance Table

#Model 1: PUBRookstatusRoker ~ HHBGeslachtOP + AFLLeeftijdsklassenGrof
#Model 2: PUBRookstatusRoker ~ HHBGeslachtOP * AFLLeeftijdsklassenGrof
#Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
#1      8742     7575.3                        
#2      8741     7568.2  1   7.0279 0.008025 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#
#
#


#Possible different way to do variable selection: Stepwise  ---------------------------------------
####
model17=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn - AFLGeboorteJaarOP - AFLOpleidingsniveauVoltooid, family = binomial, data = gezo_29_var)
anova(model17)
library(MASS)

###Given that the stepwise does not handle missing values, we have to replace some missing values in order to let it work

# Function to replace null values in a column with the mode
replace_null_with_mode=function(column) {
  if (any(is.na(column))) {
    mode_value=as.numeric(names(sort(table(column), decreasing = TRUE)[1]))
    column[is.na(column)]=mode_value
  }
  return(column)
}


#Another Function to replace NA with mode
replace_na_with_mode=function(column) {
  if (any(is.na(column))) {
    mode_value=as.numeric(names(sort(table(column), decreasing = TRUE)[1]))
    column[is.na(column)]=mode_value  
  }
  return(column)
}


#Then, I check again if there are null values in the column
any(is.na(gezo_29_var))
#
#---
#Starting doing stepwise:

model17=glm(PUBRookstatusRoker ~ . - REGGemeentecode - REGGGD - AFLGeboorteMaandOP - AFLGeneratie - AFLLeeftijdOudsteKind - HHBAantalPersonen - AFLInkomensKwintiel - AFLWelvaartsKwintiel - HHBLeeftijdOP - AFLOplNivVoltooidva25jr - AFLOpleidingsniveauVoltooidva25jr3kl - AFLUrenWerk - AFLMigratieachtergrond - AFLGeboortelandDriedeling - AFLLeeftijdJongsteKind - AFLPositieInHuishoudenOP - AFLLeeftijdsklassenFijn, family = binomial, data = gezo_29_var)
AIC(model17)
null_model=glm(gezo_29_var$PUBRookstatusRoker ~ 1, data=gezo_29_var, family = binomial)
AIC(null_model)# which is 7694.479

###Performing StepAIC: start forward, then I go backward, then I go both
library(MASS)

####forward selection model: it uses 8 variables
f_model <- stepAIC(object=null_model, direction = "forward", scope = formula(model17), trace = F)

summary(f_model)
f_model$anova##I can see the change in the AIC for each variable added. 
AIC(f_model)


anova(model17)
##
b_model <- stepAIC(object=model17, direction = "backward", scope = formula(null_model), trace = F)
coef(b_model)
AIC(b_model)##7039.137
#scope = list(upper=full_model)
summary(b_model)
b_model$anova##I can see the change in the AIC for each variable added. 


####Both selection model:
s_model <- stepAIC(model17, direction = "both", trace = F)

coef(s_model)
AIC(s_model)##7039.137
summary(s_model)
s_model$anova##I can see the change in the AIC for each variable added. 
