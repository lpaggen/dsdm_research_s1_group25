# Import Libraries --------------------------------------------------------
library(lme4)
library(performance)
library(rcompanion)
library(ggplot2)
library(caret)
library(haven)
library(data.table)
library(pROC)
library(MuMIn)
library(merTools)
library(Deducer)
library(arm)
library(jtools)
library(effects)
library(dplyr)
library(ggplot2)

# Data import -------------------------------------------------------------

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
gezo <- gezo[!is.na(gezo$PUBRookstatusRoker), ] # Drops the last missing value

# All Jeldrik selected variables
gezo_29_var <- gezo[, .(
  PUBRookstatusRoker,         # Smoking status
  AFLDrugsOoit,               # Drugs
  HHBGeslachtOP,              # Gender
  HHBLeeftijdOP = as.numeric(as.character(HHBLeeftijdOP)),  # Convert to numeric inline
  AFLLeeftijdsklassenFijn,    # Breakdown of population by age (fine)
  AFLLeeftijdsklassenGrof,    # Breakdown of population by age (coarse)
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

# Data preprocessing for the glm
#Substitue the unknown in quintiles
gezo_29_var$AFLVermogensKwintiel[gezo_29_var$AFLVermogensKwintiel == "onbekend"] <- "derde kwintiel"

#As an exploratory example, we use a Simple logistic model ---------------------------------------------------
#Afterwards, we will use the the variables of the model we ended up in the logistic regression. 
simple_logistic_model=glm(PUBRookstatusRoker~ HHBGeslachtOP + AFLVermogensKwintiel, family = binomial,data = balanced_data)
exp(coef(simple_logistic_model))
summary(simple_logistic_model)
r_squared=nagelkerke(simple_logistic_model)
r_squared

deviance(simple_logistic_model)
AIC(simple_logistic_model)

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
#In my case, most points are very low, which suggest that the majority of the observations have little influence on the model's coefficients
#If the values are relatively small and few points are extremely influential, the model appears stable and not overly sensitive to individual data points
#

#ROC Curve: 0.72
rocplot(model17)
#
#Hoslem Test: Goodness of fit test
hoslem.test(model17$y,fitted(model17))
#The p-value is 0.569, indicating that there is no significant difference between observed and predicted values.



# Cross validation
cv_model=train(
  PUBRookstatusRoker ~ HHBGeslachtOP + AFLVermogensKwintiel, 
  data = balanced_data, 
  method = "glm",
  family = binomial(),
  trControl = train_control)


print(cv_model)

# ROC AUC
gezo_29_var$PUBRookstatusRoker=ifelse(gezo_29_var$PUBRookstatusRoker == "Ja", 1, 0)
gezo_29_var$PUBRookstatusRoker=factor(gezo_29_var$PUBRookstatusRoker, levels = c(0, 1))


predicted_prob <- predict(simple_logistic_model, newdata = gezo_29_var, type = "response")
roc_curve <- roc(gezo_29_var$PUBRookStatusRoker, predicted_prob) # Not working
auc(roc_curve)
plot(roc_curve, col = "blue", main = "ROC Curve")



# Simple Mixed Model ------------------------------------------------------
#Before, some theory:
#LMMs have underlying assumptions: both residuals and random effects should be normally distributed.
#In particular, residuals should have a uniform variance over different values of the dependent variable, exactly as assumed in a classic linear model.

#Keep in mind that, in general, factors with fewer than 5 levels should be considered fixed and factors with numerous levels should be considered random effects in order to increase the accuracy in the estimation of the variance
#
#

#Fixed Effects: are the coefficients that represent the average effect of predictors across all observations. They are similar to those in standard linear models 
#
#Random Effects: account for variability at different levels of the data hierarchy, such as subjects or experimental units. They allow for the modeling of correlations within grouped data
#
#Putting like (1|REGGGD), I am specifying that REGGGGD is a random effect. I am essentially telling to R to assume a different intercept for each level of region(REGGGD).
#

#Baseline model for the regions
baseline_model_reg=glmer(PUBRookstatusRoker ~ 1 +  (1|REGGGD), family=binomial,data=gezo_29_var)
summary(baseline_model_reg) # Random effects with variance and sd
exp(fixef(baseline_model_reg)) # Returns the odds ratio of being a smoker at a national level

#A check to do, before there is not enough variation across the regions.
icc(baseline_model_reg)
#Adjusted ICC: 0.001
#Unadjusted ICC: 0.001

#Baseline model for the municipalities:
baseline_model_mun=glmer(PUBRookstatusRoker ~ 1 +  (1|REGGemeentecode), family=binomial,data=gezo_29_var)
summary(baseline_model_mun) # Random effects with variance and sd

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: PUBRookstatusRoker ~ 1 + (1 | REGGemeentecode)
#Data: gezo_29_var

#AIC      BIC   logLik deviance df.resid 
#7678.5   7692.6  -3837.2   7674.5     8743 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.5314 -0.4396 -0.4203 -0.3980  2.7140 

#Random effects:
#  Groups          Name        Variance Std.Dev.
#REGGemeentecode (Intercept) 0.0614   0.2478  
#Number of obs: 8745, groups:  REGGemeentecode, 344

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.72138    0.03776  -45.59   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

exp(fixef(baseline_model_mun)) # Returns the odds ratio of being a smoker at a national level


#Quantifying the variability with ICC
icc(baseline_model_mun) # Less than 0.05, probably useless the Mixed Model, but at least it is close

#Adjusted ICC: 0.018
#Unadjusted ICC: 0.018


###Adding more Model Coefficients:
#First Attempt:
simple_model1_gllm = glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP   + (1|REGGemeentecode), family=binomial, data=gezo_29_var)
confint(simple_model1_gllm)

#The (1|REGGemeentecode) means that we are allowing the intercept, represented by 1, to vary by city.
summary(simple_model1_gllm)

##From the output:
#Random effects:
#  Groups          Name        Std.Dev.
# REGGemeentecode (Intercept) 0.2495  
# Number of obs: 8745, groups:  REGGemeentecode, 344
# Fixed Effects:
#  (Intercept)  HHBGeslachtOPVrouw  
#  -1.5297             -0.3976  

###To interpret the point estimates of the fixed part, we can say that the log odd
exp(fixef(simple_model1_gllm_h))


# Estimates of the random effects
#We obtain the empirical Bayes modal(MAP) predictions of the random intercepts for regions.
ranef(simple_model1_gllm)

#I can try to display the residual plot
x=predict(simple_model1_gllm)
y=resid(simple_model1_gllm)
binnedplot(x,y)


#The residuals are just the difference between the actual values and the values predicted by the model.This binned plot divides the data into categories(bins) based on their fitted(predicted) values and then plots the average residual versus the average fitted value for each bin.
#In the plot, the grey lines indicate plus and minus 2 standard-error bounds.
#We expect about 95 percent of the binned residuals to fall between the two grey lines if the model is actually true.
#(By default, with datasets larger than 100 tokens, the number of bins is the square root of the total number of tokens.)
#
#
#
####

r.squaredGLMM(simple_model1_gllm)
##This is the result:
#                R2m        R2c
#theoretical 0.011636606 0.02999934
#delta       0.005086061 0.01311194

#the first, R2m represents the variance explained by the fixed effects alone. It tells that just the 1.1 percent of the variance is explained by the fixed effects.
#The second, represents the variance that is explained by the fixed effects plus the random effects. It tells that 2.9 of the variance is explained by the combination of fixed and random effects.
#
#What about the delta?
#
###
#
summary(simple_model1_gllm)
#I obtained this results:
#Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)        -1.52980    0.04613 -33.162  < 2e-16 ***
#  HHBGeslachtOPVrouw -0.39752    0.05915  -6.721 1.81e-11 ***

  ---
#The likelihood of smoking for all levels of Regions considered together is found below under the fixed effect.
#It is the Estimate value of (Intercept), like -1.52980, log odds
  
#The Variance and the standard deviation are two different ways of expressing how much the levels of Region vary around the baseline value.
#The standard Deviation is simply the square root of the variance. It is also the same units as the intercept.
#
#What about the random effects?
# 
#Random effects:
#Groups          Name        Variance Std.Dev.
#REGGemeentecode (Intercept) 0.06228  0.2496
#
#Since we assume these likelihoods are normally-distributed, 95 percent of the regions likelihoods will be within two standard deviations around the overall likelihood
#We can calculate it using an idealized normal distribution
#  
simple_model1_gllm.intercept=-1.52980
simple_model1_gllm.rsd=0.2496 
qnorm(c(0.025,0.975), mean = simple_model1_gllm.intercept, sd=simple_model1_gllm.rsd)  
##These are the results obtained, which are reported in log odds.
#-2.019007 -1.040593
#I can convert them to probabilities:
invlogit(simple_model1_gllm.intercept) ##0.178023

invlogit(qnorm(c(0.025,0.975), mean = simple_model1_gllm.intercept, sd=simple_model1_gllm.rsd) ) 
#0.1172217 0.2610356

#We can report that the mean baseline probability of Smoking in the data is 19% and that the 95% range for individual regions baseline probabilities is 11.2% to 26%.

#To get the baseline likelihood for individual region, I can extract the random effect values 
ranef(simple_model1_gllm)
#
#
#Now, for each individual region, I add their random effect value to evaluate the overall baseline likelihood to get that speakers baseline likelihood.
#Then, I can convert the log odds to probability
#Get random effect for Maastricht : 0.0825
ranef(simple_model1_gllm)$REGGemeentecode["Maastricht",]
#
#Calculate the sum of the random effect for Maastricht and (Intercept):-1.447
sum(ranef(simple_model1_gllm)$REGGemeentecode["Maastricht",],fixef(simple_model1_gllm)["(Intercept)"])

#Convert the result from log odds to probability:0.1904
plogis(sum(ranef(simple_model1_gllm)$REGGemeentecode["Maastricht",],fixef(simple_model1_gllm)["(Intercept)"]))

#Comments: the random effect for Maastricht is 0.0825 from the overall baseline likelihood
#The combination of maastricht and intercept is -1.447 log odds.
#We can therefore report that the baseline probability of Smoking for the region Maastricht is 0.1904, or 19.4%
#
# Fixed Effects -----------------------------------------------------------

summary(simple_model1_gllm)
#Again, the coefficient for the (Intercept), as described above, is the overall baseline likelihood.
#It is the likelihood, all things being equal, that any given token will have the application value rather than the non-application value.
#It is the mean of the baseline likelihoods of all the parameters in the model.
#
#We can extract the specific values using the fixef function and the position of the coefficients in the list.
fixef(simple_model1_gllm)

#Keep in mind that the sum of all coefficients for a single parameter will equal zero. So, the coefficient of the missing level (in our case is "being a man") will be 0 minus the sum of all the remaining coefficients for that parameter.

#Substract the sum of the coefficients from 0: 0.3975229
#This is the coefficient of being a man
0-sum(fixef(simple_model1_gllm)["HHBGeslachtOPVrouw"])

#Using the following function, I can also calculate its probability.
#we have the result: 0.5980924
invlogit(0-sum(fixef(simple_model1_gllm)["HHBGeslachtOPVrouw"]))

#But, keep in mind that, in order to get the actual probability of a given level, I have to add its estimate to the (Intercept).
#Actual Probability
invlogit(sum(fixef(simple_model1_gllm)["HHBGeslachtOPVrouw"],fixef(simple_model1_gllm)["(Intercept)"]))

##

#Fitting the Model with other Random Effects -----------------------------------
#
#This is the model with only the fixed effects of gender and wealth quintile.
simple_model2_gllm = glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP+AFLVermogensKwintiel+ (1|REGGemeentecode), family=binomial, data=gezo_29_var)
summary(simple_model2_gllm)
# Estimates of the random effects
ranef(simple_model2_gllm)

#What about if I put one or both the covariates as random levels?

#I put gender as random effect
#I specify that family=binomial(logit), as this model is essentially a binary logistic regression model
simple_model2_gllm = glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP+AFLVermogensKwintiel+ (1 + HHBGeslachtOP|REGGemeentecode), family=binomial(logit), data=gezo_29_var)


x=predict(simple_model2_gllm)
y=resid(simple_model2_gllm)
binnedplot(binnedplot(x,y, main = "Binned Residual Plot (Gender)"))



summary(simple_model2_gllm)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel +      (1 + HHBGeslachtOP | REGGemeentecode)
#Data: gezo_29_var

#AIC      BIC   logLik deviance df.resid 
#7356.1   7419.8  -3669.1   7338.1     8736 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.8580 -0.4773 -0.3623 -0.2923  3.6069 

#Random effects:
#  Groups          Name               Variance Std.Dev. Corr 
#REGGemeentecode (Intercept)        0.08265  0.2875        
#HHBGeslachtOPVrouw 0.02756  0.1660   -1.00
#Number of obs: 8745, groups:  REGGemeentecode, 344

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                             -0.73281    0.08112  -9.034  < 2e-16 ***
#  HHBGeslachtOPVrouw                                      -0.39859    0.06766  -5.891 3.85e-09 ***
#  AFLVermogensKwintieltweede kwintiel                     -0.28777    0.09510  -3.026  0.00248 ** 
#  AFLVermogensKwintielderde kwintiel                      -0.70609    0.09106  -7.754 8.91e-15 ***
#  AFLVermogensKwintielvierde kwintiel                     -1.22378    0.09819 -12.464  < 2e-16 ***
#  AFLVermogensKwintielvijfde kwintiel (hoogste vermogens) -1.35484    0.09765 -13.874  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) HHBGOP AFLVrmgnsKwntltk AFLVrmgnsKwntldk AFLVrmgnsKwntlvk
#HHBGslchOPV      -0.492                                                          
#AFLVrmgnsKwntltk -0.600  0.023                                                   
#AFLVrmgnsKwntldk -0.644  0.049  0.522                                            
#AFLVrmgnsKwntlvk -0.611  0.067  0.485            0.512                           
#AFLVrmKk(v)      -0.604  0.055  0.486            0.512            0.480          
#optimizer (Nelder_Mead) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')
#

#On the municipality level, sex(woman or man) has a significant and negative influence on the odd of a person smoking. 
#Also, all the quintiles wealthfare have a significant and negative influence. 
#
#We can also use the summ() function to retrieve exponentiated coefficient estimates for easier interpretation.
summ(simple_model2_gllm, exp=T)

#We can also use this function to visualize the effects of the parameter estimates.
plot(allEffects(simple_model2_gllm))


#I put wealth quintile as random effect
simple_model2_gllm_1= glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP+AFLVermogensKwintiel+ (1 + AFLVermogensKwintiel|REGGemeentecode), family=binomial(logit), data=gezo_29_var, control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=2e5)))


x=predict(simple_model2_gllm_1)
y=resid(simple_model2_gllm_1)
binnedplot(binnedplot(x,y, main = "Binned Residual Plot (Wealth Quantile)"))


summary(simple_model2_gllm_1)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel +      (1 + AFLVermogensKwintiel | REGGemeentecode)
#Data: gezo_29_var
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

#AIC      BIC   logLik deviance df.resid 
#7377.7   7526.3  -3667.9   7335.7     8724 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.7799 -0.4770 -0.3615 -0.2810  4.0257 

#Random effects:
#  Groups          Name                                                  Variance Std.Dev. Corr                   
#REGGemeentecode (Intercept)                                             0.01664  0.1290                          
#AFLVermogensKwintieltweede kwintiel                     0.05770  0.2402   -0.51                  
#AFLVermogensKwintielderde kwintiel                      0.03939  0.1985   -0.07  0.87            
#AFLVermogensKwintielvierde kwintiel                     0.06009  0.2451    0.71 -0.49 -0.03      
#AFLVermogensKwintielvijfde kwintiel (hoogste vermogens) 0.09482  0.3079    0.43  0.37  0.78  0.60
#Number of obs: 8745, groups:  REGGemeentecode, 344

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                             -0.68573    0.07766  -8.830  < 2e-16 ***
#  HHBGeslachtOPVrouw                                      -0.44137    0.06035  -7.313 2.61e-13 ***
#  AFLVermogensKwintieltweede kwintiel                     -0.29845    0.10383  -2.874  0.00405 ** 
#  AFLVermogensKwintielderde kwintiel                      -0.73458    0.09802  -7.494 6.68e-14 ***
#  AFLVermogensKwintielvierde kwintiel                     -1.29155    0.11411 -11.319  < 2e-16 ***
#  AFLVermogensKwintielvijfde kwintiel (hoogste vermogens) -1.44407    0.11587 -12.462  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) HHBGOP AFLVrmgnsKwntltk AFLVrmgnsKwntldk AFLVrmgnsKwntlvk
#HHBGslchOPV      -0.386                                                          
#AFLVrmgnsKwntltk -0.646  0.013                                                   
#AFLVrmgnsKwntldk -0.670  0.037  0.515                                            
#AFLVrmgnsKwntlvk -0.582  0.046  0.416            0.447                           
#AFLVrmKk(v)      -0.580  0.041  0.452            0.460            0.419          
#optimizer (bobyqa) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')

#I put both as random effect:
simple_model2_gllm_full= glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP+AFLVermogensKwintiel+ (1 + HHBGeslachtOP +AFLVermogensKwintiel |REGGemeentecode), family=binomial(logit), data=gezo_29_var, control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=2e5)))


x=predict(simple_model2_gllm_full)
y=resid(simple_model2_gllm_full)
binnedplot(x,y, main = "Binned Residual Plot (Full Model)")


summary(simple_model2_gllm_full)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel +  
#  (1 + HHBGeslachtOP + AFLVermogensKwintiel | REGGemeentecode)
#Data: gezo_29_var
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

#AIC      BIC   logLik deviance df.resid 
#7387.0   7578.1  -3666.5   7333.0     8718 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.7951 -0.4741 -0.3533 -0.2840  3.8883 

#Random effects:
#  Groups          Name                                                    Variance Std.Dev. Corr                         
#REGGemeentecode (Intercept)                                             0.02690  0.1640                                
#HHBGeslachtOPVrouw                                      0.04759  0.2182   -0.37                        
#AFLVermogensKwintieltweede kwintiel                     0.08914  0.2986   -0.24 -0.70                  
#AFLVermogensKwintielderde kwintiel                      0.08634  0.2938    0.16 -0.93  0.90            
#AFLVermogensKwintielvierde kwintiel                     0.07709  0.2776    0.61 -0.66 -0.05  0.33      
#AFLVermogensKwintielvijfde kwintiel (hoogste vermogens) 0.07892  0.2809    0.77 -0.87  0.40  0.75  0.69
#Number of obs: 8745, groups:  REGGemeentecode, 344

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                             -0.70905    0.08041  -8.818  < 2e-16 ***
#  HHBGeslachtOPVrouw                                      -0.40092    0.06913  -5.799 6.66e-09 ***
#  AFLVermogensKwintieltweede kwintiel                     -0.30240    0.10507  -2.878    0.004 ** 
#  AFLVermogensKwintielderde kwintiel                      -0.74414    0.10012  -7.432 1.07e-13 ***
#  AFLVermogensKwintielvierde kwintiel                     -1.29242    0.11464 -11.274  < 2e-16 ***
#  AFLVermogensKwintielvijfde kwintiel (hoogste vermogens) -1.43649    0.11523 -12.467  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) HHBGOP AFLVrmgnsKwntltk AFLVrmgnsKwntldk AFLVrmgnsKwntlvk
#HHBGslchOPV      -0.431                                                          
#AFLVrmgnsKwntltk -0.619 -0.029                                                   
#AFLVrmgnsKwntldk -0.627 -0.030  0.516                                            
#AFLVrmgnsKwntlvk -0.560  0.010  0.424            0.450                           
#AFLVrmKk(v)      -0.572  0.024  0.454            0.465            0.422          
#optimizer (bobyqa) convergence code: 0 (OK)
#boundary (singular) fit: see help('isSingular')



##Comparison of the models:


###
##Gender with full model
anova(simple_model2_gllm,simple_model2_gllm_full, test="Chisq")
#
#Data: gezo_29_var
#Models:
#  simple_model2_gllm: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel + (1 + HHBGeslachtOP | REGGemeentecode)
#simple_model2_gllm_full: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel + (1 + HHBGeslachtOP + AFLVermogensKwintiel | REGGemeentecode)
#                       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#simple_model2_gllm         9 7356.1 7419.8 -3669.1   7338.1                     
#simple_model2_gllm_full   27 7387.0 7578.1 -3666.5   7333.0 5.0759 18     0.9987
#

###
################################
#
#Wealth quintile with full model
anova(simple_model2_gllm_1,simple_model2_gllm_full, test="Chisq")
#As a rsult, the chisquared p-value is 0.8483
#
#Data: gezo_29_var
#Models:
#  simple_model2_gllm: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel + (1 + AFLVermogensKwintiel | REGGemeentecode)
#simple_model2_gllm_full: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLVermogensKwintiel + (1 + HHBGeslachtOP + AFLVermogensKwintiel | REGGemeentecode)
#                         npar    AIC    BIC  logLik deviance
#simple_model2_gllm        21 7377.7 7526.3 -3667.9   7335.7
#simple_model2_gllm_full   27 7387.0 7578.1 -3666.5   7333.0
#                         Chisq Df Pr(>Chisq)
#simple_model2_gllm                         
#simple_model2_gllm_full 2.676  6     0.8483
#

###I use a different variable.
#This is the model with only the fixed effects of gender and wealth quintile.
simple_model_both_gllm = glmer(PUBRookstatusRoker ~  1 + HHBGeslachtOP+AFLInkomensKwintiel+ (1+HHBGeslachtOP +AFLInkomensKwintiel|REGGemeentecode), family=binomial, data=gezo_29_var)
summary(simple_model_both_gllm)
# Estimates of the random effects
ranef(simple_model_both_gllm)
