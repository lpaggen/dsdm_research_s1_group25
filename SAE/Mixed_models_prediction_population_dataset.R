# Import Libraries --------------------------------------------------------
library(haven)
library(data.table)
library(randomForest)
library(caret)
library(dplyr)
library(lme4)
library(ggplot2)
library(leaflet)
library(rgdal)
library(cbsodataR)
library(tidyverse)
library(sf)
#First way of Data uploading ----------------------------------------------------------

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



population1 =  load("//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/populationdata_limburg_2020.RData")

# Transform the categorical variables into factors
population1 <- as_factor(dt)

# Turn it into a data.table (faster with computations)
population1 <- data.table(dt)

head(population)

#I have to map
mapping=c("Mannen"="Man", "Vrouwwen"="Vrouw")

population1$VRLGBAGESLACHT=mapping[population1$VRLGBAGESLACHT]

population1$VRLGBAGESLACHT

table(gezo_29_var$AFLInkomensKwintiel)

table(population1$inkomen)

gezo_29_var$AFLInkomensKwintiel[gezo_29_var$AFLInkomensKwintiel == "onbekend"]= "derde kwintiel"

mapping1=c("1e 20% groep"="eerste kwintiel (laagste inkomens)", "2e 20% groep"="Vrouw", "3e 20% groep"="derde kwintiel","4e 20% groep"="vierde kwintiel", "5e 20% groep"="vijfde kwintiel (hoogste inkomens)", "onbekend"="onbekend")

population1$inkomen=mapping1[population1$inkomen]

population1$inkomen[population1$inkomen == "onbekend"] <- "derde kwintiel"

population1$inkomen
#
#
#Drop the observations of those less than 12 years old.
population1=population1[population$LFT > 12, ]

population1$GEMJJJJ=as.character(population1$GEMJJJJ)



# Filter the data to see all the REGGDGeementecode for a specific REGGGD
filtered_data=gezo_29_var %>%
  filter(REGGGD == "GGD Limburg-Noord") %>%
  select(REGGemeentecode)%>%
  distinct()

# View the filtered data
print(filtered_data)

#Filter the data to see all the REGGDGeementecode for a specific REGGGD
filtered_data1=gezo_29_var %>%
  filter(REGGGD == "GGD Zuid-Limburg") %>%
  select(REGGemeentecode)%>%
  distinct()

# View the filtered data
print(filtered_data1)

#Brunssum
#2:                Heerlen   
#3:             Maastricht
#4:             Voerendaal
#5:         Sittard-Geleen
#6:              Landgraaf
#7:               Kerkrade
#8:             Simpelveld
#9:                   Beek
#10:               Meerssen
#11:                  Stein
#12: Valkenburg aan de Geul
#13:          Gulpen-Wittem
#14:             Beekdaelen
#15:      Eijsden-Margraten
#16:                  Vaals


#  1:        Nederweert
#2:          Roermond
#3:             Venlo
#4:             Weert
#5: Horst aan de Maas
#6:      Peel en Maas
#7:       Bergen (L.)
#8:            Beesel
#9:            Gennep
#10:            Venray
#11:            Leudal
#12:          Maasgouw
#13: Mook en Middelaar
#14:     Echt-Susteren
#15:         Roerdalen


mapping2 <- c(
  "888" = "Beek",
  "1954" = "Beekdaelen",
  #"899" = "Brunssum",
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

population$GEMJJJJ=mapping2[population$GEMJJJJ]

table(population$GEMJJJJ)

#Filter out those with na
population <- population[!is.na(population$GEMJJJJ), ]


#Okay, all seems to match

#Now, create a dataset for only 
important_var_dataset_1= population %>%
  select(respondentid,
         VRLGBAGESLACHT, inkomen, GEMJJJJ)  


head(population)

#Rename the columns of the dtaset
colnames(population)
names(population)[names(population) == "VRLGBAGESLACHT"] <- "HHBGeslachtOP"
names(population)[names(population) == "inkomen"] <- "AFLInkomensKwintiel"
names(population)[names(population) == "GEMJJJJ"] <- "REGGemeentecode"

#Check what are the names
colnames(population)

##################
# Alternative way for mapping data ----------------------------------------
# Data uploading ----------------------------------------------------------

# Directory of the data
directory <- "//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/"

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
# Data uploading ----------------------------------------------------------

# Directory of the data
directory <- "//CBSP.NL/Productie/secundair/MPOnderzoek/Werk/Waarneming/Gezond in mijn streek/Data/Data/"

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

population$REGGemeentecode=gsub("^0+", "", population$REGGemeentecode)

# Recode the city codes to match the city names and replace unmatched codes with NA
population$REGGemeentecode=recode(population$REGGemeentecode, 
                                     !!!city_map, 
                                     .default = NA_character_)

population=subset(population, !is.na(REGGemeentecode))

table(population$REGGemeentecode)

# Adapt data types
population$AFLInkomensKwintiel = as.integer(population$AFLInkomensKwintiel)
population$REGGemeentecode = as.factor(population$REGGemeentecode)

######
# Model Definition --------------------------------------------------------

###This is the model that I have used for the survey data, which has the variables that overlap with the population data.
#We take the variables that we are interested in examining:
simple_model1_both_gllm = glmer(PUBRookstatusRoker ~  1 +AFLInkomensKwintiel+HHBLeeftijdOP+HHBAantalPersonen +AFLBurgerlijkeStaatOP+(1|REGGemeentecode), family=binomial, data=gezo_29_var, control = glmerControl(optimizer = "bobyqa",optCtrl = list(maxfun=2e5)))
summary(simple_model1_both_gllm)

#Formula: PUBRookstatusRoker ~ 1 + AFLInkomensKwintiel + HHBLeeftijdOP +  
#  HHBAantalPersonen + AFLBurgerlijkeStaatOP + (1 | REGGemeentecode)
#Data: gezo_29_var
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

#AIC      BIC   logLik deviance df.resid 
#7408.4   7493.3  -3692.2   7384.4     8733 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.9534 -0.4586 -0.3692 -0.3058 16.1048 

#Random effects:
#  Groups          Name        Variance Std.Dev.
#REGGemeentecode (Intercept) 0.03429  0.1852  
#Number of obs: 8745, groups:  REGGemeentecode, 344

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                           -0.178481   0.202282  -0.882  0.37759    
#AFLInkomensKwintieltweede kwintiel                    -0.328824   0.103943  -3.164  0.00156 ** 
#  AFLInkomensKwintielderde kwintiel                     -0.429142   0.099896  -4.296 1.74e-05 ***
#  AFLInkomensKwintielvierde kwintiel                    -0.513964   0.097913  -5.249 1.53e-07 ***
#  AFLInkomensKwintielvijfde kwintiel (hoogste inkomens) -0.857426   0.100580  -8.525  < 2e-16 ***
#  AFLInkomensKwintielonbekend                           -0.189298   0.209041  -0.906  0.36517    
#HHBLeeftijdOP                                         -0.015259   0.002378  -6.416 1.40e-10 ***
#  HHBAantalPersonen                                     -0.181699   0.027227  -6.673 2.50e-11 ***
#  AFLBurgerlijkeStaatOPgescheiden                        0.669187   0.102402   6.535 6.37e-11 ***
#  AFLBurgerlijkeStaatOPweduwe/weduwnaar                 -0.070365   0.158504  -0.444  0.65709    
#AFLBurgerlijkeStaatOPnooit gehuwd geweest              0.209983   0.092857   2.261  0.02374 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) AFLInkmnsKwntltk AFLInkmnsKwntldk AFLInkmnsKwntlvk AFLk(i AFLInK HHBLOP
#                                                                 AFLInkmnsKwntltk -0.215                                                                        
#                                                                 AFLInkmnsKwntldk -0.265  0.539                                                                 
#                                                                 AFLInkmnsKwntlvk -0.279  0.547            0.582                                                
#                                                                 AFLInkKk(i)      -0.298  0.528            0.562            0.579                               
#                                                                 AFLInkmnsKw      -0.176  0.239            0.254            0.263            0.260              
#                                                                 HHBLeftjdOP      -0.847 -0.071           -0.013            0.009            0.027  0.065       
#                                                                 HHBAntlPrsn      -0.670 -0.044           -0.064           -0.088           -0.050  0.036  0.484
#                                                                 AFLBrgrlSOP      -0.286  0.006            0.033            0.046            0.066  0.024  0.103
#                                                                 AFLBrgrSOP/      -0.046 -0.017            0.024            0.031            0.058  0.014 -0.135
#                                                                 AFLBrgSOPgg      -0.740  0.009            0.046            0.056            0.076  0.018  0.699
#                                                                 HHBAnP AFLBrSOP AFLBSOP/
#                                                                   AFLInkmnsKwntltk                         
#                                                                 AFLInkmnsKwntldk                         
#                                                                 AFLInkmnsKwntlvk                         
#                                                                 AFLInkKk(i)                              
#                                                                 AFLInkmnsKw                              
#                                                                 HHBLeftjdOP                              
#                                                                 HHBAntlPrsn                              
#                                                                 AFLBrgrlSOP       0.234                  
#                                                                 AFLBrgrSOP/       0.136  0.189           
#                                                                 AFLBrgSOPgg       0.345  0.326    0.068  



########

#Family: binomial  ( logit )
#Formula: PUBRookstatusRoker ~ 1 + HHBGeslachtOP + AFLInkomensKwintiel +  
#  (1 + HHBGeslachtOP + AFLInkomensKwintiel | REGGemeentecode)
#Data: gezo_29_var
#Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))

#AIC      BIC   logLik deviance df.resid 
#7569.5   7760.6  -3757.8   7515.5     8718 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.7741 -0.4535 -0.3886 -0.3214  3.6434 

#Random effects:
#  Groups          Name                                Variance Std.Dev. Corr 
#REGGemeentecode (Intercept)                           0.066760 0.25838       
#HHBGeslachtOPVrouw                                    0.045971 0.21441  -0.85
#AFLInkomensKwintieltweede kwintiel                    0.008771 0.09365   0.38
#AFLInkomensKwintielderde kwintiel                     0.029021 0.17036   0.99
#AFLInkomensKwintielvierde kwintiel                    0.112561 0.33550  -0.22
#AFLInkomensKwintielvijfde kwintiel (hoogste inkomens) 0.046074 0.21465   0.82



#-0.16                  
#-0.81  0.53            
#-0.31 -0.22 -0.24      
#-0.48  0.11  0.77 -0.66
#Number of obs: 8745, groups:  REGGemeentecode, 344

##Fixed effects:
##  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                           -0.94425    0.09090 -10.387  < 2e-16 ***
##  HHBGeslachtOPVrouw                                    -0.36854    0.06894  -5.346 8.98e-08 ***
##  AFLInkomensKwintieltweede kwintiel                    -0.48154    0.11442  -4.209 2.57e-05 ***
#  AFLInkomensKwintielderde kwintiel                     -0.57826    0.10826  -5.341 9.23e-08 ***
#  AFLInkomensKwintielvierde kwintiel                    -0.65945    0.10903  -6.048 1.46e-09 ***
#  AFLInkomensKwintielvijfde kwintiel (hoogste inkomens) -1.05861    0.11905  -8.892  < 2e-16 ***
  ---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  #
  #Correlation of Fixed Effects:
  #(Intr) HHBGOP AFLInkmnsKwntltk AFLInkmnsKwntldk AFLInkmnsKwntlvk
  #HHBGslchOPV      -0.424                                                          
  #AFLInkmnsKwntltk -0.634 -0.002                                                   
  #AFLInkmnsKwntldk -0.651  0.020  0.526                                            
  #AFLInkmnsKwntlvk -0.690  0.018  0.530            0.544                           
#AFLInkKk(i)      -0.617  0.024  0.501            0.510            0.500          
#optimizer (bobyqa) convergence code: 0 (OK)


#Extract the random effects of the municipalities
ranef(simple_model1_both_gllm)

#Now that I have obtained the estimates of the municipalities.
#I have to:

#Fixed effect coefficients
coefficients=summary(simple_model1_both_gllm)$coefficients
coefficients

#Random Effects:
random_effects=ranef(simple_model1_both_gllm)
random_effects

#
# Convert all columns of the dataset to factors
population[]=lapply(population, as.factor)

#I convert some variables as numeric:
population$HHBLeeftijdOP=as.numeric(as.character(population$HHBLeeftijdOP))
population$HHBAantalPersonen=as.numeric(as.character(population$HHBAantalPersonen))

head(population)

#Now, I turn my values into probabilities of smoking for each person:

#Extract the Fixed effect coefficients:Each coefficient corresponds to the effect of a predictor variable on the log odds of smoking.
beta_0=coefficients["(Intercept)", "Estimate"]
#beta_1=coefficients["HHBGeslachtOPVrouw", "Estimate"] 
beta_q2=coefficients["AFLInkomensKwintieltweede kwintiel", "Estimate"]  # Coefficient for Q2
beta_q3=coefficients["AFLInkomensKwintielderde kwintiel", "Estimate"]  # Coefficient for Q3
beta_q4=coefficients["AFLInkomensKwintielvierde kwintiel", "Estimate"]  # Coefficient for Q4
beta_q5=coefficients["AFLInkomensKwintielvijfde kwintiel (hoogste inkomens)", "Estimate"]  # Coefficient for Q5
beta_q6=coefficients["HHBLeeftijdOP","Estimate"]
beta_q7=coefficients["HHBAantalPersonen","Estimate"]
beta_q8=coefficients["AFLBurgerlijkeStaatOPgescheiden","Estimate"]
beta_q9=coefficients["AFLBurgerlijkeStaatOPweduwe/weduwnaar","Estimate"]
beta_q10=coefficients["AFLBurgerlijkeStaatOPnooit gehuwd geweest","Estimate"]


#table(population$AFLBurgerlijkeStaatOP)
#I calculate the logit for each individual in the population data
population$logit <- beta_0 + 
  #beta_1 * (population$HHBGeslachtOP == "Vrouw") + 
  beta_q6 * (population$HHBLeeftijdOP) + beta_q7 * (population$HHBAantalPersonen)


#Now, I do this for handling the categorical variables:
population$logit <- population$logit + 
  ifelse(population$AFLInkomensKwintiel == "Q2", beta_q2, 0) +
  ifelse(population$AFLInkomensKwintiel == "Q3", beta_q3, 0) +
  ifelse(population$AFLInkomensKwintiel == "Q4", beta_q4, 0) +
  ifelse(population$AFLInkomensKwintiel == "Q5", beta_q5, 0) +
  ifelse(population$AFLBurgerlijkeStaatOP == "Q6", beta_q5, 0) +
  ifelse(population$AFLBurgerlijkeStaatOP == "Q7", beta_q5, 0) +
  ifelse(population$AFLBurgerlijkeStaatOP == "Q8", beta_q5, 0)

#I Add the random effect for the municipality
random_intercept_values=sapply(random_effects, function(x) x[["(Intercept)"]])
print(random_intercept_values)

#Now, I match the municipality to the random effect:These are matched to each individual’s municipality
population$random_intercept=random_intercept_values[population$REGGemeentecode]  


#I add the random intercept to the logit
population$logit=population$logit + population$random_intercept

#I then convert logit to probability
population$predicted_smoking_prob=exp(population$logit) / (1 + exp(population$logit))

#Results:
head(population)

#It works, I can see the smoking behavior across the different municipalities.


#give me some statistics for the results (this are the ones of survey data):
##Now, I can take the metrics:
predicted_prob=predict(simple_model1_both_gllm ,newdata = gezo_29_var, type = "response")
length(predicted_prob)

#Setting the threshold; we can play by changing it.
predicted_values=ifelse(predicted_prob > 0.250,1,0)
length(predicted_values)

#Confusion Matrix:
conf=table(predicted=predicted_values,actual=gezo_29_var$PUBRookstatusRoker)
conf

#I can manually extract the values from the confusion matrix:
TN=conf[1, 1]
FP=conf[1, 2]
FN=conf[2, 1]
TP=conf[2, 2]

#Some metrics
accuracy=sum(diag(conf)) / sum(conf)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
specificity=TN / (TN + FP)
f1_score= 2 * (precision * recall) / (precision + recall)
balanced_accuracy=(recall + specificity) / 2

#I print: 
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("Specificity: ", specificity, "\n")
cat("F1-Score: ", f1_score, "\n")
cat("Balanced Accuracy: ", balanced_accuracy, "\n")

##############

##Now, I can take the metrics (these are the ones for population register):
predicted_prob_pop=predict(simple_model1_both_gllm ,newdata = population, type = "response")
length(predicted_prob)


predicted_values=ifelse(population$predicted_smoking_prob > 0.250,1,0)
length(predicted_values)

#Confusion Matrix:
conf=table(predicted=predicted_values,actual=gezo_29_var$PUBRookstatusRoker)
conf

#I can manually extract:
TN=conf[1, 1]
FP=conf[1, 2]
FN=conf[2, 1]
TP=conf[2, 2]

#Metrics:
accuracy=sum(diag(conf)) / sum(conf)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
specificity=TN / (TN + FP)
f1_score= 2 * (precision * recall) / (precision + recall)
balanced_accuracy=(recall + specificity) / 2

#Results:
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("Specificity: ", specificity, "\n")
cat("F1-Score: ", f1_score, "\n")
cat("Balanced Accuracy: ", balanced_accuracy, "\n")

##############
#Now, I can plot it:
#Here, I plot the histogram of predicted smoking probabilities across municipalities
ggplot(population, aes(x = predicted_smoking_prob, fill = REGGemeentecode)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Predicted Smoking Probabilities by City",
       x = "Predicted Smoking Probability",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~ REGGemeentecode, scales = "free_y") +
  theme(legend.position = "none")

#Here, I plot the histogram of predicted smoking values across municipalities
ggplot(population, aes(x = predicted_values, fill = REGGemeentecode)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Predicted Smoking behavior by City",
       x = "Predicted Smoking",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~ REGGemeentecode, scales = "free_y") +
  theme(legend.position = "none")
