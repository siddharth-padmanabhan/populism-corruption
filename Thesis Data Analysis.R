
# Installing Packages -----------------------------------------------------

library(haven)
library(table1)
library(tidyverse)
library(ggplot2)
library(questionr)
library(gtsummary)
library(tidyr)
library(plyr)
library(tibble)
library(stargazer)
library(sjPlot)
library(labelled)
library(sjlabelled)
library(rio)
library(purrr)
library(plyr)
library(lmtest)
library(corrplot)
library(stargazer)
library(modelsummary)
library(stringr)
library(sandwich)
library(gplots)
library(viridis)
library(car)

# Setting Working Directory -----------------------------------------------


getwd()
setwd("C:/Users/yoga/OneDrive/Documents/Hertie Readings/Thesis/Coding Things")
getwd()

#gcb <- read_sav("GCB_TOTAL_SPSS_DATA_filteredversion6 (1).sav")
#sample_df <- read_dta("governance_df_final_sept17_f.dta")
#gcb2 <- read_spss("GCB_TOTAL_SPSS_DATA_filteredversion6 (1).sav")

# Import all Datasets -----------------------------------------------------


gps <- import("Global Party Survey by Party Stata V2_1_Apr_2020.dta")
gcb <- import("GCB_TOTAL_SPSS_DATA_filteredversion6 (1).sav")
polp <- import("Political Parties (2).csv")

options(scipen = 999)


# Global Corruption Barometer (GCB) Dataset Manipulation ------------------------------------------------

countries <- rio::factorize(gcb$GCBCOUNTRY)
gcb <- cbind(gcb,countries)

gcbfinal <- gcb %>% 
  select( 'GCBRESPID', 'GCBAGE','GCBGENDER', 'GCBCORRCHANGE', 'GCBCORRGOVRES', 'GCBWORKINGSTATUS1',
          'GCBEDUC', 'GCB_HHEARNS', 'countries')

#gcbfinal <- gcb %>% 
 # select( 'GCBRESPID', 'GCBCOUNTRY', 'GCBYEAR', 'GCBAREATYPE', 'GCBAGE', 'GCBAGET1','GCBAGET2', 
  #       'GCBAGET3', 'GCBAGET4', 'GCBAGERECODE_FINAL', 'GCBGENDER', 'GCBGEN_AGE', 
   #      'GCBQ2', 'GCBQ2B', 'GCBCORRCHANGE', 'GCBCORRCHANGEB', 'GCBCORRCHANGE2',
    #     'GCBCORRCHANGE2B', 'GCBCORRGOVRES', 'GCBCORRGOVRESB', 'GCBWORKINGSTATUS1',
     #    'GCBEDUC', 'GCBEDUC2', 'GCB_HHEARNS', 'countries')


#Factorising Countries for fixed effects
gcbfinal$countries <- as.factor(gcbfinal$countries)

#GCBAGE
gcbfinal <- gcbfinal %>% filter(GCBAGE>18 & GCBAGE<100) #Birth year

#New Variable: 'age' - Factorised Age
gcbfinal$age <- "NA"
gcbfinal$age[gcbfinal$GCBAGE >=18] <- "18 - 29"
gcbfinal$age[gcbfinal$GCBAGE >=30] <- "30 - 44"
gcbfinal$age[gcbfinal$GCBAGE >=45] <- "45 - 64"
gcbfinal$age[gcbfinal$GCBAGE >=65] <- "65+ "
gcbfinal$age <- as.factor(gcbfinal$age)
table(gcbfinal$age)


var_label(gcbfinal$GCBAREATYPE) 
get_labels(gcb$GCBAREATYPE)


#GCBCORRCHANGE
var_label(gcbfinal$GCBCORRCHANGE) #has the level of corruption in this country increased, decreased, or stayed the same?
get_labels(gcb$GCBCORRCHANGE)
typeof(gcbfinal$GCBCORRCHANGE)
freq(gcbfinal$GCBCORRCHANGE)
gcbfinal <- gcbfinal %>% filter(GCBCORRCHANGE>=2 & GCBCORRCHANGE<=6) 

#New Variable: 'corrchange' - Factorised Corruption Change
gcbfinal$corrchange <- "NA"
gcbfinal$corrchange[gcbfinal$GCBCORRCHANGE == 2 | gcbfinal$GCBCORRCHANGE == 3] <- "Corruption Increased"
gcbfinal$corrchange[gcbfinal$GCBCORRCHANGE == 4] <- "Corruption Stayed the Same"
gcbfinal$corrchange[gcbfinal$GCBCORRCHANGE == 5 | gcbfinal$GCBCORRCHANGE == 6] <- "Corruption Decreased"
gcbfinal$corrchange <- as.factor(gcbfinal$corrchange)
gcbfinal$corrchange<-relevel(gcbfinal$corrchange, ref="Corruption Stayed the Same")
table(gcbfinal$corrchange)



#GCBCORRGOVRES
var_label(gcb$GCBCORRGOVRES) #How well or badly would you say the current government is handling fighting corruption in the government?
get_labels(gcb$GCBCORRGOVRES)
typeof(gcbfinal$GCBCORRGOVRES)
freq(gcbfinal$GCBCORRGOVRES)
gcbfinal <- gcbfinal %>% filter(GCBCORRGOVRES>=1 & GCBCORRGOVRES<=4) 

#New Variable: 'DEP' - Factorised Corruption Handling of Current Government - Main Dependent Variable
gcbfinal$DEP <- "NA"
gcbfinal$DEP[gcbfinal$GCBCORRGOVRES == 1 | gcbfinal$GCBCORRGOVRES == 2] <- "Corruption Handled Badly"
gcbfinal$DEP[gcbfinal$GCBCORRGOVRES == 3 | gcbfinal$GCBCORRGOVRES == 4] <- "Corruption Handled Well"
gcbfinal$DEP <- as.factor(gcbfinal$DEP)
table(gcbfinal$DEP)


#New Variable: 'DEP1' - Factorised Corruption Handling of Current Government - Main Dependent Variable (numeric)
gcbfinal$DEP1 <- -1
gcbfinal$DEP1[gcbfinal$GCBCORRGOVRES == 1 | gcbfinal$GCBCORRGOVRES == 2] <- 0
gcbfinal$DEP1[gcbfinal$GCBCORRGOVRES == 3 | gcbfinal$GCBCORRGOVRES == 4] <- 1
gcbfinal$DEP1 <- as.numeric(gcbfinal$DEP1)
table(gcbfinal$DEP1)


#GCBEDUC
var_label(gcb$GCBEDUC) #What is your highest level of education?
get_labels(gcb$GCBEDUC)
typeof(gcbfinal$GCBEDUC)
freq(gcbfinal$GCBEDUC)
gcbfinal <- gcbfinal %>% filter(GCBEDUC>=1 & GCBEDUC<=14) 

#New Variable: 'educ' - Factorised Education
gcbfinal$educ <- "NA"
gcbfinal$educ[gcbfinal$GCBEDUC >= 1 & gcbfinal$GCBEDUC <= 4] <- "Primary School Education"
gcbfinal$educ[gcbfinal$GCBEDUC >= 5 & gcbfinal$GCBEDUC <= 9] <- "High School Education"
gcbfinal$educ[gcbfinal$GCBEDUC >= 10 & gcbfinal$GCBEDUC <= 14] <- "Tertiary Education"
gcbfinal$educ <- as.factor(gcbfinal$educ)
gcbfinal$educ<- relevel(gcbfinal$educ, ref="Primary School Education")
table(gcbfinal$educ)



#GCBWORKINGSTATUS1
#var_label(gcb$GCBWORKINGSTATUS1) #Working or Unemployed
#get_labels(gcb$GCBWORKINGSTATUS1)
#freq(gcbfinal$GCBWORKINGSTATUS1)
#gcbfinal <- gcbfinal %>% filter(GCBWORKINGSTATUS1>=1 & GCBWORKINGSTATUS1<=2) 

#New Variable: 'work' - Factorised Working Status
#gcbfinal$work <- "NA"
#gcbfinal$work[gcbfinal$GCBWORKINGSTATUS1 == 1] <- "Employed"
#gcbfinal$work[gcbfinal$GCBWORKINGSTATUS1 == 2] <- "Not Employed"
#gcbfinal$work <- as.factor(gcbfinal$work)
#table(gcbfinal$work)



#GCBGENDER
var_label(gcb$GCBGENDER)
get_labels(gcb$GCBGENDER)
gcbfinal <- gcbfinal %>% filter(GCBGENDER>=1 & GCBGENDER<=2) 

#New Variable: 'gender' - Factorised Gender
gcbfinal$gender <- "NA"
gcbfinal$gender[gcbfinal$GCBGENDER == 1] <- "Male"
gcbfinal$gender[gcbfinal$GCBGENDER == 2] <- "Female"
gcbfinal$gender <- as.factor(gcbfinal$gender)
table(gcbfinal$gender)


#GCB_HHEARNS
var_label(gcb$GCB_HHEARNS) #Household Income
get_labels(gcb$GCB_HHEARNS)
freq(gcbfinal$GCB_HHEARNS)
gcbfinal <- gcbfinal %>% filter(GCB_HHEARNS>=2 & GCB_HHEARNS<=6) 

#New Variable: 'finstatus' - Factorised Financial Status - developed from Household Income
gcbfinal$finstatus <- "NA"
gcbfinal$finstatus[gcbfinal$GCB_HHEARNS == 2 | gcbfinal$GCB_HHEARNS == 3] <- "Financially Well Off"
gcbfinal$finstatus[gcbfinal$GCB_HHEARNS == 4] <- "Financially Managing"
gcbfinal$finstatus[gcbfinal$GCB_HHEARNS == 5 | gcbfinal$GCB_HHEARNS == 6] <- "Financially Worse off"
gcbfinal$finstatus <- as.factor(gcbfinal$finstatus)
gcbfinal$finstatus <-relevel(gcbfinal$finstatus, ref="Financially Worse off")
table(gcbfinal$finstatus)


# Global Party Survey (GPS) Dataset Manipulation --------------------------------------------------------

gpsfinal <- gps %>% 
  select('Country', 'Partyname','Partyabb', 
          'Type_Populism',  'GDP','Longevity', 'Corruption',
         'Electoral_Integrity','Turnout') 

gpsfinal <- gpsfinal %>% 
  dplyr::rename('countries'='Country') 

gpsfinal$combo <- str_c(gpsfinal$countries, '', gpsfinal$Partyabb)

#gpsfinal <- gps %>% 
#  select('ISO', 'Country', 'Partyname','Partyabb', 'CPARTY', 'CPARTYABB', 'Type_Values', 
#        'Type_Populism', 'Type_Populist_Values', 'V1', 'V8_Scale', 'V8_Bin','V8_Ord', 'GDP',
#         'Longevity', 'Corruption','Electoral_Integrity','Turnout') 

#gpsplot <- gpsplot %>% 
 # dplyr::rename('countries'='Country')

#Type_Populism
#freq(gpsfinal$Type_Populism)
#table(gpsfinal$Type_Populism)
gpsfinal <- gpsfinal %>% filter(Type_Populism>=1 & Type_Populism<=4)

#New Variable: 'popul' - Factorised Type_Populism
gpsfinal$popul <- "NA"
gpsfinal$popul[gpsfinal$Type_Populism == 1] <- "Strongly Pluralist"
gpsfinal$popul[gpsfinal$Type_Populism == 2] <- "Moderately Pluralist"
gpsfinal$popul[gpsfinal$Type_Populism == 3] <- "Moderately Populist"
gpsfinal$popul[gpsfinal$Type_Populism == 4] <- "Strongly Populist"
gpsfinal$popul <- factor(gpsfinal$popul, levels = c('Strongly Populist' , 'Moderately Populist',
                                                    'Moderately Pluralist' , 'Strongly Pluralist'))
table(gpsfinal$popul)


#New Variable: 'popul1' - Factorised Type_Populism Binary
gpsfinal$popul1 <- "NA"
gpsfinal$popul1[gpsfinal$Type_Populism == 1 | gpsfinal$Type_Populism == 2 ] <- "Pluralist"
gpsfinal$popul1[gpsfinal$Type_Populism == 3 | gpsfinal$Type_Populism == 4] <- "Populist"
gpsfinal$popul1 <- as.factor(gpsfinal$popul1)
table(gpsfinal$popul1)

#GDP
#freq(gpsfinal$GDP)
gpsfinal <- gpsfinal[!is.na(gpsfinal$GDP),]

#Longevity
#freq(gpsfinal$Longevity)
#gpsfinal <- gpsfinal[!is.na(gpsfinal$Longevity),]

#Corruption Perception Index - from TI
#freq(gpsfinal$Corruption)
#gpsfinal <- gpsfinal[!is.na(gpsfinal$Corruption),]

#Electoral Integrity
#freq(gpsfinal$Electoral_Integrity)
gpsfinal <- gpsfinal[!is.na(gpsfinal$Electoral_Integrity),]

#Deleting country columns to avoid repetition
gpsfinal <- gpsfinal %>% 
  select(-countries, -Partyabb)


# Political Parties Dataset Manipulation ----------------------------------

polpfinal <- polp %>% 
  select('Country', 'Party Code', 'Press Freedom', 'E-Citizenship') 


polpfinal <- polpfinal %>% 
  dplyr::rename('countries'='Country') %>% 
  dplyr::rename('pressfree'='Press Freedom') %>% 
  dplyr::rename('ecitizen'='E-Citizenship') %>% 
  dplyr::rename('Partyabb'='Party Code')

polpfinal$combo <- str_c(polpfinal$countries, '', polpfinal$Partyabb) 

polpfinal <- polpfinal %>% 
  select(-Partyabb)
  

# Merging all Datasets ----------------------------------------------------

testdf <- gpsfinal %>% 
  inner_join(polpfinal, by ='combo') 

finaldf <- gcbfinal %>% 
  inner_join(testdf, by ='countries') 

finaldf$countries <- as.factor(finaldf$countries)

class(finaldf$countries)
#plotdf <- gcbplot %>% 
 # inner_join(gpsfinal, by ='countries') %>% 
#  select(-'countries')


# Final Dataset Manipulation ----------------------------------------------

#New Variable: 'Continent' - Factorised country clusters
finaldf$Continent <- "NA"
finaldf$Continent[finaldf$countries == 'Austria' | finaldf$countries == 'Belgium' | finaldf$countries == 'Bulgaria' | finaldf$countries == 'Denmark' | finaldf$countries == 'France' | finaldf$countries == 'Germany' | finaldf$countries == 'Greece' | finaldf$countries == 'Hungary' | finaldf$countries == 'Ireland' | finaldf$countries == 'Luxembourg' | finaldf$countries == 'Malta' | finaldf$countries == 'Netherlands' | finaldf$countries == 'Poland' | finaldf$countries == 'Romania' | finaldf$countries == 'Slovakia' | finaldf$countries == 'Slovenia' | finaldf$countries == 'Spain' | finaldf$countries == 'Sweden' | finaldf$countries == 'Norway'] <- "Europe"
finaldf$Continent[finaldf$countries == 'Papua New Guinea' | finaldf$countries == 'Vanuatu' | finaldf$countries == 'Fiji' | finaldf$countries == 'Samoa'] <- "Oceania"
finaldf$Continent[finaldf$countries == 'Guyana' | finaldf$countries == 'Jamaica' | finaldf$countries == 'Bahamas'| finaldf$countries == 'Argentina' | finaldf$countries == 'Brazil' | finaldf$countries == 'Colombia' | finaldf$countries == 'Costa Rica' | finaldf$countries == 'Dominican Republic' | finaldf$countries == 'El Salvador' | finaldf$countries == 'Guatemala' | finaldf$countries == 'Mexico' | finaldf$countries == 'Panama' | finaldf$countries == 'Venezuela'] <- "Americas"
finaldf$Continent[finaldf$countries == 'Lebanon' | finaldf$countries == 'Maldives' | finaldf$countries == 'Philippines' | finaldf$countries == 'Mongolia' | finaldf$countries == 'Japan' | finaldf$countries == 'Indonesia' | finaldf$countries == 'India' | finaldf$countries == 'Bangladesh' | finaldf$countries == 'Myanmar' | finaldf$countries == 'Cambodia' | finaldf$countries == 'Nepal'] <- "Asia"
finaldf$Continent <- as.factor(finaldf$Continent)

# Miscellaneous Playground ------------------------------------------------

unique(finaldf$countries)
unique(gcbfinal$countries)
unique(gpsfinal$countries)
unique(polpfinal$countries)

count(finaldf$countries)
count(gcbfinal$countries)
count(gpsfinal$countries)
df <- data.frame(colA, colB)

count(finaldf$popul)

# Data Description ------------------------------------------------------

#Prop table
ptable <- table(finaldf$DEP, finaldf$popul)
ptable
crosstable <- round(prop.table(ptable, 2),2)
crosstable

crosstable1 <- as.data.frame(crosstable)
crosstable1

#Labelling the Variables
table1::label(finaldf$educ) <- "Education Level of Respondents"
table1::label(finaldf$gender) <- "Gender of Respondents"
table1::label(finaldf$finstatus) <- "Financial Status of Respondents"
table1::label(finaldf$popul) <- "Characteristic of Ruling Party"
table1::label(finaldf$corrchange) <- "Perception of Corruption"
table1::label(finaldf$ecitizen) <- "E-Citizenship (Country Fixed Effects)"
table1::label(finaldf$age) <- "Age of Respondents"
table1::label(finaldf$GDP) <- "GDP per capita (Country Fixed Effects)"
table1::label(finaldf$pressfree) <- "Press Freedom (Country Fixed Effects)"
table1::label(finaldf$Continent) <- "Continent"


#Summary Table
table1::table1(~educ + gender + finstatus + corrchange + Continent | popul, data = finaldf) #Question: Do I include popul here?


# Data Analysis - Robustness Checks ---------------------------------------

model1 <- glm(DEP ~ popul, data = finaldf, family=binomial())
vcov1 <- vcovHC(model1, cluster = "popul")
tab1 <- tbl_regression(model1, exponentiate = T)
tab1

model2 <- glm(DEP~popul + educ + gender + finstatus + corrchange , data = finaldf, family=binomial())
vcov2 <- vcovHC(model2, cluster = "popul")
tab2 <- tbl_regression(model2, exponentiate = T)
tab2

model3a <- glm(DEP~popul + educ + gender + finstatus + corrchange + Continent , data = finaldf, family=binomial())
vcov3a <- vcovHC(model3a, cluster = "popul")
tab3a <- tbl_regression(model3a, exponentiate = T)
tab3a


tbl_merge(
  tbls = list(tab3a, tabc),
  tab_spanner = c("**Model 1**", "**Model 2**"))


modelb <- glm(DEP~popul + educ + gender + finstatus + corrchange + ecitizen , data = finaldf, family=binomial())
vcovb <- vcovHC(modelb, cluster = "popul")
tabb <- tbl_regression(modelb, exponentiate = T)
tabb

modelc <- glm(DEP~popul + educ + gender + finstatus + corrchange + ecitizen + Continent , data = finaldf, family=binomial())
vcovc <- vcovHC(modelc, cluster = "popul")
tabc <- tbl_regression(modelc, exponentiate = T)
tabc

tbl_merge(
  tbls = list(tabb, tabc),
  tab_spanner = c("**Model 3**", "**Model 4**"))

# Data Analysis -----------------------------------------------------------

#Basic Naive Model
model1 <- glm(DEP~popul, data = finaldf, family=binomial())
vcov1 <- vcovHC(model1, cluster = "popul")
tab1 <- tbl_regression(model1, exponentiate = T)
tab1

#Model with countries only
modelc <- glm(DEP~countries, data = finaldf, family=binomial())
vcovc <- vcovHC(modelc, cluster = "popul")
tabc <- tbl_regression(modelc, exponentiate = T)
tabc

#Model with Some respondent-level Controls
model2 <- glm(DEP~popul + educ + gender + finstatus + corrchange, data = finaldf, family=binomial())
vcov2 <- vcovHC(model2, cluster = "popul")
tab2 <- tbl_regression(model2, exponentiate = T)
tab2

#Model with Some respondent-level Controls plus some country level effects(GDP, Press Freedom, E Citizenship)
model3 <- glm(DEP~popul + educ + gender + finstatus + corrchange + GDP + ecitizen + pressfree, data = finaldf, family=binomial())
vcov3 <- vcovHC(model3, cluster = "popul")
tab3 <- tbl_regression(model3, exponentiate = T)
tab3

#Model with full country fixed effects
model4 <- glm(DEP~ popul + educ + gender + finstatus + corrchange + countries, data = finaldf, family=binomial())
vcov4 <- vcovHC(model4, cluster = "popul")
tab4 <- tbl_regression(model4, exponentiate = T)
tab4

#Model with full country fixed effects INTERACTED with popul
model5 <- glm(DEP~ popul*countries + educ + gender + finstatus + corrchange, data = finaldf, family=binomial())
vcov5 <- vcovHC(model5, cluster = "popul")
tab5 <- tbl_regression(model5, exponentiate = T)
tab5

#Interaction bw Education and Corruption Change perception - do we even need interaction? No? Unless...? Yes? but?
model2 <- glm(DEP~popul + gender + finstatus + ecitizen + educ:corrchange, data = finaldf, family=binomial())
tab2 <- tbl_regression(model2, exponentiate = T)
tab2



#including 'work' is useless. Hardly any difference in OR and highly insignificant. 
#Model with Some respondent-level Controls plus some country level effects(GDP, Press Freedom, E Citizenship)
modelx <- glm(DEP~popul + educ + gender + finstatus + corrchange + ecitizen, data = finaldf, family=binomial())
vcovx <- vcovHC(modelx, cluster = "popul")
tabx <- tbl_regression(modelx, exponentiate = T)
tabx

modely <- glm(DEP~popul + educ + gender + finstatus + corrchange + GDP + Continent, data = finaldf, family=binomial())
vcovy <- vcovHC(modely, cluster = "popul")
taby <- tbl_regression(modely, exponentiate = T)
taby

#The winner! Michaela suggestion method + lease loglik score. Stick to ecitizen. fk the GDP. 
modelz <- glm(DEP~popul + educ + gender + finstatus + corrchange + ecitizen + Continent, data = finaldf, family=binomial())
vcovz <- vcovHC(modelz, cluster = "popul")
tabz <- tbl_regression(modelz, exponentiate = T)
tabz

# More Standard Errors Clustering -----------------------------------------

# Estimate the logistic regression model with clustered standard errors
vcov <- vcovHC(model1, cluster = "countryscore") # Clustered standard errors
model2 <- coeftest(model1, vcov = vcov) # Use clustered standard errors in the model

# Extract p-values from both models
pvalues1 <- summary(model1)$coefficients[, "Pr(>|z|)"] # P-values without clustered standard errors
pvalues2 <- summary(model2)$coefficients[, "Pr(>|z|)"] # P-values with clustered standard errors

# Create a data frame to store the p-values for comparison
pvalue_comparison <- data.frame(Predictor = rownames(summary(model1)$coefficients),
                                Pvalue_Without_Clustered_SE = pvalues1,
                                Pvalue_With_Clustered_SE = pvalues2)

# Display the p-value comparison
print(pvalue_comparison)
# Clustering SE -----------------------------------------------------------

# Cluster standard errors at the individual level
vcov <- vcovHC(model1, cluster = "popul")

robust_se <- sqrt(diag(vcov))
results <- coeftest(model1, vcov = vcov)
pvalues <- summary(model1)$coefficients[, "Pr(>|z|)"]

# Create a data frame with coefficients, robust standard errors, and p-values for odds ratios
results_df <- data.frame(Odds_Ratio = exp(coef(model1)),
                         Robust_SE = robust_se,
                         P_Value = pvalues)

# Output the results using stargazer
stargazer(model1, title = "Logistic Regression Model Results", align = TRUE, type = "html", out = "vcov.html",
          se = list(robust_se), coef = list(results_df$Odds_Ratio), 
          add.lines = list(c("P-Value (Odds Ratio)", paste0(round(pvalues, 3), collapse = " | "))))





# Calculate robust covariance matrix with country-level clustering
vcov <- vcovHC(model1, cluster = "popul")

# Calculate robust standard errors
robust_se <- sqrt(diag(vcov))

# Extract odds ratios and their p-values
odds_ratios <- exp(coef(model1))
pvalues <- summary(model1)$coefficients[, "Pr(>|z|)"]

# Create a data frame with coefficients, robust standard errors, and p-values for odds ratios
results_df <- data.frame(Odds_Ratio = odds_ratios,
                         Robust_SE = robust_se,
                         P_Value = pvalues)

# Output the results using stargazer with separate columns for odds ratios and p-values
stargazer(model1, title = "Logistic Regression Model Results", align = TRUE, type = "html", out = "vcov.html",
          se = list(robust_se), coef = list(odds_ratios), 
          add.lines = list(c("P-Value", paste0(round(pvalues, 3), collapse = " | "))),
          column.labels = c("Odds Ratio", "Robust SE", "P-Value"))









# Data Description --------------------------------------------------------

#DV vs IV Crosstable Plot
ggplot(crosstable1, aes(fill=Var1, x=Var2, y=Freq)) +
  geom_bar(stat="identity") +
  xlab("") +
  labs(title = "Perceptions of Policy Effectiveness of Anticorruption \n\tMeasures across Types of Governments",
       x ="Type of Government",
       y = "Share of Respondents") +
  scale_fill_manual(name = "Perception of Anticorruption \nEffectiveness",
                    labels = c("Corruption Handled Badly", "Corruption Handled Well"),
                    values = c("brown3","forest green")) +
  theme_bw() +
  theme(legend.position = "right")


#Predicted Probabilities
plot_model(modelb, type = "pred", terms = "ecitizen[all]") + 
  theme_bw() + 
  geom_line(color = "red") +
  labs(title = "Predicted Probabilities of Governments' Corruption Handling Perception vs E-Citizenship",
       x = "E-Citizenship score of Country (scale of 0 - 10)",
       y = "Predicted Probability of DEP")

plot_model(modelc, type = "pred", terms = c("educ", "finstatus")) +
  theme_bw() + 
  labs(title = "Predicted Probabilities of Governments' Corruption \nHandling Perception vs Education and Financial Status",
       x = "Level of Highest Attained Education",
       y = "Predicted Probability of DEP") +
  theme(legend.position = "right")

plot_model(modelc, type = "pred", terms = c("Continent")) +
  theme_bw() + 
  labs(title = "Predicted Probabilities of Governments' Corruption Handling Perception by Continent",
       x = "Continent",
       y = "Predicted Probability of DEP") +
  theme(legend.position = "right")



#Forrest Plot
summary(modelc)
betadf<-summary(modelc)$coefficients
CI.vector <- as.data.frame(exp(confint(modelc)))
betadf2 <- cbind(betadf, CI.vector)

betadf2$Labels = c("Intercept" ,"Moderately Populist", "Moderately Pluralist", "Strongly Pluralist",
                   "High School Education","Tertiary Education", "Male", "Financially Managing","Financially Well Off",
                   "Corruption Decreased", "Corruption Increased", "E Citizenship", "Asia", "Europe")

betadf2$Labels <- factor(betadf2$Labels, levels = c( "Moderately Populist", "Moderately Pluralist", "Strongly Pluralist",
                                                     "High School Education","Tertiary Education", "Male", "Financially Managing","Financially Well Off",
                                                     "Corruption Decreased", "Corruption Increased", "E Citizenship", "Asia", "Europe"))
betadf2$OR=exp(betadf2$Estimate)
betadf2 <- betadf2[-1, ]

ggplot(betadf2,aes(x=OR,y=Labels)) +
  geom_point(color="red", size = 2) +
  geom_vline(aes(xintercept = 1), 
             size = .25, 
             linetype = "dashed") +
  labs (title ="Forest Plot: Model with Individual and Country Specifications",
        x = "Odds Ratios",
        y = "Categories of Variables of Interest") +
  geom_errorbarh(aes(xmax = `97.5 %`, xmin = `2.5 %`, height = .1)) +
  theme_bw()


testdf %>% 
  group_by(popul) %>% 
  dplyr::summarise(partyCount = n())

#Scatter Plot of Avg outcome vs DEP
avg_outcome_by_country <- finaldf %>%
  group_by(countries) %>%
  dplyr::summarise(avg_outcome = mean(DEP1))

finaldf <- inner_join(finaldf, avg_outcome_by_country, by = "countries")
correlation <- cor(finaldf$avg_outcome, finaldf$DEP1)

ggplot(data = finaldf, aes(x = avg_outcome, y = DEP1)) +
  geom_point() +
  stat_smooth(method = "glm", se = TRUE, colour = 'red', method.args = list(family=binomial)) +
  labs(title = "Scatter Plot of mean of outcome variable for each country and Dependent Variable ", 
       subtitle = "Correlation with Dependent Variable: 0.39",
       x = "Average Outcome by Country", 
       y = "Dependent Variable") +
  theme_bw()

# Diagnostics -------------------------------------------------------------

#Pearson's Chi-square test
chisq.test(finaldf$DEP, finaldf$popul)

#Modelfit Improvement Diagnostics
logLik(modelx)
logLik(modely)
logLik(modelz)

logLik(modela)
logLik(modelb)
logLik(modelc)

logLik(model2)
logLik(model3)
logLik(model4)

lrtest(model1, model2)
lrtest(modelb, modelc)

# Perform ANOVA with likelihood ratio tests
anova_result <- anova(modelc, test = "LRT")

# View the ANOVA table
print(anova_result)

#VIF test
vif(modelc)

#Testing for heteroscedasticity with Breusch-Pagan test
bptest(modelc) 
