### Loading Packages ###

library(tidyverse)
library(sjPlot)
library(ggcorrplot)


### Loading Source Files ###

# Data cleaning
source("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\disability_data_cleaning.R")

# Data preparation
source("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\loglinear_data_preparation.R")


### Importing and Cleaning Data ###

# Importing raw survey data
data = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\Phys Disability Behavioral Health_September 6, 2022_14.22.csv")

# Cleaning data
disability = clean_data(data)

# Preparing data for loglinear analysis
disability = prepare_data(disability)


### Analysis ###

# Is a diagnosis of depressive disorder associated with alcohol and illicit drug use?
# Does it vary by type of physical disability?

# Setting seed
set.seed(99)

# Chi-squared tests

chisq.test(x = disability$depression_diagnosis, y = disability$uses_alcohol) # significant
chisq.test(x = disability$depression_diagnosis, y = disability$uses_drugs) # significant

chisq.test(x = disability$depression_diagnosis, y = disability$AEE) # significant
chisq.test(x = disability$depression_diagnosis, y = disability$BBL) # significant
chisq.test(x = disability$depression_diagnosis, y = disability$HL) # significant
chisq.test(x = disability$depression_diagnosis, y = disability$NSBO) # significant

### Log-linear Models ###

# Source: https://library.virginia.edu/data/articles/an-introduction-to-loglinear-models

# Depression diagnosis, drug use, and alcohol use

predictors = subset(disability, select = c(depression_diagnosis, uses_alcohol, uses_drugs))

predictors_table = ftable(x = predictors, row.vars = c("uses_alcohol", "uses_drugs"))

predictors_df = as.data.frame(predictors_table)

## Running models

third_order_model = glm(Freq ~ (depression_diagnosis + uses_alcohol + uses_drugs) ^ 3, data = predictors_df, family ="poisson")
summary(third_order_model) # AIC: 78.236
pchisq(deviance(third_order_model), df = df.residual(third_order_model), lower.tail = F) # p-value: 1
third_order_model$deviance # Deviance: -9.392487e-14

second_order_model = glm(Freq ~ (depression_diagnosis + uses_alcohol + uses_drugs) ^ 2, data = predictors_df, family ="poisson")
summary(second_order_model) # AIC: 78.444
pchisq(deviance(second_order_model), df = df.residual(second_order_model), lower.tail = F) # p-value: 0.1373041
second_order_model$deviance # Deviance: 2.207916

first_order_model = glm(Freq ~ depression_diagnosis + uses_alcohol + uses_drugs, data = predictors_df, family ="poisson")
summary(first_order_model) # AIC: 363.93
pchisq(deviance(first_order_model), df = df.residual(first_order_model), lower.tail = F) # 2.482191e-62
first_order_model$deviance # Deviance: 293.6946

######################### Model 4: Conditional independence between depression diagnosis and alcohol use ######################
model4 = glm(Freq ~ (depression_diagnosis * uses_drugs) + (uses_alcohol * uses_drugs), data = predictors_df, family ="poisson")
summary(model4) # AIC: 76.944
pchisq(deviance(model4), df = df.residual(model4), lower.tail = F) # p-value: 0.2582115
model4$deviance # Deviance: 2.707953
###############################################################################################################################

model5 = glm(Freq ~ (depression_diagnosis * uses_drugs) + uses_alcohol, data = predictors_df, family ="poisson")
summary(model5) # AIC: 156.81
pchisq(deviance(model5), df = df.residual(model5), lower.tail = F) # p-value: 3.198646e-18
model5$deviance # Deviance: 84.57693

model6 = glm(Freq ~ (uses_alcohol * uses_drugs) + depression_diagnosis, data = predictors_df, family ="poisson")
summary(model6) # AIC: 284.06
pchisq(deviance(model6), df = df.residual(model6), lower.tail = F) # p-value: 1.173841e-45
model6$deviance # Deviance: 211.8257

## Comparing models

## Comparing model4 and model5:
anova(model5, model4)
pchisq(81.869, df = 1, lower.tail = F) # p-value: 1.454123e-19

## Comparing model5 and model6:
anova(model6, model5)
pchisq(127.25, df = 0, lower.tail = F) # p-value: 0

## Comparing model4 and model6:
anova(model6, model4)
pchisq(211.826, df = 1, lower.tail = F) # p-value: 5.488989e-48

###################
### Use model 4 ###
###################

### Model 4 residuals

model4_residuals = rstandard(model = model4, type = "pearson")
plot(model4_residuals) # All residuals < |3|

### Model 4 odds ratios and confidence intervals

#### Creating list of terms

terms = list("intercept", "depression_diagnosis", "drug_use", "alcohol_use", "DD_by_drug_use", "drug_use_by_alcohol_use")

#### Creating empty lists for values

estimate = list()
OR = list()
OR_CL_L = list()
OR_CL_U = list()

#### Setting z value

z = 1.96

#### Extracting model coefficients & odds ratios

for(i in 1:length(model4$coefficients)) {
  estimate[[i]] = rep(model4$coefficients[[i]])
  OR[[i]] = rep(exp(model4$coefficients[[i]]))
}

#### Extracting SE's & making list

SE1 = unname(summary(model4)$coefficients[, 2][1])
SE2 = unname(summary(model4)$coefficients[, 2][2])
SE3 = unname(summary(model4)$coefficients[, 2][3])
SE4 = unname(summary(model4)$coefficients[, 2][4])
SE5 = unname(summary(model4)$coefficients[, 2][5])
SE6 = unname(summary(model4)$coefficients[, 2][6])
SE7 = unname(summary(model4)$coefficients[, 2][7])

SE = list(SE1, SE2, SE3, SE4, SE5, SE6)

#### Creating dataframe

model4_df = as.data.frame(cbind(estimate, OR, SE))
model4_df[] = lapply(model4_df, as.numeric)
model4_df$term = terms
model4_df = model4_df[, c("term", "estimate", "OR", "SE")]

#### Creating confidence intervals

model4_df$OR_CL_L = exp(model4_df$estimate - (z * model4_df$SE))
model4_df$OR_CL_U = exp(model4_df$estimate + (z * model4_df$SE))

### Correlation Heat Map

model.matrix(~ 0 + ., data = predictors_df) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(show.diag = FALSE, type = "lower", lab = TRUE, lab_size = 2, tl.cex = 6)

### Forest plot

model4_forest_plot = plot_model(model4, show.values = TRUE, value.size = 4.5, vline.color = "black", title = '',
                                dot.size = 3, line.size = 1.75, labels.y = 10, colors = "blue",
                                terms = c("depression_diagnosisYes:uses_drugs [Yes]", "uses_drugsYes:uses_alcohol [Yes]"),
                                axis.labels = c("depression_diagnosisYes:uses_drugsYes" = "Depression Diagnosis by Drug Use",
                                                "uses_drugsYes:uses_alcoholYes" = "Drug Use by Alcohol Use"))
model4_forest_plot

# Depression diagnosis and disability models

disability_predictors = subset(disability, select = c(depression_diagnosis, AEE, BBL, HL, NSBO))

disability_predictors_table = ftable(x = disability_predictors, row.vars = c("depression_diagnosis", "AEE", "BBL", "HL", "NSBO"))

disability_predictors_df = as.data.frame(disability_predictors_table)

## Running models

disability_3O_model = glm(Freq ~ (depression_diagnosis + AEE + BBL + HL + NSBO) ^ 3, data = disability_predictors_df, family ="poisson")
summary(disability_3O_model) # AIC: 450.49
pchisq(deviance(disability_3O_model), df = df.residual(disability_3O_model), lower.tail = F) # p-value: 1.481391e-24
disability_3O_model$deviance # Deviance: 208.0628

########################################### Homogeneous Association Model ##############################################################
disability_2O_model = glm(Freq ~ (depression_diagnosis + AEE + BBL + HL + NSBO) ^ 2, data = disability_predictors_df, family ="poisson")
summary(disability_2O_model) # AIC: 1505.4
pchisq(deviance(disability_2O_model), df = df.residual(disability_2O_model), lower.tail = F) # p-value: 2.69657e-214
disability_2O_model$deviance # Deviance: 1282.975
########################################################################################################################################

disability_1O_model = glm(Freq ~ depression_diagnosis + AEE + BBL + HL + NSBO, data = disability_predictors_df, family ="poisson")
summary(disability_1O_model) # AIC: 2517.1
pchisq(deviance(disability_1O_model), df = df.residual(disability_1O_model), lower.tail = F) # 0
disability_1O_model$deviance # Deviance: 1282.975

disability_M4 = glm(Freq ~ (depression_diagnosis * AEE) + (depression_diagnosis * BBL) + (depression_diagnosis * HL) +
                      (depression_diagnosis * NSBO), data = disability_predictors_df, family ="poisson")
summary(disability_M4) # AIC: 2424.2
pchisq(deviance(disability_M4), df = df.residual(disability_M4), lower.tail = F) # p-value: 0
disability_M4$deviance # Deviance: 2213.739

### Model comparisons

anova(disability_M4, disability_3O_model)
pchisq(1840.5, df = 16, lower.tail = F) # p-value: 0

anova(disability_M4, disability_2O_model)
pchisq(911.18, df = 6, lower.tail = F) # p-value: 1.438142e-193

anova(disability_1O_model, disability_M4)
pchisq(101.89, df = 4, lower.tail = F) # 3.894138e-21

####################################
# Use homogenous association model #
####################################

### disability_M4 residuals

disability_2O_residuals = rstandard(disability_2O_model, type = "pearson")
disability_2O_residuals
plot(disability_2O_residuals)
# std.res1 = xtabs(disability_M4_residuals ~ disability_predictors_df$depression_diagnosis + disability_predictors_df$AEE + disability_predictors_df$BBL +
#                    disability_predictors_df$HL + disability_predictors_df$NSBO)

table(disability_predictors_df$depression_diagnosis, disability_predictors_df$AEE)

### Second-order model odds ratios and confidence intervals

#### Creating list of terms

disability_terms = list("intercept", "depression_diagnosis", "AEE", "BBL", "HL", "NSBO", "DD by AEE", "DD by BBL", "DD by HL", "DD by NSBO", "AEE by BBL",
                        "AEE by HL", "AEE by NSBO", "BBL by HL", "BBL by NSBO", "HL by NSBO")

#### Creating empty lists for values

estimate = list()
OR = list()
OR_CL_L = list()
OR_CL_U = list()

#### Setting z value

z = 1.96

#### Extracting model coefficients & odds ratios

for(i in 1:length(disability_2O_model$coefficients)) {
  estimate[[i]] = rep(disability_2O_model$coefficients[[i]])
  OR[[i]] = rep(exp(disability_2O_model$coefficients[[i]]))
}

#### Extracting SE's & making list

SE1 = unname(summary(disability_2O_model)$coefficients[, 2][1])
SE2 = unname(summary(disability_2O_model)$coefficients[, 2][2])
SE3 = unname(summary(disability_2O_model)$coefficients[, 2][3])
SE4 = unname(summary(disability_2O_model)$coefficients[, 2][4])
SE5 = unname(summary(disability_2O_model)$coefficients[, 2][5])
SE6 = unname(summary(disability_2O_model)$coefficients[, 2][6])
SE7 = unname(summary(disability_2O_model)$coefficients[, 2][7])
SE8 = unname(summary(disability_2O_model)$coefficients[, 2][8])
SE9 = unname(summary(disability_2O_model)$coefficients[, 2][9])
SE10 = unname(summary(disability_2O_model)$coefficients[, 2][10])
SE11 = unname(summary(disability_2O_model)$coefficients[, 2][11])
SE12 = unname(summary(disability_2O_model)$coefficients[, 2][12])
SE13 = unname(summary(disability_2O_model)$coefficients[, 2][13])
SE14 = unname(summary(disability_2O_model)$coefficients[, 2][14])
SE15 = unname(summary(disability_2O_model)$coefficients[, 2][15])
SE16 = unname(summary(disability_2O_model)$coefficients[, 2][16])

SE = list(SE1, SE2, SE3, SE4, SE5, SE6, SE7, SE8, SE9, SE10, SE11, SE12, SE13, SE14, SE15, SE16)

#### Creating dataframe

disability_model_df = as.data.frame(cbind(estimate, OR, SE))
disability_model_df[] = lapply(disability_model_df, as.numeric)
disability_model_df$term = disability_terms
disability_model_df = disability_model_df[, c("term", "estimate", "OR", "SE")]

#### Creating confidence intervals

disability_model_df$OR_CL_L = exp(disability_model_df$estimate - (z * disability_model_df$SE))
disability_model_df$OR_CL_U = exp(disability_model_df$estimate + (z * disability_model_df$SE))

### Correlation Heat Map

model.matrix(~ 0 + ., data = disability_predictors_df) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(show.diag = FALSE, type = "lower", lab = TRUE, lab_size = 2, tl.cex = 6)

### Forest plot

disability_2O_model_plot = plot_model(disability_2O_model, show.values = TRUE, value.offset = 0.4, value.size = 4, vline.color = "black", title = '',
                                dot.size = 4, line.size = 1.75, labels.y = 10, colors = c("red", "blue"),
                                terms = c("depression_diagnosisYes:AEE [Yes]", "depression_diagnosisYes:BBL [Yes]", "depression_diagnosisYes:HL [Yes]", "depression_diagnosisYes:NSBO [Yes]",
                                          "AEEYes:BBL [Yes]", "AEEYes:HL [Yes]", "AEEYes:NSBO [Yes]", "BBLYes:HLYes [Yes]", "BBLYes:NSBO [Yes]", "HLYes:NSBO [Yes]"),
                                axis.labels = c("depression_diagnosisYes:AEEYes" = "Depression Diagnosis by Allergies/Eye/Ear Disability",
                                                "depression_diagnosisYes:BBLYes" = "Depression Diagnosis by Back/Bone/Limb Disability",
                                                "depression_diagnosisYes:HLYes" = "Depression Diagnosis by Heart/Lung Disability",
                                                "depression_diagnosisYes:NSBOYes" = "Depression Diagnosis by Neuro/Spine/Brain/Other Disability",
                                                "AEEYes:BBLYes" = "Allergies/Eye/Ear Disability by Back/Bone/Limb Disability",
                                                "AEEYes:HLYes" = "Allergies/Eye/Ear Disability by Heart/Lung Disability",
                                                "AEEYes:NSBOYes" = "Allergies/Eye/Ear Disability by Neuro/Spine/Brain/Other Disability",
                                                "BBLYes:HLYes" = "Back/Bone/Limb Disability by Heart/Lung Disability", # not shown
                                                "BBLYes:NSBOYes" = "Back/Bone/Limb Disability by Neuro/Spine/Brain/Other Disability",
                                                "HLYes:NSBOYes" = "Heart/Lung Disability by Neuro/Spine/Brain/Other Disability"))
disability_2O_model_plot


















