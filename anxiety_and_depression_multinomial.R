### Loading Packages ###

library(tidyverse)
library(nnet)
library(car)
library(ggcorrplot)
library(sjPlot)
library(stargazer)
library(pROC)


### Loading Source Files ###

# Data cleaning
source("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\disability_data_cleaning.R")

# Data preparation
source("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\multinomial_data_preparation.R")


### Importing and Cleaning Data ###

# Importing raw survey data
data = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\Physical_Disability_Analysis\\Phys Disability Behavioral Health_September 6, 2022_14.22.csv")

# Cleaning data
disability = clean_data(data)

# Preparing data
data = prepare_data(disability)


## Subsetting to predictors appropriate for analysis
predictors = subset(data, select = c(gender, age, race, education_level, occupational_status, yearly_income, current_living_situation,
                               diabetes, peripheral_vascular, gastrointestinal, obesity, eating_disorder,
                               alcohol_quantity,  health_quality, two_week_sleep_problems, assistive_device,
                               two_week_tiredness, two_week_appetite, two_week_trouble_concentrating, two_week_movement,
                               hunger, physically_hurt, twelve_month_food_worry, twelve_month_utility_threat, transportation_problems,
                               dementia, personality_disorder, autism, ADHD, feel_supported,
                               family_support, two_week_trouble_concentrating, schizophrenia_mental_health, disability_visibility,
                               physical_disability_discrimination, cooperation, skill_improvement, social_behavior,
                               family_support, family_knowledge, likeability, communicate_feelings,
                               feel_belonging, friends_stand_by, family_stand_by, treated_fairly,
                               act_responsibly_opportunities, feel_secure_with_family, apply_abilities_opportunities, enjoy_family_culture,
                               threaten_harm, screamed_at, humiliated, social_media_check,
                               allergy_disability, back_disability, bone_disability, heart_disability, eye_disability, ear_disability,
                               limb_disability, lung_disability, neuro_disability,  other_disability, spine_or_brain_disability,
                               census_region, is_urban, is_rural, has_anxiety, has_depression))


### Analysis ###

#  Setting seed
set.seed(99)

# Establishing relationship

anxiety_depression_table = table(x = predictors$has_anxiety, y = predictors$has_depression, dnn = c("Anxiety", "Depression"))
A_D_test = chisq.test(anxiety_depression_table)
A_D_test

# Creating mosaic plot
mosaic = create_mosaic(predictors)
mosaic

# Recoding anxiety and depression as factors
predictors$A_D = ifelse(predictors$has_anxiety == "No" & predictors$has_depression == "No", 0,
                 ifelse(predictors$has_anxiety == "No" & predictors$has_depression == "Yes", 1,
                 ifelse(predictors$has_anxiety == "Yes" & predictors$has_depression == "No", 2,
                 ifelse(predictors$has_anxiety == "Yes" & predictors$has_depression == "Yes", 3, NA))))

# Dropping original anxiety and depression variables
predictors2 = predictors[ , !names(predictors) %in% c("has_anxiety", "has_depression")]

# Making all variables factors
predictors2 = as.data.frame(unclass(predictors2), stringsAsFactors = TRUE)

# Building model through stepwise regression based on AIC
##  multinom_model = glm(A_D ~ ., data = na.omit(predictors))
## stepwise = step(multinom_model, direction = "both")

# Original model
model1 = multinom(A_D ~ gender + age + race + education_level + occupational_status +
                  yearly_income + gastrointestinal + obesity + alcohol_quantity +
                  two_week_sleep_problems + assistive_device + two_week_tiredness +
                  two_week_appetite + two_week_trouble_concentrating + two_week_movement +
                  twelve_month_utility_threat + dementia + personality_disorder +
                  ADHD + schizophrenia_mental_health + physical_disability_discrimination +
                  feel_belonging + treated_fairly + allergy_disability + back_disability +
                  is_urban + is_rural, data = predictors2, family = "poisson")

# Checking p-values and removing variables with p >= 0.05
Anova(model1)

# Model
model2 = multinom(A_D ~ gender + age + race + yearly_income + gastrointestinal + obesity + two_week_sleep_problems + assistive_device +
                    twelve_month_utility_threat + dementia + personality_disorder + ADHD + schizophrenia_mental_health +
                    physical_disability_discrimination + feel_belonging + allergy_disability + back_disability,
                  data = predictors2, family = "poisson")

Anova(model2)

stargazer(model2, type = "text", title = "Model Summary Statistics", out = "model_output.txt", report = "vctp*s")

# Saving model coefficients
coefficients = subset(predictors2, select = c(A_D, gender, age, race, yearly_income, gastrointestinal, obesity, two_week_sleep_problems,
                                             assistive_device, twelve_month_utility_threat, dementia, personality_disorder, ADHD,
                                             schizophrenia_mental_health, physical_disability_discrimination, feel_belonging,
                                             allergy_disability, back_disability))

# Correlation matrix
model.matrix(~ 0 + ., data = coefficients) %>%
  cor(use = "pairwise.complete.obs") %>%
  ggcorrplot(show.diag = FALSE, type = "lower", lab = TRUE, lab_size = 2, tl.cex = 6)

# Forest Plot (No commas!)
model_forest_plot = plot_model(model2, show.values = TRUE, vline.color = "red", title = "Anxiety and Depression Model Plots", wrap.labels = 60,
                               value.offset = 0.5, line.size = 0.5, dot.size = 1.5, width = 0.5, colors = "blue", value.size = 2.1,
                               terms = c("gender [Male]",
                                         "age [2) 26-35]", "age [3) 36-45]", "age [4) 46-55]", "age [5) 56-65]", "age [6) Over 65]",
                                         "race [Hispanic]", "race [Other]", "race [White]",
                                         "yearly_income [2) 15000-24999]", "yearly_income [3) 25000-34999]", "yearly_income [4) 35000-49999]", "yearly_income [5) 50000-74999]", "yearly_income [6) 75000 or more]", "yearly_income [7) Don't know or prefer not to say]",
                                         "gastrointestinal [2) I do have this and it partially influences my daily functioning]", "gastrointestinal [3) I do have this and it severely influences my daily functioning]",
                                         "obesity [2) I do have this and it partially influences my daily functioning]", "obesity [3) I do have this and it severely influences my daily functioning]",
                                         "two_week_sleep_problems [2) Several days]", "two_week_sleep_problems [3) More than half the days]", "two_week_sleep_problems [4) Nearly every day]",
                                         "assistive_device [2) Yes I use crutches/cane/walker/rollator]", "assistive_device [3) Yes I use a prosthetic limb]", "assistive_device [4) Yes I use a wheelchair]",
                                         "twelve_month_utility_threat [2) Yes]", "twelve_month_utility_threat [3) Already shut off]",
                                         "dementia [2) I do have this and it partially influences my daily functioning]", "dementia [3) I do have this and it severely influences my daily functioning]",
                                         "personality_disorder [2) I experienced this in the past but do not anymore]", "personality_disorder [3) I am currently experiencing this]",
                                         "ADHD [2) I experienced this in the past but do not anymore]", "ADHD [3) I am currently experiencing this]",
                                         "schizophrenia_mental_health [2) I experienced this in the past but do not anymore]", "schizophrenia_mental_health [3) I am currently experiencing this]",
                                         "physical_disability_discrimination [2) Maybe]", "physical_disability_discrimination [3) Yes]",
                                         "feel_belonging [2) A little]", "feel_belonging [3) Somewhat]", "feel_belonging [4) Quite a bit]", "feel_belonging [5) A lot]",
                                         "allergy_disability [Yes]",
                                         "back_disability [Yes]"),
                               axis.labels = c("genderMale" = "Gender: Male",
                                               "age2) 26-35" = "Age: 26-35", "age3) 36-45" = "Age: 36-45", "age4) 46-55" = "Age: 46-55", "age5) 56-65" = "Age: 56-65", "age6) Over 65" = "Age: Over 65",
                                               "raceHispanic" = "Race: Hispanic", "raceOther" = "Race: Other", "raceWhite" = "Race: White",
                                               "yearly_income2) 15000-24999" = "Yearly Income: $15,000 to $24,999", "yearly_income3) 25000-34999" = "Yearly Income: $25,000 to 34,999", "yearly_income4) 35000-49999" = "Yearly Income: $35,000 to $49,999", "yearly_income5) 50000-74999" = "Yearly Income: $50,000 to $74,999", "yearly_income6) 75000 or more" = "Yearly Income: $75,000 or More", "yearly_income7) Don't know or prefer not to say" = "Yearly Income: Don't Know/Prefer not to Say",
                                               "gastrointestinal2) I do have this and it partially influences my daily functioning" = "Gastro: Partial Influence", "gastrointestinal3) I do have this and it severely influences my daily functioning"  = "Gastro: Severe Influence",
                                               "obesity2) I do have this and it partially influences my daily functioning" = "Obesity: Partial Influence", "obesity3) I do have this and it severely influences my daily functioning"  = "Obesity: Severe Influence",
                                               "two_week_sleep_problems2) Several days" = "2W Sleep Problems: Several Days", "two_week_sleep_problems3) More than half the days" = "2W Sleep Problems: More than half days", "two_week_sleep_problems4) Nearly every day" = "2W Sleep Problems: Nearly Every Day",
                                               "assistive_device2) Yes I use crutches/cane/walker/rollator" = "Assistive Device: Crutches/Cane/Walker/Rollator", "assistive_device3) Yes I use a prosthetic limb" = "Assistive Device: Prosthetic Limb", "assistive_device4) Yes I use a wheelchair" = "Assistive Device: Wheelchair",
                                               "twelve_month_utility_threat2) Yes" = "12 Month Utility Threat: Yes", "twelve_month_utility_threat3) Already shut off" = "12 Month Utility Threat: Already Shut Off",
                                               "dementia2) I do have this and it partially influences my daily functioning" = "Dementia: Partial Influence", "dementia3) I do have this and it severely influences my daily functioning" = "Dementia: Severe Influence",
                                               "personality_disorder2) I experienced this in the past but do not anymore" = "Personality Disorder: Previously Experienced", "personality_disorder3) I am currently experiencing this" = "Personality Disorder: Currently Experiencing",
                                               "ADHD2) I experienced this in the past but do not anymore" = "ADHD: Previously Experienced", "ADHD3) I am currently experiencing this" = "ADHD: Currently Experiencing",
                                               "schizophrenia_mental_health2) I experienced this in the past but do not anymore" = "Schizophrenia: Previously Experienced", "schizophrenia_mental_health3) I am currently experiencing this" = "Schizophrenia: Currently Experiencing",
                                               "physical_disability_discrimination2) Maybe" = "Physical Disability Discrimination: Maybe", "physical_disability_discrimination3) Yes" = "Physical Disability Discrimination: Yes",
                                               "feel_belonging2) A little" = "Feel Belonging: A Little", "feel_belonging3) Somewhat" = "Feel Belonging: Somewhat", "feel_belonging4) Quite a bit" = "Feel Belonging: Quite a Bit", "feel_belonging5) A lot" = "Feel Belonging: A Lot",
                                               "allergy_disabilityYes" = "Allergy Disability: Yes", "back_disabilityYes" = "Back Disability: Yes"))
model_forest_plot

# Forest plot without changed labels
model_forest_plot2 = plot_model(model2, show.values = TRUE, vline.color = "red", title = "Anxiety and Depression Model Plots",
                               value.offset = 0.5, line.size = 0.5, dot.size = 1.5, width = 0.5, colors = "blue", value.size = 2.1)
model_forest_plot2

# Forest plot with 6 most important predictors
model_forest_plot_importance = plot_model(model2, show.values = TRUE, vline.color = "red", title = "Anxiety and Depression Model Plots", wrap.labels = 60,
                               value.offset = 0.5, line.size = 0.5, dot.size = 1.5, width = 0.5, colors = "blue", value.size = 2.1,
                               terms = c("two_week_sleep_problems [2) Several days]", "two_week_sleep_problems [3) More than half the days]", "two_week_sleep_problems [4) Nearly every day]",
                                         "schizophrenia_mental_health [2) I experienced this in the past but do not anymore]", "schizophrenia_mental_health [3) I am currently experiencing this]",
                                         "ADHD [2) I experienced this in the past but do not anymore]", "ADHD [3) I am currently experiencing this]",
                                         "dementia [2) I do have this and it partially influences my daily functioning]", "dementia [3) I do have this and it severely influences my daily functioning]",
                                         "personality_disorder [2) I experienced this in the past but do not anymore]", "personality_disorder [3) I am currently experiencing this]",
                                         "obesity [2) I do have this and it partially influences my daily functioning]", "obesity [3) I do have this and it severely influences my daily functioning]"),
                               axis.labels = c("two_week_sleep_problems2) Several days" = "2W Sleep Problems: Several Days", "two_week_sleep_problems3) More than half the days" = "2W Sleep Problems: More than half days", "two_week_sleep_problems4) Nearly every day" = "2W Sleep Problems: Nearly Every Day",
                                               "schizophrenia_mental_health2) I experienced this in the past but do not anymore" = "Schizophrenia: Previously Experienced", "schizophrenia_mental_health3) I am currently experiencing this" = "Schizophrenia: Currently Experiencing",
                                               "ADHD2) I experienced this in the past but do not anymore" = "ADHD: Previously Experienced", "ADHD3) I am currently experiencing this" = "ADHD: Currently Experiencing",
                                               "dementia2) I do have this and it partially influences my daily functioning" = "Dementia: Partial Influence", "dementia3) I do have this and it severely influences my daily functioning" = "Dementia: Severe Influence",
                                               "personality_disorder2) I experienced this in the past but do not anymore" = "Personality Disorder: Previously Experienced", "personality_disorder3) I am currently experiencing this" = "Personality Disorder: Currently Experiencing",
                                               "obesity2) I do have this and it partially influences my daily functioning" = "Obesity: Partial Influence", "obesity3) I do have this and it severely influences my daily functioning"  = "Obesity: Severe Influence"))
model_forest_plot_importance

# ROC Curves
probs = predict(model2, newdata = predictors2, type = "prob")
head(probs)

## Generating ROC curves for each class
roc_A = roc(predictors2$A_D == "1", probs[, "1"])
roc_B = roc(predictors2$A_D == "2", probs[, "2"])
roc_C = roc(predictors2$A_D == "3", probs[, "3"])

## AUC Values
roc_A # Depression only
roc_B # Anxiety only
roc_C # Anxiety and depression

## Plotting ROC curves
plot(roc_A, col = "red", xlab = "1 - Specificity")
lines(roc_B, col = "green")
lines(roc_C, col = "blue")
legend("bottomright", legend = c("Depression AUC: 0.685", "Anxiety AUC: 0.692", "Depression & Anxiety AUC: 0.850"), cex = 0.65,
       col = c("red", "green", "blue"), lty = 1)






























