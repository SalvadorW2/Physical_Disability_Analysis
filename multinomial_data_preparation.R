### Installing Legacy Packages - requires rTools v. 4.2 ###

# install.packages("rgdal", repos = "https://packagemanager.posit.co/cran/2023-10-13")
# install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")


### Loading Packages ###

library(tidyverse)
library(rgdal) # legacy
library(maptools) # legacy
library(sqldf)
library(conflicted)
library(maps)
library(ggmosaic)

# Resolving conflict
conflicted::conflicts_prefer(maps::map)
conflict_prefer("filter", "dplyr")


prepare_data = function(data) {

  # Removing Irrelevant Variables
  
  data = data[ , !names(data) %in% c("X", "start_date", "end_date", "status", "IP_address", "progress", "duration",
                                     "finished", "recorded_date", "response_ID", "last_name", "first_name", "email",
                                     "external_reference", "distribution_channel", "user_language",
                                     "q_relevant_ID_fraud_score", "q_relevant_ID_last_start_date", "consent",
                                     "survey_feedback", "opp", "qpmid", "RISN", "rid", "V", "PID", "psid", "K2", "ID",
                                     "q_ballot_box_stuffing", "order_number", "gc", "term", "q_total_duration", "LS",
                                     "q_recaptcha_score", "q_relevant_ID_duplicate", "q_relevant_ID_duplicate_score")]
  
  # Removing Unused Variables
  
  data = subset(data[ , !names(data) %in% c("disability_other", "disability_most_serious", "disability_most_serious_other",
                                            "drug_type_other", "diabetes_type", "diabetes_type_other", "prosthesis_type",
                                            "diabetes_LLDI", "mental_health_other", "mental_health_other_description",
                                            "mental_healthcare_sought", "mental_healthcare_unobtained",
                                            "mental_healthcare_unobtained_reason", "mental_healthcare_provider",
                                            "mental_healthcare_duration", "mental_healthcare_method",
                                            "mental_healthcare_relation", "mental_healthcare_effectiveness", "life_change_details",
                                            "life_change_details_accuracy", "expectations_vs_reality", "living_situation_problems",
                                            "health_insurance_other", "social_media_other", "social_media_most_frequent_other")])
  
  # Marking missing values with "NA"
  data[data == ''] <- NA
  data[data == "Missing"] <- NA
  
  # Creating age variable
  data$age = as.numeric(substring(Sys.Date(), 1, 4)) - as.numeric(data$birth_year)
  
  # Refactoring variables
  
  ## Demographics
  ### Removing "Other" & "Prefer not to say" categories due to low counts
  data$gender = ifelse(data$gender == "Female", "Female",
                ifelse(data$gender == "Male", "Male",
                ifelse(data$gender == "Other" | data$race == "Prefer not to say", NA, NA)))
  
  data$race = ifelse(data$ethnicity == "Hispanic or Latino", "Hispanic",
              ifelse(data$race == "White", "White",
              ifelse(data$race == "Black or African American", "Black",
              ifelse(data$race == "Asian" | data$race == "Other", "Other", "Other"))))
  
  data$age = ifelse(data$age < 25, "1) Under 25",
             ifelse(data$age >= 25 & data$age < 36, "2) 26-35",
             ifelse(data$age >= 35 & data$age < 46, "3) 36-45",
             ifelse(data$age >= 45 & data$age < 56, "4) 46-55",
             ifelse(data$age >= 55 & data$age < 66, "5) 56-65",
             ifelse(data$age > 65, "6) Over 65", NA))))))
  
  data$yearly_income = ifelse(data$yearly_income == "$0 to $14,999", "1) 0-14999",
                       ifelse(data$yearly_income == "$15,000 to $24,999", "2) 15000-24999",
                       ifelse(data$yearly_income == "$25,000 to $34,999", "3) 25000-34999",
                       ifelse(data$yearly_income == "$35,000 to $49,999", "4) 35000-49999",
                       ifelse(data$yearly_income == "$50,000 to $74,999", "5) 50000-74999",
                       ifelse(data$yearly_income == "$75,000 or more", "6) 75000 or more",
                       ifelse(data$yearly_income == "Don't know or prefer not to say", "7) Don't know or prefer not to say", NA)))))))
  
  data$occupational_status = ifelse(data$occupational_status == "Disabled, not able to work", "1) Disabled",
                             ifelse(data$occupational_status == "Employed, working 1-39 hours per week", "4) Employed: 1-39 hours/week",
                             ifelse(data$occupational_status == "Employed, working 40 hours per week",  "5) Employed: 40 hours/week",
                             ifelse(data$occupational_status == "Not employed, looking for work" | data$occupational_status == "Not employed, not looking for work", "2) Not Employed",
                             ifelse(data$occupational_status == "Retired", "6) Retired",
                             ifelse(data$occupational_status == "Student", "3) Student", "Missing"))))))
  
  data$education_level = ifelse(data$education_level == "High school graduation, GED or equivalent" | data$education_level == "Some High School" | data$education_level == "Did not complete high school", "1) High School or Lower",
                         ifelse(data$education_level == "Some college, Associate Degree", "2) Some College",
                         ifelse(data$education_level == "Bachelor's degree", "3) Bachelor's",
                         ifelse(data$education_level == "Master's degree" | data$education_level == "Doctoral degree (e.g. PhD, EdD)" | data$education_level == "Professional school degree (e.g. MD, DDS, DVM, JD)", "4) Advanced Degree", "5) Other"))))
  
  data$age_acquired = ifelse(data$age_acquired == "Congenital (from birth)", "1) Congenital",
                      ifelse(data$age_acquired == "During childhood (prior to age 19)", "2) Under 19",
                      ifelse(data$age_acquired == "Age 19-39 years", "3) 19-39",
                      ifelse(data$age_acquired == "Age 40-59 years", "4) 40-59",
                      ifelse(data$age_acquired == "Age 60 years or older", "5) 60 and over", NA)))))
  
  ## Physical Health
  data$assistive_device = ifelse(data$assistive_device == "No", "1) No",
                          ifelse(data$assistive_device == "Yes, I use crutches, a cane, or a walker/rollator", "2) Yes I use crutches/cane/walker/rollator",
                          ifelse(data$assistive_device == "Yes, I use a prosthetic limb", "3) Yes I use a prosthetic limb",
                          ifelse(data$assistive_device == "Yes, I use a wheelchair", "4) Yes I use a wheelchair", NA))))
  
  data$current_living_situation = ifelse(data$current_living_situation == "I do not have a steady place to live (I am temporarily staying with others, in a hotel, in a shelter, living outside on the street, on a beach, in a car, abandoned building, bus or train station, or in a park)", "No steady place to live",
                                  ifelse(data$current_living_situation == "I have a place to live today, but I am worried about losing it in the future", "Have place, but worried about future",
                                  ifelse(data$current_living_situation == "I have a steady place to live", "Have place", NA)))
  
  data$current_living_situation = ifelse(data$current_living_situation == "No steady place to live", "1) No steady place to live",
                                  ifelse(data$current_living_situation == "Have place, but worried about future", "2) Have place, but worried about future",
                                  ifelse(data$current_living_situation == "Have place", "3) Have place", NA)))
  
  data$health_quality = ifelse(data$health_quality == "Poor", "1) Poor",
                        ifelse(data$health_quality == "Fair", "2) Fair",
                        ifelse(data$health_quality == "Good", "3) Good",
                        ifelse(data$health_quality == "Very good", "4) Very good",
                        ifelse(data$health_quality == "Excellent", "5) Excellent", NA)))))
  
  data$two_week_tiredness = ifelse(data$two_week_tiredness == "Not at all", "1) Not at all",
                            ifelse(data$two_week_tiredness == "Several days", "2) Several days",
                            ifelse(data$two_week_tiredness == "More than half the days", "3) More than half the days",
                            ifelse(data$two_week_tiredness == "Nearly every day", "4) Nearly every day", NA))))
  
  data$two_week_sleep_problems = ifelse(data$two_week_sleep_problems == "Not at all", "1) Not at all",
                                 ifelse(data$two_week_sleep_problems == "Several days", "2) Several days",
                                 ifelse(data$two_week_sleep_problems == "More than half the days", "3) More than half the days",
                                 ifelse(data$two_week_sleep_problems == "Nearly every day", "4) Nearly every day", NA))))
  
  data$two_week_appetite = ifelse(data$two_week_appetite == "Not at all", "1) Not at all",
                           ifelse(data$two_week_appetite == "Several days", "2) Several days",
                           ifelse(data$two_week_appetite == "More than half the days", "3) More than half the days",
                           ifelse(data$two_week_appetite == "Nearly every day", "4) Nearly every day", NA))))
  
  data$two_week_trouble_concentrating = ifelse(data$two_week_trouble_concentrating == "Not at all", "1) Not at all",
                                        ifelse(data$two_week_trouble_concentrating == "Several days", "2) Several days",
                                        ifelse(data$two_week_trouble_concentrating == "More than half the days", "3) More than half the days",
                                        ifelse(data$two_week_trouble_concentrating == "Nearly every day", "4) Nearly every day", NA))))
  
  data$two_week_movement = ifelse(data$two_week_movement == "Not at all", "1) Not at all",
                           ifelse(data$two_week_movement == "Several days", "2) Several days",
                           ifelse(data$two_week_movement == "More than half the days", "3) More than half the days",
                           ifelse(data$two_week_movement == "Nearly every day", "4) Nearly every day", NA))))
  
  data$hunger = ifelse(data$hunger == "Not at all", "1) Not at all",
                ifelse(data$hunger == "A little", "2) A little",
                ifelse(data$hunger == "Somewhat", "3) Somewhat",
                ifelse(data$hunger == "Quite a bit", "4) Quite a bit",
                ifelse(data$hunger == "A lot", "5) A lot", NA)))))
  
  data$physically_hurt = ifelse(data$physically_hurt == "Never", "1) Never",
                         ifelse(data$physically_hurt == "Rarely", "2) Rarely",
                         ifelse(data$physically_hurt == "Sometimes", "3) Sometimes",
                         ifelse(data$physically_hurt == "Fairly often", "4) Fairly often",
                         ifelse(data$physically_hurt == "Frequently", "5) Frequently", NA)))))
  
  data$diabetes = ifelse(data$diabetes == "I do not have this", "1) I do not have this",
                  ifelse(data$diabetes == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                  ifelse(data$diabetes == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$peripheral_vascular = ifelse(data$peripheral_vascular == "I do not have this", "1) I do not have this",
                             ifelse(data$peripheral_vascular == "I do have this, and it partially influences my daily functioning", "2) I do have this, and it partially influences my daily functioning",
                             ifelse(data$peripheral_vascular == "I do have this, and it severely influences my daily functioning", "3) I do have this, and it severely influences my daily functioning", NA)))
  
  data$gastrointestinal = ifelse(data$gastrointestinal == "I do not have this", "1) I do not have this",
                          ifelse(data$gastrointestinal == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                          ifelse(data$gastrointestinal == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$obesity = ifelse(data$obesity == "I do not have this", "1) I do not have this",
                 ifelse(data$obesity == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                 ifelse(data$obesity == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$alcohol_quantity = ifelse(data$alcohol_quantity == "None; I do not drink alcohol", "1) None; I do not drink alcohol",
                          ifelse(data$alcohol_quantity == "1 or 2", "2) 1 or 2",
                          ifelse(data$alcohol_quantity == "3 or 4", "3) 3 or 4",
                          ifelse(data$alcohol_quantity == "5 or more", "4) 5 or more", NA))))
  
  data$twelve_month_utility_threat = ifelse(data$twelve_month_utility_threat == "No", "1) No",
                                     ifelse(data$twelve_month_utility_threat == "Yes", "2) Yes",
                                     ifelse(data$twelve_month_utility_threat == "Already shut off", "3) Already shut off", NA)))
  
  ## Mental Health
  data$social_media_check = ifelse(data$social_media_check == "Less often", "1) Less often",
                            ifelse(data$social_media_check == "Every few days", "2) Every few days",
                            ifelse(data$social_media_check == "Daily", "3) Daily",
                            ifelse(data$social_media_check == "Several times per day", "4) Several times per day", NA))))
  
  data$feel_supported = ifelse(data$feel_supported == "Not at all", "1) Not at all",
                              ifelse(data$feel_supported == "A little", "2) A little",
                              ifelse(data$feel_supported == "Somewhat", "3) Somewhat",
                              ifelse(data$feel_supported == "Quite a bit", "4) Quite a bit",
                              ifelse(data$feel_supported == "A lot", "5) A lot", NA)))))
  
  data$family_support = ifelse(data$family_support == "Not at all", "1) Not at all",
                              ifelse(data$family_support == "A little", "2) A little",
                              ifelse(data$family_support == "Somewhat", "3) Somewhat",
                              ifelse(data$family_support == "Quite a bit", "4) Quite a bit",
                              ifelse(data$family_support == "A lot", "5) A lot", NA)))))
  
  data$cooperation = ifelse(data$cooperation == "Not at all", "1) Not at all",
                           ifelse(data$cooperation == "A little", "2) A little",
                           ifelse(data$cooperation == "Somewhat", "3) Somewhat",
                           ifelse(data$cooperation == "Quite a bit", "4) Quite a bit",
                           ifelse(data$cooperation == "A lot", "5) A lot", NA)))))
  
  data$skill_improvement = ifelse(data$skill_improvement == "Not at all", "1) Not at all",
                                 ifelse(data$skill_improvement == "A little", "2) A little",
                                 ifelse(data$skill_improvement == "Somewhat", "3) Somewhat",
                                 ifelse(data$skill_improvement == "Quite a bit", "4) Quite a bit",
                                 ifelse(data$skill_improvement == "A lot", "5) A lot", NA)))))
  
  data$social_behavior = ifelse(data$social_behavior == "Not at all", "1) Not at all",
                               ifelse(data$social_behavior == "A little", "2) A little",
                               ifelse(data$social_behavior == "Somewhat", "3) Somewhat",
                               ifelse(data$social_behavior == "Quite a bit", "4) Quite a bit",
                               ifelse(data$social_behavior == "A lot", "5) A lot", NA)))))
  
  data$family_knowledge = ifelse(data$family_knowledge == "Not at all", "1) Not at all",
                                ifelse(data$family_knowledge == "A little", "2) A little",
                                ifelse(data$family_knowledge == "Somewhat", "3) Somewhat",
                                ifelse(data$family_knowledge == "Quite a bit", "4) Quite a bit",
                                ifelse(data$family_knowledge == "A lot", "5) A lot", NA)))))
  
  data$likeability = ifelse(data$likeability == "Not at all", "1) Not at all",
                           ifelse(data$likeability == "A little", "2) A little",
                           ifelse(data$likeability == "Somewhat", "3) Somewhat",
                           ifelse(data$likeability == "Quite a bit", "4) Quite a bit",
                           ifelse(data$likeability == "A lot", "5) A lot", NA)))))
  
  data$communicate_feelings = ifelse(data$communicate_feelings == "Not at all", "1) Not at all",
                                    ifelse(data$communicate_feelings == "A little", "2) A little",
                                    ifelse(data$communicate_feelings == "Somewhat", "3) Somewhat",
                                    ifelse(data$communicate_feelings == "Quite a bit", "4) Quite a bit",
                                    ifelse(data$communicate_feelings == "A lot", "5) A lot", NA)))))
  
  data$feel_belonging = ifelse(data$feel_belonging == "Not at all", "1) Not at all",
                              ifelse(data$feel_belonging == "A little", "2) A little",
                              ifelse(data$feel_belonging == "Somewhat", "3) Somewhat",
                              ifelse(data$feel_belonging == "Quite a bit", "4) Quite a bit",
                              ifelse(data$feel_belonging == "A lot", "5) A lot", NA)))))
  
  data$friends_stand_by = ifelse(data$friends_stand_by == "Not at all", "1) Not at all",
                                ifelse(data$friends_stand_by == "A little", "2) A little",
                                ifelse(data$friends_stand_by == "Somewhat", "3) Somewhat",
                                ifelse(data$friends_stand_by == "Quite a bit", "4) Quite a bit",
                                ifelse(data$friends_stand_by == "A lot", "5) A lot", NA)))))
  
  data$family_stand_by = ifelse(data$family_stand_by == "Not at all", "1) Not at all",
                               ifelse(data$family_stand_by == "A little", "2) A little",
                               ifelse(data$family_stand_by == "Somewhat", "3) Somewhat",
                               ifelse(data$family_stand_by == "Quite a bit", "4) Quite a bit",
                               ifelse(data$family_stand_by == "A lot", "5) A lot", NA)))))
  
  data$treated_fairly = ifelse(data$treated_fairly == "Not at all", "1) Not at all",
                              ifelse(data$treated_fairly == "A little", "2) A little",
                              ifelse(data$treated_fairly == "Somewhat", "3) Somewhat",
                              ifelse(data$treated_fairly == "Quite a bit", "4) Quite a bit",
                              ifelse(data$treated_fairly == "A lot", "5) A lot", NA)))))
  
  data$act_responsibly_opportunities = ifelse(data$act_responsibly_opportunities == "Not at all", "1) Not at all",
                                             ifelse(data$act_responsibly_opportunities == "A little", "2) A little",
                                             ifelse(data$act_responsibly_opportunities == "Somewhat", "3) Somewhat",
                                             ifelse(data$act_responsibly_opportunities == "Quite a bit", "4) Quite a bit",
                                             ifelse(data$act_responsibly_opportunities == "A lot", "5) A lot", NA)))))
  
  data$feel_secure_with_family = ifelse(data$feel_secure_with_family == "Not at all", "1) Not at all",
                                 ifelse(data$feel_secure_with_family == "A little", "2) A little",
                                 ifelse(data$feel_secure_with_family == "Somewhat", "3) Somewhat",
                                 ifelse(data$feel_secure_with_family == "Quite a bit", "4) Quite a bit",
                                 ifelse(data$feel_secure_with_family == "A lot", "5) A lot", NA)))))
  
  data$apply_abilities_opportunities = ifelse(data$apply_abilities_opportunities == "Not at all", "1) Not at all",
                                       ifelse(data$apply_abilities_opportunities == "A little", "2) A little",
                                       ifelse(data$apply_abilities_opportunities == "Somewhat", "3) Somewhat",
                                       ifelse(data$apply_abilities_opportunities == "Quite a bit", "4) Quite a bit",
                                       ifelse(data$apply_abilities_opportunities == "A lot", "5) A lot", NA)))))
  
  data$enjoy_family_culture = ifelse(data$enjoy_family_culture == "Not at all", "1) Not at all",
                              ifelse(data$enjoy_family_culture == "A little", "2) A little",
                              ifelse(data$enjoy_family_culture == "Somewhat", "3) Somewhat",
                              ifelse(data$enjoy_family_culture == "Quite a bit", "4) Quite a bit",
                              ifelse(data$enjoy_family_culture == "A lot", "5) A lot", NA)))))
  
  data$threaten_harm = ifelse(data$threaten_harm == "Never", "1) Never",
                       ifelse(data$threaten_harm == "Rarely", "2) Rarely",
                       ifelse(data$threaten_harm == "Sometimes", "3) Sometimes",
                       ifelse(data$threaten_harm == "Fairly often", "4) Fairly often",
                       ifelse(data$threaten_harm == "Frequently", "5) Frequently", NA)))))
  
  data$screamed_at = ifelse(data$screamed_at == "Never", "1) Never",
                     ifelse(data$screamed_at == "Rarely", "2) Rarely",
                     ifelse(data$screamed_at == "Sometimes", "3) Sometimes",
                     ifelse(data$screamed_at == "Fairly often", "4) Fairly often",
                     ifelse(data$screamed_at == "Frequently", "5) Frequently", NA)))))
  
  data$insulted = ifelse(data$insulted == "Never", "1) Never",
                  ifelse(data$insulted == "Rarely", "2) Rarely",
                  ifelse(data$insulted == "Sometimes", "3) Sometimes",
                  ifelse(data$insulted == "Fairly often", "4) Fairly often",
                  ifelse(data$insulted == "Frequently", "5) Frequently", NA)))))
  
  data$humiliated = ifelse(data$humiliated == "Never", "1) Never",
                    ifelse(data$humiliated == "Rarely", "2) Rarely",
                    ifelse(data$humiliated == "Sometimes", "3) Sometimes",
                    ifelse(data$humiliated == "Fairly often", "4) Fairly often",
                    ifelse(data$humiliated == "Frequently", "5) Frequently", NA)))))
  
  data$depression = ifelse(data$depression == "I do not have this", "1) I do not have this",
                    ifelse(data$depression == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                    ifelse(data$depression == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$anxiety = ifelse(data$anxiety == "I do not have this", "1) I do not have this",
                 ifelse(data$anxiety == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                 ifelse(data$anxiety == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$dementia = ifelse(data$dementia == "I do not have this", "1) I do not have this",
                  ifelse(data$dementia == "I do have this, and it partially influences my daily functioning", "2) I do have this and it partially influences my daily functioning",
                  ifelse(data$dementia == "I do have this, and it severely influences my daily functioning", "3) I do have this and it severely influences my daily functioning", NA)))
  
  data$two_week_feel_let_down = ifelse(data$two_week_feel_let_down == "Not at all", "1) Not at all",
                                ifelse(data$two_week_feel_let_down == "Several days", "2) Several days",
                                ifelse(data$two_week_feel_let_down == "More than half the days", "3) More than half the days",
                                ifelse(data$two_week_feel_let_down == "Nearly every day", "4) Nearly every day", NA))))
  
  data$two_week_self_harm = ifelse(data$two_week_self_harm == "Not at all", "1) Not at all",
                            ifelse(data$two_week_self_harm == "Several days", "2) Several days",
                            ifelse(data$two_week_self_harm == "More than half the days", "3) More than half the days",
                            ifelse(data$two_week_self_harm == "Nearly every day", "4) Nearly every day", NA))))
  
  data$disability_visibility = ifelse(data$disability_visibility == "No", "1) No",
                               ifelse(data$disability_visibility == "Maybe", "2) Maybe",
                               ifelse(data$disability_visibility == "Yes", "3) Yes", NA)))
  
  data$physical_disability_discrimination = ifelse(data$physical_disability_discrimination == "No", "1) No",
                                            ifelse(data$physical_disability_discrimination == "Maybe", "2) Maybe",
                                            ifelse(data$physical_disability_discrimination == "Yes", "3) Yes", NA)))
  
  data$substance_mental_health = ifelse(data$substance_mental_health == "I have never experienced this", "1) I have never experienced this",
                                       ifelse(data$substance_mental_health == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                                       ifelse(data$substance_mental_health == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$eating_disorder = ifelse(data$eating_disorder == "I have never experienced this", "1) I have never experienced this",
                         ifelse(data$eating_disorder == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                         ifelse(data$eating_disorder == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$personality_disorder = ifelse(data$personality_disorder == "I have never experienced this", "1) I have never experienced this",
                              ifelse(data$personality_disorder == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                              ifelse(data$personality_disorder == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$autism = ifelse(data$autism == "I have never experienced this", "1) I have never experienced this",
                ifelse(data$autism == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                ifelse(data$autism == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$ADHD = ifelse(data$ADHD == "I have never experienced this", "1) I have never experienced this",
              ifelse(data$ADHD == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
              ifelse(data$ADHD == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$schizophrenia_mental_health = ifelse(data$schizophrenia_mental_health == "I have never experienced this", "1) I have never experienced this",
                                     ifelse(data$schizophrenia_mental_health == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                                     ifelse(data$schizophrenia_mental_health == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$mental_health_mood = ifelse(data$mental_health_mood == "I have never experienced this", "1) I have never experienced this",
                            ifelse(data$mental_health_mood == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                            ifelse(data$mental_health_mood == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  data$mental_health_stress = ifelse(data$mental_health_stress == "I have never experienced this", "1) I have never experienced this",
                              ifelse(data$mental_health_stress == "I experienced this in the past but do not anymore", "2) I experienced this in the past but do not anymore",
                              ifelse(data$mental_health_stress == "I am currently experiencing this", "3) I am currently experiencing this", NA)))
  
  ## Limb Loss
  data$limb_disability = ifelse(grepl("upper or lower limb loss (amputation), difference, or impairment", data$disability_type, fixed = TRUE), "Yes", "No")
  
  # data$LLDI = ifelse(data$assistive_device == "Yes, I use a prosthetic limb" |
  #                    data$limb_disability == "Yes", "Yes", "No")
  
  ### Creating Binary Variables ###
  
  ## Disability_Type
  data$allergy_disability = ifelse(grepl("allergies and breathing problems", data$disability_type, fixed = TRUE), "Yes", "No")
  data$back_disability = ifelse(grepl("back pain and disc problem", data$disability_type, fixed = TRUE), "Yes", "No")
  data$bone_disability = ifelse(grepl("bone or joint issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$heart_disability = ifelse(grepl("heart issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$eye_disability = ifelse(grepl("blindness and vision loss", data$disability_type, fixed = TRUE), "Yes", "No")
  data$ear_disability = ifelse(grepl("deafness and impaired hearing, speech and language difficulties, dyslexia", data$disability_type, fixed = TRUE), "Yes", "No")
  data$lung_disability = ifelse(grepl("lung or breathing issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$neuro_disability = ifelse(grepl("neurological disorder", data$disability_type, fixed = TRUE), "Yes", "No")
  data$other_disability = ifelse(grepl("other", data$disability_type, fixed = TRUE), "Yes", "No")
  data$spine_or_brain_disability = ifelse(grepl("spinal cord injury, traumatic brain injury, or stroke", data$disability_type, fixed = TRUE), "Yes", "No")
  
  ## Drug_Type
  data$uses_drugs = ifelse(is.na(data$drug_type), "No", "Yes")
  data$uses_coca = ifelse(grepl("coca", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_hallucinogens = ifelse(grepl("hallucinogens", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_heroin = ifelse(grepl("heroin", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_THC = ifelse(grepl("marijuana, hash, THC, or grass", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_painkillers = ifelse(grepl("painkillers", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_sedatives = ifelse(grepl("sedatives", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_stimulants = ifelse(grepl("stimulants", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_tranquilizers = ifelse(grepl("tranquilizers or anti-anxiety drugs", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_inhalents = ifelse(grepl("inhalants or solvents", data$drug_type, fixed = TRUE), "Yes", "No")
  data$uses_other = ifelse(grepl("other", data$drug_type, fixed = TRUE), "Yes", "No")
  
  data$uses_other = ifelse(data$uses_inhalents == "Yes" | data$uses_other == "Yes", "Yes", "No")
  
  ## Response Variables
  data$has_depression = ifelse(data$depression == "1) I do not have this", "No", "Yes")
  data$has_anxiety = ifelse(data$anxiety == "1) I do not have this", "No", "Yes")
  
  ## Replacing impossible values with NA
  data = data %>% mutate(birth_year = replace(birth_year, birth_year > 2023, NA))
  data = data %>% mutate(birth_year = replace(birth_year, birth_year < 1900, NA))
  
  ### Acquiring Geographic Data ###
  
  # Function for getting counties
  find_counties = function(pointsDF) {
    
    counties = map("county", fill = TRUE, col = "transparent", plot = FALSE)
    IDs = sapply(strsplit(counties$names, ':'), function(x) x[1])
    counties_sp = map2SpatialPolygons(counties, IDs = IDs,
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    pointsSP = SpatialPoints(pointsDF, 
                             proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    indices = over(pointsSP, counties_sp)
    
    countyNames = sapply(counties_sp@polygons, function(x) x@ID)
    countyNames[indices]
  }
  ### Source: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
  
  # Cleaning County Data
  coordinates = data.frame(longitude = as.numeric(data$longitude), latitude = as.numeric(data$latitude))
  data$County = find_counties(coordinates)
  
  data$State = gsub(",.*$", "", data$County)
  data$county = sub(".*,", "", data$County)
  
  countiesData = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\KSU\\Spring 2023 GRA\\ruralurbancodes2013.csv")
  ### Source: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
  
  ## Preparing to Join Geographic Data to Main Dataframe
  data$State = stri_trans_general(data$State, id = "Title")
  data$county = stri_trans_general(data$county, id = "Title")
  data$County_Name = paste(data$county, "County", sep = ' ')
  
  ### Creating state abbreviations with lookup table
  state_lookup = data.frame(
    State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delware", "Florida", "Georgia",
              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
              "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
              "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
              "Wisconsin", "Wyoming"),
    State_Abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HW", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                  "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  )
  
  # Merging dataframe with lookup table
  data = merge(x = data, y = state_lookup, by.x = "State", by.y = "State")
  
  data$State_Abb[is.na(data$State_Abb)] = "Other"
  
  # Joining with SQL
  
  data = subset(data, select = -c(County, county))
  
  data = sqldf("SELECT D.*, C.Population_2010, C.RUCC_2013 AS rurality
                FROM data AS D
                LEFT JOIN countiesData AS C
                  ON D.County_Name = C.County_Name
                    AND D.State_Abb = C.State")
  
  # Distinguishing between urban and rural
  data$is_rural = ifelse((data$rurality == 5 |
                          data$rurality == 6 |
                          data$rurality == 7), "Yes", "No")
  
  data$is_urban = ifelse((data$rurality == 1 |
                          data$rurality == 2 |
                          data$rurality == 3), "Yes", "No")
  
  # Getting Census Regions/Divisions
  data$census_region = ifelse(data$State == "Alaska" |
                              data$State == "Hawaii" |
                              data$State == "Washington" |
                              data$State == "Oregon" |
                              data$State == "California" |
                              data$State == "Idaho" |
                              data$State == "Nevada" |
                              data$State == "Montana" |
                              data$State == "Wyoming" |
                              data$State == "Utah" |
                              data$State == "Colorado" |
                              data$State == "Arizona" |
                              data$State == "New Mexico", "West",
                       ifelse(data$State == "North Dakota" |
                              data$State == "South Dakota" |
                              data$State == "Minnesota" |
                              data$State == "Nebraska" |
                              data$State == "Kansas" |
                              data$State == "Iowa" |
                              data$State == "Missouri" |
                              data$State == "Wisconsin" |
                              data$State == "Michigan" |
                              data$State == "Indiana" |
                              data$State == "Illinois" |
                              data$State == "Ohio", "Midwest",
                       ifelse(data$State == "Texas" |
                              data$State == "Oklahoma" |
                              data$State == "Arkansas" |
                              data$State == "Louisiana" |
                              data$State == "Mississippi" |
                              data$State == "Alabama" |
                              data$State == "Tennessee" |
                              data$State == "Kentucky" |
                              data$State == "West Virginia" |
                              data$State == "Virginia" |
                              data$State == "North Carolina" |
                              data$State == "South Carolina" |
                              data$State == "Georgia" |
                              data$State == "Maryland" |
                              data$State == "Delaware" |
                              data$State == "District Of Columbia" |
                              data$State == "Florida", "South",
                       ifelse(data$State == "Maine" |
                              data$State == "New Hampshire" |
                              data$State == "Vermont" |
                              data$State == "Massachusetts" |
                              data$State == "Rhode Island" |
                              data$State == "Connecticut" |
                              data$State == "New York" |
                              data$State == "New Jersey", "Northeast", NA))))
  
  ### Removing Variables Not Used in Analysis
  
  data = data[ , !names(data) %in% c("latitude", "longitude", "birth_month", "birth_day", "birth_year",
                                      "ethnicity", "ZIP_Code", "State", "County_Name", "alcohol_self_perception",
                                      "alcohol_use", "alcohol_start_age", "alcohol_self_perception", "alcohol_criticism",
                                      "alcohol_guilt", "alcohol_morning", "social_media_most_frequent",
                                      "State_Abb", "Population_2010", "uses_inhalents", "drug_type", "diabetes_limb_loss", "")]
  
  return(data)

}

create_mosaic = function(data) {
  
  # Source: https://www.kaggle.com/code/dhafer/mosaics-plots-using-ggmosaic

  data = na.omit(data)
  
  plot = ggplot(data = data) +
    geom_mosaic(aes(x = product(has_anxiety, has_depression), fill = has_depression))
  
  plot_data = ggplot_build(plot)$data %>% as.data.frame() %>% filter(.wt > 0)
  
  compt_perc = function(x){
    d = c(x, 1) - c(0, x)
    d[-length(d)]
  }
  
  x = tapply(plot_data$ymax, factor(plot_data$fill, levels = unique(plot_data$fill)), compt_perc)
  x = unlist(x)
  
  plot_data$percentage = paste0(round(100 * x, 2), "%")
  
  plot = plot + geom_label(data = plot_data, aes(x = (xmin + xmax) / 2,  y = (ymin + ymax) / 2, label = percentage))
  plot = plot + xlab("Anxiety") + ylab("Depression") + theme_bw()
  plot = plot + theme(legend.position = "none")
  
  x = chisq.test(xtabs( ~ has_anxiety + has_depression, data = data))
  
  plot = plot + geom_label(data = plot_data,  aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = percentage))
  plot + ggtitle("Anxiety vs. Depression")
  
  return(plot)

}
























