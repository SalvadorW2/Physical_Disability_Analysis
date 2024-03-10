### Loading Packages ###

library(tidyverse)


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
  data = subset(data[ , !names(data) %in% c("disability_other", "disability_most_serious_other",
                                                              "drug_type_other", "diabetes_type", "diabetes_type_other", "prosthesis_type",
                                                              "diabetes_LLDI", "mental_health_other",
                                                              "mental_health_other_description", "mental_healthcare_sought",
                                                              "mental_healthcare_unobtained", "mental_healthcare_unobtained_reason",
                                                              "mental_healthcare_provider", "mental_healthcare_duration",
                                                              "mental_healthcare_method", "mental_healthcare_relation",
                                                              "mental_healthcare_effectiveness", "life_change_details", "life_change_details_accuracy",
                                                              "expectations_vs_reality", "living_situation_problems", "health_insurance_other",
                                                              "social_media_other", "social_media_most_frequent_other")])
  
  # Marking missing values with "NA"
  data[data == ''] <- NA
  data[data == "Missing"] <- NA
  
  # Refactoring variables
  data$alcohol_quantity = ifelse(data$alcohol_quantity == "None; I do not drink alcohol", "1) None; I do not drink alcohol",
                          ifelse(data$alcohol_quantity == "1 or 2", "2) 1 or 2",
                          ifelse(data$alcohol_quantity == "3 or 4", "3) 3 or 4",
                          ifelse(data$alcohol_quantity == "5 or more", "4) 5 or more", NA))))
  
  
  ### Creating Binary Variables ###
  
  ## Disability_Type
  data$allergy_disability = ifelse(grepl("allergies and breathing problems", data$disability_type, fixed = TRUE), "Yes", "No")
  data$back_disability = ifelse(grepl("back pain and disc problem", data$disability_type, fixed = TRUE), "Yes", "No")
  data$bone_disability = ifelse(grepl("bone or joint issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$heart_disability = ifelse(grepl("heart issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$eye_disability = ifelse(grepl("blindness and vision loss", data$disability_type, fixed = TRUE), "Yes", "No")
  data$ear_disability = ifelse(grepl("deafness and impaired hearing, speech and language difficulties, dyslexia", data$disability_type, fixed = TRUE), "Yes", "No")
  data$limb_disability = ifelse(grepl("upper or lower limb loss (amputation), difference, or impairment", data$disability_type, fixed = TRUE), "Yes", "No")
  data$lung_disability = ifelse(grepl("lung or breathing issue", data$disability_type, fixed = TRUE), "Yes", "No")
  data$neuro_disability = ifelse(grepl("neurological disorder", data$disability_type, fixed = TRUE), "Yes", "No")
  data$other_disability = ifelse(grepl("other", data$disability_type, fixed = TRUE), "Yes", "No")
  data$spine_or_brain_disability = ifelse(grepl("spinal cord injury, traumatic brain injury, or stroke", data$disability_type, fixed = TRUE), "Yes", "No")
  
  ## Replacing impossible values with NA
  data = data %>% mutate(birth_year = replace(birth_year, birth_year > 2022, NA))
  data = data %>% mutate(birth_year = replace(birth_year, birth_year < 1900, NA))
  
  # Responses
  data$uses_alcohol = ifelse(data$alcohol_quantity != "1) None; I do not drink alcohol", "Yes", "No")
  data$uses_drugs = ifelse(is.na(data$drug_type), "No", "Yes")
  
  ### Disability Groups
  data$AEE = ifelse(data$allergy_disability == "Yes" | data$eye_disability == "Yes" | data$ear_disability == "Yes", "Yes", "No")
  data$BBL = ifelse(data$back_disability == "Yes" | data$bone_disability == "Yes" | data$limb_disability == "Yes", "Yes", "No")
  data$HL = ifelse(data$heart_disability == "Yes" | data$lung_disability == "Yes", "Yes", "No")
  data$NSBO = ifelse(data$neuro_disability == "Yes" | data$spine_or_brain_disability == "Yes" | data$other_disability == "Yes", "Yes", "No")

  return (data)
  
}