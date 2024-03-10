### Importing Packages ###

library(tidyverse)
library(stringi)
library(labelled)
library(codebookr)
library(dataMaid)
library(officer)
library(usefun)

clean_data = function(data) {
  
  ### Data cleaning ###
  
  # Removing unnecessary rows
  
  data = data[-c(1, 2), ]
  
  # Marking missing values with "NA"
  
  data = data %>% 
    mutate_all(na_if, "")
  
  # Sorting string variables alphabetically
  
  ## Creating binary variables in order to put string variables in sortable format.
  ## This is done because the different options in these survey questions have been
  ## selected in different orders, inflating the number of different combinations.
  
  ### Disability_Type
  
  data$allergy_disability = ifelse(grepl("Allergies and breathing problems (e.g., asthma, COPD)", data$Q3, fixed = TRUE) == TRUE, "allergies and breathing problems; ", '')
  data$back_disability = ifelse(grepl("Back pain and disc problem", data$Q3, fixed = TRUE) == TRUE, "back pain and disc problem; ", NA)
  data$eye_disability = ifelse(grepl("Blindness and vision loss  (despite using glasses or contact lenses)", data$Q3, fixed = TRUE) == TRUE, "blindness and vision loss; ", '')
  data$bone_disability = ifelse(grepl("Bone or joint issue, such as arthritis or other joint pain, osteoporosis, fracture, etc.", data$Q3, fixed = TRUE) == TRUE, "bone or joint issue; ", '')
  data$ear_disability = ifelse(grepl("Deafness and impaired hearing (despite use of hearing aids or implants), speech and language difficulties, dyslexia", data$Q3, fixed = TRUE) == TRUE, "deafness and impaired hearing, speech and language difficulties, dyslexia; ", '')
  data$heart_disability = ifelse(grepl("Heart issue, such as a-fib, angina, etc.", data$Q3, fixed = TRUE) == TRUE, "heart issue; ", NA)
  data$breathing_disability = ifelse(grepl("Lung or breathing issue, such as asthma, COPD, emphysema, etc.", data$Q3, fixed = TRUE) == TRUE, "lung or breathing issue; ", '')
  data$neuro_disability = ifelse(grepl("Neurological disorder, such as cerebral palsy, multiple sclerosis, Parkinson's disease, etc.", data$Q3, fixed = TRUE) == TRUE, "neurological disorder; ", '')
  data$other_disability = ifelse(grepl("Something else (please specify):", data$Q3, fixed = TRUE) == TRUE, "other; ", '')
  data$spine_or_brain_disability = ifelse(grepl("Spinal cord injury, Traumatic brain injury, or Stroke", data$Q3, fixed = TRUE) == TRUE, "spinal cord injury, traumatic brain injury, or stroke; ", '')
  data$limb_disability = ifelse(grepl("Upper or lower limb loss (amputation), difference, or impairment", data$Q3, fixed = TRUE) == TRUE, "upper or lower limb loss (amputation), difference, or impairment; ", '')
  
  ### Drug_Type
  
  data$uses_coca = ifelse(grepl("Cocaine or crack", data$Q31, fixed = TRUE) == TRUE, "coca; ", NA)
  data$uses_hallucinogens = ifelse(grepl("Hallucinogens (e.g., Ecstasy, LSD, mescaline, psilocybin, PCP, angel dust, or peyote)", data$Q31, fixed = TRUE) == TRUE, "hallucinogens; ", NA)
  data$uses_heroin = ifelse(grepl("Heroin", data$Q31, fixed = TRUE) == TRUE, "heroin; ", NA)
  data$uses_inhalants = ifelse(grepl("Inhalants or Solvents (e.g., amyl nitrate, nitrous oxide, glue, toluene, or gasoline", data$Q31, fixed = TRUE) == TRUE, "inhalants or solvents; ", NA)
  data$uses_THC = ifelse(grepl("Marijuana, hash, THC, or grass", data$Q31, fixed = TRUE) == TRUE, "marijuana, hash, THC, or grass; ", '')
  data$uses_other = ifelse(grepl("Other, specify (e.g., Methadone, Elavil, steroids, Thorazine, Haldol, etc.)", data$Q31, fixed = TRUE) == TRUE, "other; ", '')
  data$uses_painkillers = ifelse(grepl("Painkillers (e.g., Codeine, Darvon, Percodan, Dilaudid, or Demerol)", data$Q31, fixed = TRUE) == TRUE, "painkillers; ", '')
  data$uses_sedatives = ifelse(grepl("Sedatives (e.g., sleeping pills, barbiturates, Seconal, Quaaludes, or Chloral Hydrate)", data$Q31, fixed = TRUE) == TRUE, "sedatives; ", '')
  data$uses_stimulants = ifelse(grepl("Stimulants (e.g., Preludin, Benzedrine, Methadrine, uppers, or speed)", data$Q31, fixed = TRUE) == TRUE, "stimulants; ", '')
  data$uses_tranquilizers = ifelse(grepl("Tranquilizers or anti-anxiety drugs (e.g., Valium, Librium, muscle relaxants, or Zanax)", data$Q31, fixed = TRUE) == TRUE, "tranquilizers or anti-anxiety drugs; ", '')
  
  ### Health_Insurance
  
  data$federal_exchange = ifelse(grepl("Insurance from the federal exchange (HealthCare.gov)", data$Q19, fixed = TRUE) == TRUE, "federal exchange; ", '')
  data$indian_health_service = ifelse(grepl("Indian Health Service", data$Q19, fixed = TRUE) == TRUE, "Indian Health Service; ", '')
  data$employer_insurance = ifelse(grepl("Insurance through a current or former employer or union", data$Q19, fixed = TRUE) == TRUE, "insurance through current or former employer or union; ", '')
  data$other_health_insurance = ifelse(grepl("Any other type of health insurance or health coverage plan (please specify)", data$Q19, fixed = TRUE) == TRUE, "other; ", '')
  data$direct_purchase = ifelse(grepl("Insurance purchased directly from an insurance company", data$Q19, fixed = TRUE) == TRUE, "purchased directly; ", '')
  data$medicaid = ifelse(grepl("Medicaid", data$Q19, fixed = TRUE) == TRUE, "Medicaid; ", '')
  data$medicare = ifelse(grepl("Medicare, for people 65 and older, or people with certain disabilities", data$Q19, fixed = TRUE) == TRUE, "Medicare; ", '')
  data$military_care = ifelse(grepl("TRICARE or other military health care", data$Q19, fixed = TRUE) == TRUE, "TRICARE or other military health care; ", '')
  data$VA = ifelse(grepl("VA", data$Q19, fixed = TRUE) == TRUE, "VA", NA)
  
  ### Concatenating new binary variables
  
  data$disability_type = paste(data$allergy_disability,
                               data$back_disability,
                               data$eye_disability,
                               data$bone_disability,
                               data$ear_disability,
                               data$heart_disability,
                               data$breathing_disability,
                               data$neuro_disability,
                               data$other_disability,
                               data$spine_or_brain_disability,
                               data$limb_disability,
                               sep = '', collapse = NULL)
  
  data$drug_type = paste(data$uses_coca,
                         data$uses_hallucinogens,
                         data$uses_heroin,
                         data$uses_inhalants,
                         data$uses_THC,
                         data$uses_other,
                         data$uses_painkillers,
                         data$uses_sedatives,
                         data$uses_stimulants,
                         data$uses_tranquilizers,
                         sep = '', collapse = NULL)
  
  data$health_insurance = paste(data$federal_exchange,
                                data$indian_health_service,
                                data$employer_insurance,
                                data$other_health_insurance,
                                data$direct_purchase,
                                data$medicaid,
                                data$medicare,
                                data$military_care,
                                data$VA,
                                sep = '', collapse = NULL)
  
  ### Removing NA's
  
  data$disability_type = str_remove_all(data$disability_type,
                                              paste("NA;", collapse = NULL))
  data$disability_type = str_remove_all(data$disability_type,
                                              paste(";NA", collapse = NULL))
  data$disability_type = str_remove_all(data$disability_type,
                                              paste("NA", collapse = NULL))
  
  data$drug_type = str_remove_all(data$drug_type,
                                        paste("NA;",collapse = NULL))
  data$drug_type = str_remove_all(data$drug_type,
                                        paste(";NA",collapse = NULL))
  data$drug_type = str_remove_all(data$drug_type,
                                        paste("NA", collapse = NULL))
  
  data$health_insurance = str_remove_all(data$health_insurance,
                                               paste("NA;", collapse = NULL))
  data$health_insurance = str_remove_all(data$health_insurance,
                                               paste(";NA", collapse = NULL))
  data$health_insurance = str_remove_all(data$health_insurance,
                                               paste("NA", collapse = NULL))
  
  ### Removing whitespace
  
  data$disability_type = str_trim(data$disability_type)
  data$drug_type = str_trim(data$drug_type)
  data$health_insurance = str_trim(data$health_insurance)
  
  ### Removing final ';'
  
  data$disability_type = stri_sub(data$disability_type, 1, -2)
  data$drug_type = stri_sub(data$drug_type, 1, -2)
  data$health_insurance = stri_sub(data$health_insurance, 1, -2)
  
  ## Removing survey-specific and temporary variables
  
  data = subset(data, select = -c(Q3, Q31, Q19))
  
  data = subset(data, select = -c(allergy_disability,
                                              back_disability,
                                              eye_disability,
                                              bone_disability,
                                              ear_disability,
                                              heart_disability,
                                              breathing_disability,
                                              neuro_disability,
                                              other_disability,
                                              spine_or_brain_disability,
                                              limb_disability))
  
  data = subset(data, select = -c(uses_coca,
                                              uses_hallucinogens,
                                              uses_heroin,
                                              uses_inhalants,
                                              uses_THC,
                                              uses_other,
                                              uses_painkillers,
                                              uses_sedatives,
                                              uses_stimulants,
                                              uses_tranquilizers))
  
  data = subset(data, select = -c(federal_exchange,
                                              indian_health_service,
                                              employer_insurance,
                                              other_health_insurance,
                                              direct_purchase,
                                              medicaid,
                                              medicare,
                                              military_care,
                                              VA))
  
  # Changing variable names to be more intuitive
  
  data = rename(data, start_date = StartDate)
  data = rename(data, end_date = EndDate)
  data = rename(data, status = Status)
  data = rename(data, IP_address = IPAddress)
  data = rename(data, progress = Progress)
  data = rename(data, duration = Duration..in.seconds.)
  data = rename(data, finished = Finished)
  data = rename(data, recorded_date = RecordedDate)
  data = rename(data, response_ID = ResponseId)
  data = rename(data, last_name = RecipientLastName)
  data = rename(data, first_name = RecipientFirstName)
  data = rename(data, email = RecipientEmail)
  data = rename(data, external_reference = ExternalReference)
  data = rename(data, latitude = LocationLatitude)
  data = rename(data, longitude = LocationLongitude)
  data = rename(data, distribution_channel = DistributionChannel)
  data = rename(data, user_language = UserLanguage)
  data = rename(data, q_recaptcha_score = Q_RecaptchaScore)
  data = rename(data, q_relevant_ID_duplicate = Q_RelevantIDDuplicate)
  data = rename(data, q_relevant_ID_duplicate_score = Q_RelevantIDDuplicateScore)
  data = rename(data, q_relevant_ID_fraud_score = Q_RelevantIDFraudScore)
  data = rename(data, q_relevant_ID_last_start_date = Q_RelevantIDLastStartDate)
  data = rename(data, consent = Q1)
  data = rename(data, disability_other = Q3_11_TEXT)
  data = rename(data, disability_most_serious = Q4)
  data = rename(data, disability_most_serious_other = Q4_11_TEXT)
  data = rename(data, disability_visibility = Q5)
  data = rename(data, physical_disability_discrimination = Q6)
  data = rename(data, age_acquired = Q22)
  data = rename(data, dementia = Q20_1)
  data = rename(data, peripheral_vascular = Q20_2)
  data = rename(data, diabetes = Q20_3)
  data = rename(data, gastrointestinal = Q20_4)
  data = rename(data, obesity = Q20_5)
  data = rename(data, depression = Q20_6)
  data = rename(data, anxiety = Q20_7)
  data = rename(data, diabetes_type = Q21)
  data = rename(data, diabetes_type_other = Q21_4_TEXT)
  data = rename(data, assistive_device = Q23)
  data = rename(data, prosthesis_type = Q24)
  data = rename(data, diabetes_limb_loss = Q25)
  data = rename(data, mental_health_impact = Q63)
  data = rename(data, substance_mental_health = Q8_1)
  data = rename(data, schizophrenia_mental_health = Q8_2)
  data = rename(data, mental_health_mood = Q8_3)
  data = rename(data, mental_health_stress = Q8_4)
  data = rename(data, personality_disorder = Q8_5)
  data = rename(data, autism = Q8_6)
  data = rename(data, ADHD = Q8_7)
  data = rename(data, eating_disorder = Q8_8)
  data = rename(data, mental_health_other = Q62)
  data = rename(data, mental_health_other_description = Q62_2_TEXT)
  data = rename(data, depression_diagnosis = Q35)
  data = rename(data, mental_health_visibility = Q9)
  data = rename(data, mental_health_discrimination = Q10)
  data = rename(data, alcohol_use = Q26)
  data = rename(data, alcohol_quantity = Q27)
  data = rename(data, alcohol_start_age = Q28)
  data = rename(data, alcohol_self_perception = Q29_1)
  data = rename(data, alcohol_criticism = Q29_2)
  data = rename(data, alcohol_guilt = Q29_3)
  data = rename(data, alcohol_morning = Q29_4)
  data = rename(data, drug_use = Q30)
  data = rename(data, drug_type_other = Q31_10_TEXT)
  data = rename(data, health_quality = Q32)
  data = rename(data, two_week_lack_interest = Q33_1)
  data = rename(data, two_week_depression = Q33_2)
  data = rename(data, two_week_sleep_problems = Q33_3)
  data = rename(data, two_week_tiredness = Q33_4)
  data = rename(data, two_week_appetite = Q33_5)
  data = rename(data, two_week_feel_let_down = Q33_6)
  data = rename(data, two_week_trouble_concentrating = Q33_7)
  data = rename(data, two_week_movement = Q33_8)
  data = rename(data, two_week_self_harm = Q33_9)
  data = rename(data, two_week_problem_difficulty = Q34)
  data = rename(data, mental_healthcare = Q36)
  data = rename(data, mental_healthcare_sought = Q59)
  data = rename(data, mental_healthcare_unobtained = Q60)
  data = rename(data, mental_healthcare_unobtained_reason = Q60_4_TEXT)
  data = rename(data, mental_healthcare_provider = Q37)
  data = rename(data, mental_healthcare_duration = Q38)
  data = rename(data, mental_healthcare_method = Q39)
  data = rename(data, mental_healthcare_relation = Q39_4_TEXT)
  data = rename(data, mental_healthcare_effectiveness = Q40_1)
  data = rename(data, life_change_details = Q42)
  data = rename(data, life_change_details_accuracy = Q43)
  data = rename(data, expectations_vs_reality = Q56)
  data = rename(data, cooperation = Q44_1)
  data = rename(data, skill_improvement = Q44_2)
  data = rename(data, social_behavior = Q44_3)
  data = rename(data, family_support = Q44_4)
  data = rename(data, family_knowledge = Q44_5)
  data = rename(data, hunger = Q44_6)
  data = rename(data, likeability = Q44_7)
  data = rename(data, communicate_feelings = Q44_8)
  data = rename(data, feel_supported = Q44_9)
  data = rename(data, feel_belonging = Q44_10)
  data = rename(data, family_stand_by = Q44_11)
  data = rename(data, friends_stand_by = Q44_12)
  data = rename(data, treated_fairly = Q44_13)
  data = rename(data, act_responsibly_opportunities = Q44_14)
  data = rename(data, feel_secure_with_family = Q44_15)
  data = rename(data, apply_abilities_opportunities = Q44_16)
  data = rename(data, enjoy_family_culture = Q44_17)
  data = rename(data, current_living_situation = Q45)
  data = rename(data, living_situation_problems = Q47)
  data = rename(data, twelve_month_food_worry = Q48)
  data = rename(data, transportation_problems = Q50)
  data = rename(data, twelve_month_utility_threat = Q51)
  data = rename(data, physically_hurt = Q52_1)
  data = rename(data, insulted = Q52_2)
  data = rename(data, threaten_harm = Q52_3)
  data = rename(data, screamed_at = Q52_4)
  data = rename(data, humiliated = Q52_5)
  data = rename(data, birth_month = Q11_1)
  data = rename(data, birth_day = Q11_2)
  data = rename(data, birth_year = Q11_3)
  data = rename(data, gender = Q12)
  data = rename(data, ethnicity = Q13)
  data = rename(data, race = Q14)
  data = rename(data, ZIP_Code = Q15)
  data = rename(data, education_level = Q16)
  data = rename(data, yearly_income = Q17)
  data = rename(data, occupational_status = Q18)
  data = rename(data, health_insurance_other = Q19_9_TEXT)
  data = rename(data, social_media = Q53)
  data = rename(data, social_media_other = Q53_6_TEXT)
  data = rename(data, social_media_most_frequent = Q54)
  data = rename(data, social_media_most_frequent_other = Q54_6_TEXT)
  data = rename(data, social_media_check = Q55)
  data = rename(data, survey_feedback = Q57)
  data = rename(data, q_ballot_box_stuffing = Q_BallotBoxStuffing)
  data = rename(data, order_number = orderNumber)
  data = rename(data, q_total_duration = Q_TotalDuration)
  
  # Removing prefixed and suffixed whitespace
  
  data$disability_other = str_trim(data$disability_other)
  data$disability_most_serious_other = str_trim(data$disability_most_serious_other)
  data$diabetes_type_other = str_trim(data$diabetes_type_other)
  data$mental_health_other_description = str_trim(data$mental_health_other_description)
  data$drug_type_other = str_trim(data$drug_type_other)
  data$mental_healthcare_unobtained_reason = str_trim(data$mental_healthcare_unobtained_reason)
  data$mental_healthcare_relation = str_trim(data$mental_healthcare_relation)
  data$expectations_vs_reality = str_trim(data$expectations_vs_reality)
  data$health_insurance_other = str_trim(data$health_insurance_other)
  data$social_media_other = str_trim(data$social_media_other)
  data$social_media_most_frequent_other = str_trim(data$social_media_most_frequent_other)
  data$survey_feedback = str_trim(data$survey_feedback)
  
  ### Codebook ###
  
  # Setting labels
  
  data = data |> set_variable_labels(
    start_date = "Start Date",
    end_date = "End Date",
    status = "Response Type",
    IP_address = "IP Address",
    progress = "Progress",
    duration = "Duration (in seconds)",
    finished = "Finished",
    recorded_date = "Recorded Date",
    response_ID = "Response ID",
    last_name = "Recipient Last Name",
    first_name = "Recipient First Name",
    email = "Recipient Email",
    external_reference = "External Data Reference",
    latitude = "Location Latitude",
    longitude = "Location Longitude",
    distribution_channel = "Distribution Channel",
    user_language = "User Language",
    q_recaptcha_score = "Q_RecaptchaScore",
    q_relevant_ID_duplicate = "Q_RelevantIDDuplicate",
    q_relevant_ID_duplicate_score = "Q_RelevantIDDuplicateScore",
    q_relevant_ID_fraud_score = "Q_RelevantIDFraudScore",
    q_relevant_ID_last_start_date = "Q_RelevantIDLastStartDate",
    consent = "Consent for Online Survey",
    disability_type = "Please tell us more about your long-term physical disability. How would you describe it? If you have more than one, please check all that apply. - Selected Choice",
    disability_other = "Please tell us more about your long-term physical disability. How would you describe it? If you have more than one, please check all that apply. - Something else (please specify): - Text",
    disability_most_serious = "What is your most serious physical disability? Please select only one. - Selected Choice",
    disability_most_serious_other = "What is your most serious physical disability? Please select only one. - Something else (please specify): - Text",
    disability_visibility = "Would a stranger recognize within 5 minutes that you have a physical disability?",
    physical_disability_discrimination = "Do you feel that you are discriminated against because of your physical disability?",
    age_acquired = "At what age did you acquire your first physical disability?",
    dementia = "Please indicate if you have any of the following and how much it affects you. - Dementia or other neurocognitive disorder",
    peripheral_vascular = "Please indicate if you have any of the following and how much it affects you. - Peripheral vascular disease",
    diabetes = "Please indicate if you have any of the following and how much it affects you. - Diabetes",
    gastrointestinal = "Please indicate if you have any of the following and how much it affects you. - Gastrointestinal disease",
    obesity = "Please indicate if you have any of the following and how much it affects you. - Obesity or overweight",
    depression = "Please indicate if you have any of the following and how much it affects you. - Depression (or other psychiatric diagnosis causing mood disturbances, e.g. bipolar disporder)",
    anxiety = "Please indicate if you have any of the following and how much it affects you. - Anxiety or panic disorder",
    diabetes_type = "What type of diabetes do you have? - Selected Choice",
    diabetes_type_other = "What type of diabetes do you have? - Another type (please specify): - Text",
    assistive_device = "Do you routinely use an assistive device?",
    prosthesis_type = "What type of prosthetic limb(s) do you use?",
    diabetes_limb_loss = "Is your limb loss related to diabetes?",
    mental_health_impact = "Next, we will be asking about your mental or behavioral health. Has your physical disability impacted your mental health?",
    substance_mental_health = "Please tell us about any mental health concerns or disabilities you might have experienced. - Mental disorders cause by alcohol or substance use",
    schizophrenia_mental_health = "Please tell us about any mental health concerns or disabilities you might have experienced. - Schizophrenia or psychosis",
    mental_health_mood = "Please tell us about any mental health concerns or disabilities you might have experienced. - Mood disorders including depression and bipolar disorder",
    mental_health_stress = "Please tell us about any mental health concerns or disabilities you might have experienced. - Stress, phobias, anxiety, obsessive compulsive disorders (OCD), or post traumatic stress disorder (PTSD)",
    personality_disorder = "Please tell us about any mental health concerns or disabilities you might have experienced. - Personality disorders",
    autism = "Please tell us about any mental health concerns or disabilities you might have experienced. - Autism spectrum disorder",
    ADHD = "Please tell us about any mental health concerns or disabilities you might have experienced. - Attention deficit hyperactivity disorders or similar disorders",
    eating_disorder = "Please tell us about any mental health concerns or disabilities you might have experienced. - Eating disorders",
    mental_health_other = "Have you experienced any other mental or behavioral health disorders not already listed? - Selected Choice",
    mental_health_other_description = "Have you experienced any other mental or behavioral health disorders not already listed? - Yes (please describe) - Text",
    depression_diagnosis = "Has a doctor, nurse, or other health professional ever told you that you had a depressive disorder (depression, major depression, dysthymia, or minor depression)?",
    mental_health_visibility = "Would a stranger recognize within 5 minutes that you have a mental health issue?",
    mental_health_discrimination = "Do you feel that you are discriminated against because of your mental health issue?",
    alcohol_use = "Can you please tell us about your alcohol use? How often did you have a drink containing alcohol in the past year?",
    alcohol_quantity = "How many alcoholic drinks did you have on a typical day when you were drinking in the past year?",
    alcohol_start_age = "How old were you when you first began to drink alcohol, more than just a few sips?",
    alcohol_self_perception = "Please tell us about your drinking - Have you ever felt you should cut down on your drinking?",
    alcohol_criticism = "Please tell us about your drinking - Have people annoyed you by criticizing your drinking?",
    alcohol_guilt = "Please tell us about your drinking - Have you ever felt bad or guilty about your drinking?",
    alcohol_morning = "Please tell us about your drinking - Have you ever had a drink first thing in the morning to steady your nerves or to get rid of a hangover?",
    drug_use = "Do you use drugs or illicit substances?",
    drug_type = "What types of drugs or illicit substances do you use (select all that apply)? - Selected Choice",
    drug_type_other = "What types of drugs or illicit substances do you use (select all that apply)? - Other, specify (e.g., Methadone, Elavil, steroids, Thorazine, Haldol, etc.) - Text",
    health_quality = "In general, would you say your health is:",
    two_week_lack_interest = "Over the last 2 weeks, how often have you been bothered by any of the following? - Little interest or pleasure in doing things",
    two_week_depression = "Over the last 2 weeks, how often have you been bothered by any of the following? - Feeling down, depressed or hopeless",
    two_week_sleep_problems = "Over the last 2 weeks, how often have you been bothered by any of the following? - Trouble falling or staying asleep, or sleeping too much",
    two_week_tiredness = "Over the last 2 weeks, how often have you been bothered by any of the following? - Feeling tired or having little energy",
    two_week_appetite = "Over the last 2 weeks, how often have you been bothered by any of the following? - Poor appetite or overeating",
    two_week_feel_let_down = "Over the last 2 weeks, how often have you been bothered by any of the following? - Feeling bad about yourself - or that you are a failure or have let yourself or your family down",
    two_week_trouble_concentrating = "Over the last 2 weeks, how often have you been bothered by any of the following? - Trouble concentrating on things, such as reading the newspaper or watching television",
    two_week_movement = "Over the last 2 weeks, how often have you been bothered by any of the following? - Moving or speaking so slowly that other people could have noticed? Or the opposite, being so fidgety or restless that you have been moving around a lot more than usual",
    two_week_self_harm = "Over the last 2 weeks, how often have you been bothered by any of the following? - Thoughts that you would be better off dead, or of hurting yourself",
    two_week_problem_difficulty = "How difficult have these problems made it for you to do your work, take care of things at home, or get along with other people?",
    mental_healthcare = "Have you received mental or behavioral healthcare related to your physical disability, such as counseling about dealing with life changes, acceptance therapy, etc.?",
    mental_healthcare_sought = "Did you seek mental healthcare related to your physical disability, but were unable to obtain it?",
    mental_healthcare_unobtained = "Why were you unable to obtain mental healthcare related to your physical disability? - Selected Choice",
    mental_healthcare_unobtained_reason = "Why were you unable to obtain mental healthcare related to your physical disability? - Another reason (please specify) - Text",
    mental_healthcare_provider = "Who provided the mental healthcare related to your physical disability (select all that apply)?",
    mental_healthcare_duration = "How do you feel about the length or duration of the mental health services you received?",
    mental_healthcare_method = "How did you receive mental health services related to your physical disability (select all that apply)? - Selected Choice",
    mental_healthcare_relation = "How did you receive mental health services related to your physical disability (select all that apply)? - Other (please specify) - Text",
    mental_healthcare_effectiveness = "Overall, how effective was the mental healthcare related to your physical disability? - Mental healthcare effectiveness",
    life_change_details = "Did any of your healthcare providers give you details about ways your life would change following your disability?",
    life_change_details_accuracy = "Were those details accurate?",
    expectations_vs_reality = "Please provide examples of ways your expectations about life with a disability might not have met the realities of life with a disability.",
    cooperation = "To what extent do the following statements apply to you? There are no right or wrong answers. - I cooperate with people around me",
    skill_improvement = "To what extent do the following statements apply to you? There are no right or wrong answers. - Getting and improving qualifications or skills is important to me",
    social_behavior = "To what extent do the following statements apply to you? There are no right or wrong answers. - I know how to behave in different social situaitons",
    family_support = "To what extent do the following statements apply to you? There are no right or wrong answers. - My family has usually supported me through life",
    family_knowledge = "To what extent do the following statements apply to you? There are no right or wrong answers. - My family knows a lot about me",
    hunger = "To what extent do the following statements apply to you? There are no right or wrong answers. - if I am hungry, I can get food to eat",
    likeability = "To what extent do the following statements apply to you? There are no right or wrong answers. - People like to spend time with me",
    communicate_feelings = "To what extent do the following statements apply to you? There are no right or wrong answers. - I talk to my family/partner about how I feel",
    feel_supported = "To what extent do the following statements apply to you? There are no right or wrong answers. - I feel supported by my friends",
    feel_belonging = "To what extent do the following statements apply to you? There are no right or wrong answers. - I feel that I belong in my community",
    family_stand_by = "To what extent do the following statements apply to you? There are no right or wrong answers. - My family/partner stands by me during difficult times",
    friends_stand_by = "To what extent do the following statements apply to you? There are no right or wrong answers. - My friends stand by me during difficult times",
    treated_fairly = "To what extent do the following statements apply to you? There are no right or wrong answers. - I am treated fairly in my community",
    act_responsibly_opportunities = "To what extent do the following statements apply to you? There are no right or wrong answers. - I have opportunities to show others that I can act responsibly",
    feel_secure_with_family = "To what extent do the following statements apply to you? There are no right or wrong answers. - I feel secure when I am with my family/partner",
    apply_abilities_opportunities = "To what extent do the following statements apply to you? There are no right or wrong answers. - I have opportunities to apply my abilities in life (like skills, a job, caring for others)",
    enjoy_family_culture = "To what extent do the following statements apply to you? There are no right or wrong answers. - I enjoy my family's/partner's cultural and family traditions",
    current_living_situation = "We would like to understand a little more about how you live and things that may impact your life. What is your living situation today?",
    living_situation_problems = "Think about the place that you live. Do you have problems with any of the following? (check all that apply)",
    twelve_month_food_worry = "Within the past 12 months, have you worried that your food would run out before you got money to buy more?",
    transportation_problems = "Do you put off or neglect going to the doctor because of distance or transportation?",
    twelve_month_utility_threat = "In the past 12 months has the electric, gas, oil, or water company threatened to shut off services in your home?",
    physically_hurt = "We want to know more about your personal safety. How often does anyone, including family... - physically hurt you?",
    insulted = "We want to know more about your personal safety. How often does anyone, including family... - insult or talk down to you?",
    threaten_harm = "We want to know more about your personal safety. How often does anyone, including family... - threaten you with harm?",
    screamed_at = "We want to know more about your personal safety. How often does anyone, including family... - scream or curse at you?",
    humiliated = "We want to know more about your personal safety. How often does anyone, including family... - humiliate or degrade you?",
    birth_month = "Tell us more about yourself. Please enter your date of birth - Month (mm)",
    birth_day = "Tell us more about yourself. Please enter your date of birth - Day (dd)",
    birth_year = "Tell us more about yourself. Please enter your date of birth - Year (yyyy)",
    gender = "Please select your gender",
    ethnicity = "Please select your ethnicity",
    race = "Please select your race",
    ZIP_Code = "What is the zip code where you currently live?",
    education_level = "What is the highest grade or level of school you have completed?",
    yearly_income = "What was your yearly income last year (in 2021) from all sources before taxes? This includes all income from both formal and informal employment.",
    occupational_status = "What is your current occupational status?",
    health_insurance = "What is your health insurance status (select all that apply)? - Selected Choice",
    health_insurance_other = "What is your health insurance status (select all that apply)? - Any other type of health insurance or health coverage plan (please specify) - Text",
    social_media = "We know that many people use social media to get information and to stay connected with work, family and friends. Which of the following social media platforms do you use (select all that apply)? - Selected Choice",
    social_media_other = "We know that many people use social media to get information and to stay connected with work, family and friends. Which of the following social media platforms do you use (select all that apply)? - Other - Text",
    social_media_most_frequent = "In any given week, which of the following social media sites do you visit most frequently? - Selected Choice",
    social_media_most_frequent_other = "In any given week, which of the following social media sites do you visit most frequently? - Other - Text",
    social_media_check = "How often do you check-in to your social media accounts in any given week?",
    survey_feedback = "We want to thank you for participating in our survey and for sharing your experiences. We have a comment box below if you have any thoughts about the survey or if you want to share anything about your own experiences with physical disability and mental health.",
    opp = "opp",
    qpmid = "qpmid",
    RISN = "RISN",
    rid = "rid",
    V = "V",
    PID = "PID",
    psid = "psid",
    K2 = "K2",
    ID = "ID",
    q_ballot_box_stuffing = "Q_BallotBoxStuffing",
    order_number = "orderNumber",
    gc = "gc",
    term = "term",
    q_total_duration = "Q_TotalDuration")
  
  # Creating data report
  
  # makeDataReport(disability)
  
  # Creating codebook
  
  # disability_codebook = codebook(df = data, title = "Codebook for Wellstar Disability Data",
  #                                subtitle = "Phys Disability Behavioral Health_September 6, 2022_14.22",
  #                                description = "In July of 2022, WCHHS conducted a cross-sectional survey to determine the behavioral needs among those with physical disabilities. The Principal Investigators for this study are Dr. Monica Swahn, Professor and Dean and Dr. Mark Geil, Professor and Associate Dean for Research of the WCHHS.
  #   To conduct the survey, a Qualtrics team was hired to recruit participants for the online survey.
  #   The study included participants 18 years and older (N = 4890). Participants were compensated by Qualtrics for taking the survey at an amount agreed upon prior to survey completion. The consent language is listed as the first part of the survey and participants had to agree by clicking “yes” to take the survey. The survey is anonymous as we did not ask for any identifiable information. Because of the survey dissemination strategy used by Qualtrics, a specific response rate cannot be computed.
  #   The 15-minute survey was designed to get a brief overview of the health, and more specifically the mental health and behaviors support needs of U.S. adults who has a physical disability. While the survey was distributed to US adults, it was not designed to be “representative” of all US adults. Also, it is assumed based on the skip pattern and screening questions that all those participating in the survey had a physical disability.")
  # 
  # print(x = disability_codebook,
  #       target = ""C:\\Users\\scwag\\OneDrive\\Desktop\\KSU_Workspace\\disability_codebook.docx")
  
  ### Exporting data frame to .csv format ###
  
  # write.csv(disability, "C:\\Users\\scwag\\OneDrive\\Desktop\\KSU\\Spring 2023 GRA\\Disability_Data_Cleaned.csv")
  
  return (data)
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
