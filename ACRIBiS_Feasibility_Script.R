source("ACRIBiS_Feasibility_install_R_packages.R")
source("ACRIBiS_Feasibility_support_functions.R")
source("ACRIBiS_Feasibility_config.R")

#source config
# if(file.exists("ACRIBiS_Feasibility_config.R")&&!dir.exists("ACRIBiS_Feasibility_config.R")){
#   source("ACRIBiS_Feasibility_config.R")
# }else{
#   source("config.R.default")  
# }


#create log file, named for date and time of creation
if(!dir.exists("Logs")){dir.create(paste0(diz_short, "Logs"))}
log <- paste0("Logs/", format(Sys.time(), "%Y%m%d_%H%M%S"),".txt", collapse = "")
write(paste("Starting Script ACRIBiS_Feasibility_Script.R at", Sys.time()), file = log, append = T)


# Setup -------------------------------------------------------------------
#create output directory
if(!dir.exists("Output")){dir.create(paste0(diz_short, "Output"))}
if(!dir.exists("errors")){dir.create(paste0(diz_short, "errors"))}
if(!dir.exists("XML_Bundles")){dir.create("XML_Bundles")}

#If needed disable peer verification
if(!ssl_verify_peer){httr::set_config(httr::config(ssl_verifypeer = 0L))}

#remove trailing slashes from endpoint
diz_url <- if(grepl("/$", diz_url)){strtrim(diz_url, width = nchar(diz_url)-1)}else{diz_url}



# Table Descriptions and Codes --------------------------------------------
## Patient
tabledescription_patient <- fhir_table_description(
  resource = "Patient",
  cols = c(patient_identifier  = "id",
           #only gender (administrative), not sex available
           patient_gender      = "gender",
           patient_birthdate   = "birthDate"
  )
)
## Observation
tabledescription_observation <- fhir_table_description(
  resource = "Observation",
  cols = c(observation_identifier  = "id",
           observation_subject     = "subject/reference",
           observation_code        = "code/coding/code",
           observation_value       = "valueQuantity/value",
           observation_unit        = "valueQuantity/unit",
           observation_datetime    = "effectiveDateTime"
  )
)
## medicationAdministration
tabledescription_medicationAdministration <- fhir_table_description(
  resource = "MedicationAdministration",
  cols = c(medicationAdministration_identifier            = "id",
           medicationAdministration_subject               = "subject/reference", 
           medicationAdministration_status                = "status",
           medicationAdministration_medication_reference  = "medicationReference/reference",
           medicationAdministration_effective_dateTime    = "effectivedateTime",
           medicationAdministration_effective_period      = "effectivePeriod"
  )
)
## Medication
tabledescription_medication <- fhir_table_description(
  resource = "Medication",
  cols = c(medication_identifier      = "id",
           #Codingsystem auf bfarm/atc gesetzt, andere Systeme werden vorraussichtlich nicht erkannt; ggf iterativ einbauen, Frage in Zulip Chat stellen
           medication_system          = "code/coding[system[@value='http://fhir.de/CodeSystem/bfarm/atc']]/system",
           medication_code            = "code/coding[system[@value='http://fhir.de/CodeSystem/bfarm/atc']]/code",
           medication_display         = "code/coding[system[@value='http://fhir.de/CodeSystem/bfarm/atc']]/display",
           medication_text            = "code/text",
           medication_strength        = "ingredient/strength/numerator/value",
           medication_strength_per    = "ingredient/strength/denominator/value",
           medication_unit            = "ingredient/strength/numerator/unit"
  )
)
##Condition 
tabledescription_condition <- fhir_table_description(
  resource = "Condition",
  cols = c(condition_identifier     = "id",
           condition_code           = "code/coding/code",
           condition_system         = "code/coding/system",
           condition_recordedDate   = "recordedDate",
           condition_onsetDate      = "onsetDateTime",
           condition_subject        = "subject/reference"
  )
)


# Limit to patients in 2024 ----------------------------------------------

tabledescription_encounter <- fhir_table_description(
  resource = "Encounter",
  cols = c(encounter_identifier     = "id",
           encounter_periodStart    = "period/start",
           encounter_subject        = "subject/reference"
  )
)

print("Downloading Encounter Bundles.")
body_encounters <- fhir_body(content = list("date" = "gt2024-01-01", "date" = "le2024-12-31", "_count" = page_count))
request_encounters <- fhir_url(url = diz_url, resource = "Encounter")
bundles_encounters <- fhir_search(request = request_encounters, body = body_encounters, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_encounters)==0){
  #rerun the call to fhir_search()
  bundles_encounters <- fhir_search(request = request_encounters, body = body_encounters, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
# no saving necessary
table_encounter <- fhir_crack(bundles = bundles_encounters, design = tabledescription_encounter, verbose = 1)

#ggf. anpassen (Bezeichung in Spalte eventuell je nach Server unterschiedlich)
relevant_encounter_subjects <- sub("Patient/", "", table_encounter$encounter_subject)



## find patient IDs with relevant ICD-10 Codes for Condition 
# Define relevant ICD-codes for CVD Diagnoses                                               
icd10_codes_patient_conditions <- data.frame( icd_code = c("I05", "I06", "I07", "I08", "I09", 
                                                           "I20", "I21", "I22", "I23", "I24", "I25", 
                                                           "I30", "I31", "I32", "I33", "I34", "I35", 
                                                           "I36", "I37", "I38", "I39", "I40", "I41", 
                                                           "I42", "I43", "I44", "I45", "I46", "I47", 
                                                           "I48", "I49", "I50", "I51", "I52"))
icd10_codes_patient_conditions <- icd_expand(icd10_codes_patient_conditions, col_icd = "icd_code", year = 2023)


# Identify Required Patients
print("Downloading Condition Bundles to identify relevant patients.")
#download all conditions with respective ICD10-Codes and for patients (subjects) from relevant time frame
#use "code" as FHIR-Search parameter for Condition resource
body_patient_conditions <- fhir_body(content = list("code" = paste(icd10_codes_patient_conditions$icd_normcode, collapse = ","), "subject" = paste(relevant_encounter_subjects, collapse = ","), "_count" = page_count))
request_patient_conditions <- fhir_url(url = diz_url, resource = "Condition")
bundles_patient_conditions <- fhir_search(request = request_patient_conditions, body = body_patient_conditions, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_patient_conditions)==0){
  #rerun the call to fhir_search()
  bundles_patient_conditions <- fhir_search(request = request_patient_conditions, body = body_patient_conditions, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
# no saving necessary
table_patient_conditions <- fhir_crack(bundles = bundles_patient_conditions, design = tabledescription_condition, verbose = 1)

#search for patients who have the specified conditions
patient_ids_with_conditions_prefix <- table_patient_conditions$condition_subject
#remove "Patient/" prefix from referenced Patient IDs
patient_ids_with_conditions <- sub("Patient/", "", table_patient_conditions$condition_subject)


# Lists of relevant LOINC Codes for Observations
LOINC_codes_height <- c("8302-2", "3137-7", "8301-4", "8306-3", "91370-7")
LOINC_codes_weight <- c("29463-7", "3141-9", "3142-7", "8335-2", "75292-3", "79348-9", "8350-1")
LOINC_codes_bp_overall <- c("55284-4", "96607-7", "8478-0")
LOINC_codes_bp_sys <- c("8480-6", "8459-0", "76534-7", "8489-7", "11378-7", "8479-8")
LOINC_codes_bp_dia <- c("8462-4", "8453-3", "76213-8", "76535-4", "8469-9", "8475-6")
LOINC_codes_lvef <- c("10230-1", "18043-0", "8808-8", "8809-6", "18045-5", "8811-2", "8806-2", "79991-6")
LOINC_codes_creatinine <- c("14682-9", "2160-0", "38483-4", "77140-2")
LOINC_codes_egfr <- c("69405-9", "62238-1", "98979-8", "50210-4", "98980-6")
LOINC_codes_cholesterol_overall <- c("14647-2", "2093-3")
LOINC_codes_cholesterol_hdl <- c("14646-4", "2085-9", "49130-8", "18263-4")
LOINC_codes_hscrp <- c("71426-1", "30522-7", "76486-0")
LOINC_codes_crp <- c("1988-5", "76485-2", "48421-2")
LOINC_codes_bmi <- c("39156-5", "89270-3")
LOINC_code_rankin_scale <- "7585-9"
LOINC_codes_smoking <- c("70483-3", "72166-2", "74011-8")
LOINC_codes_all <- paste (c(LOINC_codes_height, LOINC_codes_weight, LOINC_codes_bp_overall, LOINC_codes_bp_sys, LOINC_codes_bp_dia, 
                            LOINC_codes_lvef, LOINC_codes_creatinine, LOINC_codes_egfr, LOINC_codes_cholesterol_overall, LOINC_codes_cholesterol_hdl,
                            LOINC_codes_hscrp, LOINC_codes_crp, LOINC_codes_bmi, LOINC_code_rankin_scale), collapse = ",")


# Required Codes for medicationAdministration / Medication
medications_betablockers <- c(codes <- c("C07AB04", "C07BB04", "C07CB04", "C07FB26", "C07AB03", "C07BB03", "C07CB03", "C07CB23", "C07CB53", "C07FX18", 
                                         "C07AB05", "S01ED02", "S01ED52", "C07AB07", "C07FX04", "C07FB07", "C07BB27", "C07BB07", "C09BX04", "C09BX02", 
                                         "C09BX05", "C07AB08", "C07BC08", "C07AB09", "C07AB02", "C07BB02", "C07BB22", "C07BB52", "C07CB02", "C07CB22", 
                                         "C07FB02", "C07FB13", "C07FB22", "C07FX03", "C07FX05", "C07AB12", "C07BB12", "C07FB12", "C09DX05", "C07AA19", 
                                         "C07EA19", "C07FX17", "C07FX19", "S01ED08", "C07AA15", "S01ED05", "S01ED55", "C07AA12", "C07BA12", "C07AA02", 
                                         "C07FX15", "C07CA02", "C07BA02", "C07AA03", "C07CA03", "C07EA03", "S01ED07", "C07AA05", "C07DA25", "C07CA05", 
                                         "C07FX01", "C07BA05", "C07EA05", "C07AA07", "C07FX02", "C07BA07", "C07AA06", "S01ED01", "C07DA26", "S01ED01", 
                                         "C07DA26", "S01ED51", "C07DA06", "S01ED62", "S01ED67", "S01ED66", "S01ED61", "S01ED68", "S01ED70", "C07BA06", 
                                         "S01ED63", "C07AG01", "C07CG01", "C07BG01", "C07AG02", "C07FX06", "C07BG02"))
#ACEi
medications_acei_arb <- c("C09AA07", "C09BA07", "C09BA27", "C09AA01", "C09BA01", "C09BA21", "C09AA08", "C09BA08", "C09BA82", "C09AA02", "C09BA02", "C09BA22", 
                          "C09BB02", "C09BB06", "C09AA09", "C09BA09", "C09BA29", "C09AA16", "C09AA03", "C09BB03", "C09BA03", "C09BA23", "C09AA13", "C09BA13", 
                          "C09BA33", "C09AA04", "C09BX01", "C09BX04", "C09BB04", "C09BX02", "C09BA04", "C09BA54", "C09AA06", "C09BA06", "C09BA26", "C09AA05", 
                          "C09BX03", "C09BB07", "C09BX05", "C09BA05", "C09BB05", "C09BA25", "C09BA55", "C09AA10", "C09BB10", "C09AA15", "C09BA15", "C09BA35",
#ARB
                          "C09CA09", "C09DA09", "C09CA06", "C09DX06", "C09DB07", "C09DA06", "C09DA26", "C10BX19", "C09CA02", "C09DA02", "C09DA22", "C09CA04", 
                          "C09DX07", "C09DB05", "C09DA04", "C09DA24", "C09CA01", "C09DB06", "C09DA01", "C09DA21", "C09CA08", "C09DX03", "C09DB02", "C09DA08", 
                          "C09DA28", "C09CA07", "C09DB04", "C09DA07", "C09DA27", "C09CA03", "C09DX01", "C09DX02", "C09DB01", "C09DA03", "C09DA23", "C09DB08", 
                          "C09DX05", "C09DX04", "C10BX10")

medications_antithrombotic <- c("B01AA01", "B01AA02", "B01AA03", "B01AA04", "B01AA05", "B01AA06", "B01AA07", "B01AA08", "B01AA09", "B01AA10", "B01AA11", "B01AA12", 
                                "B01AB01", "B01AB02", "B01AB03", "B01AB04", "B01AB05", "B01AB06", "B01AB07", "B01AB08", "B01AB09", "B01AB10", "B01AB11", "B01AB12", 
                                "B01AB13", "B01AB51", "B01AB63", "B01AC01", "B01AC02", "B01AC03", "B01AC04", "B01AC05", "B01AC06", "B01AC07", "B01AC08", "B01AC09", 
                                "B01AC10", "B01AC11", "B01AC12", "B01AC13", "B01AC15", "B01AC16", "B01AC17", "B01AC18", "B01AC19", "B01AC21", "B01AC22", "B01AC23", 
                                "B01AC24", "B01AC25", "B01AC26", "B01AC27", "B01AC30", "B01AC34", "B01AC36", "B01AC56", "B01AC86", "B01AD01", "B01AD02", "B01AD03", 
                                "B01AD04", "B01AD05", "B01AD06", "B01AD07", "B01AD08", "B01AD09", "B01AD10", "B01AD11", "B01AD12", "B01AD51", "B01AE01", "B01AE02", 
                                "B01AE03", "B01AE04", "B01AE05", "B01AE06", "B01AE07", "B01AF01", "B01AF02", "B01AF03", "B01AF04", "B01AX01", "B01AX04", "B01AX05", 
                                "B01AX07", "B01AX11", "B01AY01", "B01AY02")

#no paste, because that creats a single string instead of a list
medications_all <- c(medications_betablockers, medications_acei_arb, medications_antithrombotic)

#give out statements after certain chunks to document progress
write(paste("Finished Setup at", Sys.time(), "\n"), file = log, append = T)


if (search_for_bundles == TRUE) {
# FHIR Searches ------------------------------------------------------------------------------------------------------------------------------------------------------------
# (only for first download, then load saved bundles to save time)

# Patients
print("Downloading Patient Bundles.")
#create the search body which lists all the found Patient IDs and restricts on specified parameters (birthdate)
#use "_id" as global FHIR-Search parameter in patient resource
#Update birthdate, automatisch berechnen
body_patient <- fhir_body(content = list("_id" = paste(patient_ids_with_conditions, collapse = ","), "birthdate" = "lt2006-07-01", "_count" = page_count))
#create request for specified URL and Resource
request_patients <- fhir_url(url = diz_url, resource = "Patient")
#Execute the fhir search using the above defined request and body
bundles_patient <- fhir_search(request = request_patients, body = body_patient, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_patient)==0){
  #rerun the call to fhir_search()
  bundles_patient <- fhir_search(request = request_patients, body = body_patient, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
#give out statements after certain chunks to document progress
write(paste("Finished Search for Patient-Ressources at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_patient), " Bundles for the Patient-Ressource were found \n"), file = log, append = T)


#Condition
print("Downloading Condition Bundles.")
#now load all CONDITIONS for relevant patient IDs, to obtain other conditions (comorbidities) of relevant patients
#use "patient" as FHIR-search parameter in Condition resource
body_conditions <- fhir_body(content = list("subject" = paste(patient_ids_with_conditions, collapse = ","), "_count" = page_count))
request_conditions <- fhir_url(url = diz_url, resource = "Condition")
#code or normcode?; normcode appears to work
bundles_condition <- fhir_search(request = request_conditions, body = body_conditions, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_condition)==0){
  #rerun the call to fhir_search()
  bundles_condition <- fhir_search(request = request_conditions, body = body_conditions, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
#give out statements after certain chunks to document progress
write(paste("Finished Search for Condition-Ressources at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_condition), " Bundles for the Condition-Ressource were found \n"), file = log, append = T)


# Observation
print("Downloading Observation Bundles.")
#use "subject" as FHIR search parameter for Observation resource
body_observation <- fhir_body(content = list("subject" = paste(patient_ids_with_conditions, collapse = ","), "code" = LOINC_codes_all, "_count" = page_count))
request_observations <- fhir_url(url = diz_url, resource = "Observation")
bundles_observation <- fhir_search(request = request_observations, body = body_observation, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_observation)==0){
  #rerun the call to fhir_search()
  bundles_observation <- fhir_search(request = request_observations, body = body_observation, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
#give out statements after certain chunks to document progress
write(paste("Finished Search for Observation-Ressources at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_observation), " Bundles for the Observation-Ressource were found \n"), file = log, append = T)


# medicationAdministration
print("Downloading MedicationAdministration Bundles.")
#use "subject" as FHIR-Search parameter in medicationAdministration resource
#1. search for all medicationAdministrations of the patients
body_medicationAdministration <- fhir_body(content = list("subject" = paste(patient_ids_with_conditions, collapse = ","), "_count" = page_count))
request_medicationAdministrations <- fhir_url(url = diz_url, resource = "MedicationAdministration")
bundles_medicationAdministration <- fhir_search(request = request_medicationAdministrations, body = body_medicationAdministration, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_observation)==0){
  #rerun the call to fhir_search()
  bundles_medicationAdministration <- fhir_search(request = request_medicationAdministrations, body = body_medicationAdministration, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
#give out statements after certain chunks to document progress
write(paste("Finished Search for MedicationAdministration-Ressources at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_medicationAdministration), " Bundles for the MedicationAdministration-Ressource were found \n"), file = log, append = T)

#crack immediately to provide ids for medication-search
if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table will be created instead.")
  #create empty list of medications in the medicationAdministrations of the Patients to fill in next step
  medicationAdministration_ids <- ""
  table_medicationAdministrations <- data.frame(medicationAdministration_identifier            = character(length(unique(patient_ids_with_conditions))),
                                                medicationAdministration_subject               = unique(patient_ids_with_conditions), 
                                                medicationAdministration_status                = character(length(unique(patient_ids_with_conditions))),
                                                medicationAdministration_medication_reference  = character(length(unique(patient_ids_with_conditions))),
                                                medicationAdministration_effective_dateTime    = character(length(unique(patient_ids_with_conditions))),
                                                medicationAdministration_effective_period      = character(length(unique(patient_ids_with_conditions))),
                                                stringsAsFactors = FALSE)
  


} else {
  message("Cracking ", length(bundles_medicationAdministration), " medicationAdministration Bundles.\n")
  table_medicationAdministrations <- fhir_crack(bundles = bundles_medicationAdministration, design = tabledescription_medicationAdministration, verbose = 1)
  #create list of medication_ids that are referenced in the medicationAdministrations of the Patients
  # !! was steht in medicationAdministration_medication_reference?? !!
  medicationAdministration_ids <- sub("Medication/", "", table_medicationAdministrations$medicationAdministration_medication_reference)
  #also adjust the prefixes in the whole column
  table_medicationAdministrations$medicationAdministration_subject <- sub("Patient/", "", table_medicationAdministrations$medicationAdministration_subject) 
  table_medicationAdministrations$medicationAdministration_medication_reference <- sub("Medication/", "", table_medicationAdministrations$medicationAdministration_medication_reference)
  #give out statements after certain chunks to document progress
  write(paste("Cracked Table for MedicationAdministration-Ressources at", Sys.time(), "\n"), file = log, append = T)
  write(paste(nrow(table_medicationAdministrations), " Elements were created for MedicationAdministration \n"), file = log, append = T)
}


#2. search for all medications from the identified medication administrations
print("Downloading Medication Bundles.")
# Medication
body_medication <- fhir_body(content = list("_id" = paste(medicationAdministration_ids, collapse = ","), "_count" = page_count))
request_medications <- fhir_url(url = diz_url, resource = "Medication")
bundles_medication <- fhir_search(request = request_medications, body = body_medication, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_observation)==0){
  #rerun the call to fhir_search()
  bundles_medication <- fhir_search(request = request_medications, body = body_medication, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}

# Save Bundles -------------------------------------------------------------------------------------------------------------------------------------------------------------

if (save_bundles == TRUE)   {
# (only after first download) move to FHIR Search section (so far only medicationAdministration)
#save and load to circumvent long download times for bundles; comment and uncomment with line above (fhir_search) as necessary
message("Saving  Bundles.\n")
fhir_save(bundles = bundles_patient, directory = "XML_Bundles/bundles_patient")
fhir_save(bundles = bundles_condition, directory = "XML_Bundles/bundles_condition")
fhir_save(bundles = bundles_observation, directory = "XML_Bundles/bundles_observation")
fhir_save(bundles = bundles_medicationAdministration, directory = "XML_Bundles/bundles_medicationAdministration")
fhir_save(bundles = bundles_medication, directory = "XML_Bundles/bundles_medication")
#give out statements after certain chunks to document progress
write(paste("Saved Bundles at ", Sys.time(), "\n"), file = log, append = T)
 }
} else {
# Load Bundles ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#executed if search_for_bundles == FALSE
message("Loading saved Bundles.\n")
bundles_patient <- fhir_load(directory = "XML_Bundles/bundles_patient")
bundles_condition <- fhir_load(directory = "XML_Bundles/bundles_condition")
bundles_observation <- fhir_load(directory = "XML_Bundles/bundles_observation")
bundles_medication <- fhir_load(directory = "XML_Bundles/bundles_medication")
bundles_medicationAdministration <- fhir_load(directory = "XML_Bundles/bundles_medicationAdministration")
#give out statements after certain chunks to document progress
write(paste("Loaded Bundles at", Sys.time(), "\n"), file = log, append = T)
 }


# Crack Into Tables -----------------------------------------------------------------------------------------------------------------------------------------------------------
#crack bundles into table

if(is_fhir_bundle_empty(bundles_patient) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table has been created.")
  #create empty table to prevent errors in following code
  table_patients <- data.frame(patient_identifier  = character(),
                               patient_gender      = numeric(),
                               patient_birthdate   = as.Date(character()))                    
} else {
message("Cracking ", length(bundles_patient), " Patient Bundles.\n")
table_patients <- fhir_crack(bundles = bundles_patient, design = tabledescription_patient, verbose = 1)
write(paste(nrow(table_patients), " Elements were created for Patients \n"), file = log, append = T)
  }

if(is_fhir_bundle_empty(bundles_condition) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table has been created.")
  table_conditions <- data.frame(condition_identifier     = character(length(unique(patient_ids_with_conditions))),
                                 condition_code           = character(length(unique(patient_ids_with_conditions))),
                                 condition_system         = character(length(unique(patient_ids_with_conditions))),
                                 condition_recordedDate   = as.Date(character(length(unique(patient_ids_with_conditions)))),
                                 condition_onsetDate      = as.Date(character(length(unique(patient_ids_with_conditions)))),
                                 condition_subject        = unique(patient_ids_with_conditions))
} else {
message("Cracking ", length(bundles_condition), " Condition Bundles.\n")
table_conditions <- fhir_crack(bundles = bundles_condition, design = tabledescription_condition, verbose = 1)
write(paste(nrow(table_conditions), " Elements were created for Conditions \n"), file = log, append = T)
}

if(is_fhir_bundle_empty(bundles_observation) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table has been created.")
  table_observations <- data.frame(observation_identifier  = character(length(unique(patient_ids_with_conditions))),
                                   observation_subject     = unique(patient_ids_with_conditions),
                                   observation_code        = character(length(unique(patient_ids_with_conditions))),
                                   observation_value       = numeric(length(unique(patient_ids_with_conditions))),
                                   observation_unit        = character(length(unique(patient_ids_with_conditions))),
                                   observation_datetime    = as.Date(character(length(unique(patient_ids_with_conditions)))))
} else {
message("Cracking ", length(bundles_observation), " Observation Bundles.\n")
table_observations <- fhir_crack(bundles = bundles_observation, design = tabledescription_observation, verbose = 1)
write(paste(nrow(table_observations), " Elements were created for Observations \n"), file = log, append = T)
}

#check data availability and crack bundles to extract medication_ids
if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked.  An empty table has been created.")
  table_medications <- data.frame(medication_identifier      = character(),
                                  medication_system          = character(),
                                  medication_code            = character(),
                                  medication_display         = character(),
                                  medication_text            = character(),
                                  medication_strength        = character(),
                                  medication_strength_per    = character(),
                                  medication_unit            = character(),
                                  stringsAsFactors = FALSE)
} else {
  message("Cracking ", length(bundles_medication), " Medication Bundles.\n")
  table_medications <- fhir_crack(bundles = bundles_medication, design = tabledescription_medication, verbose = 1)
  write(paste(nrow(table_medications), " Elements were created for Medications \n"), file = log, append = T)
}
#Log Documentation
write(paste("Finished Search for Medication-Ressources at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_medication), " Bundles for the Medication-Ressource were found \n"), file = log, append = T)


#3. combine tables and retain the medicationAdministrations with the relevant medications (removed if-clause, as table will exist in any case (might be empty though))
#merge medication information with data in medicationAdministration
table_meds <- merge(table_medicationAdministrations, table_medications, by.x = "medicationAdministration_medication_reference", by.y = "medication_identifier", all.x = TRUE)
#remove medicationAdministrations that do not concern relevant medications
if(is_fhir_bundle_empty(bundles_medicationAdministration) == FALSE){
table_meds <- table_meds[table_meds$medication_code %in% medications_all,]
}

#check if any patients were found, stop analysis if no patient are in bundles
if(length(table_patients$patient_identifier)==0){
  write("Es konnten keine Patienten mit den angegebene ICD-10 Codes auf dem Server gefunden werden. Abfrage abgebrochen.", file ="errors/error_message.txt")
  #stop("No Patients found - aborting.")
}

#give out statements after certain chunks to document progress
write(paste("Bundles were cracked into tables at", Sys.time(), "\n"), file = log, append = T)


# Data Cleaning ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
message("Cleaning the Data.\n")

#convert birthday to birthyear and calculate age
#fhircracking-process makes all variables into character-variables, year should always be given first (according to Implementation Guide/FHIR), first four characters can be extracted for birthyear
if(is_fhir_bundle_empty(bundles_patient) == TRUE) {
  message("The action you trying to carry out is not possible due to empty resources. Executing the action would result in an error. Therefore the action will not be carried out.")
} else {
#apply function to date column
if("patient_birthdate" %in% names(table_patients)){
table_patients$patient_birthdate <- sapply(table_patients$patient_birthdate, convert_date_to_year)
} else {
  message("The column birthdate is not present in the Patient-Table. If this is not the first execution of the code, this is likely due to the removal of the column in the next line, to protect patient privacy. Please check if the column patient_birthyear already exists.")
}
#rename column for clarity
colnames(table_patients)[colnames(table_patients) == "patient_birthdate"] <- "patient_birthyear"
#calculate age
current_year <- as.numeric(format(Sys.Date(), "%Y"))
table_patients$patient_age <- current_year - table_patients$patient_birthyear
  }

if(is_fhir_bundle_empty(bundles_observation) == TRUE) {
  message("The action you trying to carry out is not possible due to empty resources. Executing the action would result in an error. Therefore the action will not be carried out.")
} else {
#values must be changed to numeric to show distribution
table_observations$observation_value_num <- as.numeric(table_observations$observation_value)

#change character columns for date to datetime
table_conditions$condition_recordedDate <- anytime(table_conditions$condition_recordedDate)
table_conditions$condition_onsetDate <- ymd_hms(table_conditions$condition_onsetDate, tz = "UTC")
table_observations$observation_datetime <- ymd_hms(table_observations$observation_datetime, tz = "UTC")

#calculate time since onset for all conditions 
table_conditions$time_since_first_diagnosis_using_recordeddate <- difftime(Sys.time(), table_conditions$condition_recordedDate, units = "days")
table_conditions$time_since_first_diagnosis_using_onsetdate <- difftime(Sys.time(), table_conditions$condition_onsetDate, units = "days")


#import table with LOINC Code for reference, send CSV with Script, change path to be universally applicable
loinc_codes <- read.csv("Loinc_2.78/LoincTable/Loinc.csv")
#add content of LOINC codes to observation table
table_observations <-  merge(table_observations, loinc_codes[, c("LOINC_NUM", "COMPONENT")], by.x = "observation_code", by.y = "LOINC_NUM", all.x = TRUE)
# Rename column COMPONENT to observation_LOINC_term
colnames(table_observations)[colnames(table_observations) == "COMPONENT"] <- "observation_LOINC_term"
}

#remove "Patient/" prefix from subject-column to allow merging of tables, same for medication if necessary
table_conditions$condition_subject <- sub("Patient/", "", table_conditions$condition_subject) 
table_observations$observation_subject <- sub("Patient/", "", table_observations$observation_subject)


#give out statements after certain chunks to document progress
write(paste("Data Cleaning was finished at", Sys.time(), "\n"), file = log, append = T)



# Additional columns for analysis  ----------------------------------------------------------------------------------------------------------------------------------------------
#column with time since first CVD Diagnosis for each patient
#calculate time since first Cardiovascular Diagnosis (I05-I09, I20-I25, I30-I52) taking into consideration both dates (no matter if diagnosed or documented, the event exists)

#for recordedDate (mandatory)
table_conditions <- table_conditions %>%
  filter(condition_code %in% icd10_codes_patient_conditions$icd_normcode) %>%
  group_by(condition_subject) %>%
  mutate(condition_first_diagnosis_date = pmin(condition_recordedDate, condition_onsetDate, na.rm = TRUE),
         condition_type_of_date = case_when(
           condition_first_diagnosis_date == condition_recordedDate ~ "recordedDate",
           condition_first_diagnosis_date == condition_onsetDate ~ "onsetDate",
           TRUE ~ NA_character_),
         condition_time_since_first_cvd = as.numeric(difftime(Sys.Date(), condition_first_diagnosis_date, units = "days"))) %>%
  ungroup()
  
# #same for onsetDate if available (optional)
# table_conditions <- table_conditions %>%
#   filter(condition_code %in% icd10_codes_patient_conditions$icd_normcode) %>%
#   group_by(condition_subject) %>%
#   mutate(condition_first_onset_date = min(condition_onsetDate),
#          condition_time_since_first_cvd_record = as.numeric(difftime(Sys.Date(), condition_first_onset_date, units = "days"))) %>%
#   ungroup()



#create vectors to check eligibility
### Create Tables for Observation (not Patients) with respective Score criteria 
#shorten ICD-10 code to simplify operations (only first three characters are needed)
table_conditions$condition_code_short <- substr(table_conditions$condition_code,1,3)

## CHA2DS2VASc
# age > 18, Atrial fibrillation (previous 12 months), "non-valvular AF" (ICD-10 Code?)
chadsvasc_inclusion_icd_codes <- data.frame(icd_code = c("I48"))
chadsvasc_inclusion_icd_codes <- icd_expand(chadsvasc_inclusion_icd_codes, col_icd = "icd_code", year = 2023)
chadsvasc_exclusion_icd_codes <- data.frame(icd_code = c("I05"))
chadsvasc_exclusion_icd_codes <- icd_expand(chadsvasc_exclusion_icd_codes, col_icd = "icd_code", year = 2023)
#ops codes for heart valve surgery; requires procedure module
#chadsvasc_exclusion_ops_codes <- c("5-350", "5-351", "5-352", "5-353", "5-354", "5-355", "5-356", "5-357", "5-358", "5-359", "5-35a", "5-35b")

## SMART2
# 40 < age < 80, CHD, CeVD, I06-I09, I20-I25, I70, I71, I73-I79
smart_inclusion_icd_codes <- data.frame(icd_code = c("I06", "I07", "I08", "I09", "I20", "I21", "I22", "I23", "I24", "I25", "I70", "I71", "I73", "I74", "I77", "I78", "I79"))
smart_inclusion_icd_codes <- icd_expand(smart_inclusion_icd_codes, col_icd = "icd_code", year = 2023)

## MAGGIC
# age > 18 years, chronic HF (I50)
maggic_inclusion_icd_codes <- data.frame(icd_code = c("I50"))
maggic_inclusion_icd_codes <- icd_expand(maggic_inclusion_icd_codes, col_icd = "icd_code", year = 2023)


#indicate which score applies by way of indication: CVD = I20 - I25, AF = I48, HF = I50
table_conditions$condition_cvd <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% smart_inclusion_icd_codes$icd_normcode, 1, 0)
table_conditions$condition_af <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% chadsvasc_inclusion_icd_codes$icd_normcode, 1, 0)
table_conditions$condition_hf <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% maggic_inclusion_icd_codes$icd_normcode, 1, 0)


## Eligibility Assessment ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#create additional columns in original resources tables, later extract relevant columns for eligibility/can_calc if necessary
#NAs result in "Ineligibility" and "cannot calc"
#eligibility column in each resource table for each score per patient

#CHADSVASC
#18years or older
table_patients$eligible_patient_age_chadsvasc <- ifelse(!is.na(table_patients$patient_age) & table_patients$patient_age >= 18, 1, 0)
#specified conditions of atrial fibrillation
table_conditions$eligible_conditions_AF_chadsvasc <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% chadsvasc_inclusion_icd_codes$icd_normcode & (table_conditions$condition_time_since_first_cvd < 366), 1, 0)
#potentially insert heart valve surgery if module for procedures is available
table_conditions$eligible_conditions_AF_chadsvasc <- ifelse(table_conditions$condition_code %in% chadsvasc_exclusion_icd_codes$icd_normcode, 0, table_conditions$eligible_conditions_AF_chadsvasc)
#no eligibility criteria regarding observations for chadsvasc
table_observations$eligible_observations_chadsvasc <- 1
#no eligibility criteria regarding observations for chadsvasc
table_meds$eligible_meds_chadsvasc <- if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  table_meds$eligible_meds_chadsvasc <- 99
} else {
  table_meds$eligible_meds_chadsvasc <- 1
  }

#SMART
#between 40 and 80 years old (validated population)
table_patients$eligible_patient_age_smart <- ifelse(!is.na(table_patients$patient_age) & table_patients$patient_age >= 40 & table_patients$patient_age <= 80, 1, 0)
#specified conditions of cardiovascular disease that must be present
#check each
table_conditions$eligible_conditions_I06_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I06"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I07_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I07"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I08_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I08"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I09_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I09"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I20_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I20"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I21_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I21"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I22_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I22"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I23_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I23"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I24_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I24"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I25_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I25"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I70_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I70"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I71_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I71"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I73_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I73"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I74_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I74"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I77_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I77"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I78_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I78"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
table_conditions$eligible_conditions_I79_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% icd_expand(data.frame(icd_code = "I79"), col_icd = "icd_code", year = 2023)$icd_normcode, 1, 0)
#check any
table_conditions$eligible_conditions_smart <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% smart_inclusion_icd_codes$icd_normcode, 1, 0)
#rankin scale must be 3 or less, LOINC: 75859-9
table_observations$eligible_observations_smart <- ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_code_rankin_scale & table_observations$observation_value_num > 3, 0, 1)
#if there are no medications, SMART eligibility cannot be determined
if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  table_meds$eligible_meds_smart <- 99
} else {
    table_meds$eligible_meds_smart <- 1
  }


#MAGGIC
#18years or older
table_patients$eligible_patient_maggic <- ifelse(!is.na(table_patients$patient_age) & table_patients$patient_age >= 18, 1, 0)
#specified conditions of heart failure
table_conditions$eligible_conditions_HF_maggic <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% maggic_inclusion_icd_codes$icd_normcode, 1, 0)
#no eligibility criteria regarding observations for maggic
table_observations$eligible_observations_maggic <- 1
#no eligibility criteria regarding medications for maggic
if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  table_meds$eligible_meds_maggic <- 99
} else {
  table_meds$eligible_meds_maggic <- 1
  }


## Can Calc Assessment -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#sex and age are required from patient table
table_patients$can_calc_patient_chadsvasc <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)
table_patients$can_calc_patient_smart <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)
table_patients$can_calc_patient_maggic <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)

# are required data available from observations table
#group by patient to assess all corresponding observations, if any combination of the observations pertains to required values, the score can be calculated

# give indicator for can_calc for each observation, aggregation in later step
table_observations <- table_observations %>%
  mutate(can_calc_observations_chadsvasc = 1) 

table_observations <- table_observations %>%
  mutate(
    #ggf anpassen
    can_calc_observation_cholesterolhdl_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_cholesterol_hdl, 1, 0),
    can_calc_observation_cholesterol_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_cholesterol_overall, 1, 0),
    can_calc_observation_bloodpressure_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_bp_sys, 1, 0),
    can_calc_observation_egfr_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_egfr, 1, 0),
    can_calc_observation_crp_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_crp, 1, 0),
    #if no info for smoking is available, no smoking assumed
    can_calc_observation_bloodpressure_smart = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_smoking, 1, 99),
    
    can_calc_observations_smart = ifelse(!is.na(table_observations$observation_code) & (any(table_observations$observation_code %in% LOINC_codes_cholesterol_hdl | table_observations$observation_code %in% LOINC_codes_cholesterol_overall)) & any(table_observations$observation_code %in% LOINC_codes_bp_sys), 1, 0))

table_observations <- table_observations %>%
  mutate(
    can_calc_observations_bloodpressure_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_bp_sys, 1, 0),
    can_calc_observations_lvef_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_lvef, 1, 0),
    can_calc_observations_creatinine_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_creatinine, 1, 0),
    can_calc_observations_bmi_maggic = ifelse((!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_bmi) | (!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_height & table_observations$observation_code %in% LOINC_codes_weight), 1, 0),
    #if no info for smoking is available, no smoking assumed
    can_calc_observations_smoking_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_smoking, 1, 99),
    can_calc_observations_maggic = ifelse(!is.na(table_observations$observation_code) & (any(table_observations$observation_code %in% LOINC_codes_bp_sys & table_observations$observation_code %in% LOINC_codes_lvef & table_observations$observation_code %in% LOINC_codes_creatinine & table_observations$observation_code %in% LOINC_codes_bmi)), 1, 0))



#conditions and medications are presumed to be not present in the patient if not available in data, therefore give value of 1
table_conditions$can_calc_conditions_chadsvasc <- 1
table_conditions$can_calc_conditions_smart <- 1
table_conditions$can_calc_conditions_maggic <- 1

#accommodate empty medication tables
if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
  table_meds$can_calc_meds_chadsvasc <- 99
  table_meds$can_calc_meds_smart <- 99
  table_meds$can_calc_meds_maggic <- 99
} else {
  table_meds$can_calc_meds_chadsvasc <- 1
  table_meds$can_calc_meds_smart <- 1
  table_meds$can_calc_meds_maggic <- 1
}


## Merge Reosurces -------------------------------------------------------------------------------------------------------------------------------------------------------------

#create tables with selected columns for merge; for tables with multiple entries only use subject_identifer (aggregation for other variables not useful)
table_patients_merge_eligibility <- table_patients[, c(1:4, grep("eligible", names(table_patients)))]
table_conditions_merge_eligibility <- table_conditions[, c(6, 13:15, grep("eligible", names(table_conditions)))]
table_observations_merge_eligibility <- table_observations[, c(3, grep("eligible", names(table_observations)))]
table_meds_merge_eligibility <- table_meds[, c(3, grep("eligible", names(table_meds))), drop = FALSE]

table_patients_merge_can_calc <- table_patients[, c(1:4, grep("can_calc", names(table_patients)))]
table_conditions_merge_can_calc <- table_conditions[, c(6, 13:15, grep("can_calc", names(table_conditions)))]
table_observations_merge_can_calc <- table_observations[, c(3, grep("can_calc", names(table_observations)))]
table_meds_merge_can_calc <- table_meds[, c(3, grep("can_calc", names(table_meds))), drop = FALSE]


#reduce tables to 1 entry per patient, if any of the rows have a 0 (ineligible), the whole patient becomes ineligible (exclusion criterion)
#table_conditions_merge_eligibility <- aggregate(. ~ condition_subject, data = table_conditions_merge_eligibility, function(x) ifelse(any(x == 0), 0, 1))
table_conditions_merge_eligibility <- table_conditions_merge_eligibility %>%
                                      group_by(condition_subject) %>%
                                      summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                      ungroup()
#table_observations_merge_eligibility <- aggregate(. ~ observation_subject, data = table_observations_merge_eligibility, FUN = function(x) ifelse(any(x == 0), 0, 1))
table_observations_merge_eligibility <- table_observations_merge_eligibility %>%
                                        group_by(observation_subject) %>%
                                        summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                        ungroup()
if (is_fhir_bundle_empty(bundles_medicationAdministration) == FALSE) {table_meds_merge_eligibility <- aggregate(. ~ medicationAdministration_subject, data = table_meds_merge_eligibility, FUN = function(x) ifelse(any(x == 0), 0, 1))}

#reduce tables to 1 entry per patient, if any of the rows have a 1 (can_calc), the whole patient becomes can_calc
table_conditions_merge_can_calc <- table_conditions_merge_can_calc %>%
                                   group_by(condition_subject) %>%
                                   summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                   ungroup()
table_observations_merge_can_calc <- table_observations_merge_can_calc %>%
                                     group_by(observation_subject) %>%
                                     summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                     ungroup()
if (is_fhir_bundle_empty(bundles_medicationAdministration) == FALSE) {table_meds_merge_can_calc <- aggregate(. ~ medicationAdministration_subject, data = table_meds_merge_can_calc, FUN = function(x) ifelse(any(x == 0), 0, 1))}



#Create relevant tables for analysis from resource tables
table_eligibility_merge <- merge(table_patients_merge_eligibility, table_conditions_merge_eligibility, by.x = "patient_identifier", by.y = "condition_subject", all.x = TRUE)
table_eligibility_merge <- merge(table_eligibility_merge, table_observations_merge_eligibility, by.x = "patient_identifier", by.y = "observation_subject", all.x = TRUE)
table_eligibility_merge <- merge(table_eligibility_merge, table_meds_merge_eligibility, by.x = "patient_identifier", by.y = "medicationAdministration_subject", all.x = TRUE)

table_can_calc_merge <- merge(table_patients_merge_can_calc, table_conditions_merge_can_calc, by.x = "patient_identifier", by.y = "condition_subject", all.x = TRUE)
table_can_calc_merge <- merge(table_can_calc_merge, table_observations_merge_can_calc, by.x = "patient_identifier", by.y = "observation_subject", all.x = TRUE)
table_can_calc_merge <- merge(table_can_calc_merge, table_meds_merge_can_calc, by.x = "patient_identifier", by.y = "medicationAdministration_subject", all.x = TRUE)

table_eligibility_can_calc <- merge (table_eligibility_merge, table_can_calc_merge, by = c("patient_identifier", "patient_gender", "patient_birthyear", "patient_age", "condition_cvd", "condition_af", "condition_hf"), all = TRUE)


table_eligibility <- table_eligibility_can_calc %>%
  select(1:4, 13:15, contains("eligible"))
  
table_can_calc <- table_eligibility_can_calc %>%
  select(1:4, 13:15, contains("can_calc"))


#where medications are empty/missing, we assume that they are not present, the score can be calculated in either case (however result might not be entirely accurate)
table_can_calc <- table_can_calc %>%
  mutate(across(c(can_calc_meds_chadsvasc, can_calc_meds_maggic, can_calc_meds_smart), ~replace_na(.,1)))
#Assumption: if Medications or Conditions are missing, the patient does not have them; non-existence cannot be confirmed by routine data 


# Feasibility Analysis ----------------------------------------------------
message("Analysing Data.\n")

# #take into account the possibility, that certain columns do no exist, if data is unavailable (eg medication)
eligibility_required_columns_chadsvasc <- c("eligible_patient_age_chadsvasc", "eligible_conditions_AF_chadsvasc", "eligible_observations_chadsvasc", "eligible_meds_chadsvasc")
eligibility_available_columns_chadsvasc <- eligibility_required_columns_chadsvasc[eligibility_required_columns_chadsvasc %in% colnames(table_eligibility_can_calc)]
eligibility_required_columns_smart <- c("eligible_patient_age_smart", "eligible_conditions_smart", "eligible_observations_smart", "eligible_meds_smart")
eligibility_available_columns_smart <- eligibility_required_columns_smart[eligibility_required_columns_smart %in% colnames(table_eligibility_can_calc)]
eligibility_required_columns_maggic <- c("eligible_patient_maggic", "eligible_conditions_maggic", "eligible_observations_maggic", "eligible_meds_maggic")
eligibility_available_columns_maggic <- eligibility_required_columns_maggic[eligibility_required_columns_maggic %in% colnames(table_eligibility_can_calc)]
# eligibility_required_columns_charge <- c("eligible_patient_charge", "eligible_conditions_charge", "eligible_observations_charge", "eligible_meds_charge")
# eligibility_available_columns_charge <- eligibility_required_columns_charge[eligibility_required_columns_charge %in% colnames(table_eligibility)]

#create summary column for eligibility (if any 0, then 0; if no 0s but any NAs, then NA, if no 0s or NAs then 1)
#only consider columns for this score, that are available
table_eligibility_can_calc$eligible_chadsvasc_overall <- apply(table_eligibility_can_calc[,eligibility_available_columns_chadsvasc], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc$eligible_smart_overall <- apply(table_eligibility_can_calc[,eligibility_available_columns_smart], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc$eligible_maggic_overall <- apply(table_eligibility_can_calc[,eligibility_available_columns_maggic], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
# table_eligibility$eligible_charge_overall <- apply(table_eligibility[,eligibility_available_columns_charge], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))

#Currently not needed for analysis
#give back result per patient, whether certain criterion has been fulfilled; adjusted to certain columns from larger table in select
table_eligibility_all_criteria <- table_eligibility_can_calc %>%
  group_by(patient_identifier) %>%
  #mutate correct? otherwise remove from analysis
  mutate(across(contains("eligible"), ~ ifelse(any(. == 1), 1, 0))) %>%
  select(-starts_with("patient"), -patient_identifier) %>%
  ungroup() 

#check availability of any score
#previous code
#table_eligibility_can_calc$any_score_eligible <- rowSums(table_eligibility_can_calc[, c("eligible_chadsvasc_overall", "eligible_smart_overall", "eligible_maggic_overall")], na.rm = TRUE) > 0

# Apply logic row-wise
table_eligibility_can_calc$any_score_eligible <- apply(
  table_eligibility_can_calc[, c("eligible_chadsvasc_overall", "eligible_smart_overall", "eligible_maggic_overall")],
  1,
  function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else if (any(row == 1, na.rm = TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
)



# ability of calculating scores (all parameters that need to be available, are available), absence of parameters is interpreted as not present in patient
can_calc_required_columns_chadsvasc <- c("can_calc_patient_chadsvasc", "can_calc_conditions_chadsvasc", "can_calc_observations_chadsvasc", "can_calc_meds_chadsvasc")
can_calc_available_columns_chadsvasc <- can_calc_required_columns_chadsvasc[can_calc_required_columns_chadsvasc %in% colnames(table_eligibility_can_calc)]
can_calc_required_columns_smart <- c("can_calc_patient_smart", "can_calc_conditions_smart", "can_calc_observations_smart", "can_calc_meds_smart")
can_calc_available_columns_smart <- can_calc_required_columns_smart[can_calc_required_columns_smart %in% colnames(table_eligibility_can_calc)]
can_calc_required_columns_maggic <- c("can_calc_patient_maggic", "can_calc_conditions_maggic", "can_calc_observations_maggic", "can_calc_meds_maggic")
can_calc_available_columns_maggic <- can_calc_required_columns_maggic[can_calc_required_columns_maggic %in% colnames(table_eligibility_can_calc)]

#create summary column for can_calc (if any 0, then 0; if no 0s but any NAs, then NA, if no 0s or NAs then 1)
table_eligibility_can_calc$can_calc_chadsvasc_overall <- apply(table_eligibility_can_calc[,can_calc_available_columns_chadsvasc], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc$can_calc_smart_overall <- apply(table_eligibility_can_calc[,can_calc_available_columns_smart], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc$can_calc_maggic_overall <- apply(table_eligibility_can_calc[,can_calc_available_columns_maggic], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))

#check availability of any score
#previous coding
#table_eligibility_can_calc$any_score_can_calc <- rowSums(table_eligibility_can_calc[, c("can_calc_chadsvasc_overall", "can_calc_smart_overall", "can_calc_maggic_overall")], na.rm = TRUE) > 0

# Apply logic row-wise
table_eligibility_can_calc$any_score_can_calc <- apply(
  table_eligibility_can_calc[, c("can_calc_chadsvasc_overall", "can_calc_smart_overall", "can_calc_maggic_overall")],
  1,
  function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else if (any(row == 1, na.rm = TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
)




#as last step to include all columns
export_table_can_calc <- table_can_calc %>%
  select(-starts_with("patient"))
#check whether this drops all relevant patient information



## check availability of parameters 
#sum up all entries of NA in each of the columns (maybe not as useful, entries in each resource are only created if some data is available)
#unlikely, but if there are NAs here it would be interesting 
navalues_patient_columns <- table_eligibility_can_calc %>%
                            summarise(across(contains("patient"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                            mutate(total_entries = nrow(table_eligibility_can_calc))
navalues_condition_columns <- table_eligibility_can_calc %>%
                              summarise(across(contains("condition"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                              mutate(total_entries = nrow(table_eligibility_can_calc))
navalues_observation_columns <- table_eligibility_can_calc %>%
                                summarise(across(contains("observation"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                                mutate(total_entries = nrow(table_eligibility_can_calc))
navalues_medication_columns <- table_eligibility_can_calc %>%
                               summarise(across(contains("medication"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                               mutate(total_entries = nrow(table_eligibility_can_calc))

#count number of patients (navalues_all_columns counts data entries not patients) who have NA in condition code, observation code or medication code 
precentage_patients_with_no_code_condition <- table_conditions %>% filter(is.na(condition_code)) %>% summarise(condition_code_na = n_distinct(condition_subject)/length(unique(table_conditions$condition_subject)))
precentage_patients_with_no_code_observation <- table_observations %>% filter(is.na(observation_code)) %>% summarise(observation_code_na = n_distinct(observation_subject)/length(unique(table_observations$observation_subject))) 
precentage_patients_with_no_code_medication <- table_meds %>% filter(is.na(medication_code)) %>% summarise(medication_code_na = n_distinct(medicationAdministration_subject)/length(unique(table_meds$medicationAdministration_subject)))

## Check distribution of parameters ##########################################################################
#gives min, max, mean, median and n where applicable (discrete and continuous data)
desc_patient_age <- table_patients %>% summarise(min_age = min(patient_age), 
                                                 q1_age = quantile(patient_age, 0.25, na.rm = TRUE), 
                                                 mean_age = mean(patient_age), 
                                                 median_age = median(patient_age), 
                                                 q3_age = quantile(patient_age, 0.75, na.rm = TRUE), 
                                                 max_age = max(patient_age), 
                                                 n = n())
desc_patient_gender <- as.data.frame(table(table_patients$patient_gender))
#observation
desc_observation <- table_observations %>% filter(!is.na(observation_code) & !is.na(observation_value_num)) %>% group_by(observation_code, observation_LOINC_term) %>% summarise(min = min(observation_value_num), mean = mean(observation_value_num), median = median (observation_value_num), max = max(observation_value_num), n=n())
#condition
desc_conditions <- table_conditions %>% filter(!is.na(condition_code)) %>% count(condition_code)
#medications
desc_medication <- table_meds %>% filter(!is.na(medication_code)) %>% count(medication_code)


#eligibility
eligibility_chadsvasc <- table(table_eligibility$eligible_chadsvasc_overall, useNA = "ifany")
eligibility_smart <- table(table_eligibility$eligible_smart_overall, useNA = "ifany")
eligibility_maggic <- table(table_eligibility$eligible_maggic_overall, useNA = "ifany")
#eligibility_charge <- table(table_eligibility$eligible_charge_overall, useNA = "ifany")

#calculable
can_calc_chadsvasc <- table(table_can_calc$can_calc_chadsvasc_overall, useNA = "ifany")
can_calc_smart <- table(table_can_calc$can_calc_smart_overall, useNA = "ifany")
can_calc_maggic <- table(table_can_calc$can_calc_maggic_overall, useNA = "ifany")
#can_calc_charge <- table(table_can_calc$can_calc_charge_overall, useNA = "ifany")


#analyse all score-specific eligibility variables to see where exclusions occur
table_chadsvasc_eligibility <- table_eligibility[, grepl("chadsvasc", names(table_eligibility)) & names(table_eligibility) != "id"]
table_smart_eligibility <- table_eligibility[, grepl("smart", names(table_eligibility)) & names(table_eligibility) != "id"]
table_maggic_eligibility <- table_eligibility[, grepl("maggic", names(table_eligibility)) & names(table_eligibility) != "id"]
#table_charge_eligibility <- table_eligibility[, grepl("charge", names(table_eligibility)) & names(table_eligibility) != "id"]

#analyse all score-specific calculation variables to see where exclusions occur
table_chadsvasc_can_calc <- table_can_calc[, grepl("chadsvasc", names(table_can_calc)) & names(table_can_calc) != "id"]
table_smart_can_calc <- table_can_calc[, grepl("smart", names(table_can_calc)) & names(table_can_calc) != "id"]
table_maggic_can_calc <- table_can_calc[, grepl("maggic", names(table_can_calc)) & names(table_can_calc) != "id"]
#table_charge_can_calc <- table_can_calc[, grepl("charge", names(table_can_calc)) & names(table_can_calc) != "id"]


#turn columns into vectors for crosstabs
eligible_to_factor <- grep("eligible", names(table_eligibility_can_calc), value = TRUE)
table_eligibility_can_calc[eligible_to_factor] <- lapply(table_eligibility_can_calc[eligible_to_factor], function(x) factor(x, levels = c(0, 1)))
can_calc_to_factor <- grep("can_calc", names(table_eligibility_can_calc), value = TRUE)
table_eligibility_can_calc[can_calc_to_factor] <- lapply(table_eligibility_can_calc[can_calc_to_factor], function(x) factor(x, levels = c(0, 1)))

#Percentage of patients who are eligible for at least 1 score
prob_eligibility_any_score <- prop.table(table(table_eligibility_can_calc$any_score_eligible))
#Percentage for which at least one score can be calculated
prob_can_calc_any_score <- prop.table(table(table_eligibility_can_calc$any_score_can_calc))

#bei Calc noch die Elig hinzufügen, da ja die Frage ist von wie vielen die infrage kommen, kann  der Score berechnet werden
#remove here and apply to results table that will be send back
crosstabs_eligibility_can_calc_any_score <- table(table_eligibility_can_calc$any_score_eligible, table_eligibility_can_calc$any_score_can_calc, dnn = c("Eligible", "Calculable"))


#give out statements after certain chunks to document progress
write(paste("Analysis Steps were finished at", Sys.time(), "\n"), file = log, append = T)


export_table_eligibility_can_calc <- table_eligibility_can_calc %>%
  select(-starts_with("patient"))

# Data Export -------------------------------------------------------------
message("Writing Results into CSV-Files.\n")
#overview over NAs in resource columns
write.csv(navalues_patient_columns, "Output/number_of_missing_values_patient_columns.csv")
write.csv(navalues_condition_columns, "Output/number_of_missing_values_condition_columns.csv")
write.csv(navalues_observation_columns, "Output/number_of_missing_values_observation_columns.csv")
write.csv(navalues_medication_columns, "Output/number_of_missing_medication_columns.csv")
#percentage of patients with missings in crucial columns; no additional value
# write.csv(precentage_patients_with_no_code_condition, "Output/percentage_patients_missing_condition_code.csv")
# write.csv(precentage_patients_with_no_code_observation, "Output/percentage_patients_missing_observation_code.csv")
# write.csv(precentage_patients_with_no_code_medication, "Output/percentage_patients_missing_medication_code.csv")

#combinations of conditions corresponding observations and medications
#write.csv(patients_with_condition_observation_medication, "Output/number_of_patients_per_combinations_of_condition_observation_medication.csv")
#descriptives (min, max, mean, n) of available Observations, Conditions and Medications
write.csv(desc_patient_age, "Output/Descriptives_of_Patient_Age.csv")
write.csv(desc_patient_gender, "Output/Descriptives_of_Patient_Gender.csv")
write.csv(desc_observation, "Output/Descriptives_of_Observations.csv")
write.csv(desc_conditions, "Output/Descriptives_of_Conditions.csv")
write.csv(desc_medication, "Output/Descriptives_of_Medications.csv")
# #check eligibility criteria
write.csv(table_eligibility_all_criteria, "Output/eligibility_criteria_per_patient.csv")
# #check all can_calc variables
# write.csv(export_table_can_calc, "Output/can_calc_all_variables_per_patient.csv")
#eligiblity and calc
write.csv(export_table_eligibility_can_calc, "Output/export_table_eligibility_can_calc.csv")
#number of eligible and calculable observations per Risk Score
# write.csv(crosstabs_eligibility_availability_chadsvasc, "Output/crosstabs_eligible_calculable_chadsvasc.csv")
# write.csv(crosstabs_eligibility_availability_smart, "Output/crosstabs_eligible_calculable_smart.csv")
# write.csv(crosstabs_eligibility_availability_maggic, "Output/crosstabs_eligible_calculable_maggic.csv")
#write.csv(crosstabs_eligibility_availability_charge, "Output/crosstabs_eligible_calculable_charge.csv")
#number of eligible and calculable observations for any Score
write.csv(as.data.frame(crosstabs_eligibility_can_calc_any_score, "Output/crosstabs_eligible_calculable_anyscore.csv", row.names = NULL))
#number patients who have NAs in any of the columns, per Risk Score
#write.csv(navalues_inResources_allScores, "Output/number_of_patients_with_NAs_inResources_allScores.csv")
#percentage of patients for which score is eligible/can be calculated
write.csv(prob_eligibility_any_score,"Output/percentage_patients_eligible_anyscore.csv")
write.csv(prob_can_calc_any_score,"Output/percentage_patients_cancalc_anyscore.csv")

#give out statements after certain chunks to document progress
write(paste("Data Exports were finished at", Sys.time(), "\n"), file = log, append = T)

#description of populations that eligible for each risk score (age, gender, maybe condition/observation?)?
#additional variables for description of observation, condition, medication?
message("End.\n")




