#source supporting scripts
source("ACRIBiS_Feasibility_install_R_packages.R")
source("ACRIBiS_Feasibility_support_functions.R")
source("ACRIBiS_Feasibility_config_local.R")

#source score calculations
source("barcelona_hf_v3.R")
source("smart.R")
source("maggic.R")
source("chads_vasc.R")

#after loading config, so that correct reference exists
setwd(working_directory)

#create log file, named for date and time of creation
if(!dir.exists("logs")){
  dir.create(paste0(diz_short, "logs"))
  }
log <- paste0(diz_short, "logs/", format(Sys.time(), "%Y%m%d_%H%M%S"),".txt", collapse = "")
write(paste("Starting Script ACRIBiS_Feasibility_Script.R at", Sys.time()), file = log, append = T)


#in case of timeout use 
#httr::set_config(httr::timeout(1800)) #timeout in seconds

#check if loinc codes are present in working directory, other folders will be created
#@ KG ggf auf 2.82 updaten; gab es hier änderungen?
if(file.exists(loinc_path)) {
  write(paste("LOINC-Folder available and CSV-File present.", Sys.time()), file = log, append = T)
  message("LOINC-Folder available and CSV-File present.")
  } else {
    write(paste("The LOINC Code Information required to execute this analysis is missing. Please download and save the files in accordance with the readme on github.", Sys.time()), file = log, append = T)
    message("The LOINC Code Information required to execute this analysis is missing. Please download and save the files in accordance with the readme on github.")
    stop("Script halted: required LOINC-file is missing.")
    }


# Setup -------------------------------------------------------------------
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


#Procedure
tabledescription_procedure <- fhir_table_description(
  resource = "Procedure",
  cols = c(procedure_identifier     = "id",
           procedure_subject        = "subject/reference",
           procedure_encounter      = "encounter/reference",
           procedure_code           = "code/coding/code",
           precedure_date           = "performedDatTime"
  )
)


#Encounter
tabledescription_encounter <- fhir_table_description(
  resource = "Encounter",
  cols = c(encounter_identifier     = "id",
           encounter_periodStart    = "period/start",
           encounter_subject        = "subject/reference"
  )
)


## Encounters ----------------------------------------------------------------------------------------------------------------
print("Downloading Encounter Bundles.")

request_encounters <- fhir_url(url = diz_url, resource = "Encounter")

print("Downloading Encounter Bundles.")
body_encounters <- fhir_body(content = list("date" = "gt2024-01-01", "date" = "le2024-12-31", "_count" = page_count))
request_encounters <- fhir_url(url = diz_url, resource = "Encounter")
bundles_encounters <- fhir_search(request = request_encounters, body = body_encounters, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
if(length(bundles_encounters)==0){
  #rerun the call to fhir_search()
  bundles_encounters <- fhir_search(request = request_encounters, body = body_encounters, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0)
}
write(paste("Finished Search for Encounter-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_encounters), " Bundles for the Encounter-Resource were found \n"), file = log, append = T)

#no saving necessary
table_encounter <- fhir_crack(bundles = bundles_encounters, design = tabledescription_encounter, verbose = 1)
#remove bundles once cracked
rm(bundles_encounters)


#remove Prefix to match reference column; add unique to limit to one entry
relevant_encounter_subjects <- unique(sub("Patient/", "", table_encounter$encounter_subject))

## find patient IDs with relevant ICD-10 Codes for Condition 
# Define relevant ICD-codes for CVD Diagnoses                                               
icd10_codes_patient_conditions <- data.frame( icd_code = c("I05", "I06", "I07", "I08", "I09", 
                                                           "I20", "I21", "I22", "I23", "I24", "I25", 
                                                           "I30", "I31", "I32", "I33", "I34", "I35", 
                                                           "I36", "I37", "I38", "I39", "I40", "I41", 
                                                           "I42", "I43", "I44", "I45", "I46", "I47", 
                                                           "I48", "I49", "I50", "I51", "I52"))
icd10_codes_patient_conditions <- icd_expand(icd10_codes_patient_conditions, col_icd = "icd_code", year = 2023)



# define code System to improve search performance
code_system <- "http://fhir.de/CodeSystem/bfarm/icd-10-gm"
#add column that includes code system for fhir searches
icd10_codes_patient_conditions <- icd10_codes_patient_conditions %>%
  mutate(icd_search_string = paste0(code_system, "|", icd10_codes_patient_conditions$icd_normcode))



# Extract IDs from patients with encounter in 2024 (via IDs list) and relevant diagnosis (ICD-10 list) Identify Required Patients
print("Downloading Condition Bundles to identify relevant patients.")
#download all conditions with respective ICD10-Codes and for patients (subjects) from relevant time frame
#use "code" as FHIR-Search parameter for Condition resource
#body_patient_conditions <- fhir_body(content = list("code" = paste(icd10_codes_patient_conditions$icd_normcode, collapse = ","), "subject" = paste(relevant_encounter_subjects, collapse = ","), "_count" = page_count))
#bundles_patient_conditions <- fhir_search(request = request_patient_conditions, body = body_patient_conditions, max_bundles = bundle_limit, username = username, password = password, rm_tag = rm_tag, stop_on_error = 0, log_errors = "logs/fhir_search_errors.txt")

#Split into chunks
relevant_encounter_subjects_split <- split(relevant_encounter_subjects, ceiling(seq_along(relevant_encounter_subjects) / chunk_size))
relevant_encounter_subjects_list <- lapply(relevant_encounter_subjects_split,  paste , collapse = ",")
#Download Patients via POST
request_patient_conditions <- fhir_url(url = diz_url, resource = "Condition")
bundles_patient_conditions <- lapply(relevant_encounter_subjects_list, function(x) {
  #body_patient_conditions <- fhir_body(content = list(paste0(icd10_codes_patient_conditions$icd_search_string, collapse = ","), "patient" = x, "_count" = page_count))
  body_patient_conditions <- fhir_body(content = list(code = paste0(icd10_codes_patient_conditions$icd_normcode, collapse = ","), "patient" = x, "_count" = page_count))
  fhir_search(
    request = request_patient_conditions,
    body = body_patient_conditions,
    max_bundles = bundle_limit, 
    username = username, 
    password = password, 
    rm_tag = rm_tag, 
    stop_on_error = 0,
    log_errors = "logs/fhir_search_errors.txt"
  )
})
#rerun the call to fhir_search(), in case of timeout
if(length(bundles_patient_conditions)==0){
  bundles_patient_conditions <- lapply(relevant_encounter_subjects_list, function(x) {
    body_patient_conditions <- fhir_body(content = list("code" = paste(icd10_codes_patient_conditions$icd_normcode, collapse = ","), "patient" = x, "_count" = page_count))
    fhir_search(
      request = request_patient_conditions,
      body = body_patient_conditions,
      max_bundles = bundle_limit, 
      username = username, 
      password = password, 
      rm_tag = rm_tag, 
      stop_on_error = 0,
      log_errors = "logs/fhir_search_errors.txt"
    )
  })
}
#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#still required for other xml methods
bundles_patient_conditions <- unlist(bundles_patient_conditions, recursive = FALSE)
#bundles_patient_conditions <- unlist(bundles_patient_conditions, recursive = F)
# no saving necessary
table_patient_conditions <- fhir_crack(bundles = bundles_patient_conditions, design = tabledescription_condition, verbose = 1)
#remove bundle once cracked
rm(bundles_patient_conditions)
#search for patients who have the specified conditions
patient_ids_with_conditions_prefix <- unique(table_patient_conditions$condition_subject)
#remove "Patient/" prefix from referenced Patient IDs
patient_ids_with_conditions <- sub("Patient/", "", patient_ids_with_conditions_prefix)


# List of Codes --------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
LOINC_codes_natrium <- c("2951-2", "2947-0", "77139-4")
LOINC_codes_haemoglobin <- c("30350-3", "718-7", "30313-1")
LOINC_codes_st2 <- c("90239-5")
LOINC_codes_ntprobnp <- c("33762-6", "83107-3", "33763-4", "42637-9")
LOINC_codes_hsctnt <- c("67151-1", "6598-7", "89576-3", "89577-1", "89579-7")


LOINC_codes_all <- paste (c(LOINC_codes_height, LOINC_codes_weight, LOINC_codes_bp_overall, LOINC_codes_bp_sys, LOINC_codes_bp_dia, LOINC_codes_lvef, LOINC_codes_creatinine, 
                            LOINC_codes_egfr, LOINC_codes_cholesterol_overall, LOINC_codes_cholesterol_hdl, LOINC_codes_hscrp, LOINC_codes_crp, LOINC_codes_bmi, LOINC_code_rankin_scale, 
                            LOINC_codes_smoking, LOINC_codes_natrium, LOINC_codes_haemoglobin, LOINC_codes_st2, LOINC_codes_ntprobnp, LOINC_codes_hsctnt), collapse = ",")

#mapping to pick latest observation per group of LOINC Codes
loinc_groups <- tibble(
  observation_code = c(
    LOINC_codes_height,
    LOINC_codes_weight,
    LOINC_codes_bp_overall,
    LOINC_codes_bp_sys,
    LOINC_codes_bp_dia,
    LOINC_codes_lvef,
    LOINC_codes_creatinine,
    LOINC_codes_egfr,
    LOINC_codes_cholesterol_overall,
    LOINC_codes_cholesterol_hdl,
    LOINC_codes_hscrp,
    LOINC_codes_crp,
    LOINC_codes_bmi,
    LOINC_code_rankin_scale,
    LOINC_codes_smoking,
    LOINC_codes_natrium,
    LOINC_codes_haemoglobin,
    LOINC_codes_st2,
    LOINC_codes_ntprobnp,
    LOINC_codes_hsctnt
  ),
  observation_group = c(
    rep("height", length(LOINC_codes_height)),
    rep("weight", length(LOINC_codes_weight)),
    rep("bp_overall", length(LOINC_codes_bp_overall)),
    rep("bp_sys", length(LOINC_codes_bp_sys)),
    rep("bp_dia", length(LOINC_codes_bp_dia)),
    rep("lvef", length(LOINC_codes_lvef)),
    rep("creatinine", length(LOINC_codes_creatinine)),
    rep("egfr", length(LOINC_codes_egfr)),
    rep("cholesterol_overall", length(LOINC_codes_cholesterol_overall)),
    rep("cholesterol_hdl", length(LOINC_codes_cholesterol_hdl)),
    rep("hscrp", length(LOINC_codes_hscrp)),
    rep("crp", length(LOINC_codes_crp)),
    rep("bmi", length(LOINC_codes_bmi)),
    "rankin_scale",
    rep("smoking", length(LOINC_codes_smoking)),
    rep("natrium", length(LOINC_codes_natrium)),
    rep("haemoglobin", length(LOINC_codes_haemoglobin)),
    rep("st2", length(LOINC_codes_st2)),
    rep("ntprobnp", length(LOINC_codes_ntprobnp)),
    rep("hsctnt", length(LOINC_codes_hsctnt))
  )
)





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

medications_torasemid <- "C03CA04"
medications_furosemid <- "C03CA01"
medications_loopdiuretic <- c(medications_torasemid, medications_furosemid)

#C10AA +C10BX + C10BA0
medications_statins <-  c("C10AA01", "C10AA02", "C10AA03", "C10AA04", "C10AA05", "C10AA06", "C10AA07", "C10AA08", 
                          "C10BX01", "C10BX02", "C10BX03", "C10BX04", "C10BX05", "C10BX06", "C10BX07", "C10BX08", "C10BX09", "C10BX10", "C10BX11", "C10BX12", "C10BX13", "C10BX14", "C10BX15", "C10BX16", "C10BX17", "C10BX18", "C10BX19", "C10BX20", "C10BX21", "C10BX22",
                          "C10BA01", "C10BA02", "C10BA03", "C10BA04", "C10BA05", "C10BA06", "C10BA07", "C10BA08", "C10BA09", "C10BA10", "C10BA11", "C10BA12", "C10BA13")

#B01AB-
medications_heparin <- c("B01AB01", "B01AB02", "B01AB04", "B01AB05", "B01AB06", "B01AB07", "B01AB08", "B01AB09", "B01AB10", "B01AB11", "B01AB12", "B01AB13", "B01AB51", "B01AB63")

#C03DA-
medications_mra <- c("C03DA01", "C03DA02", "C03DA03", "C03DA04", "C03DA05")

medications_arni <- "C09DX04"

#A10BK-
medications_sglt2i <- c("A10BK01", "A10BK02", "A10BK03", "A10BK04", "A10BK05", "A10BK06", "A10BK07", "A10BK08", "A10BK09")
  

#no paste, because that creates a single string instead of a list
medications_all <- c(medications_betablockers, medications_acei_arb, medications_antithrombotic, 
                     medications_torasemid, meidcations_furosemid, medications_statins, medications_heparin, medications_mra, medications_arni, medications_sglt2i)


#Procedures
# cardiac devices: 5-377.x
procedure_crt <- c("5-377.4", "5-377.40", "5-377.41",
                   "5-377.7", "5-377.70", "5-377.71",
                   "5-377.f0", "5-377.f1", "5-377.f2", "5-377.f3", "5-377.f4", "5-377.fx",
                   "5-377.h0", "5-377.h1", "5-377.hx",
                   "5-377.m", "5-377.m0", "5-377.n", "5-377.n0", "5-377.n1", "5-377.n2")

procedure_icd <- c("5-377.5", "5-377.50", "5-377.51", "5-377.6",
                   "5-377.c", "5-377.c0", "5-377.c1", "5-377.c2", "5-377.j", "5-377.j0", "5-377.j1")

procedure_cardiac_device <- c("5-377", "5-377.1", "5-377.2", "5-377.3", "5-377.30", "5-377.31", "5-377.4", "5-377.40", "5-377.41", "5-377.5", "5-377.50", "5-377.51", 
                              "5-377.6", "5-377.7", "5-377.70", "5-377.71", "5-377.8", "5-377.b", "5-377.c", "5-377.c0", "5-377.c1", "5-377.c2", "5-377.d", "5-377.f", 
                              "5-377.f0", "5-377.f1", "5-377.f2", "5-377.f3", "5-377.f4", "5-377.fx", "5-377.g", "5-377.g0", "5-377.g1", "5-377.g2", "5-377.h", "5-377.h0", 
                              "5-377.h1", "5-377.hx", "5-377.j", "5-377.k", "5-377.m", "5-377.m0", "5-377.m1", "5-377.n", "5-377.n0", "5-377.n1", "5-377.n2", "5-377.x", 
                              "5-377.y")


#cavk: 5-381, 5-393, 8-840
procedure_cavk <- c("5-381", "5-381.0", "5-381.00", "5-381.01", "5-381.02", "5-381.03", "5-381.04", "5-381.05", "5-381.06", "5-381.0x", "5-381.1", "5-381.11", "5-381.12", 
                    "5-381.13", "5-381.1x", "5-381.2", "5-381.20", "5-381.24", "5-381.28", "5-381.2x", "5-381.3", "5-381.30", "5-381.31", "5-381.32", "5-381.33", 
                    "5-381.35", "5-381.3x", "5-381.4", "5-381.40", "5-381.41", "5-381.42", "5-381.43", "5-381.4x", "5-381.5", "5-381.51", "5-381.52", "5-381.53", 
                    "5-381.54", "5-381.55", "5-381.56", "5-381.5x", "5-381.6", "5-381.60", "5-381.61", "5-381.62", "5-381.63", "5-381.64", "5-381.65", "5-381.66", 
                    "5-381.67", "5-381.6x", "5-381.7", "5-381.70", "5-381.71", "5-381.72", "5-381.73", "5-381.7x", "5-381.8", "5-381.80", "5-381.82", "5-381.83", 
                    "5-381.84", "5-381.87", "5-381.8x", "5-381.x", "5-381.y", 
                    "5-393", "5-393.0", "5-393.00", "5-393.01", "5-393.02", "5-393.03", "5-393.0x", "5-393.1", "5-393.11", "5-393.12", "5-393.13", "5-393.14", "5-393.15", 
                    "5-393.16", "5-393.17", "5-393.18", "5-393.1x", "5-393.2", "5-393.3", "5-393.30", "5-393.31", "5-393.32", "5-393.33", "5-393.35", "5-393.36", 
                    "5-393.38", "5-393.39", "5-393.3a", "5-393.3x", "5-393.4", "5-393.41", "5-393.42", "5-393.43", "5-393.44", "5-393.45", "5-393.46", "5-393.47", 
                    "5-393.48", "5-393.49", "5-393.4x", "5-393.5", "5-393.51", "5-393.52", "5-393.53", "5-393.54", "5-393.55", "5-393.56", "5-393.57", "5-393.5x", 
                    "5-393.6", "5-393.61", "5-393.62", "5-393.63", "5-393.6x", "5-393.7", "5-393.8", "5-393.9", "5-393.x", "5-393.y",
                    "8-840", "8-840.0", "8-840.00", "8-840.02", "8-840.03", "8-840.04", "8-840.05", "8-840.06", "8-840.07", "8-840.08", "8-840.0a", "8-840.0c", "8-840.0d", 
                    "8-840.0e", "8-840.0f", "8-840.0g", "8-840.0h", "8-840.0j", "8-840.0k", "8-840.0m", "8-840.0n", "8-840.0p", "8-840.0q", "8-840.0r", "8-840.0s", 
                    "8-840.0t", "8-840.0x", "8-840.1", "8-840.10", "8-840.12", "8-840.13", "8-840.14", "8-840.15", "8-840.16", "8-840.17", "8-840.18", "8-840.1a", 
                    "8-840.1c", "8-840.1d", "8-840.1e", "8-840.1f", "8-840.1g", "8-840.1h", "8-840.1j", "8-840.1k", "8-840.1m", "8-840.1n", "8-840.1p", "8-840.1q", 
                    "8-840.1r", "8-840.1s", "8-840.1t", "8-840.1x", "8-840.2", "8-840.20", "8-840.22", "8-840.23", "8-840.24", "8-840.25", "8-840.26", "8-840.27", 
                    "8-840.28", "8-840.2a", "8-840.2c", "8-840.2d", "8-840.2e", "8-840.2f", "8-840.2g", "8-840.2h", "8-840.2j", "8-840.2k", "8-840.2m", "8-840.2n", 
                    "8-840.2p", "8-840.2q", "8-840.2r", "8-840.2s", "8-840.2t", "8-840.2x", "8-840.3", "8-840.30", "8-840.32", "8-840.33", "8-840.34", "8-840.35", 
                    "8-840.36", "8-840.37", "8-840.38", "8-840.3a", "8-840.3c", "8-840.3d", "8-840.3e", "8-840.3f", "8-840.3g", "8-840.3h", "8-840.3j", "8-840.3k", 
                    "8-840.3m", "8-840.3n", "8-840.3p", "8-840.3q", "8-840.3r", "8-840.3s", "8-840.3t", "8-840.3x", "8-840.4", "8-840.40", "8-840.42", "8-840.43", 
                    "8-840.44", "8-840.45", "8-840.46", "8-840.47", "8-840.48", "8-840.4a", "8-840.4c", "8-840.4d", "8-840.4e", "8-840.4f", "8-840.4g", "8-840.4h", 
                    "8-840.4j", "8-840.4k", "8-840.4m", "8-840.4n", "8-840.4p", "8-840.4q", "8-840.4r", "8-840.4s", "8-840.4t", "8-840.4x", "8-840.5", "8-840.50", 
                    "8-840.52", "8-840.53", "8-840.54", "8-840.55", "8-840.56", "8-840.57", "8-840.58", "8-840.5a", "8-840.5c", "8-840.5d", "8-840.5e", "8-840.5f", 
                    "8-840.5g", "8-840.5h", "8-840.5j", "8-840.5k", "8-840.5m", "8-840.5n", "8-840.5p", "8-840.5q", "8-840.5r", "8-840.5s", "8-840.5t", "8-840.5x")
#Klappenbehandlung: 5-351, 5-353
procedure_heart_valve <- c("5-351", "5-351.0", "5-351.01", "5-351.02", "5-351.03", "5-351.04", "5-351.05", "5-351.06", "5-351.07", "5-351.08", "5-351.09", "5-351.0a", "5-351.0b", 
                           "5-351.0c", "5-351.0d", "5-351.0e", "5-351.0f", "5-351.0g", "5-351.0h", "5-351.0j", "5-351.0k", "5-351.0m", "5-351.0n", "5-351.0x", "5-351.1", 
                           "5-351.11", "5-351.12", "5-351.13", "5-351.14", "5-351.1f", "5-351.1g", "5-351.1x", "5-351.2", "5-351.21", "5-351.22", "5-351.23", "5-351.24", 
                           "5-351.2f", "5-351.2g", "5-351.2x", "5-351.3", "5-351.31", "5-351.32", "5-351.33", "5-351.34", "5-351.37", "5-351.38", "5-351.39", "5-351.3a", 
                           "5-351.3x", "5-351.4", "5-351.41", "5-351.42", "5-351.43", "5-351.44", "5-351.4x", "5-351.x", "5-351.x1", "5-351.x2", "5-351.x3", "5-351.x4", 
                           "5-351.xx", "5-351.y",
                           "5-353", "5-353.0", "5-353.1", "5-353.2", "5-353.3", "5-353.4", "5-353.5", "5-353.6", "5-353.7", "5-353.x", "5-353.y")

procedures_all <- c(procedure_icd, procedure_crt, procedure_cardiac_device, procedure_cavk, procedure_heart_valve)


#give out statements after certain chunks to document progress
write(paste("Finished Setup at", Sys.time(), "\n"), file = log, append = T)


if (search_for_bundles == TRUE) {
# FHIR Searches & Cracking ------------------------------------------------------------------------------------------------------------------------------------------------------------
# (only for first download, then load saved bundles to save time)

## Patients ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Downloading Patient Bundles.")
#create request for specified URL and Resource
request_patients <- fhir_url(url = diz_url, resource = "Patient")
#Split relevant ids into chunks; only one split needed, left out later
patient_ids_with_conditions_split <- split(patient_ids_with_conditions, ceiling(seq_along(patient_ids_with_conditions) / chunk_size))
patient_ids_with_conditions_list <- lapply(patient_ids_with_conditions_split,  paste , collapse = ",")

#Update birthdate, automatic and individual calculation for each run
inclcusion_birthdate <- today() %m-% years(18)
inclcusion_birthdate <- paste0("lt", format(inclcusion_birthdate, "%Y-%m-%d"))

#Execute the fhir search as loop for each chunk of ids
bundles_patient <-  lapply(patient_ids_with_conditions_list, function(x) {
  #create the search body which lists all the found Patient IDs and restricts on specified parameters (birthdate)
  #use "_id" as global FHIR-Search parameter in patient resource
  
  body_patient <- fhir_body(content = list("_id" = x, "birthdate" = inclcusion_birthdate, "_count" = page_count))
  fhir_search(request = request_patients, 
              body = body_patient, 
              max_bundles = bundle_limit, 
              username = username, 
              password = password, 
              rm_tag = rm_tag, 
              stop_on_error = 0, 
              log_errors = "logs/fhir_search_errors.txt")
})
#rerun the call to fhir_search() in case of timeout
if(length(bundles_patient)==0){
  bundles_patient <-  lapply(patient_ids_with_conditions_list, function(x) {
    #create the search body which lists all the found Patient IDs and restricts on specified parameters (birthdate)
    #use "_id" as global FHIR-Search parameter in patient resource
    #Update birthdate  for 01.01.2024
    body_patient <- fhir_body(content = list("_id" = x, "birthdate" = inclcusion_birthdate, "_count" = page_count))
    fhir_search(request = request_patients, 
                body = body_patient, 
                max_bundles = bundle_limit, 
                username = username, 
                password = password, 
                rm_tag = rm_tag, 
                stop_on_error = 0, 
                log_errors = "logs/fhir_search_errors.txt")
  })
}
#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#give out statements after certain chunks to document progress
write(paste("Finished Search for Patient-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_patient), " Bundles for the Patient-Resource were found \n"), file = log, append = T)

#crack bundles into table
#if(is_fhir_bundle_empty(bundles_patient) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_patient) == TRUE) {
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
#remove bundle once cracked
rm(bundles_patient)



## Condition ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Downloading Condition Bundles.")
#now load all CONDITIONS for relevant patient IDs, to obtain other conditions (comorbidities) of relevant patients
#use "patient" as FHIR-search parameter in Condition resource
request_conditions <- fhir_url(url = diz_url, resource = "Condition")
#split relevant ids for fhir_search into chunks
# patient_ids_with_conditions <- split(patient_ids_with_conditions, ceiling(seq_along(patient_ids_with_conditions) / chunk_size))
# patient_ids_with_conditions_list <- lapply(patient_ids_with_conditions,  paste , collapse = ",")
bundles_condition <- lapply(patient_ids_with_conditions_list, function(x) {
  #"system" = "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
body_conditions <- fhir_body(content = list("patient" = x, "_count" = page_count))
fhir_search(request = request_conditions, 
                                 body = body_conditions, 
                                 max_bundles = bundle_limit, 
                                 username = username, 
                                 password = password, 
                                 rm_tag = rm_tag, 
                                 stop_on_error = 0, 
                                 log_errors = "logs/fhir_search_errors.txt")
})
#rerun the call to fhir_search() in case of timeout
if(length(bundles_condition)==0){
  bundles_condition <- lapply(patient_ids_with_conditions_list, function(x) {
    body_conditions <- fhir_body(content = list("patient" = x, "_count" = page_count))
    fhir_search(request = request_conditions, 
                body = body_conditions, 
                max_bundles = bundle_limit, 
                username = username, 
                password = password, 
                rm_tag = rm_tag, 
                stop_on_error = 0, 
                log_errors = "logs/fhir_search_errors.txt")
  })
}

#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#bundles_condition <- unlist(bundles_condition, recursive = F)
#give out statements after certain chunks to document progress
write(paste("Finished Search for Condition-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_condition), " Bundles for the Condition-Resource were found \n"), file = log, append = T)


#if(is_fhir_bundle_empty(bundles_condition) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_condition) == TRUE) {
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
#bundles nach cracken entfernen
rm(bundles_condition)


## Observation --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Downloading Observation Bundles.")
#use "subject" as FHIR search parameter for Observation resource
request_observations <- fhir_url(url = diz_url, resource = "Observation")
#@KG: LOINC-Codes und ICD Codes müssen vermutlich nicht gesplittet werden

bundles_observation <- lapply(patient_ids_with_conditions_list, function(x) {
body_observation <- fhir_body(content = list("patient" = x, "code" = LOINC_codes_all, "_count" = page_count))
fhir_search(request = request_observations, 
            body = body_observation, 
            max_bundles = bundle_limit, 
            username = username, 
            password = password, 
            rm_tag = rm_tag, 
            stop_on_error = 0, 
            log_errors = "logs/fhir_search_errors.txt")
})
#rerun the call to fhir_search() in case of timeout
if(length(bundles_observation)==0){
  bundles_observation <- lapply(patient_ids_with_conditions_list, function(x) {
    body_observation <- fhir_body(content = list("patient" = x, "code" = LOINC_codes_all, "_count" = page_count))
    fhir_search(request = request_observations, 
                body = body_observation, 
                max_bundles = bundle_limit, 
                username = username, 
                password = password, 
                rm_tag = rm_tag, 
                stop_on_error = 0, 
                log_errors = "logs/fhir_search_errors.txt")
  })
}

#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#bundles_observation <- unlist(bundles_observation, recursive = F)
#give out statements after certain chunks to document progress
write(paste("Finished Search for Observation-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_observation), " Bundles for the Observation-Resource were found \n"), file = log, append = T)

#if(is_fhir_bundle_empty(bundles_observation) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_observation) == TRUE) {
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

rm(bundles_observation)



## medicationAdministration ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Downloading MedicationAdministration Bundles.")
#use "subject" as FHIR-Search parameter in medicationAdministration resource
#1. search for all medicationAdministrations of the patients
request_medicationAdministrations <- fhir_url(url = diz_url, resource = "MedicationAdministration")

bundles_medicationAdministration <- lapply(patient_ids_with_conditions_list, function(x) {
  body_medicationAdministration <- fhir_body(content = list("patient" = x, "_count" = page_count))
  fhir_search(request = request_medicationAdministrations, 
              body = body_medicationAdministration, 
              max_bundles = bundle_limit, 
              username = username, 
              password = password, 
              rm_tag = rm_tag, 
              stop_on_error = 0, 
              log_errors = "logs/fhir_search_errors.txt")
})
 if(length(bundles_medicationAdministration)==0){
  #rerun the call to fhir_search()
   bundles_medicationAdministration <- lapply(patient_ids_with_conditions_list, function(x) {
     body_medicationAdministration <- fhir_body(content = list("patient" = x, "_count" = page_count))
     fhir_search(request = request_medicationAdministrations, 
                 body = body_medicationAdministration, 
                 max_bundles = bundle_limit, 
                 username = username, 
                 password = password, 
                 rm_tag = rm_tag, 
                 stop_on_error = 0, 
                 log_errors = "logs/fhir_search_errors.txt")
   })
 }
#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#bundles_medicationAdministration <- unlist(bundles_medicationAdministration, recursive = F)
#give out statements after certain chunks to document progress
write(paste("Finished Search for MedicationAdministration-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_medicationAdministration), " Bundles for the MedicationAdministration-Resource were found \n"), file = log, append = T)

#crack immediately to provide ids for medication-search
#if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_medicationAdministration) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table will be created instead.")
  #create empty list of medications in the medicationAdministrations of the Patients to fill in next step
  medicationAdministration_medication_ids <- ""
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
  medicationAdministration_medication_ids <- sub("Medication/", "", table_medicationAdministrations$medicationAdministration_medication_reference)
  #also adjust the prefixes in the whole column
  table_medicationAdministrations$medicationAdministration_subject <- sub("Patient/", "", table_medicationAdministrations$medicationAdministration_subject) 
  table_medicationAdministrations$medicationAdministration_medication_reference <- sub("Medication/", "", table_medicationAdministrations$medicationAdministration_medication_reference)
  #give out statements after certain chunks to document progress
  write(paste("Cracked Table for MedicationAdministration-Resource at", Sys.time(), "\n"), file = log, append = T)
  write(paste(nrow(table_medicationAdministrations), " Elements were created for MedicationAdministration \n"), file = log, append = T)
}


# account for multiple medication references per medication administration; code above should list all medication_references in medicationAdministration for each patient
# # Assuming ATC codes are space-separated (adjust if comma-separated)
# med_table_long <- med_table %>%
#   separate_rows(medication_reference, sep = " ")


#2. search for all medications from the identified medication administrations
print("Downloading Medication Bundles.")
## Medication ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
request_medications <- fhir_url(url = diz_url, resource = "Medication")

medicationAdministration_medication_ids <- split(medicationAdministration_medication_ids, ceiling(seq_along(medicationAdministration_medication_ids) / chunk_size))
medicationAdministration_medication_ids_list <- lapply(medicationAdministration_medication_ids,  paste , collapse = ",")

bundles_medication <- lapply(medicationAdministration_medication_ids_list, function(x) {
  body_medication <- fhir_body(content = list("_id" = x, "_count" = page_count))
  fhir_search(request = request_medications, 
              body = body_medication, 
              max_bundles = bundle_limit, 
              username = username, 
              password = password, 
              rm_tag = rm_tag, 
              stop_on_error = 0, 
              log_errors = "logs/fhir_search_errors.txt")
})
#rerun the call to fhir_search()
 if(length(bundles_medication)==0){
   bundles_medication <- lapply(medicationAdministration_medication_ids_list, function(x) {
     body_medication <- fhir_body(content = list("_id" = x, "_count" = page_count))
     fhir_search(request = request_medications, 
                 body = body_medication, 
                 max_bundles = bundle_limit, 
                 username = username, 
                 password = password, 
                 rm_tag = rm_tag, 
                 stop_on_error = 0, 
                 log_errors = "logs/fhir_search_errors.txt")
   })
 }

#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#bundles_medication <- unlist(bundles_medication, recursive = F)
#check data availability and crack bundles to extract medication_ids
#if(is_fhir_bundle_empty(bundles_medicationAdministration) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_medicationAdministration) == TRUE) {
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
#remove medications that do not concern relevant ATC-Codes
#if(is_fhir_bundle_empty(bundles_medication) == FALSE){
if(fhircrackr::fhir_is_empty(bundles_medication) == TRUE) {
  #give out number of entries that will be removed
  write(paste(sum(!(table_medications$medication_code %in% medications_all)), " Medications were removed (irrelevant ATC-Code). \n"), file = log, append = T)
  #remove entries
  table_medications <- table_medications[table_medications$medication_code %in% medications_all,]
}


#Log Documentation
write(paste("Finished Search for Medication-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_medication), " Bundles for the Medication-Resource were found \n"), file = log, append = T)



#3. combine tables and retain the medicationAdministrations with the relevant medications (removed if-clause, as table will exist in any case (might be empty though))
#merge medication information with data in medicationAdministration
table_meds <- merge(table_medicationAdministrations, table_medications, by.x = "medicationAdministration_medication_reference", by.y = "medication_identifier", all.x = TRUE)
#check if any patients were found, stop analysis if no patient are in bundles
if(length(table_patients$patient_identifier)==0){
  write("Es konnten keine Patienten mit den angegebene ICD-10 Codes auf dem Server gefunden werden. Abfrage abgebrochen.", file ="errors/error_message.txt")
  #stop("No Patients found - aborting.")
}
#give out statements after certain chunks to document progress
write(paste("Bundles were cracked into tables at", Sys.time(), "\n"), file = log, append = T)

#bundles no longer required
rm(bundles_medication)
#remove bundles after cracking
rm(bundles_medicationAdministration)


#close if search_bundles == TRUE
}

## Procedures ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
print("Downloading Procedure Bundles.")
#use "subject" as FHIR search parameter for procedure resource
request_procedures <- fhir_url(url = diz_url, resource = "Procedure")
#@KG: LOINC-Codes und ICD Codes müssen vermutlich nicht gesplittet werden

bundles_procedure <- lapply(patient_ids_with_conditions_list, function(x) {
  body_procedure <- fhir_body(content = list("patient" = x, "code" = procedures_all, "_count" = page_count))
  fhir_search(request = request_procedures, 
              body = body_procedure, 
              max_bundles = bundle_limit, 
              username = username, 
              password = password, 
              rm_tag = rm_tag, 
              stop_on_error = 0, 
              log_errors = "logs/fhir_search_errors.txt")
})
#rerun the call to fhir_search() in case of timeout
if(length(bundles_procedure)==0){
  bundles_procedure <- lapply(patient_ids_with_conditions_list, function(x) {
    body_procedure <- fhir_body(content = list("patient" = x, "code" = procedures_all, "_count" = page_count))
    fhir_search(request = request_procedures, 
                body = body_procedure, 
                max_bundles = bundle_limit, 
                username = username, 
                password = password, 
                rm_tag = rm_tag, 
                stop_on_error = 0, 
                log_errors = "logs/fhir_search_errors.txt")
  })
}

#unpack nested fhir_bundle_lists, no longer required, as fhir_crack handles this internally
#bundles_procedure <- unlist(bundles_procedure, recursive = F)
#give out statements after certain chunks to document progress
write(paste("Finished Search for Procedure-Resource at", Sys.time(), "\n"), file = log, append = T)
write(paste(length(bundles_procedure), " Bundles for the Procedure-Resource were found \n"), file = log, append = T)

#if(is_fhir_bundle_empty(bundles_procedure) == TRUE) {
if(fhircrackr::fhir_is_empty(bundles_procedure) == TRUE) {
  message("The bundle you are trying to crack is empty. This will result in an error. Therefore the bundle will not be cracked. An empty table has been created.")
  table_procedures <- data.frame(procedure_identifier         = character(length(unique(patient_ids_with_conditions))),
                                   procedure_subject          = unique(patient_ids_with_conditions),
                                   procedure_code             = character(length(unique(patient_ids_with_conditions))),
                                   procedure_perfomedDateTime = as.Date(length(unique(patient_ids_with_conditions))))
} else {
  message("Cracking ", length(bundles_procedure), " procedure Bundles.\n")
  table_procedures <- fhir_crack(bundles = bundles_procedure, design = tabledescription_procedure, verbose = 1)
  write(paste(nrow(table_procedures), " Elements were created for procedures \n"), file = log, append = T)
}

rm(bundles_procedure)





# #Save Bundles (obsolete) -------------------------------------------------------------------------------------------------------------------------------------------------------------
# 
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
# } alte Klammern für search_for_bundles
} else {
#Load Bundles (obsolete) ---------------------------------------------------------------------------------------------------------------------------------------------------------------
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



# Data Cleaning ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
message("Cleaning the Data.\n")

#convert birthday to birthyear and calculate age
#fhircracking-process makes all variables into character-variables, year should always be given first (according to Implementation Guide/FHIR), first four characters can be extracted for birthyear
if(all(is.na(table_patients$patient_identifier) | table_patients$patient_identifier == "")) {
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

if(all(is.na(table_observations$observation_identifier) | table_observations$observation_identifier == "")) {
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
# :::-Suffix entfernen falls vorhanden (siehe Bonn)
table_observations$observation_code <- sub(":::*", "", table_observations$observation_code)
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



# Additional Variables  ----------------------------------------------------------------------------------------------------------------------------------------------
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
  
#add LOINC-Code Groups to observations
table_observations <- table_observations %>%
  left_join(loinc_groups, by = "observation_code")



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
chadsvasc_exclusion_ops_codes <- c("5-350", "5-351", "5-352", "5-353", "5-354", "5-355", "5-356", "5-357", "5-358", "5-359", "5-35a", "5-35b")

## SMART
# 40 < age < 80, CHD, CeVD, I06-I09, I20-I25, I70, I71, I73-I79
smart_inclusion_icd_codes <- data.frame(icd_code = c("I06", "I07", "I08", "I09", "I20", "I21", "I22", "I23", "I24", "I25", "I70", "I71", "I73", "I74", "I77", "I78", "I79"))
smart_inclusion_icd_codes <- icd_expand(smart_inclusion_icd_codes, col_icd = "icd_code", year = 2023)

## MAGGIC
# age > 18 years, chronic HF (I50)
maggic_inclusion_icd_codes <- data.frame(icd_code = c("I50"))
maggic_inclusion_icd_codes <- icd_expand(maggic_inclusion_icd_codes, col_icd = "icd_code", year = 2023)

## BCN Bio HF
bcn_inclusion_icd_codes <- data.frame(icd_code = c("I50"))
bcn_inclusion_icd_codes <- icd_expand(bcn_inclusion_icd_codes, col_icd = "icd_code", year = 2023)


#indicate which score applies by way of indication: CVD = I20 - I25, AF = I48, HF = I50
table_conditions$condition_cvd <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% smart_inclusion_icd_codes$icd_normcode, 1, 0)
table_conditions$condition_af <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% chadsvasc_inclusion_icd_codes$icd_normcode, 1, 0)
#MAGGIC inclusion codes are same as BCN
table_conditions$condition_hf <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% maggic_inclusion_icd_codes$icd_normcode, 1, 0)


## Eligibility Assessment ---------------------------------------------------------------------------------------------------------------------------------------------------------------
#create additional columns in original resources tables, later extract relevant columns for eligibility/can_calc if necessary
#NAs result in "Ineligibility" and "cannot calc"
#eligibility column in each resource table for each score per patient

#CHADSVASC
#18years or older
table_patients$eligible_patient_age_chadsvasc <- ifelse(!is.na(table_patients$patient_age) & table_patients$patient_age >= 18, 1, 0)
#specified conditions of atrial fibrillation
table_conditions$eligible_conditions_AF_chadsvasc <- ifelse(!is.na(table_conditions$condition_code) & (table_conditions$condition_code %in% chadsvasc_inclusion_icd_codes$icd_normcode & (table_conditions$condition_time_since_first_cvd < 366)), 1, 0)
#potentially insert heart valve surgery if module for procedures is available
table_conditions$eligible_conditions_AF_chadsvasc <- ifelse(table_conditions$condition_code %in% chadsvasc_exclusion_icd_codes$icd_normcode, 0, table_conditions$eligible_conditions_AF_chadsvasc)
#no eligibility criteria regarding observations for chadsvasc
table_observations$eligible_observations_chadsvasc <- 1
#no eligibility criteria regarding observations for chadsvasc
table_meds$eligible_meds_chadsvasc <- if(all(is.na(table_medicationAdministrations$medicationAdministration_identifier) | table_medicationAdministrations$medicationAdministration_identifier == "")) {
  table_meds$eligible_meds_chadsvasc <- 99
} else {
  table_meds$eligible_meds_chadsvasc <- 1
}
table_procedure$eligible_procedure_chadsvasc <- ifelse(!is.na(table_procedure$procedure_identifier) & table_procedure$procedure_code %in% chadsvasc_exclusion_ops_codes, 0, 1)

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
if(all(is.na(table_medicationAdministrations$medicationAdministration_identifier) | table_medicationAdministrations$medicationAdministration_identifier == "")) {
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
if(all(is.na(table_medicationAdministrations$medicationAdministration_identifier) | table_medicationAdministrations$medicationAdministration_identifier == "")) {
  table_meds$eligible_meds_maggic <- 99
} else {
  table_meds$eligible_meds_maggic <- 1
}


#BCN-Bio
#no restrictions regarding age or sex; Overall criteria require at least 18 years of age
table_patients$eligible_patient_bcn <- ifelse(!is.na(table_patients$patient_age) & table_patients$patient_age >= 18, 1, 0)
#specified condition of heart failure
table_conditions$eligible_conditions_HF_bcn <- ifelse(!is.na(table_conditions$condition_code) & table_conditions$condition_code %in% bcn_inclusion_icd_codes$icd_normcode, 1, 0)
#no eligibility criteria regarding observations for bcn-bio
table_observations$eligible_observations_bcn <- 1
#no eligibility criteria regarding medications for bcn-bio
if(all(is.na(table_medicationAdministrations$medicationAdministration_identifier) | table_medicationAdministrations$medicationAdministration_identifier == "")) {
  table_meds$eligible_meds_bcn <- 99
} else {
  table_meds$eligible_meds_bcn <- 1
}


## Can Calc Assessment -------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Patient Tables ----
#sex and age are required from patient table
table_patients$can_calc_patient_chadsvasc <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)
table_patients$can_calc_patient_smart <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)
table_patients$can_calc_patient_maggic <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)
table_patients$can_calc_patient_bcn <- ifelse(!is.na(table_patients$patient_age) & !is.na(table_patients$patient_gender), 1, 0)

### Observation Tables ----
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
    can_calc_observation_bloodpressure_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_bp_sys, 1, 0),
    can_calc_observation_lvef_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_lvef, 1, 0),
    can_calc_observation_creatinine_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_creatinine, 1, 0),
    can_calc_observation_bmi_maggic = ifelse((!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_bmi) | (!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_height & table_observations$observation_code %in% LOINC_codes_weight), 1, 0),
    #if no info for smoking is available, no smoking assumed
    can_calc_observation_smoking_maggic = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_smoking, 1, 99),
    can_calc_observation_maggic = ifelse(!is.na(table_observations$observation_code) & (any(table_observations$observation_code %in% LOINC_codes_bp_sys & table_observations$observation_code %in% LOINC_codes_lvef & table_observations$observation_code %in% LOINC_codes_creatinine & table_observations$observation_code %in% LOINC_codes_bmi)), 1, 0))

table_observations <- table_observations %>%
  mutate(
    can_calc_observation_natrium_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_natrium, 1, 0),
    can_calc_observation_egfr_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_egfr, 1, 0),
    can_calc_observation_haemoglobin_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_haemoglobin, 1, 0),
    can_calc_observation_lvef_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_lvef, 1, 0),
    can_calc_observation_hsctnt_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_hsctnt, 1, 0),
    can_calc_observation_st2_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_st2, 1, 0),
    can_calc_observation_ntprobnp_bcn = ifelse(!is.na(table_observations$observation_code) & table_observations$observation_code %in% LOINC_codes_ntprobnp, 1, 0))
    
   
### Condition Tables ----
#conditions and medications are presumed to be not present in the patient if not available in data, therefore give value of 1
#accommodate empty conditions table
if(all(is.na(table_conditions$condition_identifier) | table_conditions$condition_identifier == "")) {
  table_conditions$can_calc_conditions_chadsvasc <- 99
  table_conditions$can_calc_conditions_smart <- 99
  table_conditions$can_calc_conditions_maggic <- 99
  table_conditions$can_calc_conditions_bcn <- 99
} else {
  table_conditions$can_calc_conditions_chadsvasc <- 1
  table_conditions$can_calc_conditions_smart <- 1
  table_conditions$can_calc_conditions_maggic <- 1
  table_conditions$can_calc_conditions_bcn <- 1
}

### MedicationAdministration Tables ----
#accommodate empty medication tables
if(all(is.na(table_medicationAdministrations$medicationAdministration_identifier) | table_medicationAdministrations$medicationAdministration_identifier == "")) {
  table_meds$can_calc_meds_chadsvasc <- 99
  table_meds$can_calc_meds_smart <- 99
  table_meds$can_calc_meds_maggic <- 99
  table_meds$can_calc_meds_bcn <- 99
} else {
  table_meds$can_calc_meds_chadsvasc <- 1
  table_meds$can_calc_meds_smart <- 1
  table_meds$can_calc_meds_maggic <- 1
  table_meds$can_calc_meds_bcn <- 1
}


## Score Computation ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#index most recent observation per LOINC Code per patient (no preference among loinc codes); when all are NA, keep NA/exclude observation
#choose this condition when aggregatin observations
table_observations <- table_observations %>%
  group_by(observation_subject, observation_code) %>%
  mutate(
    max_date = ifelse(all(is.na(observation_datetime))), NA, max(observation_datetime, na.rm = TRUE),
    index_most_recent_observation = case_when(
      all(is.na(observation_datetime)) & n() == 1 ~ 1,           # single observation with missing date
      all(is.na(observation_datetime)) & n() > 1 ~ 99,           # multiple observations, all missing
      !is.na(observation_datetime) & observation_datetime == max(observation_datetime, na.rm = TRUE) ~ 1,  # most recent valid date
      TRUE ~ 0                                   # all other rows
    )
  ) %>%
  ungroup()


#### Observation Units ----

#table_observations$observation_value_num

##Mapping for different units
loinc_unit_map <- tribble(
  ~loinc_code,   ~observation_type, ~from_unit, ~to_unit, ~multiplier, ~offset,
  # Height (fill your target unit, e.g., "cm")
  "8302-2",      "height",          "cm",         "m",       0.01,         0,
  "3137-7",      "height",          "cm",         "m",       0.01,         0,
  "8301-4",      "height",          "cm",         "m",       0.01,         0,
  "8306-3",      "height",          "cm",         "m",       0.01,         0,
  "91370-7",     "height",          "cm",         "m",       0.01,         0,
  # Weight (fill target unit, e.g., "kg"), should always be kg
  # "29463-7",     "weight",          NA,         NA,       NA,         0,
  # "3141-9",      "weight",          NA,         NA,       NA,         0,
  # "3142-7",      "weight",          NA,         NA,       NA,         0,
  # "8335-2",      "weight",          NA,         NA,       NA,         0,
  # "75292-3",     "weight",          NA,         NA,       NA,         0,
  # "79348-9",     "weight",          NA,         NA,       NA,         0,
  # "8350-1",      "weight",          NA,         NA,       NA,         0,
  # BMI, should always be kg/m²
  # "39156-5",     "bmi",             NA,         NA,       NA,         0,
  # "89270-3",     "bmi",             NA,         NA,       NA,         0,
  
  # Creatinine
  #mikromol?
  "14682-9",     "creatinine",      "µmol/L",   "µmol/l",       1,         0,
  "2160-0",      "creatinine",      "mg/dL",    "µmol/l",       88.4,      0,
  "38483-4",     "creatinine",      "mg/dL",    "µmol/l",       88.4,      0,
  "77140-2",     "creatinine",      "µmol/L",   "µmol/l",       1,         0,
  
  # eGFR, should always be the same
  # "69405-9",     "egfr",            NA,         NA,       NA,         0,
  # "62238-1",     "egfr",            NA,         NA,       NA,         0,
  # "98979-8",     "egfr",            NA,         NA,       NA,         0,
  # "50210-4",     "egfr",            NA,         NA,       NA,         0,
  # "98980-6",     "egfr",            NA,         NA,       NA,         0,
  
  # Cholesterol overall
  "14647-2",     "cholesterol_total", "mmol/L",  "mmol/L",       1,         0,
  "2093-3",      "cholesterol_total", "mg/dL",   "mmol/L",       0.02586,   0,
  
  # Cholesterol HDL
  "14646-4",     "cholesterol_hdl",  "mmol/L",   "mmol/L",       1,         0,
  "2085-9",      "cholesterol_hdl",  "mg/dL",    "mmol/L",       0.02586,   0,
  "49130-8",     "cholesterol_hdl",  "mg/dL",    "mmol/L",       0.02586,   0,
  "18263-4",     "cholesterol_hdl",  "mg/dL",    "mmol/L",       0.02586,   0,
  
  # hsCRP
  "71426-1",     "hscrp",           "mg/L",     "mg/dL",       0.1,       0,
  "30522-7",     "hscrp",           "mg/L",     "mg/dL",       0.1,       0,
  "30522-7",     "hscrp",           "mg/dL",    "mg/dL",       1,         0,
  "76486-0",     "hscrp",           "nmol/L",   "mg/dL",       0.0001,    0,
  
  # CRP
  "1988-5",      "crp",             "mg/dL",    "mg/dL",       1,         0,
  "1988-5",      "crp",             "mg/L",     "mg/dL",       0.1,       0,
  "76485-2",     "crp",             "nmol/L",   "mg/dL",       0.0001,    0,
  "48421-2",     "crp",             "mg/L",     "mg/dL",       0.1,       0,
  
  # Sodium / Natrium
  "2951-2",      "natrium",         "mmol/L",   "mmol/L",       1,         0,
  "2947-0",      "natrium",         "mmol/L",   "mmol/L",       1,         0,
  "77139-4",     "natrium",         "mmol/L",   "mmol/L",       1,         0,
  
  # Hemoglobin
  "30350-3",     "haemoglobin",     "g/dL",       "g/dL",       1,         0,
  "30350-3",     "haemoglobin",     "g/L",        "g/dL",       0.1,       0,
  "718-7",       "haemoglobin",     "g/dL",       "g/dL",       1,         0,
  "718-7",       "haemoglobin",     "g/L",        "g/dL",       0.1,       0,
  "30313-1",     "haemoglobin",     "g/dL",       "g/dL",       1,         0,
  "30313-1",     "haemoglobin",     "g/L",        "g/dL",       0.1,       0,
  
  # ST2
  "90239-5",     "st2",             "ng/mL",         "ng/mL",       1,         0,
  
  # NT-proBNP
  "33762-6",     "ntprobnp",        "pg/mL",         "pg/mL",       1,         0,
  "83107-3",     "ntprobnp",        "pg/mL",         "pg/mL",       1,         0,
  "33763-4",     "ntprobnp",        "pmol/L",        "pg/mL",       1,         0, #approximate
  "42637-9",     "ntprobnp",        "pg/mL",         "pg/mL",       1,         0,
  
  # hs-cTnT
  "67151-1",     "hsctnt",          "ng/L",         "ng/L",          1,         0,
  "6598-7",      "hsctnt",          "ng/L",         "ng/L",          1,         0,
  "6598-7",      "hsctnt",          "µg/L",         "ng/L",       1000,         0,
  #"89576-3",     "hsctnt",          NA,            "ng/L",       NA,         0,     information not available
  #"89577-1",     "hsctnt",          NA,            "ng/L",       NA,         0,     information not available
  "89579-7",     "hsctnt",          "ng/L",         "ng/L",          1,         0
)
  
#value transformation
table_observations <- table_observations %>%
  left_join(loinc_unit_map,
            by = c("observation_code" = "loinc_code",
                   "observation_unit" = "from_unit")) %>%
  mutate(
    observation_value_num_std = observation_value_num * multiplier + offset,
    observation_unit_std  = to_unit
  )



#### Co-Morbidities ----
icd10_codes_diabetes <- data.frame(icd_code = c("E10", "E11", "E12", "E13", "E14"))
icd10_codes_diabetes <- icd_expand(icd10_codes_diabetes, col_icd = "icd_code", year = 2023)

icd10_codes_nyha <- data.frame(icd_code = c("I50.02", "I50.03", "I50.04", "I50.05", "I50.11", "I50.12", "I50.13", "I50.14"))

#cvd (smart) transient ischemic attack, cerebral infarction, amaurosis fugax or retinal infarction, or a history of carotid surgery -> I63, H34, G45
icd10_codes_cvd <- data.frame(icd_code = c("I63", "H34", "G45"))
icd10_codes_cvd <- icd_expand(icd10_codes_cvd, col_icd = "icd_code", year = 2023)
#cad (smart) was defined as angina pectoris, myocardial infarction (MI) or coronary revascularisation (coronary bypass surgery or coronary angioplasty)
icd10_codes_cad <- data.frame(icd_code = c("I20", "I21", "I22", "I24", "I25"))
icd10_codes_cad <- icd_expand(icd10_codes_cad, col_icd = "icd_code", year = 2023)
#pad (smart) obstruction of distal arteries of the leg or surgery of the leg (percutaneous transluminal angioplasty, bypass or amputation) -> I74
icd10_codes_pad <- data.frame(icd_code = c("I74"))
icd10_codes_pad <- icd_expand(icd10_codes_pad, col_icd = "icd_code", year = 2023)
#aaa (smart) supra- or infrarenal aneurysm of the aorta (distal aortic anteroposterior diameter ≥3 cm, measured with ultrasonography) or a history of AAA surgery
icd10_codes_aaa <- data.frame(icd_code = c("I71"))
icd10_codes_aaa <- icd_expand(icd10_codes_aaa, col_icd = "icd_code", year = 2023)

# vascular disease (chadsvasc) myocardial infarction, peripheral artery disease, and complex aortic plaque
icd10_codes_vasculardisease <- data.frame(icd_code = c("I21", "I74", "I70.0"))
icd10_codes_vasculardisease <- icd_expand(vasculardisease_icd_codes, col_icd = "icd_code", year = 2023)
#hypetension (chadsvasc)
icd10_codes_hypertension <- data.frame(icd_code = c("I10"))
icd10_codes_hypertension <- icd_expand(hypertension_icd_codes, col_icd = "icd_code", year = 2023)
#heart failure (chadsvasc)
icd10_codes_heartfailure <- data.frame(icd_code = c("I50"))
icd10_codes_heartfailure <- icd_expand(heartfailure_icd_codes, col_icd = "icd_code", year = 2023)

#chornic lung disease (COPD)
icd10_codes_copd <- data.frame(icd_code = c("J44"))
icd10_codes_copd <- icd_expand(copd_icd_codes, col_icd = "icd_code", year = 2023)

#create nyha class variable and index highest / most recent?
##highest NYHA
# table_conditions <- table_conditions %>%
# mutate(
#   nyha_class = case_when(
#     condition_code %in% c("I50.02", "50.02!", "I50.11") ~ 1,
#     condition_code %in% c("I50.03", "50.03!", "I50.12") ~ 2,
#     condition_code %in% c("I50.04", "50.04!", "I50.13") ~ 3,
#     condition_code %in% c("I50.05", "50.05!", "I50.14") ~ 4,
#     TRUE ~ NA_real_
#   )
# ) %>%
#   group_by(condition_subject) %>%
#   mutate(
#     index_highest_nyha = ifelse(nyha_class == max(nyha_class, na.rm = TRUE), 1, 0)
#   ) %>%
#   ungroup()

##most recent NYHA
table_conditions <- table_conditions %>%
  mutate(
    nyha_class = case_when(
      condition_code %in% c("I50.02", "50.02!", "I50.11") ~ 1,
      condition_code %in% c("I50.03", "50.03!", "I50.12") ~ 2,
      condition_code %in% c("I50.04", "50.04!", "I50.13") ~ 3,
      condition_code %in% c("I50.05", "50.05!", "I50.14") ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(condition_subject) %>%
  mutate(
    index_most_recent_nyha =
      ifelse(!is.na(nyha_class) & condition_recordedDate == max(condition_recordedDate[!is.na(nyha_class)], na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()





### Preparation SMART -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#columns necessary for calculation
## SMART: age, sex, diabetes, smoking, systolic blood pressure, total cholesterol, hdl cholesterol, hs CRP, eGFR, time since first cvd event, history CVD, history CAD, history PAD, history AAA, antithrombotic treatment
## use TRUE and FALSE for Function input
table_patients$compute_age_smart <- table_patients$patient_age
table_patients$compute_sex_smart <- table_patients$patient_gender
table_patients$compute_sex_male_smart <- ifelse(table_patients$patient_gender=="male", TRUE, FALSE)
table_conditions$compute_diabetes_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_diabetes, TRUE, FALSE)
#table_conditions$compute_timesincefirstcvd_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_patient_conditions, as.POSIXct.Date(Sys.Date()) - table_conditions$condition_recordedDate, NA)
table_conditions$compute_timesincefirstcvd_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_patient_conditions$icd_normcode, time_length(interval(as.Date(table_conditions$condition_recordedDate), Sys.Date()),"years"), NA_real_)

table_conditions$compute_cvdhistory_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_cvd, TRUE, FALSE)
table_conditions$compute_cadhistory_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_cad, TRUE, FALSE)
table_conditions$compute_padhistory_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_pad, TRUE, FALSE)
table_conditions$compute_aaahistory_smart <- ifelse(table_conditions$condition_code %in% icd10_codes_aaa, TRUE, FALSE)

table_observations$compute_smoking_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_smoking & index_most_recent_observation == 1, TRUE, FALSE)
table_observations$compute_sysbp_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_bp_sys & index_most_recent_observation == 1, table_observations$observation_value, NA)
#`HDL-cholesterol in mmol/L`; Umrechnugn einbauen
table_observations$compute_cholhdl_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_cholesterol_hdl & index_most_recent_observation == 1, table_observations$observation_value, NA)
#`Total cholesterol in mmol/L`; Umrechnugn einbauen
table_observations$compute_totalchol_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_cholesterol_overall & index_most_recent_observation == 1, table_observations$observation_value, NA)
#`hs-CRP in mg/dL`
table_observations$compute_hscrp_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_hscrp & index_most_recent_observation == 1, table_observations$observation_value, NA)
#`eGFR in mL/min/1.73m²`; Umrechnung vermutlich nicht nötig
table_observations$compute_egfr_smart <- ifelse(table_observations$observation_code %in% LOINC_codes_egfr & index_most_recent_observation == 1, table_observations$observation_value, NA)

table_meds$compute_antithrombotictreatment_smart <- ifelse(table_meds$medication_code %in% medications_antithrombotic, TRUE, FALSE)

### Preparation ChadsVasc -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## chadsvasc: age (<65, 65-74, 75+) up to 2 points, sex, diabetes, vascular disease, hypertension, congestive heart failure, Strok/TIA/thromboembolism up to 2 points
table_patients$compute_age_chadsvasc <- case_when(
                                              table_patients$patient_age < 65 ~ 0,
                                              table_patients$patient_age >= 65 & table_patients$patient_age < 75 ~ 1,
                                              table_patients$patient_age >= 75 ~ 2
                                              TRUE ~ NA_real_)
#male = 0, female = 1; in data 1=female, 2=male
table_patients$compute_sex_chadsvasc <- case_when(
                                              table_patients$patient_gender == 1 ~ 1,
                                              table_patients$patient_gender == 2 ~ 0,
                                              TRUE ~ NA_real_)
table_conditions$compute_diabetes_chadsvasc <- ifelse(table_conditions$condition_code %in% icd10_codes_diabetes, 1, 0)
table_conditions$compute_vasculardisease_chadsvasc <- ifelse(table_conditions$condition_code %in% vasculardisease_icd_codes, 1, 0)
table_conditions$compute_hypertension_chadsvasc <- ifelse(table_conditions$condition_code %in% hypertension_icd_codes, 1, 0)
table_conditions$compute_heartfailure_chadsvasc <- ifelse(table_conditions$condition_code %in% heartfailure_icd_codes, 1, 0)
table_conditions$compute_cvd_chadsvasc <- ifelse(table_conditions$condition_code %in% icd10_codes_cvd, 2, 0)

### Preparation MAGGIC ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## MAGGIC: age (per 10 years), sex, bmi, systolic BP, NYHA, smoking, diabetes, first cvd more than 18 months, chronic lung disease, betablockers, ACEi/ARB, LVEF, Creatinine, status (inpatient)
table_patients$compute_age_maggic <- table_patients$patient_age
table_patients$compute_sex_maggic <- table_patients$patient_gender
table_patients$compute_sex_male_maggic <- ifelse(table_patients$patient_gender=="male", TRUE, FALSE)
table_observations$compute_bmi_maggic <- ifelse(table_observations$observation_code %in% LOINC_codes_bmi & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_sysbp_maggic <- ifelse(table_observations$observation_code %in% LOINC_codes_bp_sys & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_smoking_maggic <- ifelse(table_observations$observation_code %in% LOINC_codes_smoking & index_most_recent_observation == 1, TRUE, FALSE)
table_observations$compute_lvef_maggic <- ifelse(table_observations$observation_code %in% LOINC_codes_lvef & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_creatinine_maggic <- ifelse(table_observations$observation_code %in% LOINC_codes_creatinine & index_most_recent_observation == 1, table_observations$observation_value, NA)

#latest NYHA (optional highest NYHA index_highest_nyha)
table_conditions$compute_nyha_maggic <- ifelse(table_conditions$condition_code %in% icd10_codes_nyha & index_most_recent_nyha == 1, table_conditions$condition_code, NA)
table_conditions$compute_diabetes_maggic <- ifelse(table_conditions$condition_code %in% icd10_codes_diabetes, TRUE, FALSE)
table_conditions$compute_copd_maggic <- ifelse(table_conditions$condition_code %in% icd10_codes_copd, TRUE, FALSE)
#hf more than 18 months ago, difftime gives days; 18*30 = 540 days since first diagnosis
table_conditions$compute_hf18months_maggic <- ifelse(table_conditions$condition_code %in% icd10_codes_heartfailure & table_conditions$time_since_first_diagnosis_using_recordeddate > 540, TRUE, FALSE)

table_meds$compute_betablockers_maggic <- ifelse(table_meds$medication_code %in% medications_betablockers, 1, 0)
table_meds$compute_acei_arb_maggic <- ifelse(table_meds$medication_code %in% medications_acei_arb, 1, 0)
#additional variables for score calculation; adjust assignment for negative case
table_meds$compute_no_betablockers_maggic <- ifelse(table_meds$medication_code %in% medications_betablockers, FALSE, TRUE)
table_meds$compute_no_acei_arb_maggic <- ifelse(table_meds$medication_code %in% medications_acei_arb, FALSE, TRUE)

### Preparation BCN --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## BCN Bio: age, sex, nyha, NA, eGFR, Hb, LVEF (v3), HF duration (v3), diabetes (v3), No. hospitalisations (v3), loop diuretics, statins, betablockers, ACEi/ARB, MRA (v3), ARNi (v3), iSGLT2 (v3), CRT (v3), ICD (v3), hs-cTnT, ST2, NT-proBNP
table_patients$compute_age_bcn <- table_patients$patient_age
table_patients$compute_sex_bcn <- table_patients$patient_gender
table_patients$compute_sex_female_bcn <- ifelse(table_patients$patient_gender=="female", TRUE, FALSE)
#latest NYHA (optional highest NYHA index_highest_nyha)
table_conditions$compute_nyha_bcn <- ifelse(table_conditions$condition_code %in% icd10_codes_nyha & index_most_recent_nyha == 1, table_conditions$condition_code, NA)
#choose first date that is available
table_conditions$compute_hfduration_days_bcn_v3 <- ifelse(table_conditions$condition_code %in% icd10_codes_heartfailure, difftime(Sys.time(), coalesce(table_conditions$condition_onsetDate, table_conditions$condition_recordedDate)), NA) 
table_conditions$compute_hfduration_months_bcn_v3 <- ifelse(table_conditions$condition_code %in% icd10_codes_heartfailure, difftime(Sys.time(), coalesce(table_conditions$condition_onsetDate, table_conditions$condition_recordedDate))/30, NA)
table_conditions$compute_diabetes_bcn_v3 <- ifelse(table_conditions$condition_code %in% icd10_codes_diabetes, 1, 0)

table_observations$compute_natrium_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_natrium & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_egfr_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_egfr & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_haemoglobin_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_haemoglobin & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_lvef_bcn_v3 <- ifelse(table_observations$observation_code %in% LOINC_codes_lvef & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_hsctnt_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_hsctnt & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_st2_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_st2 & index_most_recent_observation == 1, table_observations$observation_value, NA)
table_observations$compute_ntprobnp_bcn <- ifelse(table_observations$observation_code %in% LOINC_codes_ntprobnp & index_most_recent_observation == 1, table_observations$observation_value, NA)
#@KG dose of loop diuretics, not in UCC formula?
table_meds$compute_loopdiuretics_torasemide_bcn <- ifelse(table_meds$medication_code %in% medications_torasemid, table_meds$medication_strength, 0)
table_meds$compute_loopdiuretics_furosemide_bcn <- ifelse(table_meds$medication_code %in% medications_furosemid, table_meds$medication_strength, 0)
table_meds$compute_statins_bcn <- ifelse(table_meds$medication_code %in% medications_statins, 1, 0)
table_meds$compute_betablockers_bcn <- ifelse(table_meds$medication_code %in% medications_betablockers, 1, 0)
table_meds$compute_acei_arb_bcn <- ifelse(table_meds$medication_code %in% medications_acei_arb, 1, 0)
table_meds$compute_mra_bcn <- ifelse(table_meds$medication_code %in% medications_mra, 1, 0)
table_meds$compute_arni_bcn <- ifelse(table_meds$medication_code %in% medications_arni, 1, 0)
table_meds$compute_isglt2_bcn <- ifelse(table_meds$medication_code %in% medications_sglt2i, 1, 0)
#number of hospitalisations @KG: currently only encounter from one year, would need to go back at least 5-10 years for this variable to be meaningful
#table_encounter$encounters_last_year
table_encounter <- table_encounter %>%
  # Ensure date type
  mutate(encounter_periodStart = as.Date(encounter_periodStart)) %>%
  # Compute last encounter date per subject
  group_by(encounter_subject) %>%
  mutate(last_encounter_date = max(encounter_periodStart, na.rm = TRUE)) %>%
  # Flag encounters in the previous year
  mutate(
    in_last_year = encounter_periodStart >= last_encounter_date - years(1) &
      encounter_periodStart <= last_encounter_date
  ) %>%
  # Count encounters in that window per subject
  mutate(
    encounters_last_year = sum(in_last_year, na.rm = TRUE)
  ) #%>%
  # Optional cleanup
  # ungroup() %>%
  # select(-in_last_year)
  
#crt & icd seperately
table_procedures$compute_crt_bcn_v3 <- ifelse(table_procedure$procedure_code %in% procedure_crt, 1, 0)
table_procedures$compute_icd_bcn_v3 <- ifelse(table_procedure$procedure_code %in% procedure_icd, 1, 0)


## Merge Resource Tables -------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Check data quality ----

quality_assurance_resource_tables <- function(tables, exclude_cols = NULL) {
  
  if (inherits(tables, "data.frame")) {
    tables <- list(single_table = tables)
  }
  
  purrr::map(tables, function(df) {
    
    df <- df %>% dplyr::select(-dplyr::any_of(exclude_cols))
    
    purrr::map_dfr(names(df), function(col) {
      
      x <- df[[col]]
      
      # Binary numeric (0/1)
      if (is.numeric(x) && all(na.omit(unique(x)) %in% c(0,1))) {
        tibble::tibble(
          column = col,
          type   = "binary_numeric",
          N      = sum(!is.na(x)),
          NA_n   = sum(is.na(x)),
          n_0    = sum(x == 0, na.rm = TRUE),
          n_1    = sum(x == 1, na.rm = TRUE)
        )
        
        # Continuous numeric
      } else if (is.numeric(x)) {
        tibble::tibble(
          column = col,
          type   = "numeric",
          N      = sum(!is.na(x)),
          NA_n   = sum(is.na(x)),
          min    = suppressWarnings(min(x, na.rm = TRUE)),
          max    = suppressWarnings(max(x, na.rm = TRUE))
        )
        
        # Categorical
      } else {
        tibble::tibble(
          column = col,
          type   = "categorical",
          N      = sum(!is.na(x)),
          NA_n   = sum(is.na(x)),
          distribution = list(table(x, useNA = "ifany"))
        )
      }
    })
    
  })
}

data_quality_patients <- quality_assurance_resource_tables(table_patients, exclude_cols = "patient_identifier")
data_quality_conditions <- quality_assurance_resource_tables(table_conditions, exclude_cols = "patient_identifier")
data_quality_observations <- quality_assurance_resource_tables(table_observations, exclude_cols = "patient_identifier")
data_quality_meds <- quality_assurance_resource_tables(table_meds, exclude_cols = "patient_identifier")
data_quality_procedures <- quality_assurance_resource_tables(table_procedures, exclude_cols = "patient_identifier")


#create tables with selected columns for merge; for tables with multiple entries only use subject_identifer (aggregation for other variables not useful)
table_patients_merge_eligibility <- table_patients[, c(1:4, grep("eligible", names(table_patients)))]
table_conditions_merge_eligibility <- table_conditions[, c(6, 13:15, grep("eligible", names(table_conditions)))]
table_observations_merge_eligibility <- table_observations[, c(3, grep("eligible", names(table_observations)))]
table_meds_merge_eligibility <- table_meds[, c(3, grep("eligible", names(table_meds))), drop = FALSE]
table_procedures_merge_eligibility <- table_procedures[, c(3, grep("eligible", names(table_procedures))), drop = FALSE]

table_patients_merge_can_calc <- table_patients[, c(1:4, grep("can_calc", names(table_patients)))]
table_conditions_merge_can_calc <- table_conditions[, c(6, 13:15, grep("can_calc", names(table_conditions)))]
table_observations_merge_can_calc <- table_observations[, c(3, grep("can_calc", names(table_observations)))]
table_meds_merge_can_calc <- table_meds[, c(3, grep("can_calc", names(table_meds))), drop = FALSE]
table_procedures_merge_can_calc <- table_procedures[, c(3, grep("can_calc", names(table_procedures))), drop = FALSE]

table_patients_merge_compute <- table_patients[, c(1:4, grep("compute", names(table_patients)))]
table_conditions_merge_compute <- table_conditions[, c(6, 13:15, grep("compute", names(table_conditions)))]
table_observations_merge_compute <- table_observations[, c(3, grep("compute", names(table_observations)))]
table_meds_merge_compute <- table_meds[, c(3, grep("compute", names(table_meds))), drop = FALSE]
table_procedures_merge_compute <- table_procedures[, c(3, grep("compute", names(table_procedures))), drop = FALSE]


#reduce tables to 1 entry per patient, if any of the rows have a 0 (ineligible), the whole patient becomes ineligible (exclusion criterion)
#Conditions
table_conditions_merge_eligibility <- table_conditions_merge_eligibility %>%
                                      group_by(condition_subject) %>%
                                      summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 0), 1, 0)), .names = "{.col}")) %>%
                                      ungroup()
#Observations
table_observations_merge_eligibility <- table_observations_merge_eligibility %>%
                                        group_by(observation_subject) %>%
                                        summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 0), 1, 0)), .names = "{.col}")) %>%
                                        ungroup()
if (all(!is.na(table_medicationAdministrations$medicationAdministration_identifier) & table_medicationAdministrations$medicationAdministration_identifier != "")) {table_meds_merge_eligibility <- aggregate(. ~ medicationAdministration_subject, data = table_meds_merge_eligibility, FUN = function(x) ifelse(any(x == 0), 0, 1))}

#reduce tables to 1 entry per patient, if any of the rows have a 1 (can_calc), the whole patient becomes can_calc
table_conditions_merge_can_calc <- table_conditions_merge_can_calc %>%
                                   group_by(condition_subject) %>%
                                   summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                   ungroup()
table_observations_merge_can_calc <- table_observations_merge_can_calc %>%
                                     group_by(observation_subject) %>%
                                     summarise(across(everything(), ~ ifelse(any(is.na(.)), NA, ifelse(any(. == 1), 1, 0)), .names = "{.col}")) %>%
                                     ungroup()
if (all(!is.na(table_medicationAdministrations$medicationAdministration_identifier) & table_medicationAdministrations$medicationAdministration_identifier != "")) {table_meds_merge_can_calc <- aggregate(. ~ medicationAdministration_subject, data = table_meds_merge_can_calc, FUN = function(x) ifelse(any(x == 0), 0, 1))}


#### Define most recent Observation
table_observations_merge_compute <- table_observations_merge_compute %>%
  filter(!is.na(observation_group)) %>%
  group_by(observation_subject, observation_group) %>%
  slice_max(observation_datetime, n = 1, with_ties = FALSE) %>%
  ungroup()


#Create relevant tables for analysis from resource tables
table_eligibility_merge <- merge(table_patients_merge_eligibility, table_conditions_merge_eligibility, by.x = "patient_identifier", by.y = "condition_subject", all.x = TRUE)
table_eligibility_merge <- merge(table_eligibility_merge, table_observations_merge_eligibility, by.x = "patient_identifier", by.y = "observation_subject", all.x = TRUE)
table_eligibility_merge <- merge(table_eligibility_merge, table_meds_merge_eligibility, by.x = "patient_identifier", by.y = "medicationAdministration_subject", all.x = TRUE)
table_eligibility_merge <- merge(table_eligibility_merge, table_procedures_merge_eligibility, by.x = "patient_identifier", by.y = "procedure_subject", all.x = TRUE)

table_can_calc_merge <- merge(table_patients_merge_can_calc, table_conditions_merge_can_calc, by.x = "patient_identifier", by.y = "condition_subject", all.x = TRUE)
table_can_calc_merge <- merge(table_can_calc_merge, table_observations_merge_can_calc, by.x = "patient_identifier", by.y = "observation_subject", all.x = TRUE)
table_can_calc_merge <- merge(table_can_calc_merge, table_meds_merge_can_calc, by.x = "patient_identifier", by.y = "medicationAdministration_subject", all.x = TRUE)
table_can_calc_merge <- merge(table_can_calc_merge, table_procedures_merge_can_calc, by.x = "patient_identifier", by.y = "procedure_subject", all.x = TRUE)

table_compute_merge <- merge(table_patients_merge_compute, table_conditions_merge_compute, by.x = "patient_identifier", by.y = "condition_subject", all.x = TRUE)
table_compute_merge <- merge(table_compute_merge, table_observations_merge_compute, by.x = "patient_identifier", by.y = "observation_subject", all.x = TRUE)
table_compute_merge <- merge(table_compute_merge, table_meds_merge_compute, by.x = "patient_identifier", by.y = "medicationAdministration_subject", all.x = TRUE)
table_compute_merge <- merge(table_compute_merge, table_procedures_merge_compute, by.x = "patient_identifier", by.y = "procedure_subject", all.x = TRUE)



table_eligibility_can_calc <- merge(table_eligibility_merge, table_can_calc_merge, by = c("patient_identifier", "patient_gender", "patient_birthyear", "patient_age", "condition_cvd", "condition_af", "condition_hf"), all = TRUE)
table_eligibility_can_calc_compute <- merge(table_eligibility_can_calc, table_compute_merge, by = c("patient_identifier", "patient_gender", "patient_birthyear", "patient_age", "condition_cvd", "condition_af", "condition_hf"), all = TRUE)

table_eligibility <- table_eligibility_can_calc_compute %>%
  select(1:4, 13:15, contains("eligible"))
  
table_can_calc <- table_eligibility_can_calc_compute %>%
  select(1:4, 13:15, contains("can_calc"))

table_compute <- table_eligibility_can_calc_compute %>%
  select(1:4, 13:15, contains("compute"))

#where medications are empty/missing, we assume that they are not present, the score can be calculated in either case (however result might not be entirely accurate)
table_can_calc <- table_can_calc %>%
  mutate(across(c(can_calc_meds_chadsvasc, can_calc_meds_maggic, can_calc_meds_smart), ~replace_na(.,1)))
#Assumption: if Medications or Conditions are missing, the patient does not have them; non-existence cannot be confirmed by routine data 


# Feasibility Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
message("Analysing Data.\n")

# #take into account the possibility, that certain columns do no exist, if data is unavailable (eg medication)
eligibility_required_columns_chadsvasc <- c("eligible_patient_age_chadsvasc", "eligible_conditions_AF_chadsvasc", "eligible_observations_chadsvasc", "eligible_meds_chadsvasc")
eligibility_available_columns_chadsvasc <- eligibility_required_columns_chadsvasc[eligibility_required_columns_chadsvasc %in% colnames(table_eligibility_can_calc_compute)]
eligibility_required_columns_smart <- c("eligible_patient_age_smart", "eligible_conditions_smart", "eligible_observations_smart", "eligible_meds_smart")
eligibility_available_columns_smart <- eligibility_required_columns_smart[eligibility_required_columns_smart %in% colnames(table_eligibility_can_calc_compute)]
eligibility_required_columns_maggic <- c("eligible_patient_maggic", "eligible_conditions_maggic", "eligible_observations_maggic", "eligible_meds_maggic")
eligibility_available_columns_maggic <- eligibility_required_columns_maggic[eligibility_required_columns_maggic %in% colnames(table_eligibility_can_calc_compute)]
eligibility_required_columns_bcn <- c("eligible_patient_bcn", "eligible_conditions_bcn", "eligible_observations_bcn", "eligible_meds_bcn")
eligibility_available_columns_bcn <- eligibility_required_columns_bcn[eligibility_required_columns_bcn %in% colnames(table_eligibility_can_calc_compute)]


#create summary column for eligibility (if any 0, then 0; if no 0s but any NAs, then NA, if no 0s or NAs then 1)
#only consider columns for this score, that are available
table_eligibility_can_calc_compute$eligible_chadsvasc_overall <- apply(table_eligibility_can_calc_compute[,eligibility_available_columns_chadsvasc], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$eligible_smart_overall <- apply(table_eligibility_can_calc_compute[,eligibility_available_columns_smart], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$eligible_maggic_overall <- apply(table_eligibility_can_calc_compute[,eligibility_available_columns_maggic], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$eligible_table_eligibility_can_calc_compute_overall <- apply(table_eligibility_can_calc_compute[,eligibility_available_columns_table_eligibility_can_calc_compute], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))


#Currently not needed for analysis
#give back result per patient, whether certain criterion has been fulfilled; adjusted to certain columns from larger table in select
table_eligibility_all_criteria <- table_eligibility_can_calc_compute %>%
  group_by(patient_identifier) %>%
  #mutate correct? otherwise remove from analysis
  mutate(across(contains("eligible"), ~ ifelse(any(. == 1), 1, 0))) %>%
  ungroup() %>%
  select(c(-patient_identifier, -starts_with("patient")))

#check availability of any score
# Apply logic row-wise
table_eligibility_can_calc_compute$any_score_eligible <- apply(
  table_eligibility_can_calc_compute[, c("eligible_chadsvasc_overall", "eligible_smart_overall", "eligible_maggic_overall", "eligible_bcn_overall")],
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
export_table_eligibility <- table_eligibility %>%
  select(-starts_with("patient"))
#check whether this drops all relevant patient information


# ability of calculating scores (all parameters that need to be available, are available), absence of parameters is interpreted as not present in patient
can_calc_required_columns_chadsvasc <- c("can_calc_patient_chadsvasc", "can_calc_conditions_chadsvasc", "can_calc_observations_chadsvasc", "can_calc_meds_chadsvasc")
can_calc_available_columns_chadsvasc <- can_calc_required_columns_chadsvasc[can_calc_required_columns_chadsvasc %in% colnames(table_eligibility_can_calc_compute)]
can_calc_required_columns_smart <- c("can_calc_patient_smart", "can_calc_conditions_smart", "can_calc_observations_smart", "can_calc_meds_smart")
can_calc_available_columns_smart <- can_calc_required_columns_smart[can_calc_required_columns_smart %in% colnames(table_eligibility_can_calc_compute)]
can_calc_required_columns_maggic <- c("can_calc_patient_maggic", "can_calc_conditions_maggic", "can_calc_observations_maggic", "can_calc_meds_maggic")
can_calc_available_columns_maggic <- can_calc_required_columns_maggic[can_calc_required_columns_maggic %in% colnames(table_eligibility_can_calc_compute)]
can_calc_required_columns_bcn <- c("can_calc_patient_bcn", "can_calc_conditions_bcn", "can_calc_observations_bcn", "can_calc_meds_bcn")
can_calc_available_columns_bcn <- can_calc_required_columns_bcn[can_calc_required_columns_bcn %in% colnames(table_eligibility_can_calc_compute)]

#create summary column for can_calc (if any 0, then 0; if no 0s but any NAs, then NA, if no 0s or NAs then 1)
table_eligibility_can_calc_compute$can_calc_chadsvasc_overall <- apply(table_eligibility_can_calc_compute[,can_calc_available_columns_chadsvasc], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$can_calc_smart_overall <- apply(table_eligibility_can_calc_compute[,can_calc_available_columns_smart], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$can_calc_maggic_overall <- apply(table_eligibility_can_calc_compute[,can_calc_available_columns_maggic], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))
table_eligibility_can_calc_compute$can_calc_bcn_overall <- apply(table_eligibility_can_calc_compute[,can_calc_available_columns_bcn], 1, function(x) ifelse(any(x == 0), 0, ifelse(any(is.na(x)), NA, 1)))


#check availability of any score
# Apply logic row-wise
table_eligibility_can_calc_compute$any_score_can_calc <- apply(
  table_eligibility_can_calc_compute[, c("can_calc_chadsvasc_overall", "can_calc_smart_overall", "can_calc_maggic_overall", "can_calc_bcn_overall")],
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
navalues_patient_columns <- table_eligibility_can_calc_compute %>%
                            summarise(across(contains("patient"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                            mutate(total_entries = nrow(table_eligibility_can_calc_compute))
navalues_condition_columns <- table_eligibility_can_calc_compute %>%
                              summarise(across(contains("condition"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                              mutate(total_entries = nrow(table_eligibility_can_calc_compute))
navalues_observation_columns <- table_eligibility_can_calc_compute %>%
                                summarise(across(contains("observation"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                                mutate(total_entries = nrow(table_eligibility_can_calc_compute))
navalues_medication_columns <- table_eligibility_can_calc_compute %>%
                               summarise(across(contains("medication"), ~ sum(is.na(.)), .names = "missing_{.col}")) %>%
                               mutate(total_entries = nrow(table_eligibility_can_calc_compute))



# Score Computation ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check columns for data completeness and data quality (min max)

#compute SMART 
#create data frame for score comuputation, then add results to original table
# `Age in years`, `Male` = FALSE, `Current smoker` = FALSE, `Systolic blood pressure in mmHg` = 110, `Diabetic` = FALSE, `History of coronary artery disease` = FALSE,
#   `History of cerebrovascular disease` = FALSE, `Abdominal aortic aneurysm` = FALSE, `Peripheral artery disease` = FALSE, `Years since first diagnosis of vascular disease` = 8,
#   `HDL-cholesterol in mmol/L` = 1, `Total cholesterol in mmol/L` = 3, `eGFR in mL/min/1.73m²` = 177, `hs-CRP in mg/dL` = 6, `Antithrombotic treatment` = FALSE


### Compute SMART ----

#rename columns to fit calculation function
compute_smart_df <- table_compute[, c(3, grep("compute", "smart")), drop = FALSE]
  
compute_smart_df <- compute_smart_df  
  rename(
    `Age in years` = compute_age_smart,
    `Male` = compute_sex_male_smart,
    `Current smoker` = compute_smoking_smart,
    `Systolic blood pressure in mmHg` = compute_sysbp_smart,
    `Diabetic` = compute_diabetes_smart,
    `History of coronary artery disease` = compute_cadhistory_smart,
    `History of cerebrovascular disease` = compute_cvdhistory_smart,
    `Abdominal aortic aneurysm` = compute_aaahistory_smart,
    `Peripheral artery disease` = compute_padhistory_smart,
    `Years since first diagnosis of vascular disease` = compute_timesincefirstcvd_smart,
    `HDL-cholesterol in mmol/L` = compute_cholhdl_smart,
    `Total cholesterol in mmol/L` = compute_totalchol_smart,
    `eGFR in mL/min/1.73m²` = compute_egfr_smart,
    `hs-CRP in mg/dL` = compute_hscrp_smart,
    `Antithrombotic treatment` = compute_antithrombotictreatment_smart
  )

#set data types  
  compute_smart_df <- compute_smart_df %>%
    mutate(
      across(c(`Male`, `Current smoker`, `Diabetic`,
               `History of coronary artery disease`,
               `History of cerebrovascular disease`,
               `Abdominal aortic aneurysm`,
               `Peripheral artery disease`,
               `Antithrombotic treatment`),
             as.logical),
      across(c(`Age in years`,
               `Systolic blood pressure in mmHg`,
               `Years since first diagnosis of vascular disease`,
               `HDL-cholesterol in mmol/L`,
               `Total cholesterol in mmol/L`,
               `eGFR in mL/min/1.73m²`,
               `hs-CRP in mg/dL`),
             as.numeric)
    )
  
  #create new data frame with provided function
  compute_smart_df_export <- run_smart_score_from_df_if_valid(compute_smart_df)
    
    
  # compute_smart_df_export <- compute_smart_df %>%
  #   mutate(smart_score = pmap(
  #     select(., everything()),
  #     ~ run_smart_score_if_valid(params = list(  "Age in years", "Male", "Current smoker", "Systolic blood pressure in mmHg", "Diabetic",
  #                                                "History of coronary artery disease", "History of cerebrovascular disease", "Abdominal aortic aneurysm", "Peripheral artery disease",
  #                                                "Years since first diagnosis of vascular disease",
  #                                                "HDL-cholesterol in mmol/L", "Total cholesterol in mmol/L", "eGFR in mL/min/1.73m²", "hs-CRP in mg/dL",
  #                                                "Antithrombotic treatment"))
  #   ))
  
 
### Compute CHA2DS2VASc -----------------------------------------------------------------------------------

compute_chadsvasc_df <- table_compute[, c(3, grep("compute", "chadsvasc")), drop = FALSE]
  
  #values for computation already assigned in preperation step
  compute_chadsvasc_df_export <- compute_chadsvasc_df %>%
    mutate(
      chadsvasc_score =
        compute_age_chadsvasc +
        compute_sex_chadsvasc +
        compute_diabetes_chadsvasc +
        compute_vasculardisease_chadsvasc +
        compute_hypertension_chadsvasc +
        compute_heartfailure_chadsvasc +
        compute_cvd_chadsvasc
    )
  
  #assume missings to be 0 to still calculate the score
  compute_chadsvasc_df_export <- compute_chadsvasc_df %>%
    mutate(
      chadsvasc_score_sens = 
        coalesce(compute_age_chadsvasc, 0) +
        coalesce(compute_sex_chadsvasc, 0) +
        coalesce(compute_diabetes_chadsvasc, 0) +
        coalesce(compute_vasculardisease_chadsvasc, 0) +
        coalesce(compute_hypertension_chadsvasc, 0) +
        coalesce(compute_heartfailure_chadsvasc, 0) +
        coalesce(compute_cvd_chadsvasc, 0)
    )
  
  
  
### Compute MAGGIC ---------------------------------------------------------------------------------------

compute_maggic_df <- table_compute[, c(3, grep("compute", "maggic")), drop = FALSE]
  
  compute_maggic_df <- compute_maggic_df  
  rename(
    `Ejection fraction (%)` = compute_lvef_maggic,
    `Age (years)` = compute_age_maggic ,
    `Systolic blood pressure (mmHg)` = compute_sysbp_maggic,
    `BMI (kg/m²)` = compute_bmi_maggic,
    `Creatinine (µmol/l)` = compute_creatinine_maggic,
    `NYHA Class` = compute_nyha_maggic,
    `Male` = compute_sex_male_maggic,
    `Current smoker` = compute_smoking_maggic,
    `Diabetic` = compute_diabetes_maggic,
    `Diagnosis of COPD` = compute_copd_maggic,
    `First diagnosis of heart failure in the past 18 months` = compute_hf18months_maggic,
    `Not on beta blocker` = compute_no_betablockers_maggic,
    `Not on ACEI/ARB` = compute_no_acei_arb_maggic
  )
  
  
  compute_maggic_df_export <- calc_maggic_score_from_df(compute_maggic_df)
  
  
  
### Compute BCN ---------------------------------------------------------------------------------------  
#    
# 
# #bcn parameters: @KG do i need to seperately list all variables?
# #bcn computation

   compute_bcn_df <- table_compute[, c(3, grep("compute", "bcn")), drop = FALSE]
  
  compute_bcn_df <- compute_bcn_df
  rename(
    `Age (years)` = compute_age_bcn,
    `Female` = compute_sex_female_bcn,
    `NYHA Class` = compute_nyha_bcn,
    `Sodium (mmol/L)` = compute_natrium_bcn,
    `eGFR in mL/min/1.73m²` = compute_egfr_bcn,
    `Hemoglobin (g/dL)` = compute_haemoglobin_bcn,
    `Loop Diuretic Furosemide Dose` = compute_loopdiuretics_furosemide_bcn,
    `Loop Diuretic Torasemide Dose` = compute_loopdiuretics_torasemide_bcn,
    `Statin` = compute_statins_bcn,
    `ACEi/ARB` = compute_acei_arb_bcn,
    `Betablocker` = compute_betablockers_bcn,
    `HF Duration in months` = compute_hfduration_months_bcn_v3,
    `Diabetes Mellitus` = compute_diabetes_bcn_v3,
    `Hospitalisation Prev. Year` = in_last_year,
    `MRA` = compute_mra_bcn,
    `ICD` = compute_icd_bcn_v3,
    `CRT` = compute_crt_bcn_v3,
    `ARNI` = compute_arni_bcn,
    `NT-proBNP in pg/mL` = compute_ntprobnp_bcn,
    `hs-cTnT in ng/L` = compute_hsctnt_bcn,
    `ST2 (ng/mL)` = compute_st2_bcn,
    `SGLT2i` = compute_isglt2_bcn,
    `Ejection fraction (%)` = compute_lvef_bcn_v3 
  )
  
  compute_bcn_df_export <- calc_barcelona_score_from_df(compute_bcn_df)

  

### Additional Info on Data ----

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
desc_observation <- table_observations %>% filter(!is.na(observation_code) & !is.na(observation_value_num_std)) %>% group_by(observation_code, observation_LOINC_term) %>% summarise(min = min(observation_value_num_std), mean = mean(observation_value_num_std), median = median (observation_value_num_std), max = max(observation_value_num_std), n=n())
#condition
desc_conditions <- table_conditions %>% filter(!is.na(condition_code)) %>% count(condition_code)
#medications
desc_medication <- table_meds %>% filter(!is.na(medication_code)) %>% count(medication_code)


#eligibility
eligibility_chadsvasc <- table(table_eligibility$eligible_chadsvasc_overall, useNA = "ifany")
eligibility_smart <- table(table_eligibility$eligible_smart_overall, useNA = "ifany")
eligibility_maggic <- table(table_eligibility$eligible_maggic_overall, useNA = "ifany")
eligibility_bcn <- table(table_eligibility$eligible_bcn_overall, useNA = "ifany")

#calculable
can_calc_chadsvasc <- table(table_can_calc$can_calc_chadsvasc_overall, useNA = "ifany")
can_calc_smart <- table(table_can_calc$can_calc_smart_overall, useNA = "ifany")
can_calc_maggic <- table(table_can_calc$can_calc_maggic_overall, useNA = "ifany")
can_calc_bcn <- table(table_can_calc$can_calc_bcn_overall, useNA = "ifany")

#analyse all score-specific eligibility variables to see where exclusions occur
table_chadsvasc_eligibility <- table_eligibility[, grepl("chadsvasc", names(table_eligibility)) & names(table_eligibility) != "id"]
table_smart_eligibility <- table_eligibility[, grepl("smart", names(table_eligibility)) & names(table_eligibility) != "id"]
table_maggic_eligibility <- table_eligibility[, grepl("maggic", names(table_eligibility)) & names(table_eligibility) != "id"]
table_bcn_eligibility <- table_eligibility[, grepl("bcn", names(table_eligibility)) & names(table_eligibility) != "id"]

#analyse all score-specific calculation variables to see where exclusions occur
table_chadsvasc_can_calc <- table_can_calc[, grepl("chadsvasc", names(table_can_calc)) & names(table_can_calc) != "id"]
table_smart_can_calc <- table_can_calc[, grepl("smart", names(table_can_calc)) & names(table_can_calc) != "id"]
table_maggic_can_calc <- table_can_calc[, grepl("maggic", names(table_can_calc)) & names(table_can_calc) != "id"]
table_bcn_can_calc <- table_can_calc[, grepl("bcn", names(table_can_calc)) & names(table_can_calc) != "id"]

#turn columns into vectors for crosstabs
eligible_to_factor <- grep("eligible", names(table_eligibility_can_calc_compute), value = TRUE)
table_eligibility_can_calc_compute[eligible_to_factor] <- lapply(table_eligibility_can_calc_compute[eligible_to_factor], function(x) factor(x, levels = c(0, 1)))
can_calc_to_factor <- grep("can_calc", names(table_eligibility_can_calc_compute), value = TRUE)
table_eligibility_can_calc_compute[can_calc_to_factor] <- lapply(table_eligibility_can_calc_compute[can_calc_to_factor], function(x) factor(x, levels = c(0, 1)))

#Percentage of patients who are eligible for at least 1 score
prob_eligibility_any_score <- prop.table(table(table_eligibility_can_calc_compute$any_score_eligible))
#Percentage for which at least one score can be calculated
prob_can_calc_any_score <- prop.table(table(table_eligibility_can_calc_compute$any_score_can_calc))

#bei Calc noch die Elig hinzufügen, da ja die Frage ist von wie vielen die infrage kommen, kann  der Score berechnet werden
#remove here and apply to results table that will be send back
crosstabs_eligibility_can_calc_any_score <- table(table_eligibility_can_calc_compute$any_score_eligible, table_eligibility_can_calc_compute$any_score_can_calc, dnn = c("Eligible", "Calculable"))




























#give out statements after certain chunks to document progress
write(paste("Analysis Steps were finished at", Sys.time(), "\n"), file = log, append = T)


export_table_eligibility_can_calc_compute <- table_eligibility_can_calc_compute %>%
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
write.csv(export_table_eligibility_can_calc_compute, "Output/export_table_eligibility_can_calc_compute.csv")
#@KG add indivudal tables for eligibility, cancal and compute?



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




