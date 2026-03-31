library(dplyr)
library(purrr)
library(tibble)
library(readr)

# Global parameter names (excluding PatientID) expected by calc_barcelona_hf_score()
param_names <- c(
  "Age (years)",
  "Female",
  "NYHA Class",
  "Sodium (mmol/L)",
  "eGFR in mL/min/1.73m²",
  "Hemoglobin (g/dL)",
  "Loop Diuretic Furosemide Dose",
  "Loop Diuretic Torasemide Dose",
  "Statin",
  "ACEi/ARB",
  "Betablocker",
  "HF Duration in months",
  "Diabetes Mellitus",
  "Hospitalisation Prev. Year",
  "MRA",
  "ICD",
  "CRT",
  "ARNI",
  "NT-proBNP in pg/mL",
  "hs-cTnT in ng/L",
  "ST2 (ng/mL)",
  "SGLT2i",
  "Ejection fraction (%)"
)

# Global vector for parameters to treat as booleans (convert 0/1 to logical)
bool_params <- c(
  "Female",
  "Statin",
  "ACEi/ARB",
  "Betablocker",
  "Diabetes Mellitus",
  "MRA",
  "ICD",
  "CRT",
  "ARNI",
  "SGLT2i"
)

imputable_params <- c(
  "Ejection fraction (%)",
  "Sodium (mmol/L)",
  "eGFR in mL/min/1.73m²",
  "Hemoglobin (g/dL)",
  "HF Duration in months"
)

Model <- list(
  MODEL_1 = "MODEL_1",
  MODEL_2 = "MODEL_2",
  MODEL_3 = "MODEL_3",
  MODEL_4 = "MODEL_4",
  MODEL_5 = "MODEL_5",
  MODEL_6 = "MODEL_6",
  MODEL_7 = "MODEL_7",
  MODEL_8 = "MODEL_8"
)

MIN_MAX_MEDIAN <- tibble(
  variable = c("Age (years)", "Ejection fraction (%)", "Sodium (mmol/L)", "eGFR in mL/min/1.73m²",
               "Hemoglobin (g/dL)", "NT-proBNP in pg/mL", "hs-cTnT in ng/L", "ST2 (ng/mL)",
               "HF Duration in months", "Hospitalisation Prev. Year"),
  lower_limit = c(31.31446954141, 13, 128, 7.63881506570324, 8.9, 37.45, 4.8245, 6.29, 0, 0),
  upper_limit = c(90.9212046543464, 78.71, 145, 119.283356873844, 16.771, 34800, 242.84, 171.1, 257.040000000001, 5.71000000000004),
  median_impute = c(70.3, 35, 138, 60.613452863849, 12.9, 1361.5, 22.6, 38.1, 6, 0)
)

check_values <- function(parameter_value, parameter_name, min_max_median) {
  row <- filter(min_max_median, variable == parameter_name)
  lower_limit <- row$lower_limit
  upper_limit <- row$upper_limit
  median_impute <- row$median_impute
  
  if (is.na(parameter_value)) {
    parameter_value <- ifelse(parameter_name %in% imputable_params, median_impute, NA)
  } else if (parameter_value < lower_limit) {
    parameter_value <- lower_limit
  } else if (parameter_value > upper_limit) {
    parameter_value <- upper_limit
  }
  return(parameter_value)
}

get_model <- function(parameters) {
  if (!is.na(parameters$`NT-proBNP in pg/mL`) &&
      is.na(parameters$`hs-cTnT in ng/L`) && is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_2)
  } else if (!is.na(parameters$`hs-cTnT in ng/L`) &&
             is.na(parameters$`NT-proBNP in pg/mL`) && is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_3)
  } else if (!is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`NT-proBNP in pg/mL`) && is.na(parameters$`hs-cTnT in ng/L`)) {
    return(Model$MODEL_4)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`hs-cTnT in ng/L`)) {
    return(Model$MODEL_5)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`hs-cTnT in ng/L`) &&
             is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_6)
  } else if (!is.na(parameters$`hs-cTnT in ng/L`) && !is.na(parameters$`ST2 (ng/mL)`) &&
             is.na(parameters$`NT-proBNP in pg/mL`)) {
    return(Model$MODEL_7)
  } else if (!is.na(parameters$`NT-proBNP in pg/mL`) && !is.na(parameters$`hs-cTnT in ng/L`) &&
             !is.na(parameters$`ST2 (ng/mL)`)) {
    return(Model$MODEL_8)
  } else {
    return(Model$MODEL_1)
  }
}

get_coefficients <- function(model, model_coefficients) {
  # Excluding the last six rows from model_coefficients
  coefficients <- model_coefficients %>% 
    slice(1:(n() - 6)) %>% 
    select(Variables, !!sym(model)) %>% 
    deframe()
  
  # Extracting the sum_product value
  sum_product <- model_coefficients %>% 
    filter(Variables == "Sum_Product") %>% 
    pull(!!sym(model))
  
  # Returning the list containing coefficients and sum_product
  return(list(coefficients = coefficients, sum_product = sum_product))
}


get_survival_estimate <- function(model, survival_year, model_coefficients) {
  survival_estimate <- switch(as.character(survival_year),
                              "1" = model_coefficients %>% filter(Variables == "One_year_survival") %>% pull(!!sym(model)),
                              "2" = model_coefficients %>% filter(Variables == "Two_year_survival") %>% pull(!!sym(model)),
                              "3" = model_coefficients %>% filter(Variables == "Three_year_survival") %>% pull(!!sym(model)),
                              "4" = model_coefficients %>% filter(Variables == "Four_year_survival") %>% pull(!!sym(model)),
                              "5" = model_coefficients %>% filter(Variables == "Five_year_survival") %>% pull(!!sym(model)),
                              stop("'survival_year' must be between 1 and 6!"))
  return(survival_estimate)
}


get_new_parameters <- function(parameters) {
  new_parameters <- parameters
  new_parameters$`NYHA Class` <- ifelse(parameters$`NYHA Class` %in% c(1, 2), 0, 1)
  new_parameters$`Ejection fraction (%)` <- ifelse(parameters$`Ejection fraction (%)` <= 45, 0, 1)
  new_parameters$`log(HF Duration in months)` <- log(parameters$`HF Duration in months`)
  loop_diuretic_dose = parameters$`Loop Diuretic Furosemide Dose` + 4 * parameters$`Loop Diuretic Torasemide Dose`
  new_parameters$`Furosemide Dose 1` <- ifelse(loop_diuretic_dose > 0 & loop_diuretic_dose <= 40, 1, 0)
  new_parameters$`Furosemide Dose 2` <- ifelse(loop_diuretic_dose > 40 & loop_diuretic_dose <= 80, 1, 0)
  new_parameters$`Furosemide Dose 3` <- ifelse(loop_diuretic_dose > 80, 1, 0)
  if (!is.na(parameters$`NT-proBNP in pg/mL`)) {
    new_parameters$`log(NT-proBNP in pg/mL)` <- ifelse(parameters$`NT-proBNP in pg/mL` == 0, 0, log(parameters$`NT-proBNP in pg/mL`))
  }
  if (!is.na(parameters$`hs-cTnT in ng/L`)) {
    new_parameters$`log(hs-cTnT in ng/L)` <- ifelse(parameters$`hs-cTnT in ng/L` == 0, 0, log(parameters$`hs-cTnT in ng/L`))
    new_parameters$`Squared log(hs-cTnT in ng/L)` <- (new_parameters$`log(hs-cTnT in ng/L)`)^2
  }
  if (!is.na(parameters$`ST2 (ng/mL)`)) {
    new_parameters$`ST2_div_10` <- parameters$`ST2 (ng/mL)` / 10
    new_parameters$`Squared ST2_div_10` <- (new_parameters$`ST2_div_10`)^2
  }
  return(new_parameters)
}

get_scores <- function(file, model, new_parameters) {
  scores <- c()
  file_path <- file
  if (!file.exists(file_path)) {
    stop(paste("Error: File does not exist -", file_path))
  }
  model_beta_coefficients <- read_csv(file_path)
  coefficients_list <- get_coefficients(model, model_beta_coefficients)
  coefficients <- coefficients_list$coefficients
  sum_product <- coefficients_list$sum_product
  if (basename(file) == 'barcelona_hf_v3_hosp_coefficients.csv') {
    sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
      if (parameter %in% names(new_parameters)) {
        new_parameters[[parameter]] * coefficients[[parameter]]
      } else {
        0
      }
    }))
  } else {
  new_parameters_copy <- new_parameters
  new_parameters_copy$`Hospitalisation Prev. Year` <- as.logical(new_parameters_copy$`Hospitalisation Prev. Year`)
  sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
    if (parameter %in% names(new_parameters_copy)) {
      new_parameters_copy[[parameter]] * coefficients[[parameter]]
    } else {
      0
    }
  }))
  }
  for (year in 1:5) {
    survival_estimate <- get_survival_estimate(model, year, model_beta_coefficients)
    score <- (1 - (survival_estimate ^ exp(sum_product_all_parameters - sum_product))) * 100
    scores <- c(scores, round(score, 1))
  }
  return(scores)
}

calc_life_expectancy <- function(model, new_parameters) {
  coefficients_life_expectancy <- '../resources/barcelona_hf_v3_life_expectancy_coefficients.csv'
  life_expectancy_limits <- '../resources/life_expectancy_limits.csv'
  if (!file.exists(coefficients_life_expectancy) || !file.exists(life_expectancy_limits)) {
    stop("Error: Life expectancy coefficient files do not exist in the resources folder.")
  }
  le_coefficients <- read_csv(coefficients_life_expectancy)
  le_limits <- read_csv(life_expectancy_limits)
  coefficients <- le_coefficients %>% filter(Variables != "Intercept" & Variables != "Gamma Value") %>% select(Variables, !!sym(model)) %>% deframe()
  new_parameters_copy <- new_parameters
  new_parameters_copy$`Hospitalisation Prev. Year` <- as.logical(new_parameters_copy$`Hospitalisation Prev. Year`)
  sum_product_all_parameters <- sum(map_dbl(names(coefficients), function(parameter) {
    if (parameter %in% names(new_parameters_copy)) {
      new_parameters_copy[[parameter]] * coefficients[[parameter]]
    } else {
      0
    }
  }))
  intercept <- le_coefficients %>% filter(Variables == "Intercept") %>% pull(!!sym(model))
  gamma_value <- le_coefficients %>% filter(Variables == "Gamma Value") %>% pull(!!sym(model))
  le <- exp(intercept + sum_product_all_parameters) * gamma_value
  key <- ifelse(new_parameters$Female, 'Women', 'Men')
  age <- as.integer(new_parameters$`Age (years)`)
  if (age %in% le_limits$Age) {
    if ((key == 'Men' && age > 63) || (key == 'Women' && age > 67)) {
      upper_limit <- le_limits %>% filter(Age == age) %>% pull(!!sym(key))
      if (le > as.numeric(upper_limit)) {
        le <- upper_limit
      }
    }
  }
  if (!is.na(le) && le > 20) {
    if (key == 'Men' && age <= 63) {
      le <- '>20'
    } else if (key == 'Women' && age <= 67) {
      le <- '>20'
    }
  }
  return(le)
}

round_life_expectancy <- function(model, parameters) {
  life_expectancy <- calc_life_expectancy(model, parameters)
  tryCatch({
    life_expectancy <- round(as.numeric(life_expectancy), 1)
  }, warning = function(w) {
    life_expectancy <- life_expectancy
  }, error = function(e) {
    life_expectancy <- life_expectancy
  })
  return(life_expectancy)
}

calc_barcelona_hf_score <- function(parameters) {
  # Convert the specified boolean parameters from numeric (0/1) to logical values.
  for (field in bool_params) {
    parameters[[field]] <- as.logical(as.integer(parameters[[field]]))
  }

  all_scores <- list()
  coefficients_death_file <- '../resources/barcelona_hf_v3_death_coefficients.csv'
  coefficients_hosp_file <- '../resources/barcelona_hf_v3_hosp_coefficients.csv'
  coefficients_hosp_death_file <- '../resources/barcelona_hf_v3_hosp_death_coefficients.csv'
  model <- get_model(parameters)
  
  for (param in MIN_MAX_MEDIAN$variable) {
    if (param %in% names(parameters)) {
      parameters[[param]] <- check_values(parameters[[param]], param, MIN_MAX_MEDIAN)
    }
  }
  
  new_parameters <- get_new_parameters(parameters)
  endpoints_without_biomarkers <- list()
  endpoints_with_biomarkers <- list()
  for (file in c(coefficients_death_file, coefficients_hosp_file, coefficients_hosp_death_file)) {
    file_path <- file
    if (!file.exists(file_path)) {
      stop(paste("Error: File does not exist -", file_path))
    }
    suffix <- sub('barcelona_hf_v3_', '', sub('_coefficients.csv', '', basename(file)))
    scores_without_biomarkers <- get_scores(file_path, Model$MODEL_1, new_parameters)
    endpoints_without_biomarkers[[suffix]] <- scores_without_biomarkers
    
    if (model != Model$MODEL_1) {
      scores_with_biomarkers <- get_scores(file_path, model, new_parameters)
      endpoints_with_biomarkers[[suffix]] <- scores_with_biomarkers
    }
    
    if (basename(file) == 'barcelona_hf_v3_death_coefficients.csv') {
      le_without_biomarkers <- round_life_expectancy(Model$MODEL_1, new_parameters)
      endpoints_without_biomarkers[['life_expectancy']] <- as.character(le_without_biomarkers)
      
      if (model != Model$MODEL_1) {
        le_with_biomarkers <- round_life_expectancy(model, new_parameters)
        endpoints_with_biomarkers[['life_expectancy']] <- as.character(le_with_biomarkers)
      }
    }
  }
  
  all_scores[['without_biomarkers']] <- endpoints_without_biomarkers
  if (model != Model$MODEL_1) {
    all_scores[['with_biomarkers']] <- endpoints_with_biomarkers
  }
  
  return(all_scores)
}

# parameters2 <- list(
#   `Age (years)` = 40,
#   `Female` = TRUE,
#   `NYHA Class` = 11,
#   `Sodium (mmol/L)` = 10,
#   `eGFR in mL/min/1.73m²` = 6,
#   `Hemoglobin (g/dL)` = 12,
#   `Loop Diuretic Furosemide Dose` = 20,
#   `Loop Diuretic Torasemide Dose` = 0,
#   `Statin` = TRUE,
#   `ACEi/ARB` = FALSE,
#   `Betablocker` = FALSE,
#   `HF Duration in months` = 1,
#   `Diabetes Mellitus` = FALSE,
#   `Hospitalisation Prev. Year` = 0,
#   `Ejection fraction (%)` = 40,
#   `MRA` = FALSE,
#   `ICD` = FALSE,
#   `CRT` = FALSE,
#   `ARNI` = FALSE,
#   `NT-proBNP in pg/mL` = NA,
#   `hs-cTnT in ng/L` = 1.112,
#   `ST2 (ng/mL)` = 4,
#   `SGLT2i` = FALSE
# )
# 
# example_scores <- calc_barcelona_hf_score(parameters2)

flatten_barcelona_scores <- function(result_barcelona) {
  # Create an empty list to collect rows.
  all_rows <- list()
  
  # Loop over each patient
  for (i in seq_along(result_barcelona$PatientIDs)) {
    pid <- result_barcelona$PatientIDs[i]
    score_entry <- result_barcelona$Scores[[i]]
    
    # Loop over each biomarker type if present ("without_biomarkers", "with_biomarkers")
    for (bio in names(score_entry)) {
      outcomes <- score_entry[[bio]]
      # For each outcome within the biomarker category:
      for (outcome in names(outcomes)) {
        value <- outcomes[[outcome]]
        if (outcome == "life_expectancy") {
          # Single value outcome
          all_rows[[length(all_rows) + 1]] <- data.frame(
            PatientID = pid,
            Biomarker = bio,
            Outcome = outcome,
            Year = NA,
            Score = value,
            stringsAsFactors = FALSE
          )
        } else {
          # Assume vector (e.g., death, hosp, hosp_death): add one row per year (1:5)
          for (year in seq_along(value)) {
            all_rows[[length(all_rows) + 1]] <- data.frame(
              PatientID = pid,
              Biomarker = bio,
              Outcome = outcome,
              Year = year,
              Score = value[year],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }
  
  # Bind all rows into one data frame
  tidy_df <- do.call(rbind, all_rows)
  return(tidy_df)
}

calc_barcelona_score_from_df <- function(patients_df) {
  # Set new column names: first column must be "PatientID", then the parameters in the expected order.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs (assumes a column called "PatientID")
  patient_ids <- patients_df$PatientID
  
  # Preallocate a list to store the score outputs for each patient
  scores <- vector("list", nrow(patients_df))
  
  # Loop through each row and compute the score
  for (i in seq_len(nrow(patients_df))) {
    cat("Processing Patient", patient_ids[i], "...\n")
    
    # Convert the row (excluding PatientID) to a named list of parameters.
    patient_params <- as.list(patients_df[i, param_names])
    
    # Calculate the Barcelona HF score endpoints for this patient.
    # The call is wrapped in a tryCatch block to capture and report errors.
    scores[[i]] <- tryCatch({
      calc_barcelona_hf_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
  }
  
  # Return a "tuple" as a list with PatientIDs and the list of score outputs.
  return(list(PatientIDs = patient_ids, Scores = scores))
}

# example_barcelona_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   `Age (years)` = c(60, 70, 55),
#   Female = c(1, 0, 1),
#   `NYHA Class` = c(2, 3, 2),
#   `Sodium (mmol/L)` = c(138, 140, 135),
#   `eGFR in mL/min/1.73m²` = c(60, 80, 55),
#   `Hemoglobin (g/dL)` = c(12, 14, 11),
#   `Loop Diuretic Furosemide Dose` = c(20, 0, 40),
#   `Loop Diuretic Torasemide Dose` = c(0, 10, 0),
#   Statin = c(1, 0, 1),
#   `ACEi/ARB` = c(0, 1, 0),
#   Betablocker = c(0, 1, 0),
#   `HF Duration in months` = c(1, 12, 6),
#   `Diabetes Mellitus` = c(0, 1, 0),
#   `Hospitalisation Prev. Year` = c(0, 1, 0),
#   MRA = c(0, 0, 1),
#   ICD = c(0, 0, 0),
#   CRT = c(0, 0, 1),
#   ARNI = c(0, 1, 0),
#   `NT-proBNP in pg/mL` = c(NA, 200, 500),
#   `hs-cTnT in ng/L` = c(1.112, 0.8, 1.5),
#   `ST2 (ng/mL)` = c(4, 3, 5),
#   SGLT2i = c(0, 1, 0),
#   `Ejection fraction (%)` = c(40, 50, 30),
#   stringsAsFactors = FALSE
# )
# 
# tidy_barcelona <- flatten_barcelona_scores(result_barcelona)
# print(tidy_barcelona)

