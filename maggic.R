library(dplyr)

# Global definitions for MAGGIC score
param_names <- c(
  "Ejection fraction (%)",
  "Age (years)",
  "Systolic blood pressure (mmHg)",
  "BMI (kg/m²)",
  "Creatinine (µmol/l)",
  "NYHA Class",
  "Male",
  "Current smoker",
  "Diabetic",
  "Diagnosis of COPD",
  "First diagnosis of heart failure in the past 18 months",
  "Not on beta blocker",
  "Not on ACEI/ARB"
)

bool_params <- c(
  "Male",
  "Current smoker",
  "Diabetic",
  "Diagnosis of COPD",
  "First diagnosis of heart failure in the past 18 months",
  "Not on beta blocker",
  "Not on ACEI/ARB"
)

maggic_check_ranges <- function(parameters) {
  if (parameters$`Ejection fraction (%)` < 1 || parameters$`Ejection fraction (%)` > 95) {
    stop("'Ejection fraction (%)' must be between 1 and 95")
  }
  if (parameters$`Age (years)` < 18 || parameters$`Age (years)` > 110) {
    stop("'Age (years)' must be between 18 and 110")
  }
  if (parameters$`Systolic blood pressure (mmHg)` < 50 || parameters$`Systolic blood pressure (mmHg)` > 250) {
    stop("'Systolic blood pressure (mmHg)' must be between 50 and 250")
  }
  if (parameters$`BMI (kg/m²)` < 10 || parameters$`BMI (kg/m²)` > 50) {
    stop("'BMI (kg/m²)' must be between 10 and 50")
  }
  if (parameters$`Creatinine (µmol/l)` < 20 || parameters$`Creatinine (µmol/l)` > 1400) {
    stop("'Creatinine (µmol/l)' must be between 20 and 1400")
  }
  if (parameters$`NYHA Class` < 1 || parameters$`NYHA Class` > 4) {
    stop("'NYHA Class' must be between 1 and 4")
  }
}

get_ef_score <- function(lv_ef) {
  if (lv_ef < 20) {
    return(7)
  } else if (lv_ef <= 24) {
    return(6)
  } else if (lv_ef <= 29) {
    return(5)
  } else if (lv_ef <= 34) {
    return(3)
  } else if (lv_ef <= 39) {
    return(2)
  } else {
    return(0)
  }
}

get_age_score <- function(age, lv_ef) {
  score_matrix <- list(
    c(0, 1, 2, 4, 6, 8, 10),
    c(0, 2, 4, 6, 8, 10, 13),
    c(0, 3, 5, 7, 9, 12, 15)
  )
  lv_ef_index <- ifelse(lv_ef < 30, 1, ifelse(lv_ef <= 39, 2, 3))
  age_index <- ifelse(age < 55, 1, ifelse(age <= 59, 2, ifelse(age <= 64, 3, ifelse(age <= 69, 4, ifelse(age <= 74, 5, ifelse(age <= 79, 6, 7))))))
  return(score_matrix[[lv_ef_index]][age_index])
}

get_sbp_score <- function(sbp, lv_ef) {
  score_matrix <- list(
    c(5, 4, 3, 2, 1, 0),
    c(3, 2, 1, 1, 0, 0),
    c(2, 1, 1, 0, 0, 0)
  )
  lv_ef_index <- ifelse(lv_ef < 30, 1, ifelse(lv_ef <= 39, 2, 3))
  sbp_index <- ifelse(sbp < 110, 1, ifelse(sbp <= 119, 2, ifelse(sbp <= 129, 3, ifelse(sbp <= 139, 4, ifelse(sbp <= 149, 5, 6)))))
  return(score_matrix[[lv_ef_index]][sbp_index])
}

get_bmi_score <- function(bmi) {
  if (bmi < 15) {
    return(6)
  } else if (bmi <= 19) {
    return(5)
  } else if (bmi <= 24) {
    return(3)
  } else if (bmi <= 29) {
    return(2)
  } else {
    return(0)
  }
}

get_creatinine_score <- function(creatinine) {
  if (creatinine < 90) {
    return(0)
  } else if (creatinine <= 109) {
    return(1)
  } else if (creatinine <= 129) {
    return(2)
  } else if (creatinine <= 149) {
    return(3)
  } else if (creatinine <= 169) {
    return(4)
  } else if (creatinine <= 209) {
    return(5)
  } else if (creatinine <= 249) {
    return(6)
  } else {
    return(8)
  }
}

get_nyha_class_score <- function(nyha_class) {
  score_array <- c(0, 2, 6, 8)
  return(score_array[nyha_class])
}

# Calculation function
calc_maggic_score <- function(parameters) {
  # Check the ranges
  maggic_check_ranges(parameters)
  
  # Calculate score
  score <- get_ef_score(parameters$`Ejection fraction (%)`) +
    get_age_score(parameters$`Age (years)`, parameters$`Ejection fraction (%)`) +
    get_sbp_score(parameters$`Systolic blood pressure (mmHg)`, parameters$`Ejection fraction (%)`) +
    get_bmi_score(parameters$`BMI (kg/m²)`) +
    get_creatinine_score(parameters$`Creatinine (µmol/l)`) +
    get_nyha_class_score(parameters$`NYHA Class`) +
    parameters$Male +
    parameters$`Current smoker` +
    parameters$Diabetic * 3 +
    parameters$`Diagnosis of COPD` * 2 +
    (!parameters$`First diagnosis of heart failure in the past 18 months`) * 2 +
    parameters$`Not on beta blocker` * 3 +
    parameters$`Not on ACEI/ARB`
  
  return(score)
}

parameters <- list(
  `Ejection fraction (%)` = 35,
  `Age (years)` = 65,
  `Systolic blood pressure (mmHg)` = 120,
  `BMI (kg/m²)` = 25,
  `Creatinine (µmol/l)` = 100,
  `NYHA Class` = 2,
  Male = TRUE,
  `Current smoker` = FALSE,
  Diabetic = TRUE,
  `Diagnosis of COPD` = FALSE,
  `First diagnosis of heart failure in the past 18 months` = TRUE,
  `Not on beta blocker` = TRUE,
  `Not on ACEI/ARB` = FALSE
)

calc_maggic_score(parameters)


# Modified function: set column names and use the global param_names and bool_params
calc_maggic_score_from_df <- function(patients_df) {
  # Set new column names: first column must be "PatientID", then the parameters in the expected order.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract Patient IDs (assumes the column is named "PatientID")
  patient_ids <- patients_df$PatientID
  
  # Preallocate a numeric vector to store MAGGIC scores
  scores <- numeric(nrow(patients_df))
  
  # Loop through each row and compute the score
  for (i in seq_len(nrow(patients_df))) {
    # Convert the current row (excluding PatientID) to a named list of parameters.
    patient_params <- as.list(patients_df[i, param_names])
    
    # Convert specified boolean parameters (if provided as 0/1) to logical values.
    for (field in bool_params) {
      patient_params[[field]] <- as.logical(as.integer(patient_params[[field]]))
    }
    
    # Calculate the MAGGIC score using calc_maggic_score() (from your maggic.R file)
    score <- tryCatch({
      calc_maggic_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
    
    scores[i] <- score
  }
  
  # Return a data frame with PatientIDs and their corresponding MAGGIC scores.
  return(data.frame(PatientID = patient_ids, Score = scores, 
                    stringsAsFactors = FALSE, check.names = FALSE))
}

# Create an example data frame with three patients.
# Note: The first column is PatientID, then columns in the order of param_names.
# example_maggic_df <- data.frame(
#   PatientID = c("P1", "P2", "P3"),
#   `Ejection fraction (%)` = c(35, 50, 42),
#   `Age (years)` = c(65, 55, 75),
#   `Systolic blood pressure (mmHg)` = c(120, 130, 140),
#   `BMI (kg/m²)` = c(25, 30, 28),
#   `Creatinine (µmol/l)` = c(100, 85, 110),
#   `NYHA Class` = c(2, 3, 2),
#   Male = c(1, 0, 1),
#   `Current smoker` = c(0, 1, 0),
#   Diabetic = c(1, 0, 1),
#   `Diagnosis of COPD` = c(0, 0, 1),
#   `First diagnosis of heart failure in the past 18 months` = c(1, 1, 0),
#   `Not on beta blocker` = c(1, 0, 1),
#   `Not on ACEI/ARB` = c(0, 1, 0),
#   stringsAsFactors = FALSE
# )
# 
# #  the MAGGIC score calculation function on the example data frame
# result_maggic <- calc_maggic_score_from_df(example_maggic_df)
# print(result_maggic)

