POINTS <- list(
  `Congestive heart failure/LV dysfunction` = 1,
  `Hypertension` = 1,
  `Age ≥75y` = 2,
  `Diabetes mellitus` = 1,
  `Stroke/TIA/TE` = 2,
  `Vascular diseases` = 1,
  `Age 65-74y` = 1,
  `Sex category` = 1
)

# Expected parameter names for CHADS score in the input data frame
param_names <- c(
  "Congestive heart failure/LV dysfunction",
  "Hypertension",
  "Age ≥75y",
  "Diabetes mellitus",
  "Stroke/TIA/TE",
  "Vascular diseases",
  "Age 65-74y",
  "Sex category"
)

calc_chads_vasc_score <- function(parameters) {
  if (parameters$`Age ≥75y` && parameters$`Age 65-74y`) {
    stop("Not both age parameters can be true!")
  }
  
  score <- sum(sapply(names(parameters), function(param) parameters[[param]] * POINTS[[param]]))
  return (score)
}

calc_chads_vasc_scores_from_df <- function(patients_df) {
  
  # Set new column names to expected.
  colnames(patients_df) <- c("PatientID", param_names)
  
  # Extract patient IDs (assumes the column is named "PatientID")
  patient_ids <- patients_df$PatientID
  
  # Preallocate a numeric vector for the scores
  scores <- numeric(nrow(patients_df))
  
  # Loop through each row (each patient)
  for(i in seq_len(nrow(patients_df))) {
    patient_params <- as.list(patients_df[i, param_names])
    # Compute the CHADS-VASc score. Note: calc_chads_vasc_score() will throw an error if both age conditions are TRUE.
    scores[i] <- tryCatch({
      calc_chads_vasc_score(patient_params)
    }, error = function(e) {
      cat("Error for Patient", patient_ids[i], ":", e$message, "\n")
      NA
    })
  }
  # Return a data frame with PatientIDs and their corresponding Scores
  return(data.frame(patient_ids, scores, stringsAsFactors = FALSE, check.names = FALSE))
}
# 
# parameters <- list(
#   `Congestive heart failure/LV dysfunction` = 1,
#   `Hypertension` = 1,
#   `Age ≥75y` = 0,
#   `Diabetes mellitus` = 1,
#   `Stroke/TIA/TE` = 0,
#   `Vascular diseases` = 1,
#   `Age 65-74y` = 1,
#   `Sex category` = 1
# )
# 
# score <- calc_chads_vasc_score(parameters)
# print(score)
# 
# # Create an example data frame with PatientID and the eight parameters (dot-separated)
# example_chads_df <- data.frame(
#   PatientID = c("P1", "P2", "P3", "P4", "P5"),
#   `Congestive heart failure/LV dysfunction` = c(1, 0, 1, 0, 1),
#   `Hypertension` = c(1, 1, 0, 0, 1),
#   `Age ≥75y` = c(0, 1, 0, 0, 0),
#   `Diabetes mellitus` = c(1, 0, 1, 0, 0),
#   `Stroke/TIA/TE` = c(0, 0, 1, 0, 0),
#   `Vascular diseases` = c(1, 1, 1, 1, 0),
#   `Age 65-74y` = c(1, 1, 1, 0, 1),
#   `Sex category` = c(1, 1, 0, 0, 1),
#   `stringsAsFactors` = FALSE, check.names = FALSE
# )
# 
# # Test the function with the example CHADS data frame
# result <- calc_chads_vasc_scores_from_df(example_chads_df)
# print(result)
# 
