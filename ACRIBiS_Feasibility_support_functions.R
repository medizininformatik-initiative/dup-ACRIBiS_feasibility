# Support Functions

#Birthyear
convert_date_to_year <- function(birthdate) {
  year <- as.numeric(substr(birthdate, 1, 4))
  return(year)
}


#Neuer Code von JP
is_fhir_bundle_empty <- function(bundlelist){
  length(xml2::xml_find_first(bundlelist[[1]], "entry"))==0
}


#create function to check whether IDs could be retrieved
contains_ids <- function(vec) {
  # Check if the vector is not empty and contains non-NA values
  return(length(vec) > 0 && any(!is.na(vec)))
}





