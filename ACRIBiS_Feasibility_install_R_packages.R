#list of required packages
required_packages <- c("fhircrackr", "dplyr", "tidyr", "sqldf", "BH", "anytime", "ICD10gm", "knitr", "xml2", "lubridate", "readr")

#install each listed package if available
for(package in required_packages){
  
  #available <- suppressWarnings(require(package, character.only = T))
  available <- require(package, character.only = T)
  
  if(!available){
    install.packages(package, repos="https://ftp.fau.de/cran/", quiet = TRUE)
  }
}

#call all packages
for(package in required_packages){
  
  library(package, character.only = TRUE)
  
}


#install development version of fhircrackr for additional features, defunct
# remotes::install_github("https://github.com/POLAR-fhiR/fhircrackr")
# library(fhircrackr)



print("install_packages complete")
