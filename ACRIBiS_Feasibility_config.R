# Konfigurations-Datei 
# Bitte die folgenden Variablen entsprechend der Gegebenheiten vor Ort anpassen!

# z.B. https://mii-agiop-3p.life.uni-leipzig.de/blaze
diz_url = ""
#shorthand for site ending with underscore
diz_short <- "_"


#add maximum number of bundles to enable faster run times for testing (e.g. 10 or 20)
bundle_limit <- Inf
#load bundles that are already saved, instead of searching for new ones
search_for_bundles <- TRUE
#save bundles from FHIR-search to avoid searching again each time; saved bundles are then loaded
save_bundles <- FALSE

#set count for how many resources can be filled into one bundle response (default most often 50); depends on FHIR-Server Settings
page_count <- as.character(50)
#Variable to define whether html elemnts should be removed or kept ("div" removes html, NULL keeps them); might impact RAM usage
rm_tag <- NULL

# Authentifizierung
# Falls Authentifizierung, bitte entsprechend anpassen (sonst ignorieren):
# Username und Passwort für Basic Authentification
username <- NULL #zB "myusername"
password <- NULL #zB "mypassword"

# Alternativ: Token für Bearer Token Authentifizierung
token <- NULL #zB "mytoken"


# SSL peer verification angeschaltet lassen?
# TRUE = peer verification anschalten, FALSE = peer verification ausschalten 
ssl_verify_peer <- TRUE