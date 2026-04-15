# About ACRIBiS_feasibility

Currently restricted to cases from 2024 via the encounter resource!
Please report any issues in the Zulip Chat: https://mii.zulipchat.com/#narrow/channel/495070-FDPG-Projekt.3AACRIBiS_Score_Machbarkeit/topic/Allgemeine.20Informationen/with/509667378

The R-files will enable you to conduct the necessary analysis steps for the ACRIBiS Data Usage Project. Goal of this project is to gain insights into the availability and completeness of data items required for the calculation of cardiovascular risk scores
In Addition you will need the folder with LOINC-Codes in the directory from which you run the r-scripts

Steps
1. Rename the config-file template to "ACRIBiS_Feasibility_config_local.R" and adjust the contents according to your local requirements
      - Add the shorthand for your site and other relevant information like usernames and passwords
      - Add your FHIR-server address under "diz_url" and run the script 
3. Run the install_packages script
4. Download the LOINC-Codes from https://fex.ukw.de/public/download-shares/5UywHUTAAZmpELZLB04YeU8SUgYpLF3X or https://loinc.org/file-access/?download-id=470626 (Free Account might be necessary) and place the folder in the working directory. Unfortunately the file is too large to upload to github. The Script currently uses Version 2.78
5. Run the analysis script
6. Document your environmant and the results in corresponding file in the "Test Logs" Folder
7. Upload Output, Log, and Error Folders with site short hand (e.g. UKW_Output) to [https://fex.ukw.de/public/upload-shares/JqcRKDMZtyL4vkerQZCurlHjQ9hSVjT9](https://share.ukw.de/index.php/s/d4eKn4twwG5SgtL)

You can find some common issues in the FAQ.

Contact: Kai Günther (guenther_k1@ukw.de)
