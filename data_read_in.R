

# Downloading the zipped csv files ----------------------------------------
# 
# https://data.medicare.gov/data/hospital-compare

dir.create("./data")

download.file("https://data.medicare.gov/views/bg9k-emty/files/4042ccf0-83a2-4b48-95ac-e72ae026ab1b?content_type=application%2Fzip%3B%20charset%3Dbinary&filename=Hospital_Revised_Flatfiles.zip", destfile = "./data/hosp_data.zip")

setwd("./data/")

unzip("./hosp_data.zip")


# Clean up file names -----------------------------------------------------

library(tidyverse)

start_directory <- "./"

files <- list.files(start_directory, pattern="*.csv")

clean_files <- function(filename){
        filename <- tolower(filename)
        filename <- str_replace_all(pattern = "-", replacement = "", string = filename)
        filename <- str_replace_all(pattern = "[[:space:]]+", replacement = "_", string = filename)
}

file.rename(from = file.path(start_directory, files), to = file.path(start_directory, clean_files(files)))


# Initial data exploration ------------------------------------------------

## Ambulatory_surgical_measures

asm_facility <- read.csv("./ambulatory_surgical_measuresfacility.csv", header = TRUE, stringsAsFactors = FALSE)

# averages
asm_national <- read.csv("./ambulatory_surgical_measuresnational.csv", header = TRUE, stringsAsFactors = FALSE)

asm_state <- read.csv("./ambulatory_surgical_measuresstate.csv", header = TRUE, stringsAsFactors = FALSE)

## Complications and deaths

comp_death_hosp <- read.csv("./complications_and_deaths_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

comp_death_national <- read.csv("./complications_and_deaths_national.csv", header = TRUE, stringsAsFactors = FALSE)

comp_death_state <- read.csv("./complications_and_deaths_state.csv", header = TRUE, stringsAsFactors = FALSE)

## Healthcare associated infections

infections_hospital <- read.csv("./healthcare_associated_infections_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

infections_national <- read.csv("./healthcare_associated_infections_national.csv", header = TRUE, stringsAsFactors = FALSE)

infections_state <- read.csv("./healthcare_associated_infections_state.csv", header = TRUE, stringsAsFactors = FALSE)


## Medicare spending

medicare_claim <- read.csv("./medicare_hospital_spending_by_claim.csv", header = TRUE, stringsAsFactors = FALSE)

medicare_patient_hosp <- read.csv("./medicare_hospital_spending_per_patient_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

medicare_patient_national<- read.csv("./medicare_hospital_spending_per_patient_national.csv", header = TRUE, stringsAsFactors = FALSE)

medicare_patient_state <- read.csv("./medicare_hospital_spending_per_patient_state.csv", header = TRUE, stringsAsFactors = FALSE)


## Outpatient imaging efficiency


outpatient_img_hospital <- read.csv("./outpatient_imaging_efficiency_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

outpatient_img_national <- read.csv("./outpatient_imaging_efficiency_national.csv", header = TRUE, stringsAsFactors = FALSE)

outpatient_img_state <- read.csv("./outpatient_imaging_efficiency_state.csv", header = TRUE, stringsAsFactors = FALSE)


## Payments

pay_value_care_hosp <- read.csv("./payment_and_value_of_care_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

pay_national <- read.csv("./payment_national.csv", header = TRUE, stringsAsFactors = FALSE)

pay_state <- read.csv("./payment_state.csv", header = TRUE, stringsAsFactors = FALSE)

## Timely and effective care

timely_care_hosp <- read.csv("./timely_and_effective_care_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

timely_care_national <- read.csv("./timely_and_effective_care_national.csv", header = TRUE, stringsAsFactors = FALSE)

timely_care_state <- read.csv("./timely_and_effective_care_state.csv", header = TRUE, stringsAsFactors = FALSE)


## Unplanned hospital visits

unplanned_hosp <- read.csv("./unplanned_hospital_visits_hospital.csv", header = TRUE, stringsAsFactors = FALSE)

unplanned_national <- read.csv("./unplanned_hospital_visits_national.csv", header = TRUE, stringsAsFactors = FALSE)

unplanned_state <- read.csv("./unplanned_hospital_visits_state.csv", header = TRUE, stringsAsFactors = FALSE)


## Switch wd back

setwd("..")

save.image(file = "health.RData")

