# 91. DATA CLEANING

# 1. SETUP ----

## 1.1 Load required packages ----
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(fs,
               here,
               tidygeocoder,
               tidyverse)

## 1.2 Declare `here` ----
here::i_am("scripts/01_clean_data.R")

setwd(here())


## 1.3. Create necessary directories ----
## Create directory to store processed data
if (!dir.exists("derived_data")) {
  dir.create("derived_data")
  
}
## Create directory to store plots
if (!dir.exists("figures")) {
  dir.create("figures")
  
}

# 2. INTERCEPT  DATA  ----

## 2.1 Download raw dataset ----
## Note: The data was downloaded from The Intercept's Github Repository
#https://github.com/firstlookmedia/climate-and-punishment-data

if (!file.exists(here::here("source_data", "climate_and_punishment.csv"))) {
  download.file(
    "https://raw.githubusercontent.com/firstlookmedia/climate-and-punishment-data/stable/climate-and-punishment.csv",
    here::here("source_data",
               "climate_and_punishment.csv"),
    quiet = FALSE,
    mode = "w"
  )
}

# If dataset has not been cleaned, clean dataset
if (!file.exists(here::here("derived_data", "intercept_tidy.csv"))) {
  ## 2.2 Read in raw dataset ----
  intercept_data <-
    read_csv(here::here("source_data", "climate_and_punishment.csv"))
  
  ## 2.3 Subset dataset ----
  ## Create merged address variable
  intercept_tidy <-
    intercept_data %>% 
    mutate(address_full = paste0(address, ", ", city, ", ", county, ", ", state, ", ", zip))
  
  ## Subset by state
  intercept_tidy <- intercept_tidy %>% filter(state == "NC")
  
  ## Replace -999 to NA
  intercept_tidy[intercept_tidy == -999] <- NA

  ## 2.4. Geocode dataset ----
  intercept_tidy <-
    intercept_tidy %>% geocode(
      address = address_full,
      method = 'arcgis',
      lat = latitude,
      long = longitude,
      full_results = FALSE
    )
  
  ## 2.5 Retain only columns of interest ----
  intercept_tidy <-
    intercept_tidy %>% select(
      c(
        "facilityid",
        "name",
        "city",
        "county",
        "state",
        "zip",
        "capacity",
        "type",
        "securelvl",
        "status",
        "flood_risk_rating",
        "flood_risk",
        "confidence",
        "latitude",
        "longitude"
      )
    )
  
  ## Detach full dataset
  rm(intercept_data)
  
  ## 2.6 Write to directory ----
  write_csv(intercept_tidy,
            here::here("derived_data", "intercept_tidy.csv"))
}

# 3. OTHER DATA ----


# TO PIERCE. JUST LIKE THE ABOVE CHUNK CLEANS INTERCEPT DATA, IF WE'RE GOING TO USE OTHER DATASETS, WE CAN ADD IN EACH OF THEIR CLEANING CODE HERE IN SECTIONS. 
# THEN, RUNNING THE FILE WOULD CLEAN ALL DATASETS AND WRITE IT TO A DIRECTORY.


