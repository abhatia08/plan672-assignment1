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
## Create directory to store source data
if (!dir.exists("source_data")) {
  dir.create("source_data")
  
}
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

# 3. HURRICANE FLORENCE DATA ----
## Hurricane Florence Flood Extent September 14 2018 
## Note: Raster files found from NCOneMap:https://www.nconemap.gov/datasets/nconemap::hurricane-florence-flood-extent-across-the-piedmont-and-coastal-plain-of-north-carolina/about

## 3.1 Download raw dataset ----
if (!file.exists(here::here("derived_data", "FloodExtentFlorence.tif"))) {
  options(timeout = 100)
  download.file(
    "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A68ea0876-33ef-4223-80dd-f563c9c39efc",
    here::here("source_data",
               "Florence_flood_extent.zip"),
    quiet = FALSE,
    mode = "wb",
    method = "libcurl"
  )
  utils::unzip(
    "source_data/Florence_flood_extent.zip",
    overwrite = TRUE,
    exdir = here::here("derived_data")
  )
  
}

# 4. HURRICANE MATTHEW DATA ----
## Hurricane Matthew October 8th 2016
## 4.1 Download raw dataset ----
if (!file.exists(here::here("derived_data", "FloodExtentMatthew.tif"))) {
  download.file(
    "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Aa162710b-a0f9-4cfa-af77-32d37ea9d148",
    here::here("source_data",
               "Matthew_flood_extent.zip"),
    quiet = FALSE,
    mode = "wb",
    method = "libcurl"
  )
  utils::unzip(
    "source_data/Matthew_flood_extent.zip",
    overwrite = TRUE,
    exdir = here::here("derived_data")
  )
}