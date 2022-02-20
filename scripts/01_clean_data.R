# 91. Cleaning datasets

# 1. SETUP ----

## 1.1 Load required packages ----
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(downloader,
               fs,
               here,
               janitor,
               tidyverse
)

## 1.2 Declare `here`
here::i_am("scripts/01_clean_data.R")

## 1.3. Create necessary directories ----
## Create directory to store processed data
if (!dir.exists("derived_data")) {
  dir.create("derived_data")
  
}
## Create directory to store plots
if (!dir.exists("figures")) {
  dir.create("figures")
  
}

# 2. INTERCEPT  DATA ----

## 2.1 Download raw dataset
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

## 2.2 Read in raw dataset

intercept <- read_csv(here::here("source_data", "climate_and_punishment.csv"))

