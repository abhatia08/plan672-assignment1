# 92. Data Visualization

# 1. SETUP ----

## 1.1 Load required packages ----
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(here,
               OpenStreetMap,
               tidyverse)

## 1.2 Declare `here` ----
here::i_am("scripts/02_visualize_data")
setwd(here())


## 1.3 Load datasets ----
## If the clean data file has been generated, load it. Else, run the cleaning script first

## Intercept Data
if (!file.exists(here::here("derived_data", "intercept_tidy.csv"))) {
  source(here::here("scripts", "01_clean_data.R"))
} else {
  intercept_tidy <-
    read_csv(here::here("derived_data", "intercept_tidy.csv"))
}

## Other Data (Placeholder in case we use other data)

# 2. DATA VISUALIZATION ----

## 2.1 Fig 1. Plot of X and Y ----
## TO PIERCE: Here, just build out space/headers for each figure that we intend on building.
