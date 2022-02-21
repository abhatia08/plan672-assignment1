# 92. Data Visualization

# 1. SETUP ----

## 1.1 Load required packages ----
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(here,
               OpenStreetMap,
               tidyverse, 
               leaflet)

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

## 1.4 Hurricane Data ----
# OPTIONALY WE CAN DECIDE ON LATER - PIERCE

### 1.4.1 Hurricane Florence Flood Extent September 14 2018 ----
## Note: Raster files found from NCOneMap:https://www.nconemap.gov/datasets/nconemap::hurricane-florence-flood-extent-across-the-piedmont-and-coastal-plain-of-north-carolina/about
if (!file.exists(here::here("source_data", "Florence_flood_extent.zip"))) {
  options(timeout=100)
  download.file(
        "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A68ea0876-33ef-4223-80dd-f563c9c39efc",
        here::here("source_data",
                   "Florence_flood_extent.zip"),
        quiet = FALSE,
        mode = "w",
        method = "libcurl"
    )
  unzip("Florence_flood_extent.zip", "source_data")
  file.remove("Florence_flood_extent.zip")
}


### 1.4.2 Hurricane Matthew October 8th 2016 ----
if (!file.exists(here::here("source_data", "Matthew_flood_extent.zip"))) {
    download.file(
        "https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A68ea0876-33ef-4223-80dd-f563c9c39efc",
        here::here("source_data",
                   "Matthew_flood_extent.zip"),
        quiet = FALSE,
        mode = "w",
        method = "libcurl"
    )
    unzip("Matthew_flood_extent.zip", "source_data")
    file.remove("Matthew_flood_extent")
    
}

# 2. DATA VISUALIZATION ----

## 2.1 Fig 1. Plot of X and Y ----

m1 <- 
    leaflet(intercept_tidy) %>%
    addProviderTiles(providers$Stamen.TonerLines, group = "Basemap") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Basemap") %>%
    addMarkers(icon = list(
      iconUrl = 'https://icons.iconarchive.com/icons/icons-land/vista-map-markers/48/Map-Marker-Ball-Azure-icon.png',
      iconSize = c(20, 20)),
      label = paste("Facility:", intercept_tidy$name, "Capacity:", intercept_tidy$capacity,
                             "Type:", intercept_tidy$type,"Security Level:", intercept_tidy$securelvl,
                             "Flood Risk:", intercept_tidy$flood_risk, sep=" ")
    )

m1
## NOTE: I'd like to color the points by value maybe?
## Change the popup label to be multiple rows 
## Change basemap?

## 2.2 Fig 2. Top 10 Facilities table by flood risk, including capacity  ----
# Create a gt table
library(gt)

top_10_risk <- intercept_tidy %>%
  arrange(desc(flood_risk_rating)) %>%
  select(name, city, county, capacity, flood_risk) %>%
  head(10) %>%
  rename("Facility Name" = name,
         "City" = city,
         "County" = county,
         "Capacity" = capacity,
         "Flood Risk" = flood_risk) %>%
  gt() %>%
  tab_header(
    title = "Top 10 Facilites with Worse Flood Risk") %>%
  opt_row_striping() %>% 
  tab_options(row.striping.include_stub = TRUE, 
              row.striping.background_color = "gray")

top_10_risk 


## 2.3 Fig 3. Prison locations overlaid with Hurricane Florence extent ----
# Read in .tiff file of flood extent

# filter tiff to only display values of 1 (flooding)

# overlay with point data of prison locations

# flag facilities which overlap flooding area

# map prisions which were flooded including flooded population?

# this flow can be repeated for hurricane matthew 
# layers should be toggleable 


