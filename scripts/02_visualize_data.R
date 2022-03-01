# 92. Data Visualization

# 1. SETUP ----

## 1.1 Load required packages ----
## Loading packages via pacman and installing if needed

if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(here,
               gt,
               leaflet,
               maps,
               OpenStreetMap,
               raster,
               sf,
               spData,
               stars,
               tidyverse,
               tmap,
               tmaptools)

## 1.2 Declare `here` ----
here::i_am("scripts/02_visualize_data.R")

setwd(here())


## 1.3 Load datasets ----
## If the clean data file has been generated, load it. Else, run the cleaning script first

### 1.3.1 Intercept Data ----
if (!file.exists(here::here("derived_data", "intercept_tidy.csv"))) {
  source(here::here("scripts", "01_clean_data.R"))
} else {
  intercept_tidy <-
    read_csv(here::here("derived_data", "intercept_tidy.csv"))
}


### Create an sf_df out of intercept data
intercept_sf = sf::st_as_sf(intercept_tidy, coords = c("longitude", "latitude"), crs = 4326)

### Convert strings to factors
intercept_sf <-
  intercept_sf %>% mutate(type = as.factor(type)) %>% 
  mutate(type = factor(type, levels=c('County', 'State', 'Federal'))) %>%
  mutate(securelvl = as.factor(securelvl)) %>%
  mutate(securelvl = factor(securelvl, levels = c('Not Available', 'Juvenile', 'Medium', 'Close', 'Maximum'))) %>%
  mutate(status = as.factor(status)) %>% 
  mutate(status = factor(status, levels = c('Not Available', 'Closed', 'Open'))) %>% 
  mutate(flood_risk_rating = as.factor(flood_risk_rating)) %>% 
  mutate(flood_risk = as.factor(flood_risk)) %>% 
  mutate(flood_risk = factor(flood_risk, levels = c('Not Available', 'Minimal', 'Low', 'Moderate', 'Major', 'Severe', 'Extreme'))) %>% 
  mutate(confidence = as.factor(confidence)) %>%
  mutate(confidence = factor(confidence, levels = c('Low', 'Medium', 'Medium-High', 'High')))


# ### 1.3.2 Hurricane Florence Raster data ----
# florence <-
#   raster::raster(here::here("derived_data", "FloodExtentFlorence.tif"))
# 
# ### 1.3.3 Hurricane Mathew Raster data ----
# mathew <-
#   raster::raster(here::here("derived_data", "FloodExtentMatthew.tif"))


### 1.3.4 Shapefiles ----
### Load shp
nc_shape <- 
  sf::st_read(here::here("derived_data", "CountyBoundary_SHP", "BoundaryCountyPolygon.shp"))

### Transform projection
nc_shape <- sf::st_transform(nc_shape, 4326)

### Merge facility sf data
nc_shape_df <- sf::st_join(nc_shape, intercept_sf , join = st_contains)

# 2. DATA VISUALIZATION ----

## 2.1 Table 1. Top 10 Facilities table by flood risk, including capacity  ----
## Create a top 10 df
top_10_risk_df <- intercept_sf %>%
  arrange(desc(flood_risk_rating)) %>%
  dplyr::select(name, city, county, capacity, flood_risk) %>%
  dplyr::mutate(capacity = as.character(capacity)) %>%
  head(10) %>%
  rename(
    "Facility Name" = name,
    "City" = city,
    "County" = county,
    "Capacity" = capacity,
    "Flood Risk" = flood_risk
  )

## Fix the "FPC" string
top_10_risk_df$`Facility Name`[top_10_risk_df$`Facility Name` == "Fpc Seymour Johnson"] <- "FPC Seymour Johnson"
## Replace NA values with "-"
top_10_risk_df$`Capacity`[is.na(top_10_risk_df$`Capacity`)] <- "-"

## Create gt table
tab1 <- sf::st_drop_geometry(top_10_risk_df) %>%
  gt() %>%
  tab_header(title = "Top 10 Facilites with the Worst Flood Risk") %>%
  opt_row_striping() %>%
  gt::tab_options(
    row.striping.include_stub = TRUE,
    row.striping.background_color = "gray97",
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent"
  ) %>%
  cols_align(align = "right", columns = "Capacity") %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(3)),
      cell_text(weight = "bold")
    )
  ) %>% opt_table_font(font = list(google_font("Chivo"),
                                   default_fonts())) %>%
  tab_style(locations = cells_title(groups = "title"),
            style = list(cell_text(weight = "bold", size = 24))) %>%
  tab_source_note(source_note = "Data: The Intercept- Climate and Punishment (2022) [via GitHub]")

## View Table
tab1

## Export table to figures directory
gt::gtsave(tab1, path =
             "figures", "tab_1.html")


## 2.2 Combined Interactive plot  ----
## Set tmap mode
tmap_mode('view')

## Adding base layers
interactive <-
  tm_basemap(leaflet::providers$CartoDB.Positron, group = "Default") +
  tm_basemap(leaflet::providers$Esri.WorldTopoMap, group = "Topography") +
  tm_basemap(leaflet::providers$OpenStreetMap.Mapnik, group = "Streets") +
  tm_layout(title = "North Carolina",
            title.position = c("left", "TOP")) +
  tm_shape(nc_shape) +
  tm_polygons(
    col = "grey100",
    border.col = "black",
    alpha = 0.05,
    border.alpha = 0.2,
    group = "NC Boundaries",
    interactive = FALSE
  ) +
  tm_view(
    view.legend.position = c("right", "bottom"),
    symbol.size.fixed = FALSE
  )

## Blues
# fill_colors <- c(
#   "#f2f8ff",
#   "#e1ebf7",
#   "#c7daf4",
#   "#a8c0f3",
#   "#79a6f6",
#   "#5492ff",
#   "#2d74da",
#   "#1e57a4",
#   "#234579",
#   "#1d3458"
# )

## Generate Oranges from Mad's color palette

fill_colors <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"), alpha = FALSE)(10)


## With all flood risk data
interactive <- interactive + tm_shape(intercept_sf) +
  tm_symbols(
    col = "flood_risk_rating",
    palette = fill_colors,
    scale = 1,
    shape = 21,
    border.col = "#540000",
    border.lwd = 1,
    title.col = "Flood Risk Rating",
    group = "All Facility Locations (Points)",
    id = "name",
    popup.vars = c(
      "City" = "city",
      "County" = "county",
      "Capacity" = "capacity",
      "Flood Risk" = "flood_risk",
      "Flood Risk Rating" = "flood_risk_rating"
    )
  )

## With top 10 facilities
interactive <- interactive + tm_shape(top_10_risk_df) +
  tm_symbols(
    col = "Flood Risk",
    palette = fill_colors,
    scale = 1,
    shape = 21,
    border.col = "#540000",
    border.lwd = 1,
    title.col = "Flood Risk",
    group = "Top 10 At-Risk Facility Locations (Points)",
    id = "name",
    popup.vars = c(
      "City" = "City",
      "County" = "County",
      "Capacity" = "Capacity",
      "Flood Risk" = "Flood Risk"
    )
  )


## With capacity size for  facilities
interactive <- interactive + tm_shape(intercept_sf) +
  tm_symbols(
    col = "flood_risk_rating",
    palette = fill_colors,
    scale = 1,
    size = "capacity",
    shape = 21,
    border.col = "#540000",
    border.lwd = 1,
    title.col = "Flood Risk Rating",
    group = "Facility Capacity",
    id = "name",
    popup.vars = c(
      "City" = "city",
      "County" = "county",
      "Capacity" = "capacity",
      "Flood Risk" = "flood_risk",
      "Flood Risk Rating" = "flood_risk_rating"
    )
  )


## Generate interactive plot
interactive %>% tmap_leaflet() %>% addLayersControl(
  baseGroups = c("Default", "Topography", "Streets"),
  overlayGroups = c(
    "All Facility Locations (Points)",
    "Top 10 At-Risk Facility Locations (Points)",
    "Facility Capacity"
  ),
  options = layersControlOptions(collapsed = FALSE)
) %>% hideGroup("Top 10 At-Risk Facility Locations (Points)") %>% 
  hideGroup("Facility Capacity")






