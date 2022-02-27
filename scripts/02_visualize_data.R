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

# ## Creating labels with paragraph breaks
# intercept_tidy$labs <- paste(
#   paste0('<p>',
#          "Facility: ",
#          intercept_tidy$name),
#   '</p><p>',
#   paste0("Capacity: ",
#          intercept_tidy$capacity),
#   '</p><p>',
#   paste0("Type: ",
#          intercept_tidy$type),
#   '</p><p>',
#   paste0("Security Level: ",
#          intercept_tidy$securelvl),
#   '</p><p>',
#   paste0("Flood Risk: ",
#          intercept_tidy$flood_risk),
#   '</p>'
# )

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


### 1.3.2 Hurricane Florence Raster data ----
florence <-
  raster::raster(here::here("derived_data", "FloodExtentFlorence.tif"))

### 1.3.3 Hurricane Mathew Raster data ----
mathew <-
  raster::raster(here::here("derived_data", "FloodExtentMatthew.tif"))


### 1.3.4 Shapefiles ----
### Load shp
nc_shape <- 
  sf::st_read(here::here("derived_data", "CountyBoundary_SHP", "BoundaryCountyPolygon.shp"))

### Transform projection
nc_shape <- sf::st_transform(nc_shape, 4326)

### Merge facility sf data
nc_shape_df <- sf::st_join(nc_shape, intercept_sf , join = st_contains)

### Prepare Basemap
tmap_mode('plot')

nc_basemap <- tm_shape(nc_shape_df) +
  tm_polygons(
    col = "grey100",
    border.col = "grey",
    alpha = 0.05,
    border.alpha = 0.6
  ) +
  tm_style("white") +
  tm_layout(
    title = "North Carolina",
    title.size = 0.8,
    title.position = c("left", "TOP"),
    legend.show = FALSE,
    outer.margins = c(0, 0, 0, 0)
  ) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) 


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

# NOTE: To include this in an RMD file, create an r chunk and add in htmltools::includeHTML("figures/tab_1.html")


## Plot of top 10 points to add to the map
nc_basemap +
  tm_shape(top_10_risk_df) +
  tm_symbols(
    col = "Flood Risk",
    palette = "steelblue",
    scale = 1,
    shape = 21,
    border.col = "grey",
    border.lwd = 1,
    title.col = "Flood Risk"
  )


## 2.2 Fig 2. Static Plot of all locations with different basemaps for context ----
## Set tmap mode
tmap_mode('plot')

nc_basemap

## Add locations of prisons
nc_basemap +
  tm_shape(intercept_sf) +
  tm_symbols(
    col = "flood_risk_rating",
    palette = c(
      "#f2f8ff",
      "#e1ebf7",
      "#c7daf4",
      "#a8c0f3",
      "#79a6f6",
      "#5492ff",
      "#2d74da",
      "#1e57a4",
      "#234579",
      "#1d3458"
    ),
    scale = 1,
    shape = 21,
    border.col = "grey",
    border.lwd = 1,
    title.col = "Flood Risk Rating",
    legend.col.is.portrait = "FALSE"
  ) 


dev.copy(
  jpeg,
  width = 800,
  height = 500,
  unit = "px",
  quality = 100,
  here("figures", "fig1.jpg")
)
dev.off()
graphics.off()


## 2.3 Fig 3. Hurricane Mathew Extent ----
# ggplot(nc_shape) + geom_sf()
# 
# ## Drop values < 1
# mathew[mathew < 1] <- NA
# 
# ## decrease resolution
# mathew_lowres <- aggregate(mathew, fact = 20, fun = mean, expand = TRUE)
# 
# 
# ## Clip data to shapefile
# 
# 
# mathew_4326 <- projectRaster(mathew, crs = crs(nc_shape))
# mathew_nc <- crop(mathew,extent(nc_shape))
# 
# nc_shape %>% crs()
# 
# mathew %>% crs()



# filter tiff to only display values of 1 (flooding)

# overlay with point data of prison locations

# flag facilities which overlap flooding area

# map prisions which were flooded including flooded population?

# this flow can be repeated for hurricane matthew 
# layers should be toggleable 


## 2.4 Combined Interactive plot  ----
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

## With all flood risk data
interactive <- interactive + tm_shape(intercept_sf) +
  tm_symbols(
    col = "flood_risk_rating",
    palette = c(
      "#f2f8ff",
      "#e1ebf7",
      "#c7daf4",
      "#a8c0f3",
      "#79a6f6",
      "#5492ff",
      "#2d74da",
      "#1e57a4",
      "#234579",
      "#1d3458"
    ),
    scale = 1,
    shape = 21,
    border.col = "grey",
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
    palette = c(
      "#f2f8ff",
      "#e1ebf7",
      "#c7daf4",
      "#a8c0f3",
      "#79a6f6",
      "#5492ff",
      "#2d74da",
      "#1e57a4",
      "#234579",
      "#1d3458"
    ),
    scale = 1,
    shape = 21,
    border.col = "grey",
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
    palette = c(
      "#f2f8ff",
      "#e1ebf7",
      "#c7daf4",
      "#a8c0f3",
      "#79a6f6",
      "#5492ff",
      "#2d74da",
      "#1e57a4",
      "#234579",
      "#1d3458"
    ),
    scale = 1,
    size = "capacity",
    shape = 21,
    border.col = "grey",
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





## Save  figure
## tmap_save(interactive, filename = here("figures", "interactive.html"))
# NOTE: You should export this as an html from the panel.


