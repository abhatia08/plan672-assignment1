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
here::i_am("scripts/02_visualize_data")
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
  mutate(securelvl = as.factor(securelvl)) %>% 
  mutate(status = as.factor(status)) %>% 
  mutate(flood_risk_rating = as.factor(flood_risk_rating)) %>% 
  mutate(flood_risk = as.factor(flood_risk)) %>% 
  mutate(confidence = as.factor(confidence))



### 1.3.2 Hurricane Florence Raster data ----
florence_raster <-
  raster::raster(here::here("derived_data", "FloodExtentFlorence.tif"))

### 1.3.3 Hurricane Mathew Raster data ----
mathew_raster <-
  raster::raster(here::here("derived_data", "FloodExtentMatthew.tif"))


### 1.3.4 Shapefiles ----
### Load shp
nc_shape <- 
    sf::st_read(here::here("derived_data", "CountyBoundary_SHP", "BoundaryCountyPolygon.shp"))

### Transform projection
nc_shape <- sf::st_transform(nc_shape, 4326)

### Merge facility sf data
nc_shape_df <- sf::st_join(nc_shape, intercept_sf , join = st_contains)





# 2. DATA VISUALIZATION ----

## 2.1 Fig 1. Plot of all locations (leaflet)----

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
## Create a gt table

# Clean Facility Names from all uppercase to only first letter capitalized
intercept_tidy$name <-intercept_tidy$name %>%
  casefold(upper = FALSE) %>%
  str_to_title() 
# Repeat for City 
intercept_tidy$city <-intercept_tidy$city %>%
  casefold(upper = FALSE) %>%
  str_to_title() 
# Repeat for County
intercept_tidy$county <-intercept_tidy$county %>%
  casefold(upper = FALSE) %>%
  str_to_title() 
  

top_10_risk <- intercept_tidy %>%
  arrange(desc(flood_risk_rating)) %>%
  dplyr::select(name, city, county, capacity, flood_risk) %>%
  head(10) %>%
  rename(
    "Facility Name" = name,
    "City" = city,
    "County" = county,
    "Capacity" = capacity,
    "Flood Risk" = flood_risk
  ) %>%
  gt() %>%
  tab_header(title = "Top 10 Facilites with the Worst Flood Risk") %>%
  opt_row_striping() %>%
  gt::tab_options(row.striping.include_stub = TRUE,
              row.striping.background_color = "gray")

## View Table
top_10_risk 

## NOTE: Change for aesthetics, background color, maybe more?

## Export table to figures directory
gt::gtsave(top_10_risk, path =
             "figures", "tab_1.png")

## NOTE: Maybe include a map of these top_10 locations?


## 2.3 Fig 3. Static Plot of all locations with different basemaps for context ----
## Set tmap mode
tmap_mode('plot')


## Create basemap 
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

nc_basemap

## Add locations of prisons
nc_basemap +
  tm_shape(intercept_sf) +
  tm_dots() +  # it seems like I needed to add this line to plot correctly 
  dev.copy(
    jpeg,
    width = 800,
    height = 500,
    unit = "px",
    quality = 100,
    here("figures", "fig3.jpg")
  )
dev.off()
graphics.off()




## 2.4 Fig 4. Prison locations overlaid with Hurricane Florence extent ----
# tm_shape(florence_raster) +
#   tm_raster() +
#   tm_layout(legend.outside = TRUE)


# tm_shape(tucker_hs)+
#   tm_raster(palette="-Greys", style="cont", legend.show=FALSE)+
#   tm_shape(tucker_dem)+
#   tm_raster(alpha=.6, palette="cividis", style="cont", title="Elevation (m)")+
#   tm_compass(type="arrow", position=c(.15, .05))+
#   tm_scale_bar(position = c(0.2, .005), text.size=.8)+
#   tm_layout(title = "Tucker County Elevation", title.size = 1.5, title.position = c("right", "top"))+
#   tm_credits("Data from NED", position= c(.87, .03))+
#   tm_layout(legend.position= c("left", "bottom"))


# filter tiff to only display values of 1 (flooding)

# overlay with point data of prison locations

# flag facilities which overlap flooding area

# map prisions which were flooded including flooded population?

# this flow can be repeated for hurricane matthew 
# layers should be toggleable 


## EXTRA ----
## Set tmap mode
# tmap_mode('view')
# 
# tm_shape(nc_shape) +
#   tm_polygons(col = "grey100", border.col = "grey", alpha = 0.05, border.alpha = 0.6) +
#   tm_layout(title = "North Carolina", title.position = c("left", "TOP")) +
#   tm_basemap("OpenStreetMap", alpha = 0.5)


## 2.5 Interactive osm plot  ----
## Set tmap mode
tmap_mode('view')

nc_osm <- tm_shape(nc_shape) +
  tm_polygons(
    col = "grey100",
    border.col = "black",
    alpha = 0.05,
    border.alpha = 0.2
  ) +
  tm_layout(
    title = "North Carolina",
    title.position = c("left", "TOP")
  ) +
  tm_basemap("OpenStreetMap", alpha = 0.5)

## with flood risk data
fig5 <- nc_osm + tm_shape(intercept_sf) +
  tm_symbols(
    col = "flood_risk_rating",
    palette = c(
      "#70b8ff",
      "#429bfa",
      "#147df5",
      "#095dd7",
      "#0000ff",
      "#0000b8",
      "#00008f",
      "#000079",
      "#000052",
      "#00003d"
    ),
    scale = 1,
    shape = 21,
    border.col = "grey",
    border.lwd = 1,
    title.col = "Flood Risk Rating",
    legend.col.is.portrait = "FALSE"
  ) +
  tm_view(view.legend.position = c("right", "bottom")) 

## Save static figure
tmap_save(fig5, filename = here("figures", "fig5.html"))