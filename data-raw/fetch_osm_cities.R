library(osmdata)
library(sf)
library(dplyr)

# Bounding box for Comoros: xmin, ymin, xmax, ymax
# (43.22, -12.43, 44.55, -11.36)

q <- opq(bbox = c(43.22, -12.43, 44.55, -11.36)) %>%
  add_osm_feature(key = "place", value = c("city", "town", "village", "hamlet", "suburb"))

cat("Querying OpenStreetMap Overpass API...\n")
result <- osmdata_sf(q)
pts <- result$osm_points

cat("Total OSM place nodes found:", nrow(pts), "\n")

# Extract relevant columns
cities_osm <- pts %>%
  filter(!is.na(name)) %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(name, place, lon, lat) %>%
  arrange(name)

cat("\nPlace type breakdown:\n")
print(table(cities_osm$place))

cat("\nAll places:\n")
print(cities_osm, n = 200)
