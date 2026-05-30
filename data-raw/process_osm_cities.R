library(jsonlite)
library(dplyr)
library(sf)
library(readxl)
library(writexl)

# ---------- 1. Parse OSM JSON ----------
d <- fromJSON("/tmp/osm_result.json", simplifyVector = FALSE)
elems <- d$elements

# Extract fields
osm_df <- do.call(rbind, lapply(elems, function(e) {
  tags <- e$tags
  name_raw <- tags$name
  if (is.null(name_raw) || name_raw == "") return(NULL)
  data.frame(
    name_raw = name_raw,
    place    = if (is.null(tags$place)) NA_character_ else tags$place,
    lat      = e$lat,
    lng      = e$lon,
    stringsAsFactors = FALSE
  )
}))

cat("Nodes with names:", nrow(osm_df), "\n")

# ---------- 2. Clean names ----------
# Many names are "French Arabic" or "Arabic French" — keep Latin characters only
clean_latin <- function(x) {
  trimws(gsub("[\\p{Arabic}]+", "", x, perl = TRUE))
}
osm_df$name <- clean_latin(osm_df$name_raw)
# Remove any remaining numeric/punctuation artifacts
osm_df$name <- trimws(gsub("\\s{2,}", " ", osm_df$name))
# Drop if name is now empty
osm_df <- osm_df[nchar(osm_df$name) > 0, ]

cat("After name cleaning:", nrow(osm_df), "\n")

# ---------- 3. Assign island by longitude ----------
# Grande Comore: lon < 43.60
# Mohéli:        43.60 <= lon < 44.10
# Anjouan:       lon >= 44.10
assign_island <- function(lng) {
  ifelse(lng < 43.60, "Grande Comore",
    ifelse(lng < 44.10, "Mohéli", "Anjouan"))
}
osm_df$admin_name <- assign_island(osm_df$lng)

cat("\nIsland breakdown from OSM:\n")
print(table(osm_df$admin_name))

# ---------- 4. Merge with existing km.xlsx ----------
km_existing <- read_excel("data-raw/shp_files/km.xlsx")
cat("\nExisting km.xlsx rows:", nrow(km_existing), "\n")
cat("Existing columns:", paste(names(km_existing), collapse=", "), "\n")

# Normalise column names to lowercase for comparison
osm_clean <- osm_df %>%
  select(city = name, admin_name, lat, lng) %>%
  distinct(city, admin_name, .keep_all = TRUE)

# Check which columns km_existing uses for city and island
# Assume: city, admin_name, lat, lng  (from the earlier data-raw work)
# Keep only matching schema
km_new <- osm_clean[!tolower(osm_clean$city) %in% tolower(km_existing$city), ]
cat("\nNew OSM cities not in km.xlsx:", nrow(km_new), "\n")
print(km_new)

# ---------- 5. Combine and save ----------
km_combined <- bind_rows(km_existing, km_new) %>%
  distinct(city, admin_name, .keep_all = TRUE) %>%
  arrange(admin_name, city)

cat("\nFinal combined rows:", nrow(km_combined), "\n")
cat("By island:\n")
print(table(km_combined$admin_name))

write_xlsx(km_combined, "data-raw/shp_files/km.xlsx")
cat("\nSaved km.xlsx with", nrow(km_combined), "rows\n")
