## code to prepare `DATASET` dataset goes here

# Comoro Islands shapefile come from the humanitarian data exchange webiste:
# https://data.humdata.org/dataset/cod-ab-com

if (!require("pacman")) install.packages("pacman")
p_load(sf, dplyr, readxl) # Necessary package to load shp files

# Load the data
admin0 <- st_read("data-raw/shp_files/com_admbnda_adm0_cosep_ocha_20191205.shp")
admin1 <- st_read("data-raw/shp_files/com_admbnda_adm1_cosep_ocha_20191205.shp")
admin2 <- st_read("data-raw/shp_files/com_admbnda_adm2_cosep_ocha_20191205.shp")

# commune level
admin3 <- st_read("data-raw/shp_files/com_admbnda_adm3_cosep_ocha_20191205.shp")

# Remove unnecessary col

country <- admin0 %>%
  select (ADM0_EN,"adminCode"=ADM0_PCODE,geometry) %>%
  mutate("name"=c("Les Comores")) %>%
  select(name,adminCode,geometry)

island <- admin1 %>%
  select (ADM1_EN,"adminCode"=ADM1_PCODE,geometry) %>%
  mutate("name"=c("Anjouan","Grande Comore","Mohéli")) %>%
  select(name,adminCode,geometry)

prefecture <- admin2 %>%
  select ("name"=ADM2_EN,"adminCode"=ADM2_PCODE,geometry)

commune <- admin3 %>%
  select ("name"=ADM3_EN,"adminCode"=ADM3_PCODE,geometry)

comoromaps_data <- rbind(country, island, prefecture, commune)

rm(admin0, admin1, admin2, admin3)
# Save the files

comoromaps_data <- sf::st_as_sf(tibble::as_tibble(comoromaps_data))



# for the cities
km_cities  <- read_excel("data-raw/shp_files/km.xlsx")

# 1.0 Data processing ----
# Assign island-level adminCode so cities can be filtered per island:
#   KM1c = Anjouan cities, KM2c = Grande Comore cities, KM3c = Mohéli cities
island_code_map <- c("Grande Comore" = "KM2c", "Anjouan" = "KM1c", "Mohéli" = "KM3c")

km_cities_sf <- km_cities %>%
  distinct(city, admin_name, .keep_all = TRUE) %>%          # remove duplicates
  filter(!is.na(admin_name)) %>%                             # keep only mapped islands
  mutate(adminCode = island_code_map[admin_name]) %>%
  filter(!is.na(adminCode)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, agr = "constant") %>%
  select(name = city, adminCode, geometry)

# 2.0 Data output ----
comoromaps_data_cities <- sf::st_as_sf(tibble::as_tibble(km_cities_sf))

# 3.0 Join the two datasets

comoromaps_data <- rbind(comoromaps_data, comoromaps_data_cities)

usethis::use_data(comoromaps_data, internal = TRUE, overwrite = TRUE)




