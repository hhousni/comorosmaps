## code to prepare `DATASET` dataset goes here

# Comoro Islands shapefile come from the humanitarian data exchange webiste:
# https://data.humdata.org/dataset/cod-ab-com

if (!require("pacman")) install.packages("pacman")
p_load(sf, dplyr, readxl) # Necessary package to load shp files

# Load the data
admin0 <- st_read("data-raw/shp_files/com_admbnda_adm0_cosep_ocha_20191205.shp")
admin1 <- st_read("data-raw/shp_files/com_admbnda_adm1_cosep_ocha_20191205.shp")
admin2 <- st_read("data-raw/shp_files/com_admbnda_adm2_cosep_ocha_20191205.shp")

#comune level
#admin3 <- st_read("data-raw/shp_files/com_admbnda_adm3_cosep_ocha_20191205.shp")

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

#commune <- admin2 %>%
#  select ("name"=ADM2_EN,"adminCode"=ADM2_PCODE,geometry)

comoromaps_data <- rbind(country,island, prefecture) #comune

rm(admin0, admin1, admin2) #admin3
# Save the files

comoromaps_data <- sf::st_as_sf(tibble::as_tibble(comoromaps_data))



# for the cities
km_cities  <- read_excel("data-raw/shp_files/km.xlsx")

# 1.0 Data processing ----

km_cities_sf = st_as_sf(km_cities, coords = c("lng", "lat"),
                        crs = 4326, agr = "constant") %>%
  select(1,3,8)

# 2.0 Data output ----
comoromaps_data_cities <- sf::st_as_sf(tibble::as_tibble(km_cities_sf))
names(comoromaps_data_cities) <- names(comoromaps_data)

# 3.0 Join the two datasets

comoromaps_data <- rbind(comoromaps_data, comoromaps_data_cities)

usethis::use_data(comoromaps_data, internal = TRUE, overwrite = TRUE)




