library(sf); library(dplyr); library(readxl); library(tibble); library(usethis)

admin0 <- st_read("data-raw/shp_files/com_admbnda_adm0_cosep_ocha_20191205.shp", quiet=TRUE)
admin1 <- st_read("data-raw/shp_files/com_admbnda_adm1_cosep_ocha_20191205.shp", quiet=TRUE)
admin2 <- st_read("data-raw/shp_files/com_admbnda_adm2_cosep_ocha_20191205.shp", quiet=TRUE)
admin3 <- st_read("data-raw/shp_files/com_admbnda_adm3_cosep_ocha_20191205.shp", quiet=TRUE)

country    <- admin0 %>% select(ADM0_EN, adminCode=ADM0_PCODE, geometry) %>% mutate(name="Les Comores") %>% select(name, adminCode, geometry)
island     <- admin1 %>% select(ADM1_EN, adminCode=ADM1_PCODE, geometry) %>% mutate(name=c("Anjouan","Grande Comore","Mohéli")) %>% select(name, adminCode, geometry)
prefecture <- admin2 %>% select(name=ADM2_EN, adminCode=ADM2_PCODE, geometry)
commune    <- admin3 %>% select(name=ADM3_EN, adminCode=ADM3_PCODE, geometry)

comoromaps_data <- rbind(country, island, prefecture, commune)
comoromaps_data <- sf::st_as_sf(tibble::as_tibble(comoromaps_data))

km_cities <- read_excel("data-raw/shp_files/km.xlsx")
island_code_map <- c("Grande Comore"="KM2c", "Anjouan"="KM1c", "Mohéli"="KM3c")

km_cities_sf <- km_cities %>%
  distinct(city, admin_name, .keep_all=TRUE) %>%
  filter(!is.na(admin_name)) %>%
  mutate(adminCode = island_code_map[admin_name]) %>%
  filter(!is.na(adminCode)) %>%
  st_as_sf(coords=c("lng","lat"), crs=4326, agr="constant") %>%
  select(name=city, adminCode, geometry)

comoromaps_data_cities <- sf::st_as_sf(tibble::as_tibble(km_cities_sf))
comoromaps_data <- rbind(comoromaps_data, comoromaps_data_cities)

cat("Total features:", nrow(comoromaps_data), "\n")
cat("City counts by island:\n")
pts <- comoromaps_data[sf::st_geometry_type(comoromaps_data) == "POINT", ]
print(table(pts$adminCode))

usethis::use_data(comoromaps_data, internal=TRUE, overwrite=TRUE)
cat("Done.\n")
