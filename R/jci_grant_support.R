library(tidyverse)
library(sf)
library(tidygeocoder)

our_zips <- c("53204",
              "53209",
              "53215",
              "53216",
              "53218")


zcta <- readRDS("../strategic_regional_analysis/data/zcta_geometries.rda") %>%
  st_transform(., crs = st_crs(4326))

our_zips_geo <- zcta %>%
  filter(ZCTA5CE10 %in% our_zips)

nbds <- read_sf("../Shapefiles/Milwaukee/Neighborhoods/neighborhood.shp") %>%
  st_transform(crs = 4326)

city <- read_sf("../Shapefiles/Milwaukee/City Limits/citylimit.shp") %>%
  st_transform(crs = 4326)

westlawn <- rbind(c(43.116010, -87.986190),
                                  c(43.119315, -87.986190),
                                  c(43.119457, -87.996060),
                                  c(43.116010, -87.996060),
                                  c(43.116010, -87.986190))
westlawn <- tibble(
  y = c(43.116010, 43.119315, 43.119457, 43.116010, 43.116010),
  x = c(-87.986190, -87.986190, -87.996060, -87.996060, -87.986190)
) %>%
  st_as_sf(., coords = c("x", "y"), crs = 4326) %>%
  mutate(NEIGHBORHD = "WESTLAWN") %>%
  group_by(NEIGHBORHD) %>%
  summarise() %>%
  st_cast(., to = "POLYGON") 

focus <- c("HAVENWOODS",
           "THURSTON WOODS")

f_n <- nbds %>%
  filter(NEIGHBORHD %in% focus) %>%
  bind_rows(., westlawn)
  

geo_schools <- read_csv("../000_data_temp/geocoded_mke_schools.csv") %>%
  select(-student_count) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  left_join(., schools %>% filter(school_year == "2020-21") %>% select(dpi_true_id, school_name))

school_in_focus <- st_intersection(geo_schools, st_buffer(st_union(f_n), units::set_units(.5, mi))) 

# Schools

school_in_focus %>%
  ggplot() +
  geom_sf(size = .01, alpha = .5) +
  geom_sf(data = f_n, fill = "blue", alpha = .5, color = "blue", size = .1) + 
  geom_sf(data = city, fill = NA) +
  theme_void()

sif_export <- school_in_focus %>%
  as_tibble() %>%
  select(dpi_true_id, school_name)

write_csv(sif_export, "data/sif_export.csv")

# zips

oz <- our_zips_geo %>%
  filter(ZCTA5CE10 %in% c("53209", "53218"))

f_n %>%
  ggplot() +
  geom_sf(data = oz, fill = "red", color = "red", alpha = .5, size = .1) +
  geom_sf_text(data = oz, aes(label = ZCTA5CE10), size = 3) +
  geom_sf(fill = "blue", color = "blue", alpha = .5, size = .1) +
  geom_sf_text(aes(label = NEIGHBORHD), size = 2) +
  theme_void()

# FLI

parents <- read_csv("data/fli_participants.csv", lazy = FALSE) %>%
  rename(street = Address) %>%
  mutate(state = "WI",
         city = "Milwaukee")

p_geo <- geocode(parents, street = street, state = state, postalcode = `Zip/Postal`, method = "cascade")

p_geocoded <- p_geo %>%
  filter(!is.na(lat)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

p_in_focus <- st_intersection(p_geocoded, st_buffer(st_union(f_n), units::set_units(.5, mi)))

p_geocoded %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = city, fill = NA) +
  geom_sf(data = f_n, fill = "blue", alpha = .5, color = "blue", size = .1)
