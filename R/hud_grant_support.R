library(tidyverse)
library(tidycensus)
library(sf)

# Get MKE Census Tract 2020 Counts for:
# - Overall Demographics for each tract
# - Under 18 population for each tract

dec_vars_20 <- load_variables(year = 2020, dataset = "pl")

demo_vars <- c("Total" = "P2_001N",
               "Total 18+" = "P4_004N",
               "Hispanic" = "P2_002N",
               "White" = "P2_005N",
               "Black" = "P2_006N",
               "American Indian" = "P2_007N",
               "Asian" = "P2_008N",
               "Native Hawaiian" = "P2_009N",
               "Other Race" = "P2_010N",
               "Two or More" = "P2_011N")

# Limit to top hud tracts
# List located here: https://www.huduser.gov/portal/qct/1statetable.html?statefp=55.0&DDAYEAR=2022

links <- read_delim("data/hud_tract_links.txt", col_names = FALSE)

hud_tracts <- links %>%
  transmute(GEOID = str_extract(X3, "(?<=locate\\=)\\d{11}"))

# Tract demographics

tract_demo <- get_decennial(geography = "tract", 
                            variables = demo_vars,
                            year = 2020,
                            state = "WI", 
                            county = "Milwaukee",
                            geometry = TRUE) %>%
  filter(GEOID %in% hud_tracts$GEOID)

our_zips <- c("53204",
              "53209",
              "53216",
              "53218")

city_limits <- st_read("../Shapefiles/Milwaukee/City Limits/citylimit.shp") %>%
  st_transform(., crs = st_crs(4326))

zcta <- readRDS("../strategic_regional_analysis/data/zcta_geometries.rda") %>%
  st_transform(., crs = st_crs(4326))

our_zips_geo <- zcta %>%
  filter(ZCTA5CE10 %in% our_zips)


tract_geo <- tract_demo %>%
  select(GEOID, geometry) %>%
  st_transform(., crs = st_crs(4326)) %>%
  st_intersection(., our_zips_geo)


td_our_zips <- tract_demo %>%
  filter(GEOID %in% tract_geo$GEOID) %>%
  select(GEOID, geometry) %>%
  unique() 
  
calc_tract_demo <- td_our_zips %>%
  as_tibble() %>%
  filter(!str_detect(variable, "Total")) %>%
  group_by(GEOID) %>%
  mutate(perc = value / sum(value)) %>%
  arrange(GEOID) %>%
  select(-value) %>%
  pivot_wider(names_from = variable, values_from = perc)

# Tract under 18

calc_tract_under <- td_our_zips %>%
  as_tibble() %>%
  filter(str_detect(variable, "Total")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Under 18` = Total - `Total 18+`)

all <- left_join(calc_tract_demo, calc_tract_under)

write_csv(all, "data/hud_tract_demographics_our_zips.csv")

# Visual check of ZIPs in city

city_limits %>%
  ggplot() +
  geom_sf(data = our_zips_geo, fill = "yellow", color = "yellow", alpha = 0.5, size = .1) +
  
  geom_sf(data = td_our_zips, fill = "red", color = "red", alpha = 0.5, size = .1) +
  
  geom_sf(fill = "blue", color = "blue", alpha = .1, size = .1) +
  
  
  theme_void()

