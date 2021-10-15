library(wisconsink12)
library(sf)

setwd("../state_of_mke_ed")

public <- read_csv("../sf_contact_update/school_directories/public.csv") 
names(public) <- str_replace_all(str_to_lower(names(public)), " ", "_")
private <- read_csv("../sf_contact_update/school_directories/private.csv")
names(private) <- str_replace_all(str_to_lower(names(private)), " ", "_")

pub_mke <- public %>%
  modify_if(is.numeric, as.character) %>%
  rename("school_name" = public_school_name) %>%
  mutate(dpi_true_id = paste(str_pad(lea_code, width = 4, "left", "0"), str_pad(school_code, width = 4, "left", "0"), sep = "_"))

priv_mke <- private %>%
  modify_if(is.numeric, as.character) %>%
  rename("school_name" = private_school_name) %>%
  mutate(dpi_true_id = paste("0000", str_pad(school_code, width = 4, "left", "0"), sep = "_"))


full_dir <- bind_rows(pub_mke, priv_mke)

clean_zip <- full_dir %>%
  mutate(zip_4 = str_extract(physical_address, "\\d{5}$|\\d{5}-\\d{4}$"),
         zip = ifelse(str_length(zip_4) == 5, zip_4, str_extract(zip_4, "^\\d{5}"))) %>%
  select(dpi_true_id, zip)



zip_sf <- st_read("shapefiles/zcta/cb_2018_us_zcta510_500k.shp")
mke_sf <- st_read("shapefiles/Milwaukee/citylimit.shp")


mke_sf <- st_transform(mke_sf, crs = st_crs(zip_sf))

full_sf <- st_intersection(zip_sf, mke_sf)

zip_mke_sf <- zip_sf %>%
  filter(ZCTA5CE10 %in% full_sf$ZCTA5CE10) %>%
  st_transform(., crs = st_crs(mke_sf))

names(full_sf)[1] <- "zip"

z <- clean_zip %>%
  filter(zip %in% full_sf$zip)

wi_rc <- make_wi_rc(exclude_milwaukee = FALSE)

zip_rc <- left_join(wi_rc, z)

mke_rc <- make_mke_rc()

per_zip_in_city <- map_dbl(1:nrow(full_sf), function(x) {
  
  zip <- full_sf[[x, "zip"]]
  
  o <- zip_sf %>%
    filter(ZCTA5CE10 == zip)
  
  one_zip <- st_intersection(o, mke_sf)
  
  per_zip_in_city <- as.numeric(st_area(one_zip) / st_area(o))
  
  return(per_zip_in_city)
  
})

full_sf <- bind_cols(full_sf, "per_zip_in_city" = per_zip_in_city)

setwd("../milwaukee_census")
