library(tidycensus)
library(sf)
library(tigris)

# 2020 Blocks

# bs <- st_read("../Shapefiles/tl_2020_55_tabblock20/tl_2020_55_tabblock20.shp")

wards <- st_read("../Shapefiles/Milwaukee/wards/ward.shp")


# 2020 Block Group Data


vd <- get_decennial(geography = "voting district",
                             year = 2020,
                             variables = c("P1_001N", "P3_001N"),
                             state = "WI",
                             county = "Milwaukee",
                             show_call = TRUE) 


vv_dpi <- vd %>%
  filter(str_detect(NAME, "^Milwaukee - C")) %>%
  mutate(WARD = str_extract(NAME, "\\d{4}"),
         WARD = str_remove_all(WARD, "^0*")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = P1_001N - P3_001N)

v_wards_dpi <- left_join(wards, vv_dpi) %>%
  st_transform(., st_crs(zip_mke_sf))

adj_zip <- map_df(1:nrow(v_wards_dpi), function(x) {
  # Isolate ward
  wrd <- v_wards_dpi[x,]
  
  # Determine intersecting ZCTAs
  zip_ward <- st_intersection(zip_mke_sf, wrd)
  zw <- zip_mke_sf %>%
    filter(ZCTA5CE10 %in% zip_ward$ZCTA5CE10)
  
  if (nrow(zw) == 1) {
    cat(crayon::cyan(paste0("If:   ", wrd[["WARD"]], "\n")))
    t <- tibble(
      ward = wrd[[1]],
      zip = zw[[1]],
      alloc = 1,
      est = wrd[["value"]]
    )
    return(t)
  } else {
    cat(crayon::cyan(paste0("Else: ", wrd[["WARD"]], "\n")))
    #Map over ZCTAs
    temp_ <- map_df(1:nrow(zw), function(y, w = wrd) {
      z <- zw[y,]
      ward_in_zip <- st_intersection(w, z)
      diff <- as.numeric(st_area(ward_in_zip) / st_area(w))
      
      t <- tibble(
        ward = w[[1]],
        zip = z[[1]],
        value = w[[1, "value"]],
        alloc = diff,
        est = round(w[[1, "value"]] * diff)
      )
    })
    
    if (sum(temp_$alloc) < 1) {
      rem <- 1 - sum(temp_$alloc)
      ind <- which(temp_[["alloc"]] == max(temp_[["alloc"]]))
      new_alloc <- rem + temp_[[ind, "alloc"]]

      
      temp_[ind, "alloc"] <- new_alloc
      temp_[ind, "est"] <- round(new_alloc * temp_[[ind, "value"]])
        
    }
    return(temp_)
  }
})


az_totals <- adj_zip %>%
  mutate(zip = as.numeric(zip)) %>%
  group_by(zip) %>%
  summarise(estimate = sum(est, na.rm = TRUE)) %>%
  mutate(zip = as.numeric(zip))

dec_total <- sum(adj_zip_totals$est_under18_total)

acs2019_under18 <- get_acs(geography = "zcta", state = "WI", year = 2019, variables = "B09001_001") %>%
  transmute(zip = as.numeric(GEOID),
            estimate = estimate)

acs_pl_combined <- left_join(adj_zip_totals, acs2019_under18) %>%
  mutate(diff = est_under18_total - estimate)

place_acs_under18 <- get_acs(geography = "place", state = "WI", year = 2019, variables = "B09001_001")

total_acs_under18 <- place_acs_under18 %>%
  filter(NAME == "Milwaukee city, Wisconsin") %>%
  .[[4]]

acs2019_off <- (total_acs_under18 - dec_total) / dec_total

acs2019_1819 <- get_acs(geography = "zcta", state = "WI", year = 2019, variables = c("B01001_007", "B01001_031"))

acs_18_19 <- acs2019_1819 %>%
  group_by(zip = GEOID) %>%
  summarise(total = sum(estimate)) %>%
  ungroup() %>%
  mutate(adj_total = round(total * (1 - acs2019_off))) %>%
  select(zip,
         estimate = adj_total)

adj_zip_totals <- bind_rows(az_totals, acs_18_19 %>% mutate(zip = as.numeric(zip))) %>%
  group_by(zip) %>%
  summarise(estimate = sum(estimate))

apc_sf <- left_join(acs_pl_combined, zip_mke_sf %>%
                      mutate(zip = as.numeric(ZCTA5CE10)))

apc_sf %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = diff))

