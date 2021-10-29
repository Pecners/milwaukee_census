library(tidyverse)
library(tidycensus)

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
                            county = "Milwaukee") %>%
  filter(GEOID %in% hud_tracts$GEOID)

calc_tract_demo <- tract_demo %>%
  filter(!str_detect(variable, "Total")) %>%
  group_by(GEOID) %>%
  mutate(perc = value / sum(value)) %>%
  arrange(GEOID) %>%
  select(-value) %>%
  pivot_wider(names_from = variable, values_from = perc)

# Tract under 18

calc_tract_under <- tract_demo %>%
  filter(str_detect(variable, "Total")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(`Under 18` = Total - `Total 18+`)

all <- left_join(calc_tract_demo, calc_tract_under)

write_csv(all, "data/hud_tract_demographics.csv")
