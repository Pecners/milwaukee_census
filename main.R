library(tidyverse)

zip_data <- read_csv("data/mke_zip_census_sex_age.csv")
names(zip_data)[3:100] <- str_replace(names(zip_data)[3:100], "B01001", "B01001_")

data_names <- read_csv("data/ACS2019_Table_Shells.csv")

for (i in 1:nrow(data_names)) {
  
  pattern <- data_names[i,] %>%
    .[[3]]
  
  replacement <- data_names[i,] %>%
    .[[4]]
  
  names(zip_data) <- str_replace_all(names(zip_data),
                                     pattern = pattern,
                                     replacement = replacement)
  
}

male <- zip_data %>%
  select("zcta" = name,
         5:52) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "male")

female <- zip_data %>%
  select("zcta" = name,
         53:100) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "female")

total_long <- bind_rows(male, female)

school_age <- c("5 to 9 years",
                "10 to 14 years",
                "15 to 17 years",
                "18 to 19 years")

totals_zip <- total_long %>%
  filter(group %in% school_age) %>%
  group_by(zcta) %>%
  summarise(total = sum(value))

source("rc_by_zip.R")

z <- zip_rc %>%
  filter(school_year == "2018-19" & overall_score >= 73.0) %>%
  group_by(zip) %>%
  summarise(quality_seats = sum(school_enrollment))

t <- left_join(totals_zip %>%
            mutate("zip" = as.character(zcta)) %>%
            select(zip, total), 
          z) %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(difference = total - quality_seats)

t %>%
  select(zip,
         "total_school_age_population" = total,
         quality_seats,
         difference) %>%
  write_csv(. , "../000_data_temp/quality_seats_by_zip.csv")

full_sf %>%
  left_join(., t) %>%
  ggplot(aes(fill = difference)) +
  geom_sf(color = "white") +
  theme_void(base_family = "serif") +
  scale_fill_continuous(labels = scales::comma) +
  labs(fill = "Difference",
       title = str_wrap("Difference between High Quality Seats and School-Aged Children by ZIP Code", 45),
       subtitle = "High-Quality = 4 or 5 stars; School-Aged = 5 to 19 years old",
       caption = "Sources: ACS 2019 5-Year and 2018-19 School Report Cards.")

make_mke_rc() %>%
  filter(school_year == "2018-19") %>%
  select(school_name,
         school_enrollment,
         accurate_agency_type) %>%
  write_csv(. , "../000_data_temp/school_enrollment_per_rc_201819.csv")
