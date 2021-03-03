library(tidyverse)

t <- "B17001"

zip_data <- read_csv("data/mke_zip_census_income_age.csv")

names(zip_data)[3:length(zip_data)] <- str_replace(names(zip_data)[3:length(zip_data)], t, paste0(t, "_"))

data_names <- read_csv("data/ACS2019_Table_Shells.csv") %>%
  filter(`Table ID` == t & !is.na(Line)) %>%
  group_by(unique_id)

for (i in 1:nrow(data_names)) {
  
  pattern <- data_names[i,] %>%
    .[[3]]
  
  replacement <- data_names[i,] %>%
    .[[4]]
  
  names(zip_data) <- str_replace_all(names(zip_data),
                                     pattern = pattern,
                                     replacement = replacement)
  
}

male_ed <- zip_data %>%
  select("zcta" = name,
         9:34) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "male",
         poverty = TRUE)

female_ed <- zip_data %>%
  select("zcta" = name,
         37:62) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "female",
         poverty = TRUE)

male <- zip_data %>%
  select("zcta" = name,
         67:92) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "male",
         poverty = FALSE)

female <- zip_data %>%
  select("zcta" = name,
         95:120) %>%
  pivot_longer(cols = -1, names_to = "group", values_to = "value") %>%
  mutate(sex = "female",
         poverty = FALSE)
  
df <- bind_rows(male_ed, female_ed, male, female)
  


school_age <- c("Under 5 years",
                "5 years",
                "6 to 11 years",
                "12 to 14 years",
                "15 years",
                "16 to 17 years")

totals_zip <- df %>%
  filter(group %in% school_age & poverty == TRUE) %>%
  group_by(zcta) %>%
  summarise(total = sum(value)) %>%
  ungroup() %>%
  mutate(perc = total / sum(total)) %>%
  modify_at("zcta", as.character)

source("rc_by_zip.R")

tz <- left_join(totals_zip %>%
                  mutate("zip" = as.character(zcta)) %>%
                  select(zip, perc_pov = perc,
                         pov_count = total), 
                ed_zip_rc) %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(diff = pov_count - ed)
  


full_sf %>%
  left_join(., tz) %>%
  ggplot(aes(fill = perc_pov)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_c(option = "plasma", breaks = c(max(tz$perc_pov) * .05, max(tz$perc_pov) * .95),
                       labels = c("Fewer Kids in Poverty", "More Kids in Poverty")) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 14))
