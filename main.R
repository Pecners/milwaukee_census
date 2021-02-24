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

school_age <- c("Under 5 years",
                "5 to 9 years",
                "10 to 14 years",
                "15 to 17 years",
                "18 to 19 years")

totals_zip <- total_long %>%
  filter(group %in% school_age) %>%
  group_by(zcta, group) %>%
  summarise(total = sum(value))

source("rc_by_zip.R")

all_w_zip <- zip_rc %>%
  filter(school_year == "2018-19") %>%
  select(school_year:accurate_agency_type,
         overall_score:overall_rating,
         school_enrollment,
         zip) %>%
  filter(!is.na(zip)) %>%
  write_csv(., "Schools with RC and ZIP.csv")

z <- zip_rc %>%
  filter(school_year == "2018-19" & overall_score >= 73.0) %>%
  group_by(zip) %>%
  summarise(quality_seats = sum(school_enrollment))

t <- left_join(totals_zip %>%
            mutate("zip" = as.character(zcta)) %>%
            select(zip, total), 
          z) %>%
  mutate_all(replace_na, replace = 0) %>%
  group_by(zip) %>%
  summarise(total = sum(total),
            quality_seats = sum(quality_seats)) %>%
  ungroup() %>%
  mutate(difference = total - quality_seats,
         hq_perc = quality_seats / sum(quality_seats),
         pop_perc = total / sum(total))

# Total Population

full_sf %>%
  left_join(., t) %>%
  arrange(desc(pop_perc)) %>%
  mutate(rn = row_number(),
         label = zip) %>%
  ggplot(aes(fill = pop_perc, label = label, geometry = geometry)) +
  geom_sf(color = "white") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_c(option = "plasma", labels = c("More Kids", "Fewer Kids"),
                       breaks = c(.12, .01),
                       limits = c(0, .13)) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 14))

# HQ Seats

full_sf %>%
  left_join(., t) %>%
  arrange(desc(hq_perc)) %>%
  mutate(rn = row_number(),
         label = ifelse(rn < 5 | zip == "53206", zip, "")) %>%
  ggplot(aes(fill = hq_perc, label = label)) +
  geom_sf(color = "white") +
  scale_color_manual(values = rev(c("white", "black")), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_c(option = "plasma",  labels = c("More HQ Seats", "Fewer HQ Seats"),
                       breaks = c(.12, .01),
                       limits = c(0, .13)) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 14))

# Difference

full_sf %>%
  left_join(., t) %>%
  arrange(desc(difference)) %>%
  ggplot(aes(fill = difference)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_c(option = "plasma", breaks = c(6000, -10000), 
                       labels = c("More kids than HQ seats","More HQ seats than kids")) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 14))


# Extremes

t %>%
  arrange(desc(difference)) %>%
  mutate(rn = row_number()) %>%
  filter(rn < 6 | rn > nrow(t) - 5) %>%
  ggplot(aes(reorder(zip, difference), difference)) +
  geom_col()


# By age group

  t <- left_join(totals_zip %>%
                   mutate("zip" = as.character(zcta)) %>%
                   select(zip, total, group), 
                 z) %>%
    mutate_all(replace_na, replace = 0) %>%
    group_by(zip, group) %>%
    summarise(total = sum(total),
              quality_seats = sum(quality_seats)) %>%
    ungroup() %>%
    group_by(group) %>%
    mutate(pop_perc = total / sum(total))
  
  full_sf %>%
    left_join(., t) %>%
    arrange(desc(pop_perc)) %>%
    mutate(rn = row_number(),
           label = ifelse(rn < 5 | zip == "53206", zip, "")) %>%
    ggplot(aes(fill = pop_perc, label = label)) +
    geom_sf(color = "white") +
    theme_void(base_family = "serif") +
    scale_fill_viridis_c(option = "plasma") +
    labs(fill = "") +
    facet_grid(~ group)
  