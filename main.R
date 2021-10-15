library(tidyverse)

t <- "B01001"

zip_data <- read_csv("data/mke_zip_census_sex_age.csv")

names(zip_data)[3:length(zip_data)] <- str_replace(names(zip_data)[3:length(zip_data)], t, paste0(t, "_"))

data_names <- read_csv("data/ACS2019_Table_Shells.csv") %>%
  filter(`Table ID` == t & !is.na(Line))

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
                "18 and 19 years")

totals_zip <- total_long %>%
  filter(group %in% school_age) %>%
  group_by(zcta, group) %>%
  summarise(total = sum(value))

under_18_comp <- totals_zip %>%
  filter(group != "18 and 19 years") %>%
  group_by(zip = zcta) %>%
  summarise(total_under18 = sum(total)) %>%
  right_join(., adj_zip_totals)

totals_zip %>%
  group_by(zcta) %>%
  summarise(all = sum(total)) %>%
  filter(zcta == "53209")

z_top_4 <- c("53218",
             "53209",
             "53215",
             "53204")

totals_zip %>%
  modify_at("zcta", as.character) %>%
  group_by(top_4 = zcta %in% z_top_4, group) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  group_by(top_4) %>%
  mutate(perc = total / sum(total)) %>%
  select(-total) %>%
  pivot_wider(names_from = top_4, values_from = perc)

totals_zip %>%
  modify_at("zcta", as.character) %>%
  group_by(group) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  mutate(perc = total / sum(total))

source("rc_by_zip.R")

all_w_zip <- zip_rc %>%
  filter(school_year == "2018-19") %>%
  select(school_year:accurate_agency_type,
         overall_score:overall_rating,
         school_enrollment,
         zip) %>%
  filter(!is.na(zip)) # %>%
# write_csv(., "Schools with RC and ZIP.csv")

ed_zip_rc <- zip_rc %>%
  filter(school_year == "2018-19") %>%
  mutate(est_ed = school_enrollment * per_ed) %>%
  group_by(zip) %>%
  summarise(ed = sum(est_ed, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(perc_ed_enr = ed / sum(ed))

z <- zip_rc %>%
  filter(school_year == "2018-19" & overall_score >= 73.0) %>%
  group_by(zip) %>%
  summarise(quality_seats = sum(school_enrollment))

zip_rc %>%
  filter(zip %in% c("53204", "53207") & overall_score >= 73.0 & school_year == "2018-19") %>%
  summarise(black = weighted.mean(per_b_aa, w = school_enrollment),
            hisp = weighted.mean(per_hisp_lat, w = school_enrollment),
            white = weighted.mean(per_white, w = school_enrollment)) 
  select(dpi_true_id, school_enrollment,
         per_b_aa,
         per_hisp_lat,
         per_asian,
         per_white) %>%
  pivot_longer(cols = -c(1:2), names_to = "group", values_to = "perc") %>%
  mutate(estimate = school_enrollment * perc)

# using 2020 ward-adjusted zip totals under 18
  
t <- left_join(adj_zip_totals %>%
            mutate("zip" = as.character(zip)) %>%
            select(zip, estimate), 
          z) %>%
  mutate_all(replace_na, replace = 0) %>%
  group_by(zip) %>%
  summarise(total = sum(estimate),
            quality_seats = mean(quality_seats)) %>%
  ungroup()

ted <- left_join(totals_zip %>%
                   mutate("zip" = as.character(zcta)) %>%
                   select(zip, total), 
                 ed_zip_rc) %>%
  mutate_all(replace_na, replace = 0) %>%
  select(zip, perc_ed_enr) %>%
  unique()

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
  theme(legend.text = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "ZIP Code Residence of Milwaukee School-Aged Population",
       subtitle = "Boundaries represent ZIP Code Tabulation Areas",
       caption = paste("School-aged defined as 19 years old and under.",
                       "Source: US Census Bureau 2019 ACS 5-Year Data", sep = "\n"))

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
  theme(legend.text = element_text(size = 14),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Distribution of High Quality Seats",
       subtitle = "Boundaries represent ZIP Code Tabulation Areas",
       caption = paste("High quality defined as 'Exceeds Expecations' or 'Significantly Exceeds Expectations'.",
                       "Source: 2018-19 School Report Cards, Wisconsin DPI", sep = "\n"))

# Difference

full_sf %>%
  left_join(., t) %>%
  mutate(adj_total = total * per_zip_in_city,
        difference = total - quality_seats,
        hq_perc = quality_seats / sum(quality_seats),
        pop_perc = total / sum(total),
        perc_diff = pop_perc - hq_perc,
        diff = case_when(difference < 5000 ~ "Less than 5,000",
                          difference > 4999 & difference < 10001 ~ "5,000 - 10,000",
                          TRUE ~ "10,000+")) %>%
  ggplot(aes(fill = diff)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(fill = "Deficit of HQ Seats") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))


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
            quality_seats = mean(quality_seats)) %>%
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
  
# By ed status

full_sf %>%
  left_join(., ted) %>%
  arrange(desc(perc)) %>%
  ggplot(aes(fill = perc)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_c(option = "plasma", breaks = c(.01, max(ted$perc - .01)),
                       labels = c("Fewer Ec. Disadv. Students", "More Ec. Disadv. Students")) +
  labs(fill = "") +
  theme(legend.text = element_text(size = 14))
  