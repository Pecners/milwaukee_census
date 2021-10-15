library(tidycensus)
library(tidyverse)

v_2019 <- load_variables(year = 2019, dataset = "acs5")

z_data <- get_acs(geography = "zcta", year = 2019, variables = "B01001_001", state = "WI")


x <- full_sf %>%
  mutate(zip = ZCTA5CE10) %>%
  left_join(., t) %>%
  mutate(difference = total - quality_seats,
         hq_perc = quality_seats / sum(quality_seats),
         pop_perc = total / sum(total),
         perc_diff = pop_perc - hq_perc) %>%
  mutate(diff = case_when(difference < 5000 ~ "Less than 5,000",
                          difference > 4999 & difference < 10001 ~ "5,000 - 10,000",
                          TRUE ~ "10,000+"))
  ggplot(aes(fill = diff)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  labs(fill = "Deficit of HQ Seats") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))
