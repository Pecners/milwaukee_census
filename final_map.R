library(tidycensus)
library(tidyverse)

v_2019 <- load_variables(year = 2019, dataset = "acs5")

z_data <- get_acs(geography = "zcta", year = 2019, variables = "B01001_001", state = "WI")


pen_table <- full_sf %>%
  left_join(., t) %>%
  mutate(difference = total - quality_seats,
         hq_perc = quality_seats / sum(quality_seats),
         pop_perc = total / sum(total),
         perc_diff = pop_perc - hq_perc,
         pop_perc  = difference / total,
         diff_perc = difference / sum(difference)) %>%
  arrange(difference) %>%
  mutate(diff = case_when(difference < mean(difference) - sd(difference) ~ "Bottom",
                          difference < mean(difference) + sd(difference) ~ "Middle",
                          TRUE ~ "Top"))

temp <- pen_table %>%
  as_tibble() %>%
  .[["difference"]]

c <- NULL

for (i in 1:length(temp)) {
  print("Running:\n")
  print(sum(temp[1:i]))
  print("Total\n")
  print(sum(temp))
  c[i] <- sum(temp[1:i]) / sum(temp)
}

p <- bind_cols(pen_table, running_total = c)

pen_table %>%
  ggplot(aes(fill = diff)) +
  geom_sf(color = "white") +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  theme_void(base_family = "serif") +
  scale_fill_viridis_d(option = "plasma") +
  labs(fill = "Deficit of HQ Seats") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))


export_table <- p %>%
  as_tibble() %>%
  select(zip,
         "population" = total,
         quality_seats,
         difference,
         percent_of_decifit = diff_perc,
         running_percent_of_decifit = running_total)

write_csv(export_table, "../000_data_temp/hq_seats_deficit_by_zip.csv")
