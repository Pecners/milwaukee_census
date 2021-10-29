library(tidycensus)
library(tidyverse)

census_api_key("f05e380c7a7b50ff2da502e55ecfaad1b04101a4", install = TRUE)



data <- map_df(c(2009:2019), function(x) {
  get_acs(geography = "place", 
          year = x, 
          variables = paste("B01001", 
                            str_pad(c(seq(from = 3, to = 7, by = 1), seq(from = 27, to = 31, by = 1)),
                                    side = "left", pad = "0", width = "3"),
                            sep = "_"),
          show_call = TRUE,
          state = "Wisconsin",
          survey = "acs5") %>%
    mutate(year = x) 
    
})

mke_totals <- data %>%
  filter(NAME == "Milwaukee city, Wisconsin") %>%
  mutate(variable = case_when(variable %in% c("B01001_003", "B01001_027") ~ "Under 5 years",
                              variable %in% c("B01001_004", "B01001_028") ~ "5 to 9 years",
                              variable %in% c("B01001_005", "B01001_029") ~ "10 to 14 years",
                              TRUE ~ "15 to 19 years")) %>%
  group_by(variable, year) %>%
  summarise(estimate = sum(estimate),
            moe = sum(moe)) %>%
  ungroup() %>%
  modify_at("variable", factor, levels = c("Under 5 years",
                "5 to 9 years",
                "10 to 14 years",
                "15 to 19 years"))

base <- mke_totals %>%
  filter(year == 2009) %>%
  select(variable,
         base = estimate)

mke_totals %>%
  left_join(., base) %>%
  filter(year > 2009) %>%
  mutate(diff_from_2009 = estimate - base) %>%
  ggplot(aes(factor(year), diff_from_2009)) +
  geom_col() +
  facet_wrap(~ variable)
