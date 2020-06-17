library(tidycensus)
library(tidyr)

census_api_key("691c56c505ab23560453873cc6768f41206683b4")

vars <- load_variables(2018, "acs5", cache = TRUE)

chesco <- get_acs(geography = "block group", 
                variables = c("B02001_001", "B02001_002", "B02001_003", "B02001_004", 
                              "B03003_003", "B19013_001"), 
                county = "Chester", 
                state = "PA", 
                geometry = TRUE) %>%  
              #  show_call = T) %>% 
  janitor::clean_names() %>%
  as_tibble() %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  rename(n_total = B02001_001,
         n_white = B02001_002,
         n_black = B02001_003,
         n_natam = B02001_004,
         n_hisp  = B03003_003,
         median_hh_inc = B19013_001) %>% 
  mutate(pct_white = n_white/n_total,
         pct_black = n_black/n_total,
         pct_natam = n_natam/n_total,
         pct_hisp = n_hisp/n_total)
 # sf::st_as_sf()

rseichesco <- inner_join(rseixw, 
                         chesco,
                         by = c("blckgrp" = "geoid"))
