library(tidycensus)
library(tidyverse)
library(leaflet)
library(mapview)

# get acs data

census_api_key("691c56c505ab23560453873cc6768f41206683b4")
data(fips_codes)

# vars <- load_variables(2018, "acs5", cache = TRUE)

chesco <- get_acs(geography = "block group", 
                variables = c(n_total = "B02001_001", n_white = "B02001_002", 
                              n_black = "B02001_003", n_natam = "B02001_004", 
                              n_hisp  = "B03003_003", median_hh_inc = "B19013_001", 
                              n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                              n_inc_15k_19k = "B19001_004"),
                county = "Philadelphia County", 
                state = "Pennsylvania", 
                geometry = TRUE) %>%  
              #  show_call = T) %>% 
  janitor::clean_names() %>%
  as_tibble() %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate) %>% 
  mutate(pct_white = n_white/n_total,
         pct_black = n_black/n_total,
         pct_natam = n_natam/n_total,
         pct_hisp = n_hisp/n_total,
         pct_inc_less10k = n_inc_less10k/n_total, 
         pct_inc_10k_14k = n_inc_10k_14k/n_total,
         pct_inc_15k_19k = n_inc_15k_19k/n_total,
         pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total)

rsei <- fst::read.fst("data/rsei_blckgrp.fst")

# merge with rsei data
rseichesco <- inner_join(rsei, 
                         chesco,
                         by = c("blckgrp" = "geoid")) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(4326)



# plot
# library(leaflet)
# 
# 
# qpal <- colorQuantile("magma", rseichesco$n_total, n = 7)
# 
# leaflet(rseichesco) %>% 
#   addPolygons(color = ~qpal(n_total), weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.8, stroke = F,
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE)) %>% 
#   addProviderTiles(providers$CartoDB.Positron)

mapview(rseichesco, zcol = c("score_blckgrp"),
        alpha.regions = 0.4) +
  mapview(rseichesco, zcol = c("pct_white"),
          alpha.regions = 0.3, col.regions = viridisLite::inferno(5, direction = -1))

# col.regions = heat.colors
# col.regions = viridisLite::inferno(5, direction = -1)
# col.regions = RColorBrewer::brewer.pal(20, "Blues")

# light areas indicate high EJ risk

