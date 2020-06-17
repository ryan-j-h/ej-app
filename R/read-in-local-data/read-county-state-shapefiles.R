counties <- sf::read_sf(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/tl_2018_us_county/tl_2018_us_county.shp"
  )

states <- sf::read_sf(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/tl_2018_us_state/tl_2018_us_state.shp"
)

saveRDS(counties, "data/shapefiles/counties.rds")

saveRDS(states, "data/shapefiles/states.rds")