options <- c("top", "bottom")

files <- map_chr(options, ~paste0(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/poly_gc14_conus_810m_", 
  .x,".shp"))

grid <- map_dfr(files, sf::read_sf)

# saveRDS(grid, "data/shapefiles/blockgroups.rds")