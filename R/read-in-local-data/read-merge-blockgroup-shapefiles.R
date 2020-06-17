library(sf)
library(purrr)

num <- c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56, 60, 69, 72, 78)
nums <- formatC(num, width = 2, flag = "0")

files <- map_chr(nums, ~paste0(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/tl_2018_", 
  .x, "_bg/tl_2018_",
  .x, "_bg.shp"))

blockgroups <- map_dfr(files, read_sf)

saveRDS(blockgroups, "data/shapefiles/blockgroups.rds")