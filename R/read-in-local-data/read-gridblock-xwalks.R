xwalk <- vroom::vroom(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/CensusBlock2010_ConUS_810m.csv"
  )

# clust <- multidplyr::new_cluster(3)
# 
# xwalk_part <- xwalk %>% 
#   multidplyr::partition(clust)

saveRDS(xwalk, "data/xwalk_raw.rds")
