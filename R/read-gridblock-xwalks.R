# To download, go to ftp://newftp.epa.gov/RSEI/Census_XWalks/CensusBlock2010_ConUS_810m.csv

xwalk <- vroom::vroom(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/CensusBlock2010_ConUS_810m.csv"
  )

saveRDS(xwalk, "data/xwalk_raw.rds") # not kept on GitHub repo due to size
