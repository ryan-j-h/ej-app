rsei <- vroom::vroom(
  "C:/Users/ryanh/Desktop/323ProjectLocalData/aggmicro2018_2018_gc14.csv",
  col_names = F)

rsei <- rsei %>% 
  rename(x = X1,
         y = X2,
         n_facilities = X3,
         n_releases = X4,
         n_chemicals = X5,
         toxconc = X6,
         score = X7,
         pop = X8,
         scorecancer = X9,
         scorenoncancer = X10)

saveRDS(rsei, "data/rsei_raw.rds")
