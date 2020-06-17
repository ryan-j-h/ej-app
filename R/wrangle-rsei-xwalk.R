library(dplyr)
library(multidplyr)

# use xwalk to wrangle rsei data

xwalk <- readRDS("data/xwalk_raw.rds") %>% janitor::clean_names()
rsei <- readRDS("data/rsei_raw.rds")

rseixw <- dplyr::inner_join(xwalk, rsei, by = c("x", "y"), keep = TRUE)

clust <- new_cluster(4)

rseixw_p <- rseixw %>% 
  multidplyr::partition(clust)
  
# join
rseixw <- rseixw_p %>% 
  filter(grid_id == 14) %>% 
  mutate(blckgrp = substring(block_id00, 1, 12), 
         blck = substring(block_id00, 13)) %>% 

# calculate block scores
  
  mutate(pop_blck_cell = pop * pct_cp_b) %>% 
  group_by(block_id00) %>% 
  mutate(
    pop_blck = sum(pop_blck_cell),
    toxconc_blck = 
      case_when(round(pop_blck) == 0 ~ 0,
                TRUE ~ sum(pop_blck_cell / pop_blck * toxconc)),
    score_blck = 
      case_when(round(pop_blck) == 0 ~ 0,
                TRUE ~ sum(pop_blck_cell / pop_blck * score)),
    scorecancer_blck = 
      case_when(round(pop_blck) == 0 ~ 0,
                TRUE ~ sum(pop_blck_cell / pop_blck * scorecancer)),
    scorenoncancer_blck = 
      case_when(round(pop_blck) == 0 ~ 0,
                TRUE ~ sum(pop_blck_cell / pop_blck * scorenoncancer))
    ) %>% 
  ungroup() %>% 
  select(blck, blckgrp, pop_blck, toxconc_blck, score_blck, scorecancer_blck, scorenoncancer_blck) %>% 
  unique() %>% 
  collect()

  
# calculate block group scores
  group_by(blckgrp) %>% 
  mutate(
    pop_blckgrp = sum(pop_blck),
    toxconc_blckgrp = 
      case_when(round(pop_blckgrp) == 0 ~ 0,
                TRUE ~ sum(pop_blck/pop_blckgrp * toxconc_blck)),
    score_blckgrp = 
      case_when(round(pop_blckgrp) == 0 ~ 0,
                TRUE ~ sum(pop_blck/pop_blckgrp * score_blck)),
    scorecancer_blckgrp = 
      case_when(round(pop_blckgrp) == 0 ~ 0,
                TRUE ~ sum(pop_blck/pop_blckgrp * scorecancer_blck)),
    scorenoncancer_blckgrp = 
      case_when(round(pop_blckgrp) == 0 ~ 0,
                TRUE ~ sum(pop_blck/pop_blckgrp * scorenoncancer_blck))
  ) %>% 
  ungroup() %>% 
  select(blckgrp, pop_blckgrp, toxconc_blckgrp, score_blckgrp, scorecancer_blckgrp, scorenoncancer_blckgrp) %>% 
  unique()


## read and write things
saveRDS(rseixw, "data/rseixw.rds")
fst::write.fst(rseixw, "data/rseixw.fst", 100)
# rseixw <- fst::read.fst("data/rseixw.fst")
# 
# 
# rsei <- readRDS("data/rsei_raw.rds")
# fst::write.fst(rsei, "data/rsei_raw.fst")
# rsei <- fst::read.fst("data/rsei_raw.fst")
# 
# 
# xwalk <- readRDS("data/xwalk_raw.rds")
# fst::write.fst(xwalk, "data/xwalk_raw.fst")
# xwalk <- fst::read.fst("data/xwalk_raw.fst")


