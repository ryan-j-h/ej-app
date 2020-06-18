library(dplyr)
library(multidplyr)

# use xwalk to wrangle rsei data

xwalk <- readRDS("data/xwalk_raw.rds") %>% janitor::clean_names()
rsei <- readRDS("data/rsei_raw.rds")

rseixw <- dplyr::inner_join(xwalk, rsei, by = c("x", "y"), keep = TRUE)%>% 
  filter(grid_id == 14) %>% 
  mutate(blckgrp = substring(block_id00, 1, 12), 
         blck = substring(block_id00, 13))

# rseixw_small <- rseixw %>% slice(1:1000)

clust <- new_cluster(8)

rseixw_p <- rseixw %>%
  group_by(blckgrp) %>%
  partition(clust)

# calculate block scores
rsei_blck <- rseixw_p %>% 
  mutate(pop_blck_cell = pop * pct_cp_b) %>% 
  group_by(block_id00) %>% 
  mutate(
    pop_blck = sum(pop_blck_cell),
    toxconc_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0,
                sum(pop_blck_cell / pop_blck * toxconc)),
    score_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0,
                sum(pop_blck_cell / pop_blck * score)),
    scorecancer_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0,
                sum(pop_blck_cell / pop_blck * scorecancer)),
    scorenoncancer_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0,
                sum(pop_blck_cell / pop_blck * scorenoncancer))
    )%>% 
  select(blck, blckgrp, pop_blck, toxconc_blck, score_blck, 
         scorecancer_blck, scorenoncancer_blck) %>% 
  collect() %>% 
  distinct(blck, blckgrp, .keep_all = TRUE)

fst::write.fst(rsei_blck, "data/rsei_blck.fst", 100)

rsei_blck_p <- rsei_blck %>% 
  group_by(blckgrp) %>% 
  partition(clust)

# calculate block group scores
rsei_blckgrp <- rsei_blck_p %>% 
  group_by(blckgrp) %>% 
  mutate(
    pop_blckgrp = sum(pop_blck),
    toxconc_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0,
                sum(pop_blck/pop_blckgrp * toxconc_blck)),
    score_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0,
                sum(pop_blck/pop_blckgrp * score_blck)),
    scorecancer_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0,
                sum(pop_blck/pop_blckgrp * scorecancer_blck)),
    scorenoncancer_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0,
                sum(pop_blck/pop_blckgrp * scorenoncancer_blck))
  ) %>% 
  select(blckgrp, pop_blckgrp, toxconc_blckgrp, score_blckgrp, 
         scorecancer_blckgrp, scorenoncancer_blckgrp) %>% 
  collect() %>% 
  distinct(blckgrp, .keep_all = TRUE)

fst::write.fst(rsei_blckgrp, "data/rsei_blckgrp.fst", 100)

## read and write files
# saveRDS(rseixw, "data/rseixw.rds")
# fst::write.fst(rseixw, "data/rseixw.fst", 100)
# rseixw <- fst::read.fst("data/rseixw.fst")
# 
# rsei <- readRDS("data/rsei_raw.rds")
# fst::write.fst(rsei, "data/rsei_raw.fst")
# rsei <- fst::read.fst("data/rsei_raw.fst")
# 
# xwalk <- readRDS("data/xwalk_raw.rds")
# fst::write.fst(xwalk, "data/xwalk_raw.fst")
# xwalk <- fst::read.fst("data/xwalk_raw.fst")


