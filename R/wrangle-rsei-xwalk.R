# use xwalk to wrangle rsei data

library(dplyr)
library(multidplyr)

# read in data
xwalk <- readRDS("data/xwalk_raw.rds") %>% janitor::clean_names()
rsei <- readRDS("data/rsei_raw.rds")

# join rsei with census crosswalk file (geographic information)

rseixw <- dplyr::inner_join(xwalk, rsei, by = c("x", "y"), keep = TRUE)%>% 
  filter(grid_id == 14) %>% # continental US
  
  # separate block group and block id (last digits)
  mutate(blckgrp = substring(block_id00, 1, 12),
         blck = substring(block_id00, 13))

# rseixw_small <- rseixw %>% slice(1:1000)


# create cluster and partition data for multidplyr
clust <- new_cluster(8)

rseixw_p <- rseixw %>%
  group_by(blckgrp) %>%
  partition(clust)


# calculate block scores
rsei_blck <- rseixw_p %>% 
  mutate(pop_blck_cell = pop * pct_cp_b) %>% 
  group_by(block_id00) %>% 
  
  # calculate block-level RSEI toxics and score (formulas on INFO page of app)
  mutate(
    pop_blck = sum(pop_blck_cell),
    toxconc_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0, # avoid dividing by zero
                sum(pop_blck_cell / pop_blck * toxconc)),
    score_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0, # avoid dividing by zero
                sum(pop_blck_cell / pop_blck * score)),
    scorecancer_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0, # avoid dividing by zero
                sum(pop_blck_cell / pop_blck * scorecancer)),
    scorenoncancer_blck = 
      dplyr::if_else(round(pop_blck) == 0, 0, # avoid dividing by zero
                sum(pop_blck_cell / pop_blck * scorenoncancer))
    )%>% 
  
  # keep relevant variables
  select(blck, blckgrp, pop_blck, toxconc_blck, score_blck, 
         scorecancer_blck, scorenoncancer_blck) %>% 
  collect() %>% 
  distinct(blck, blckgrp, .keep_all = TRUE) # keep one row per block

# save block level data
# fst::write.fst(rsei_blck, "data/rsei_blck.fst", 100)





# partition again for multidplyr
rsei_blck_p <- rsei_blck %>% 
  group_by(blckgrp) %>% 
  partition(clust)

# calculate block group scores
rsei_blckgrp <- rsei_blck_p %>% 
  group_by(blckgrp) %>% 
  
  # calculate block group-level RSEI toxics/score (formulas on INFO page of app)
  mutate(
    pop_blckgrp = sum(pop_blck),
    toxconc_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0, # avoid dividing by zero
                sum(pop_blck/pop_blckgrp * toxconc_blck)),
    score_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0, # avoid dividing by zero
                sum(pop_blck/pop_blckgrp * score_blck)),
    scorecancer_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0, # avoid dividing by zero
                sum(pop_blck/pop_blckgrp * scorecancer_blck)),
    scorenoncancer_blckgrp = 
      dplyr::if_else(round(pop_blckgrp) == 0, 0, # avoid dividing by zero
                sum(pop_blck/pop_blckgrp * scorenoncancer_blck))
  ) %>% 
  
  # keep relevant variables
  select(blckgrp, pop_blckgrp, toxconc_blckgrp, score_blckgrp, 
         scorecancer_blckgrp, scorenoncancer_blckgrp) %>% 
  collect() %>% 
  distinct(blckgrp, .keep_all = TRUE) # keep one row per block group

# save data
# fst::write.fst(rsei_blckgrp, "data/rsei_blckgrp.fst", 100)
saveRDS(rsei_blckgrp, "Explore-EJ/rsei.rds")
