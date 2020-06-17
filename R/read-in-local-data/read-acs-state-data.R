base <- "C:/Users/ryanh/Desktop/323ProjectLocalData/Tracts_Block_Groups_Only/"

names <- stringr::str_replace_all(state.name, " ", "")

tag <- "_Tracts_Block_Groups_Only.zip"

purrr::map(names, ~unzip(paste0(base, .x, tag), exdir = paste0("data/acs/", .x)))
