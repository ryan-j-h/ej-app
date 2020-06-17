names_char <- c("category", "chemical", "elements", "facility", "mcl", "media", 
                "naics", "offsite", "releases", "submissions")

purrr::map(names_char, ~assign(.x, readr::read_csv(
  paste0("data/rsei/2018/", .x, "_data_rsei_v238.csv")), envir = globalenv()))
  
names_list <- list(category, chemical, elements, facility, mcl, media, naics, 
                   offsite, releases, submissions)

for (i in 1:length(names_char)) {
  saveRDS(names_list[[i]], 
          paste0("data/rsei/2018/", names_char[i], ".rds"))
}