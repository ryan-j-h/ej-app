library(janitor)

# county-level saipe data
url <- "https://www2.census.gov/programs-surveys/saipe/datasets/2018/2018-state-and-county/est18all.xls"
url2 <- "https://www2.census.gov/programs-surveys/saipe/datasets/2018/2018-school-districts/ussd18.xls"

# download.file(url, destfile = "data/saipe/est18all.xls")
# download.file(url2, destfile = "data/saipe/ussd18.xls")

statecounty <- readxl::read_excel("data/saipe/est18all.xls", 
                                  range = "A4:AE3198") %>%
  janitor::clean_names()

schooldistr <- readxl::read_excel("data/saipe/ussd18.xls", 
                                  range = "A3:G13210") %>% 
  janitor::clean_names()


# Save links
# api_url         <- "https://www.census.gov/programs-surveys/saipe/data/api.html"
# statecounty_url <- "https://www.census.gov/data/datasets/2018/demo/saipe/2018-state-and-county.html"
# schooldistr_url <- "https://www.census.gov/data/datasets/2018/demo/saipe/2018-school-districts.html"
# inputdata_url   <- "https://www.census.gov/data/datasets/time-series/demo/saipe/model-tables.html"

