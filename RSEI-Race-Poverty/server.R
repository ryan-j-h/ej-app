library(shiny)

# read in RSEI data
rsei <- readRDS("rsei.rds")

# create vector of state names in the continental US
states <- state.name[state.name != "Alaska" & state.name != "Hawaii"]
  

shinyServer(function(input, output) {
  
# MAP PANEL
  
  # create state input
  output$state_selector_map <- renderUI({
    selectInput(inputId = "state_map", 
                label = "State:",
                choices = as.character(states),
                selected = "North Carolina") # default choice
  })

  # create county input
  output$county_selector_map <- renderUI({
    
    # create a list of counties in selected state
    data_available <- fips_codes[fips_codes$state_name == input$state_map, "county"]

    selectInput(inputId = "county_map",
                label = "County:",
                choices = unique(data_available),
                selected = unique(data_available)[1], # default choice
                multiple = TRUE)
  })
  
  # create dataset for map
  data_map <- eventReactive(input$getdata_map, {
    
    # pull acs data using tidycensus
    get_acs(geography = "block group", 
            variables = c(n_total = "B02001_001", n_white = "B02001_002",
                          n_black = "B02001_003", n_natam = "B02001_004",
                          n_hisp  = "B03003_003", median_hh_inc = "B19013_001",
                          n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                          n_inc_15k_19k = "B19001_004"),
            county = input$county_map,
            state = input$state_map,
            geometry = TRUE) %>%
      janitor::clean_names() %>%
      as_tibble() %>% # convert from spatial object to data table
      select(-moe) %>% # select only point estimates
      
      # make a column for each variable
      pivot_wider(names_from = variable, values_from = estimate) %>%
      
      # calculate percent variables
      mutate(pct_white = n_white/n_total,
             pct_black = n_black/n_total,
             pct_natam = n_natam/n_total,
             pct_hisp = n_hisp/n_total,
             pct_inc_less10k = n_inc_less10k/n_total,
             pct_inc_10k_14k = n_inc_10k_14k/n_total,
             pct_inc_15k_19k = n_inc_15k_19k/n_total,
             pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
      
      # join with rsei data
      inner_join(rsei,
                 .,
                 by = c("blckgrp" = "geoid")) %>% 
      
      # return to a spatial object
      sf::st_as_sf() %>% 
      sf::st_transform(4326)
  })
  
  
  # create rsei variable input
  output$var_selector_map_rsei <- renderUI({
    
    # create vector of rsei variables
    var_names_rsei <- c("toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                  "scorenoncancer_blckgrp")
    
    selectInput(inputId = "vars_map_rsei",
                label = "RSEI Variable:",
                choices = unique(var_names_rsei),
                selected = unique(var_names_rsei)[2], # default is score
                multiple = F)
  })
  
  # create acs variable input
  output$var_selector_map_acs <- renderUI({#creates County select box object called in ui
    
    # create vector of acs variables
    var_names_acs <- c("median_hh_inc", "pct_white", 
                      "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_map_acs",
                label = "ACS Variable:",
                choices = unique(var_names_acs),
                selected = unique(var_names_acs)[7], # default is pct_inc_less20k
                multiple = F)
  })
  
  # make variable inputs reactive to create map button
  vars_rsei <- eventReactive(input$assignattr, {
    input$vars_map_rsei
  })
  
  vars_acs <- eventReactive(input$assignattr, {
    input$vars_map_acs
  })
  
  # output rsei map
  output$rseimap <- renderLeaflet({
    
    withProgress(message = "Making map", detail = "Getting data", value = 0.4, { # progress bar
      RSEIMap <- data_map()
      m <- mapview(RSEIMap, zcol = vars_rsei())
      incProgress(0.5, detail = "Plotting data") # update progress bar
      m@map
    })
    
  })
  
  # other color options
  # col.regions = viridisLite::inferno(5, direction = ifelse(input$vars_map_acs %in% c("pct_white", "median_hh_inc"), -1, 1)), 
  # col.regions = heat.colors(5, rev = ifelse(vars_acs() %in% c("pct_white", "median_hh_inc"), T, F)
  # col.regions = gray.colors(5, start = 0, end = 0.9)
  # col.regions = heat.colors(5, rev = T)
  
  
  # output acs map
  output$acsmap <- renderLeaflet({
    
    withProgress(message = "Making map", detail = "Getting data", value = 0.4, { # progress bar
      ACSMap <- data_map()
      m2 <- mapview(ACSMap, zcol = vars_acs(), 
                    col.regions = viridisLite::viridis(5, direction = ifelse(input$vars_map_acs %in% c("pct_white", "median_hh_inc"), -1, 1))) # make direction consistent so that lighter is a more 'vulnerable' population
      incProgress(0.5, detail = "Plotting data") # update progress bar
      m2@map
    })
    
    
  })
  
  
  
  
  
  
  
  
# DATA TABLE PANEL
  
  # create state input
  output$state_selector_dt = renderUI({
    
    selectInput(inputId = "state_dt", 
                label = "State:",
                choices = as.character(states),
                selected = "North Carolina") # default
  })
  
  # create county input
  output$county_selector_dt <- renderUI({#creates County select box object called in ui
    
    # create a list of counties in selected state
    data_available <- fips_codes[fips_codes$state_name == input$state_dt, "county"]

    selectInput(inputId = "county_dt",
                label = "County:", 
                choices = unique(data_available),
                selected = unique(data_available)[1]) # default
  })
  
  # create variable input
  output$var_selector_dt <- renderUI({
    
    # create vector of variable names
    var_names <- c("name", "toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                  "scorenoncancer_blckgrp", "median_hh_inc", "pct_white", 
                  "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_dt",
                label = "Variables:", 
                choices = unique(var_names),
                selected = unique(var_names)[1], # default
                multiple = T)
  })
  
  # create dataset for data table
  
  data_dt <- eventReactive(input$getdata_dt, {
    
    # get acs data
    get_acs(geography = "block group", 
            variables = c(n_total = "B02001_001", n_white = "B02001_002",
                          n_black = "B02001_003", n_natam = "B02001_004",
                          n_hisp  = "B03003_003", median_hh_inc = "B19013_001",
                          n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                          n_inc_15k_19k = "B19001_004"),
            county = input$county_dt,
            state = input$state_dt) %>%
      janitor::clean_names() %>%
      as_tibble() %>% # convert from sf to tibble
      select(-moe) %>% # select only estimates
      pivot_wider(names_from = variable, values_from = estimate) %>% # one column per variable
      
      # calculate percents 
      mutate(pct_white = n_white/n_total,
             pct_black = n_black/n_total,
             pct_natam = n_natam/n_total,
             pct_hisp = n_hisp/n_total,
             pct_inc_less10k = n_inc_less10k/n_total,
             pct_inc_10k_14k = n_inc_10k_14k/n_total,
             pct_inc_15k_19k = n_inc_15k_19k/n_total,
             pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
              
      # join with rsei
      inner_join(rsei, ., by = c("blckgrp" = "geoid"))
  })
  
  # create output of selected variables
  output$table <- renderDataTable({
    
    display_dt <- data_dt() %>% 
      select(input$vars_dt)
    datatable(display_dt)
    
  })
  
  
  
  
  
  
  
# MODEL PANEL  
  
  # create state input
  output$state_selector_tr = renderUI({
    selectInput(inputId = "state_tr",
                label = "State:", #label displayed in ui
                choices = as.character(states),
                selected = "North Carolina") # default
  })
  
  # create county input
  output$county_selector_tr = renderUI({
    
    # create list of counties in selected state
    data_available = fips_codes[fips_codes$state_name == input$state_tr, "county"]
    
    selectInput(inputId = "county_tr",
                label = "County:", 
                choices = unique(data_available),
                selected = unique(data_available)[1], # default
                multiple = T)
  })
  
  
  # create dataset for model
  data_tr <- eventReactive(input$getdata_tr, {
    
    # get acs data
    get_acs(geography = "block group", 
            variables = c(n_total = "B02001_001", n_white = "B02001_002",
                          n_black = "B02001_003", n_natam = "B02001_004",
                          n_hisp  = "B03003_003", median_hh_inc = "B19013_001",
                          n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                          n_inc_15k_19k = "B19001_004"),
            county = input$county_tr,
            state = input$state_tr) %>%
      janitor::clean_names() %>%
      as_tibble() %>% # convert from sf to tibble
      select(-moe) %>% # select only estimates
      pivot_wider(names_from = variable, values_from = estimate) %>% # one column per variable
      
      # calculate percents
      mutate(pct_white = n_white/n_total, # calculate percents
             pct_black = n_black/n_total,
             pct_natam = n_natam/n_total,
             pct_hisp = n_hisp/n_total,
             pct_inc_less10k = n_inc_less10k/n_total,
             pct_inc_10k_14k = n_inc_10k_14k/n_total,
             pct_inc_15k_19k = n_inc_15k_19k/n_total,
             pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
      
      # join with rsei
      inner_join(rsei,
                 .,
                 by = c("blckgrp" = "geoid"))
  })
  
  # create rsei variable selector
  output$var_selector_tr_rsei <- renderUI({
    
    # create vector of rsei variables
    var_names_rsei <- c("toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                       "scorenoncancer_blckgrp")
    
    selectInput(inputId = "vars_tr_rsei",
                label = "RSEI Response Variable:",
                choices = unique(var_names_rsei), 
                selected = unique(var_names_rsei)[2], # default is score
                multiple = F)
  })
  
  # create acs variable selector
  output$var_selector_tr_acs <- renderUI({
    
    # create vector of acs variables
    var_names_acs <- c("median_hh_inc", "pct_white", 
                      "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_tr_acs", 
                label = "ACS Predictor Variables:",
                choices = unique(var_names_acs),
                selected = unique(var_names_acs)[7], # default is pct_inc_less20k
                multiple = T)
  })
  
  
  # create regression output
  output$lmtable <- renderTable({
    
    response <- input$vars_tr_rsei
    
    predictors <- input$vars_tr_acs
    
    # create model formula for input into lm
    formula <- as.formula(
      paste(response, 
            paste(predictors, collapse = " + "), 
            sep = " ~ "))
    
    # run lm
    model <- lm(formula,
              data = data_tr())
    
    # construct table
    tidy(model, conf.int = TRUE)
    
  })

})
