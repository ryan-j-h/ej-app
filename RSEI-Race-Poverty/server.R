library(shiny)

rsei <- readRDS("rsei.rds")

states <- state.name[state.name != "Alaska" & state.name != "Hawaii"]
  

shinyServer(function(input, output) {
  
  
  
# MAP PANEL
  
  output$state_selector_map = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state_map", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(states),
                # calls unique values from the State column in the previously created table
                selected = "North Carolina") #default choice (not required)
  })

  output$county_selector_map = renderUI({#creates County select box object called in ui

    data_available = fips_codes[fips_codes$state_name == input$state_map, "county"]
    #creates a reactive list of available counties based on the State selection made

    selectInput(inputId = "county_map", #name of input
                label = "County:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1],
                multiple = TRUE)
  })
  
  
  data_map <- eventReactive(input$getdata_map, {
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
      as_tibble() %>%
      select(-moe) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(pct_white = n_white/n_total,
             pct_black = n_black/n_total,
             pct_natam = n_natam/n_total,
             pct_hisp = n_hisp/n_total,
             pct_inc_less10k = n_inc_less10k/n_total,
             pct_inc_10k_14k = n_inc_10k_14k/n_total,
             pct_inc_15k_19k = n_inc_15k_19k/n_total,
             pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
      inner_join(rsei,
                 .,
                 by = c("blckgrp" = "geoid")) %>% 
      sf::st_as_sf() %>% 
      sf::st_transform(4326)
  })
  
  
  
  output$var_selector_map_rsei = renderUI({#creates County select box object called in ui
    
    var_names_rsei = c("toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                  "scorenoncancer_blckgrp")
    
    selectInput(inputId = "vars_map_rsei", #name of input
                label = "RSEI Variable:", #label displayed in ui
                choices = unique(var_names_rsei), #calls list of available counties
                selected = unique(var_names_rsei)[1],
                multiple = F)
  })
  
  output$var_selector_map_acs = renderUI({#creates County select box object called in ui
    
    var_names_acs = c("median_hh_inc", "pct_white", 
                      "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_map_acs", #name of input
                label = "ACS Variable:", #label displayed in ui
                choices = unique(var_names_acs), #calls list of available counties
                selected = unique(var_names_acs)[1],
                multiple = F)
  })
  
  vars_rsei <- eventReactive(input$assignattr, {
    input$vars_map_rsei
  })
  
  vars_acs <- eventReactive(input$assignattr, {
    input$vars_map_acs
  })
  
  output$rseimap <- renderLeaflet({
    withProgress(message = "Making map", detail = "Getting data", value = 0.4, {
      RSEIMap <- data_map()
      m <- mapview(RSEIMap, zcol = vars_rsei())
      incProgress(0.5, detail = "Plotting data")
      m@map
    })
    
  })
  
 # col.regions = viridisLite::inferno(5, direction = ifelse(input$vars_map_acs %in% c("pct_white", "median_hh_inc"), -1, 1)), 
  # col.regions = heat.colors(5, rev = ifelse(vars_acs() %in% c("pct_white", "median_hh_inc"), T, F)
  # col.regions = gray.colors(5, start = 0, end = 0.9)
  # col.regions = heat.colors(5, rev = T)
  
  output$acsmap <- renderLeaflet({
    withProgress(message = "Making map", detail = "Getting data", value = 0.4, {
      ACSMap <- data_map()
      m2 <- mapview(ACSMap, zcol = vars_acs(), 
                    col.regions = viridisLite::viridis(5, direction = ifelse(input$vars_map_acs %in% c("pct_white", "median_hh_inc"), -1, 1)))
      incProgress(0.5, detail = "Plotting data")
      m2@map
    })
    
    
  })
  
  
  
# DATA TABLE PANEL
  
  
  output$state_selector_dt = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state_dt", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(unique(fips_codes$state_name)),
                # calls unique values from the State column in the previously created table
                selected = "North Carolina") #default choice (not required)
  })
  
  output$county_selector_dt = renderUI({#creates County select box object called in ui
    
    data_available = fips_codes[fips_codes$state_name == input$state_dt, "county"]
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "county_dt", #name of input
                label = "County:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1])
  })
  
  output$var_selector_dt = renderUI({#creates County select box object called in ui
    
    var_names = c("name", "toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                  "scorenoncancer_blckgrp", "median_hh_inc", "pct_white", 
                  "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_dt", #name of input
                label = "Variables:", #label displayed in ui
                choices = unique(var_names), #calls list of available counties
                selected = unique(var_names)[1],
                multiple = T)
  })
  
  data_dt <- eventReactive(input$getdata_dt, {
    
    get_acs(geography = "block group", 
                    variables = c(n_total = "B02001_001", n_white = "B02001_002",
                                  n_black = "B02001_003", n_natam = "B02001_004",
                                  n_hisp  = "B03003_003", median_hh_inc = "B19013_001",
                                  n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                                  n_inc_15k_19k = "B19001_004"),
                    county = input$county_dt,
                    state = input$state_dt) %>%
              janitor::clean_names() %>%
              as_tibble() %>%
              select(-moe) %>%
              pivot_wider(names_from = variable, values_from = estimate) %>%
              mutate(pct_white = n_white/n_total,
                     pct_black = n_black/n_total,
                     pct_natam = n_natam/n_total,
                     pct_hisp = n_hisp/n_total,
                     pct_inc_less10k = n_inc_less10k/n_total,
                     pct_inc_10k_14k = n_inc_10k_14k/n_total,
                     pct_inc_15k_19k = n_inc_15k_19k/n_total,
                     pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
              inner_join(rsei,
                         .,
                         by = c("blckgrp" = "geoid"))
  })
  
  
  output$table <- renderDataTable({
    
    display_dt <- data_dt() %>% 
      select(input$vars_dt)
    datatable(display_dt)
    
  })
  
# TRENDS PANEL  
  
  
  output$state_selector_tr = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state_tr", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(unique(fips_codes$state_name)),
                # calls unique values from the State column in the previously created table
                selected = "North Carolina") #default choice (not required)
  })
  
  output$county_selector_tr = renderUI({#creates County select box object called in ui
    
    data_available = fips_codes[fips_codes$state_name == input$state_tr, "county"]
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "county_tr", #name of input
                label = "County:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1],
                multiple = T)
  })
  
  
  data_tr <- eventReactive(input$getdata_tr, {
    
    get_acs(geography = "block group", 
            variables = c(n_total = "B02001_001", n_white = "B02001_002",
                          n_black = "B02001_003", n_natam = "B02001_004",
                          n_hisp  = "B03003_003", median_hh_inc = "B19013_001",
                          n_inc_less10k = "B19001_002", n_inc_10k_14k = "B19001_003",
                          n_inc_15k_19k = "B19001_004"),
            county = input$county_tr,
            state = input$state_tr) %>%
      janitor::clean_names() %>%
      as_tibble() %>%
      select(-moe) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      mutate(pct_white = n_white/n_total,
             pct_black = n_black/n_total,
             pct_natam = n_natam/n_total,
             pct_hisp = n_hisp/n_total,
             pct_inc_less10k = n_inc_less10k/n_total,
             pct_inc_10k_14k = n_inc_10k_14k/n_total,
             pct_inc_15k_19k = n_inc_15k_19k/n_total,
             pct_inc_less20k = (n_inc_less10k + n_inc_10k_14k + n_inc_15k_19k)/n_total) %>%
      inner_join(rsei,
                 .,
                 by = c("blckgrp" = "geoid"))
  })
  
  output$var_selector_tr_rsei <- renderUI({#creates County select box object called in ui
    
    var_names_rsei <- c("toxconc_blckgrp", "score_blckgrp", "scorecancer_blckgrp", 
                       "scorenoncancer_blckgrp")
    
    selectInput(inputId = "vars_tr_rsei", #name of input
                label = "RSEI Variable:", #label displayed in ui
                choices = unique(var_names_rsei), #calls list of available counties
                selected = unique(var_names_rsei)[1],
                multiple = F)
  })
  
  output$var_selector_tr_acs <- renderUI({#creates County select box object called in ui
    
    var_names_acs <- c("median_hh_inc", "pct_white", 
                      "pct_black", "pct_natam", "pct_hisp", "pct_inc_less10k", "pct_inc_less20k")
    
    selectInput(inputId = "vars_tr_acs", #name of input
                label = "ACS Variable:", #label displayed in ui
                choices = unique(var_names_acs), #calls list of available counties
                selected = unique(var_names_acs)[1],
                multiple = T)
  })
  
  
  output$lmtable <- renderTable({
    
    
    response <- input$vars_tr_rsei
    
    predictors <- input$vars_tr_acs
    
    formula <- as.formula(
      paste(response, 
            paste(predictors, collapse = " + "), 
            sep = " ~ "))
    
    model <- lm(formula,
              data = data_tr())

    tidy(model, conf.int = TRUE)
    
  })

})
