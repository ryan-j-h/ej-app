library(shiny)

shinyServer(function(input, output) {
  
  # MAP STUFF
  
  output$state_selector_map = renderUI({ #creates State select box object called in ui
    selectInput(inputId = "state_map", #name of input
                label = "State:", #label displayed in ui
                choices = as.character(unique(fips_codes$state_name)),
                # calls unique values from the State column in the previously created table
                selected = "North Carolina") #default choice (not required)
  })

  output$county_selector_map = renderUI({#creates County select box object called in ui

    data_available = fips_codes[fips_codes$state_name == input$state_map, "county"]
    #creates a reactive list of available counties based on the State selection made

    selectInput(inputId = "county_map", #name of input
                label = "County:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1])
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
  
  output$rseimap <- renderLeaflet({
    withProgress(message = "Making map", detail = "Getting data", value = 0.4, {
      EJMap <- data_map()
      m <- mapview(EJMap, zcol = "n_white") + mapview(EJMap, zcol = "pct_white")
      incProgress(0.5, detail = "Last touches")
      m@map
    })
    
  })
  
  
  
  
  
  
  
  
  # DATA TABLE STUFF
  
  
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
    
    display_dt <- data_dt()
    datatable(display_dt)
    
  })
  

})
