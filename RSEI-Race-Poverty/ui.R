library(shiny)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(tidycensus)
library(DT)
library(mapview)
library(leaflet)

census_api_key("691c56c505ab23560453873cc6768f41206683b4")
data(fips_codes)

shinyUI(
    fluidPage(theme = shinytheme("sandstone"),
              useShinyalert(),
              tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
              ),
              tags$head(tags$style(HTML('
                    h1, h2, h3 {
                        font-weight: bold;
                        
                        text-transform: uppercase;
                    }
                '))),
              
              #.navbar-brand,
              # font-variant: small-caps;
              
    navbarPage("Exploring Patterns of Environmental Injustice",
        
        tabPanel(title = "Information",
                        
            titlePanel("Mapping Pollution, Race, and Income Data"),
                
            h3("Pollution Data: Risk-Screening Environmental Indicators (RSEI)"),
            
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc congue nisl vel magna viverra, 
            in condimentum purus convallis. Nam ultrices nunc efficitur lectus rutrum pretium. Nulla facilisi. 
            Sed nec consequat urna. Phasellus pretium tortor vel porta vehicula. Vivamus ornare imperdiet erat, id 
            ullamcorper sem ornare non. Morbi quis purus commodo, efficitur eros vitae, auctor enim. Proin sit 
            amet turpis vel nisl tincidunt cursus at facilisis lorem. In in sem eget nibh pellentesque scelerisque. 
            Proin ac pretium massa. Suspendisse blandit bibendum auctor. Ut cursus eleifend enim, vitae euismod 
            magna bibendum a. Pellentesque cursus, turpis nec pulvinar ullamcorper, libero est dignissim turpis, 
            a convallis ipsum sem sit amet enim."),
            
            p("Ut egestas nunc sed faucibus posuere. Suspendisse in risus faucibus, accumsan nibh eget, tincidunt 
            mi. Vestibulum volutpat velit eu mi pellentesque volutpat. Nullam enim ex, imperdiet vitae est vel, 
            vehicula malesuada nibh. Nunc eu vestibulum velit. Vivamus tortor ex, faucibus sed libero ac, 
            malesuada consequat ipsum. Nunc nec nisl eget purus malesuada iaculis."),
            
            p("Nunc ornare, ex ac consequat congue, nibh dolor interdum justo, sed ornare est leo ut 
              tellus. Aenean sed massa vel nibh fringilla malesuada. Nulla nec rutrum elit. Aenean 
              cursus nisi dapibus ante consequat, eget scelerisque erat facilisis. Pellentesque rhoncus 
              mattis nisi quis hendrerit. Aliquam consequat eget eros sed lobortis. Quisque sed elit 
              sed nisl euismod efficitur et ac ante. Donec ut mauris quis purus feugiat dictum vitae 
              commodo felis. Aenean semper auctor ultricies. Phasellus id sem et risus tempus mollis. 
              Cras sit amet mollis nisi. Nulla tempor in lorem in commodo."),
                
            h3("Race and Income Data: American Community Survey (ACS)"),
            
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc congue nisl vel magna viverra, 
            in condimentum purus convallis. Nam ultrices nunc efficitur lectus rutrum pretium. Nulla facilisi. 
            Sed nec consequat urna. Phasellus pretium tortor vel porta vehicula. Vivamus ornare imperdiet erat, id 
            ullamcorper sem ornare non. Morbi quis purus commodo, efficitur eros vitae, auctor enim. Proin sit 
            amet turpis vel nisl tincidunt cursus at facilisis lorem. In in sem eget nibh pellentesque scelerisque. 
            Proin ac pretium massa. Suspendisse blandit bibendum auctor. Ut cursus eleifend enim, vitae euismod 
            magna bibendum a. Pellentesque cursus, turpis nec pulvinar ullamcorper, libero est dignissim turpis, 
            a convallis ipsum sem sit amet enim."),
            
            p("Ut egestas nunc sed faucibus posuere. Suspendisse in risus faucibus, accumsan nibh eget, tincidunt 
            mi. Vestibulum volutpat velit eu mi pellentesque volutpat. Nullam enim ex, imperdiet vitae est vel, 
            vehicula malesuada nibh. Nunc eu vestibulum velit. Vivamus tortor ex, faucibus sed libero ac, 
            malesuada consequat ipsum. Nunc nec nisl eget purus malesuada iaculis."),
            
            p("Nunc ornare, ex ac consequat congue, nibh dolor interdum justo, sed ornare est leo ut 
              tellus. Aenean sed massa vel nibh fringilla malesuada. Nulla nec rutrum elit. Aenean 
              cursus nisi dapibus ante consequat, eget scelerisque erat facilisis. Pellentesque rhoncus 
              mattis nisi quis hendrerit. Aliquam consequat eget eros sed lobortis. Quisque sed elit 
              sed nisl euismod efficitur et ac ante. Donec ut mauris quis purus feugiat dictum vitae 
              commodo felis. Aenean semper auctor ultricies. Phasellus id sem et risus tempus mollis. 
              Cras sit amet mollis nisi. Nulla tempor in lorem in commodo.")
                     
        ), # tabPanel1
        
        tabPanel(title = "Map",
            titlePanel("Create a Map"),
            sidebarLayout(
                sidebarPanel(width = 3, 
                    
                    htmlOutput("state_selector_map"),#add selectinput boxs
                    htmlOutput("county_selector_map"), # from objects created in server
                    
                    br(),

                    div(align = "right",
                        actionButton(inputId = "getdata_map",
                            label = strong("Pull data"))),
                    
                    tags$head(
                        tags$style(HTML("h6 {font-size: 12px; text-align: right; line-height: 0.5em;}"))
                    ),
                    
                    h6("note: you must pull data before creating a map"), h6("and each time you wish to change geography"),
                    
                    hr(),
                    
                    htmlOutput("var_selector_map_rsei"),
                    
                    numericInput(inputId = "alpha_rsei", label = "RSEI Opacity:", value = 1,
                                 min = 0, max = 1, step = 0.1), br(),
                                 
                    htmlOutput("var_selector_map_acs"),
                    
                    numericInput(inputId = "alpha_acs", label = "ACS Opacity:", value = 0,
                                 min = 0, max = 1, step = 0.1),
                    
                    br(),
                    
                    div(align = "right",
                        actionButton(inputId = "assignattr",
                                     label = strong("Create map"))),
                    
                    ), # sidebar panel
                     
                mainPanel(
                    
                    tags$style(type = "text/css", "#rseimap {height: calc(90vh - 80px) !important;}"),
                    leafletOutput("rseimap")
                    
                    ) # main panel
                 ) # sidebar layout
        ), # tabPanel2
        
        tabPanel(title = "Data",
            titlePanel("Look at the Data"),
            sidebarLayout(
                sidebarPanel(width = 3,
                         
                    htmlOutput("state_selector_dt"), #add selectinput boxs
                    htmlOutput("county_selector_dt"), # from objects created in server
                    
                    br(),
                    
                    div(align = "right",
                        actionButton(inputId = "getdata_dt",
                                     label = strong("Pull data"))),
                    
                    hr(),
                    
                    htmlOutput("var_selector_dt")
                    
                    
                ), # sidebar panel
                     
                mainPanel(
                    
                    dataTableOutput(outputId = "table")
                         
                ) # main panel
            ) # sidebar layout
        ) # tabPanel3
        
        
    ) # navbarPage
        
    ) # fluidPage
) # shiny UI
