library(shiny)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(tidycensus)
library(DT)
library(mapview)

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
              
    navbarPage("Investigating Patterns of Environmental Justice",
        
        tabPanel(title = "Information",
                        
            titlePanel("Exploring RSEI, Race, and Income Data"),
                
            h3("RSEI Data"),
                
            h3("Other Data")
                     
        ), # tabPanel1
        
        tabPanel(title = "Map",
            titlePanel("Interactive Map"),
            sidebarLayout(
                sidebarPanel(width = 3, 
                    
                    htmlOutput("state_selector_map"),#add selectinput boxs
                    htmlOutput("county_selector_map"), # from objects created in server
                    
                    br(),

                    div(align = "right",
                        actionButton(inputId = "getdata_map",
                            label = strong("Create map"))),
                    hr(),
                    
                    htmlOutput("var_selector_map_rsei"),
                    htmlOutput("var_selector_map_acs")
                    
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
