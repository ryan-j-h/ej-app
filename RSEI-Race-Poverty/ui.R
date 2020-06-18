library(shiny)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(tidycensus)
library(DT)
library(mapview)

census_api_key("691c56c505ab23560453873cc6768f41206683b4")
data(fips_codes)
rsei <- readRDS("rsei.rds")

shinyUI(
    fluidPage(theme = shinytheme("sandstone"),
              useShinyalert(),
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

                    hr(),

                    div(align = "right",
                        actionButton(inputId = "getdata_map",
                            label = strong("Create map")))
                    ), # sidebar panel
                     
                mainPanel(
                    
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
                    
                    hr(),
                    
                    div(align = "right",
                        actionButton(inputId = "getdata_dt",
                                     label = strong("Pull data")))
                ), # sidebar panel
                     
                mainPanel(
                    
                    dataTableOutput(outputId = "table")
                         
                ) # main panel
            ) # sidebar layout
        ) # tabPanel3
        
    ) # navbarPage
        
    ) # fluidPage
) # shiny UI
