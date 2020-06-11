library(shiny)
library(shinythemes)

shinyUI(
    fluidPage(theme = shinytheme("sandstone"),
    navbarPage("Investigating Patterns of Environmental Justice",
        
        tabPanel(title = "Information",
                        
                titlePanel("Exploring RSEI, Race, and Poverty Data"),
                
                h3("RSEI Data"),
                
                h3("Other Data")
                     
        ), # tabPanel1
        
        tabPanel(title = "Map",
                 titlePanel(
                     "Interactive Map!"
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         
                     ), # sidebar panel
                     
                     mainPanel(
                     ) # main panel
                 ) # sidebar layout
        ), # tabPanel2
        
        tabPanel(title = "Data",
                 titlePanel(
                     "Look at the Data!"
                 ),
                 sidebarLayout(
                     sidebarPanel(
                         
                     ), # sidebar panel
                     
                     mainPanel(
                         
                     ) # main panel
                 ) # sidebar layout
        ) # tabPanel3
        
    ) # navbarPage
        
    ) # fluidPage
) # shiny UI
