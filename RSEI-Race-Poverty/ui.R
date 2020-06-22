library(shiny)
library(shinythemes)
library(shinyalert)
library(tidyverse)
library(tidycensus)
library(DT)
library(mapview)
library(leaflet)
library(broom)

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
            
            h4("What is RSEI?"), 
            
            p("According to the EPA, the \"Risk-Screening Environmental Indicators (RSEI) model helps policy makers, researchers and communities explore data on releases of toxic substances from industrial facilities. RSEI incorporates information from the Toxics Release Inventory (TRI) on the amount of toxic chemicals released, together with factors such as the chemical’s fate and transport through the environment, each chemical’s relative toxicity, and potential human exposure. RSEI Scores can be used to help establish priorities for further investigation.\""),
            
            p("RSEI data are collected on 810m x 810m grid overlaid on the United States. As such, they do not line up with typical geographic regions such as Census tracts or block groups. To read more about RSEI's methodology, go to ", a("https://www.epa.gov/sites/production/files/2014-03/documents/rsei_methodology_v2_3_2.pdf"), "."),
            
            h4("Variables"),
            
            p("The variables used in this app are described below. They are taken from the 2018 RSEI Aggregated Microdata Table and Census Crosswalk Table."),
            
            tags$ul(
                tags$li(code("X"), ": X-coordinate of grid cell"), 
                tags$li(code("Y"), ": Y-Coordinate of grid cell"), 
                tags$li(code("tox_conc"), ": Concentration multiplied by inhalation toxicity weight, summed over all chemicals impacting cell"),
                tags$li(code("score"), ": Risk-related score (surrogate dose * toxicity weight * population), summed over all chemicals impacting cell"),
                tags$li(code("pop"), ": Population of grid cell"),
                tags$li(code("score_cancer"), ": Risk-related score (surrogate dose * toxicity weight * population) using only toxicity values for cancer effects"),
                tags$li(code("score_non_cancer"), ": Risk-related score (surrogate dose * toxicity weight * population) using only toxicity values for cancer effects"),
                tags$li(code("block_id00"), ": US Census Block ID"),
                tags$li(code("pct_pc_b"), ": Percent of the cell’s population that is within the Census block (Population-Cell to Block)")
            ),
            
            p("For more information on RSEI scores and their interpretations, go to", a("https://www.epa.gov/rsei/understanding-rsei-results", ".")),
            
            h4("Calculated Variables"),
            
            p("The above variables were used to calculate RSEI data at the block group level to match the geography of ACS data. The variables generated were ", code("tox_conc_blckgrp"), ", ", code("score_blckgrp"), ", ", code("score_cancer_blckgrp"), ", and ", code("score_noncancer_blckgrp"), ". The formulas below show how these were calculated, where RSEI stands in for any one of these variables."),
            
            p(withMathJax(helpText('Population:  $$Population_{Block \\cap {Grid}_i} = {Population}_{{Grid}_i} * pct\\_pc\\_b $$'))),
            
            p(withMathJax(helpText('Block:  $$RSEI_{Block} = \\sum_{i=1}^{n_{Grids}} \\frac{{Population}_{Block \\cap {Grid}_i}}{\\sum_{j=1}^{n_{Grids}} {Population}_{Block \\cap {Grid}_j}} * RSEI_{{Grid}_i} $$'))),
            
            p(withMathJax(helpText('Block Group:  $$RSEI_{BlockGrp} = \\sum_{i=1}^{n_{Blocks}} \\frac{{Population}_{{Block}_i}}{\\sum_{j=1}^{n_{Blocks}} {Population}_{{Block}_j}} * RSEI_{{Block}_i} $$'))),
            
            h3("Race and Income Data: American Community Survey (ACS)"),
            
            p("According to the US Census Bureau, \"The American Community Survey (ACS) is an ongoing survey that provides vital information on a yearly basis about our nation and its people. Information from the survey generates data that help determine how more than $675 billion in federal and state funds are distributed each year.\""),
            
            h4("Variables"),
            
            p("The variables used in this app are described below. They are taken from 2018 ACS 5-year data at the block group level."),
            
            
            tags$ul(
                tags$li(code("n_total"), ": total population"), 
                tags$li(code("n_white"), ": population of race white alone"), 
                tags$li(code("n_black"), ": population of race black or african american alone"), 
                tags$li(code("n_natam"), ": population of race american indian or alaskan native alone"), 
                tags$li(code("n_hisp"), ": population of hispanic or latino origin"), 
                tags$li(code("median_hh_inc"), ": median household income in the past 12 months (In 2018 inflation-adjusted dollars)"), 
                tags$li(code("n_inc_less10k"), ": population with income less than $10,000"), 
                tags$li(code("n_inc_10k_14k"), ": population with income $10,000 to $14,999"), 
                tags$li(code("n_inc_15k_19k"), ": population with income $15,000 to $19,999"), 
                tags$li(code("n_inc_less20k"), ": population with income less than $20,000")
            ),
            
            p("These variables were all converted to percents by dividing by ", code("n_total"), "."),
            
            
            p()
                     
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
                                 
                    htmlOutput("var_selector_map_acs"),
                
                    br(),
                    
                    div(align = "right",
                        actionButton(inputId = "assignattr",
                                     label = strong("Create map")))
                    
                    ), # sidebar panel
                     
                mainPanel(
                    
                    column(
                        width = 6, 
                        tags$style(type = "text/css", "#rseimap {height: calc(90vh - 80px) !important;}"),
                        leafletOutput("rseimap")
                    ),
                    
                    column(
                        width = 6,
                        tags$style(type = "text/css", "#acsmap {height: calc(90vh - 80px) !important;}"),
                        leafletOutput("acsmap")
                    )
                    
                    
                    
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
        ), # tabPanel3
        
        tabPanel(title = "Model",
            titlePanel("Create Model"),
            sidebarLayout(
                sidebarPanel(width = 3, 
                             
                    htmlOutput("state_selector_tr"), #add selectinput boxs
                    htmlOutput("county_selector_tr"), # from objects created in server
                    
                    br(),
                    
                    div(align = "right",
                        actionButton(inputId = "getdata_tr",
                                     label = strong("Pull data"))),
                    
                    hr(),
                    
                    htmlOutput("var_selector_tr_rsei"),
                    htmlOutput("var_selector_tr_acs")
                    
                ), # sidebar panel
                mainPanel(
                    
                    tableOutput(outputId = "lmtable")
                    
                    
                ) # main panel
            ) # sidebarLayout
            
            
        ) # tabPanel4
        
        
    ) # navbarPage
        
    ) # fluidPage
) # shiny UI