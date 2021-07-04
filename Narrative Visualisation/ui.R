#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


list.of.packages <- c("shiny","shinythemes","rintrojs","shinycustomloader","plotly","dygraphs","leaflet"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(shinythemes)
library(rintrojs)
library(shinycustomloader)
library(plotly)
library(dygraphs)
library(leaflet)


#  UI of shiny dashboard app
shinyUI (
 
  fluidPage(  theme = shinytheme("flatly"), # theme
      introjsUI(),  # tour package initiation
            
      # external css jss file linkage in www/
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$script(src = "custom.js")
      ),
      
      
      navbarPage("Traffic Stoppages",
        
        # tab 1: dashboard
        tabPanel("Dashboard",
                 
                 
                 fixedRow(
                     column(width = 12,
                          # values populated dynamically from server
                          uiOutput("selectState_dashboard") # global state/county dropdown
                       ) 
                   
                 ),
                 
                 tags$hr(),
                 tags$br(),
                 fixedRow(
                   
                   column(width = 3,
                          wellPanel(id="well-treemap",
                            uiOutput("state_intro_text"),
                            tags$br(),
                            uiOutput("diversity_text")
                          )
                   ),
                   
                   # button which calls introjs to start the introduction
                   actionButton(
                     inputId='starthelp', 
                     label='Take a Tour',
                     `data-step`='1',
                     onclick='introJs().start();'
                   ),
                   

                   column(width = 9,
                          
                          introBox(
                            
                            wellPanel(class="filter-graph-header",
                                  # filters treemap by year
                                  selectInput("pop_by_year", label = "Select Year",
                                              choices = c("2013", "2014", "2015"),
                                              selected = "2015"),
                                  
                                      
                            ),
                            data.step = 1,
                            data.intro = "'Select Year' to get Diversity proportion of various Race/Ethnicity of that year."
                          ),
                          
                          introBox(
                            wellPanel(
                              withLoader( plotlyOutput("race_treemap"), type="html", loader='loader4')
                            ),
                            data.step = 2,
                            data.intro = "Hover over to see the percentages and population numbers."
                          )
                   )
                 ),
          
                 fixedRow(

                   column(width = 12,
                          
                          introBox(
                            wellPanel(class="filter-graph-header extended",
                               # heatmap violation dropdown
                               # values populated dynamically from server
                               tags$p(
                                 uiOutput("heatmap_narration")
                               ),
                               tags$div(
                                 uiOutput("heatmap_violation_filter")
                               )
                              
                            ),
                            data.step = 3,
                            data.intro = "'Select Violation' to get age-group wise violation and its frequency of occurrences by weekday-time."
                            
                          )

                   ),

                   column(width = 5, id="tornado-chart",
                          
                          introBox(
                            wellPanel(
                              withLoader( plotlyOutput("stoppages_age_distribution"), type="html", loader='loader4')
                            ),
                            data.step = 4,
                            data.intro = "Hover over the bars to get percentage rate conduct of the violation by gender. Feel free to filter out gender by clicking from legend."
                          )
                          
                   ),
                   column(width = 7, id="heatmap",
                          introBox(
                            wellPanel(
                              withLoader( plotlyOutput("week_time_violation_heatmap"), type="html", loader='loader4')
                            ),
                            data.step = 5,
                            data.intro = "Hover over the boxes to get the count of violations on weekday-hour basis"
                          )
                          
                   )
                   
                 ),
                 
                 fixedRow(
                   
                   column(width = 1,
                   ),
                   column(width = 10,
                          introBox(
                            wellPanel(
                              withLoader( plotlyOutput("violation_stacked_bar"), type="html", loader='loader4')
                            ),
                            data.step = 6,
                            data.intro = "Hover over the bars to get driver race, violation and rate of violation. Feel free to filter out race(s) by clicking from legend."                            
                          )
                   ),
                   column(width = 1,
                   )
                 ),
                 
                 fixedRow(
                   
                   column(width = 4,
                          wellPanel(id="extended-narrative",
                                    uiOutput("sync_plot_narration"),
                                    tags$br(),
                                    uiOutput("dynamic_sync_plot_narration")
                          )
                   ),
                   
                   column(width = 8,
                          
                          introBox(
                            
                            wellPanel(class="filter-graph-header",
                                      selectInput("arrest_plotby", label = "Filter by",
                                                  choices = c("Month and Year", "Year"),
                                                  selected = "Year")
                                    
                            ),
                            
                            data.step = 7,
                            data.intro = "'Filter by' either 'Month and Year' or 'Year' to see the data accordingly. 'Month and Year' shows the Arrest/Search Counts while the 'Year' shows Arrest/Search Rate(%)."                            
  
                          ),
                          
      
                          introBox(
                              wellPanel(
                                tags$br(),
                                tags$br(),
                                withLoader( dygraphOutput("rate_of_arrest", width = "100%", height = "300px"), type="html", loader='loader4'),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                withLoader( dygraphOutput("rate_of_search", width = "100%", height = "300px"), type="html", loader='loader4')
                              ),
                              data.step = 8,
                              data.intro = "Syncronised Arrest and Search plots by race. To change the timeframe or drill down, feel free to slide timeframe range. Hover over the points to see values along with the legend."
                          )
                          
                   )
                 ),
                 
                   fixedRow(
                     
                     introBox(
                       column(width = 12,
                              conditionalPanel(condition = "input.us_state_dashboard == 'rhode island'",
                                               wellPanel(
                                                 withLoader( dygraphOutput("drugs_stoppage"), type="html", loader='loader4')
                                               )
                              )
                              
                       ),
                       data.step = 9,
                       data.intro = "(available only for Rhode Island State) 'Select 'Rhode Island' state to see the drug rate data. Some important events which may have caused downfall 
                       in search rate due criminals being more precautious are annotated. Feel free to hover them to get the event details. Also to change the timeframe, select
                       the area of interest by select (left click) + dragging."
                       
                     )
                   ),

                 
        ),
        # tab 2: explore locations
        tabPanel("Explore Locations",
                 
                 fixedRow(
                   
                   column(width = 12,
                          
                          withLoader( leafletOutput(outputId = "leaflet_map"), type="html", loader='loader4'),
                          
                          # absolute positioned box with interaction options
                          absolutePanel(id="map-filters",
                            uiOutput("selectinputelement"),
                            actionButton(inputId = "my_button2",
                                         label = "Aggregate Stoppages"),
                            actionButton(inputId = "my_button1",
                                         label = "Play Single Day Stoppages"),
                            uiOutput("animated_time_slider")
                          )
       
                   )
                 )
                 
                 
        )
                 
                 
     )
            
            
  )
  
  
) 