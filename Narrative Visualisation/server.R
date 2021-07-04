#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("shiny","tidyverse","lubridate","ggplot2","plotly","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(reshape2)

config(plot_ly(), displaylogo = FALSE)

# server code of dashboard
shinyServer(function(input, output, session) {
  
  # wrangled population dataset by race/ethnicity
  population_dataset <- read.csv('datasets/wrangled_population_dataset.csv', header=T)
  
  # loadin up of cleaned states datasets
  rhode_island <- read.csv('datasets/cleaned_datasets/cleaned_rhoad_island.csv', header=T)
  hartford <- read.csv("datasets/cleaned_datasets/cleaned_ct_hartford.csv", header = T)
  hartford$stop_date <- as.Date(hartford$stop_date, format="%d/%m/%y")
  vermont <- read.csv("datasets/cleaned_datasets/cleaned_vermont.csv", header = T)
  vermont$stop_date <- as.Date(vermont$stop_date, format="%d/%m/%y")
  
  # defining custom color palette
  pal <- list(
    White = "rgb(44,123,182)",
    Black = "rgb(215,25,28)",
    Asian = "rgb(123,50,148)",
    Hispanic = "rgb(253,174,97)"
    
  )

  
  # binding the datasets so as to apply common operations at the same time
  states_dataset <- list(rhode_island, hartford, vermont)
  
  # attribute datatype conversion function
  ChangeType <- function(df){
    
    df$driver_gender<-as.factor(df$driver_gender)
    
    df$violation <- as.factor(df$violation)
    df$stop_outcome<-as.factor(df$stop_outcome)
    
    if("stop_duration" %in% colnames(df)){
      df$stop_duration<-as.factor(df$stop_duration)
    }
    
    
    df$search_conducted<-as.logical(df$search_conducted)
    df$is_arrested<-as.logical(df$is_arrested)
    
    if("drugs_related_stop" %in% colnames(df))
    {
      df$drugs_related_stop<-as.logical(df$drugs_related_stop)
    }
    
    # Date Time column creation
    df$stop_date_time<-paste(df$stop_date,df$stop_time)
    df$stop_time<-substr(df$stop_date_time, 12, 16)
    
    df$stop_date_time<-as.POSIXct(df$stop_date_time, tz = "EST")
    df$stop_date<-as.POSIXct(df$stop_date)
    
    df
  }
  
  
  #  applying ChangType function to the state's datasets 
  states_dataset <- lapply(states_dataset, ChangeType) 
  
  # rendering dynamic UI to selectState_dashboard dropdown element
  output$selectState_dashboard <- renderUI({
    selectInput(
      "us_state_dashboard", label = "US State/County", choices = c("rhode island","hartford","vermont"),
      selected = "rhode island", multiple = FALSE
    )
  })
  
  # reactive component which returns the state dataset according to the
  # option selected from us_state_dashboard dropdown
  state_global_filter = reactive({
    if (input$us_state_dashboard == 'rhode island'){
      dashboard_dataset <- states_dataset[[1]]
      
      output$state_intro_text <- renderUI({ 
        HTML(str_glue(
          '<div>
          <h3>Diversity</h3><br>
          <p>Rhode Island is the smallest USA state covering an area of approx 3,144 km square. It is
          a state in the New England region of the northeastern United States</p> 
          </div>'
        ) )
        
      })
      
      output$dynamic_sync_plot_narration <- renderUI({ 
        black_prop = round(r_race_treemap_data()$values[4], 2)
        
        HTML(str_glue(
          '<p>
              We saw that the population proportion of Black is <b>{black_prop} %</b> but still, they have been arrested on an average <b> 7.34 times </b> more than that of White\'s arrest rate.
              White\'s population is so high in numbers that their arrest rate when rounded tends to result in constant value over these years.
            </p>',
          '<p>
              A similar result was seen in the Search rate as well where the average was <b> 6.67 times </b> more than that of White\'s search rate.
            </p>',
          '<p>
               It is evident that there is bias. Also correlation prevails between the search and arrest rate as the trend remains nearly the same.
            </p>',
          '<p><small><small><i>If you filter by "Month and Year", counts of arrest/search are returned which do not consider the population numbers into account.<i></small></small></p>'
        ) )
        
      })
      
      
    }
    if (input$us_state_dashboard == 'hartford'){
      dashboard_dataset <- states_dataset[[2]]
      
      output$state_intro_text <- renderUI({ 
        HTML(str_glue(
          '<div>
          <h3>Diversity</h3><br>
          <p>Hartford is the capital city of the U.S. state of Connecticut covering area of 46.5 km square. 
          In 2015 the reported popultion was 124,006. </p> 
          </div>'
        ) )
        
      })
      
      output$dynamic_sync_plot_narration <- renderUI({ 
        black_prop = round(r_race_treemap_data()$values[4], 2)
        
        HTML(str_glue(
          '<p>
              We saw that the population proportion of Black is <b>{black_prop} %</b> but still, they have been arrested on an average <b> 6.5 times </b> more than that of White\'s arrest rate.
              White\'s population is so high in numbers that their arrest rate when rounded tends to result in constant value over these years.
            </p>',
          '<p>
              A similar result was seen in the Search rate as well where the average was <b> 7.05 times </b> more than that of White\'s search rate.
            </p>',
          '<p>
               It is evident that there is bias. Also correlation prevails between the search and arrest rate as the trend remains nearly the same.
            </p>',
          '<p><small><small><i>If you filter by "Month and Year", counts of arrest/search are returned which do not consider the population numbers into account.<i></small></small></p>'
        ) )
        
      })
      
    }
    if (input$us_state_dashboard == 'vermont'){
      dashboard_dataset <- states_dataset[[3]]
      
      output$state_intro_text <- renderUI({ 
        HTML(str_glue(
          '<div>
          <h3>Diversity</h3><br>
          <p>Vermont is a state in the north-eastern part of United States, known for natural landscape.</p> 
          </div>'
        ) )
        
      })
      
      output$dynamic_sync_plot_narration <- renderUI({ 
        black_prop = round(r_race_treemap_data()$values[4], 2)
        
        HTML(str_glue(
          '<p>
              We saw that the population proportion of Black is <b>{black_prop} %</b> but still, they have been arrested on an average <b> 3.25 times </b> more than that of White\'s arrest rate.
              White\'s population is so high in numbers that their arrest rate when rounded tends to result in constant value over these years.
            </p>',
          '<p>
              A similar result was seen in the Search rate as well where the average was <b> 9.83 times </b> more than that of White\'s search rate.
            </p>',
          '<p>
               It is evident that there is bias. Also correlation prevails between the search and arrest rate as the trend remains nearly the same.
            </p>',
          '<p><small><small><i>If you filter by "Month and Year", counts of arrest/search are returned which do not consider the population numbers into account.<i></small></small></p>',
        ) )
        
      })
    }
    
    return (dashboard_dataset)
  })
  
  
  # DRUGS SEARCH VISUALISATION
  
  # reactive component filtering, modifying dataset to prepare for 
  # drugs search rate visualisation
  drug_related_data =  reactive({
    # month wise year dummy dataframe creation
    month_year_df_series <- data.frame(ymd = seq(ymd("2006-01-01"), ymd("2015-12-01"), by = "months"))
    
    # summarising the data by grouping month-year and summarising count of drug_check_count
    drugs_filtered <- state_global_filter() %>%
      filter(year(stop_date) != 2005) %>%
      filter(drugs_related_stop ==  TRUE) %>%
      group_by(year(stop_date), month(stop_date)) %>%
      summarize(drug_check_count= n())
    
    # changing column names
    colnames(drugs_filtered)[1] <- "year"
    colnames(drugs_filtered)[2] <- "month"
    
    drugs_filtered$ymd <- ymd(paste0(drugs_filtered$year, " ", drugs_filtered$month, " ", "1"))
    
    # merging the dummy and actual filtered data so as to get continuous values to be plotted
    # where no value are found of the month 0 is imputed
    merged_drug_df <- merge(month_year_df_series, drugs_filtered,  all.x = TRUE) # merge data.frames (left join)
    merged_drug_df$drug_check_count <- ifelse(is.na(merged_drug_df$drug_check_count), 0, merged_drug_df$drug_check_count)
    
    # transforming to timeseries using ts package ts() function
    ts_drug <- ts(merged_drug_df$drug_check_count, freq = 12, start = c(2006, 1), end = c(2015, 12))
    
    ts_drug_year <- (aggregate(ts_drug, 1, sum))
    ts_drug_year <- data.frame(count_total_month=as.matrix(ts_drug_year), year=time(ts_drug_year))
    
    rate_drugs_merged <- merge(merged_drug_df, ts_drug_year,  all.x = TRUE)
    rate_drugs_merged$count_total_month <- ifelse(is.na(rate_drugs_merged$count_total_month), 1, rate_drugs_merged$count_total_month)
    rate_drugs_merged$rate_by_month <- round( (rate_drugs_merged$drug_check_count / rate_drugs_merged$count_total_month)*100, 2) # calculating rate(%)
    
    ts_drug <- ts(rate_drugs_merged$rate_by_month, freq = 12, start = c(2006, 1), end = c(2015, 12))
    
    return (ts_drug)
    
  })
  
  
  # plotting the drugs search graph using dygraph with annotational events
  # Source of event: https://www.justice.gov/usao-ri/pr/3rd-defendant-rhode-islands-largest-cocaine-bust-sentenced-15-12-years-federal-prison
  output$drugs_stoppage <- renderDygraph({
    
    dygraph(drug_related_data(), main="Drugs") %>%
      # dyRoller(rollPeriod = 10) %>%
      dySeries("V1", label = "Drug Search %") %>%
      dyOptions(
        fillGraph = TRUE, 
        fillAlpha = 0.6,
        axisLineColor = "navy", 
        drawPoints = TRUE,
        drawGapEdgePoints = TRUE,
        gridLineColor = "lightblue",
        strokeWidth = 1,
        animatedZooms = TRUE,
        axisLabelFontSize = 12,
        gridLineWidth = 0.25
      ) %>%
      dyAnnotation("2011-01-01", text = "A1", width = 20, height=15, tooltip = "Rhode Island's Largest Cocaine Bust of 65kgs") %>%
      dyAnnotation("2012-10-01", text = "A2", width = 20, tooltip = "Accused Pleaded Guilty") %>%
      dyAnnotation("2012-11-01", text = "A3", width = 20, tooltip = "Abettors Sentenced 156 months and 135 month in prison") %>% 
      dyAnnotation("2013-02-01", text = "A4", width = 20, tooltip = "Major Accussed Sentenced 15 1/2 Years") 
    
  })
  
  
  # ARREST AND SEARCH VISUALISATION
  
  # reactive component filtering, modifying dataset to prepare for 
  # arrest and search counts/rate synchronised visualisation
  arrest_filter_data=reactive({
    
    month_year_df_series <- data.frame(ymd = seq(ymd("2005-01-01"), ymd("2015-12-01"), by = "months"))
    
    # for each race perform the proceeding operations
    for (i in c("White", "Black", "Asian", "Hispanic") ) { 
      
      #  Arrest 
      arrest_filtered <- state_global_filter() %>%
        filter(driver_race==i) %>%
        filter(is_arrested ==  TRUE) %>%
        group_by(year(stop_date), month(stop_date)) %>%
        summarize(count_of_arrest = n())
      
      arrest_filtered$ymd <- ymd(paste0(arrest_filtered$`year(stop_date)`, " ", arrest_filtered$`month(stop_date)`, " ", "1"))
      
      merged_arrest_df <- merge(month_year_df_series, arrest_filtered,  all.x = TRUE) # merge data.frames (left join)
      merged_arrest_df$count_of_arrest <- ifelse(is.na(merged_arrest_df$count_of_arrest), 0, merged_arrest_df$count_of_arrest) # substitute NA's to 0's
      
      ts_arrest <- ts(merged_arrest_df$count_of_arrest, freq = 12, start = c(2005, 1), end = c(2015, 12)) # transform to ts
      
      #  Search
      searched_filtered <- state_global_filter() %>%
        filter(driver_race == i) %>%
        filter(search_conducted == TRUE) %>%
        group_by(year(stop_date), month(stop_date)) %>%
        summarize(count_of_search = n())
      
      searched_filtered$ymd <- ymd(paste0(searched_filtered$`year(stop_date)`, " ", searched_filtered$`month(stop_date)`, " ", "1"))
      
      merged_searched_df <- merge(month_year_df_series, searched_filtered,  all.x = TRUE)
      merged_searched_df$count_of_search <- ifelse(is.na(merged_searched_df$count_of_search), 0, merged_searched_df$count_of_search)
      
      ts_search <- ts(merged_searched_df$count_of_search, freq = 12, start = c(2005, 1), end = c(2015, 12)) # transform to ts
      
      #  if option selected is "month and year" save/update the dataframe
      # to display the counts
      if (input$arrest_plotby == 'Month and Year'){
        
        assign(paste0("arrest_",tolower(i)),ts_arrest) # arrest
        assign(paste0("search_",tolower(i)),ts_search) # searched
        
      }
      #  if option selected is "Year", aggregate the data from "Month and Year" 
      #  and save/update the dataframe to display rate (%) by dividing per year population
      #  numbers of races in the state
      if (input$arrest_plotby == 'Year'){
        
        ts_arrest_year <-  aggregate(ts_arrest, 1, sum)
        ts_search_year <- aggregate(ts_search, 1, sum)
        
        ts_arrest_year <- data.frame(count=as.matrix(ts_arrest_year), Year=time(ts_arrest_year))
        ts_search_year <- data.frame(count=as.matrix(ts_search_year), Year=time(ts_search_year))
        
        ts_arrest_year <- merge(population_dataset%>%filter(Race == i & Geography == input$us_state_dashboard), ts_arrest_year)
        ts_search_year <- merge(population_dataset%>%filter(Race == i & Geography == input$us_state_dashboard), ts_search_year)
        
        ts_arrest_year['rate_of_arrest'] <- round( (ts_arrest_year['count'] / ts_arrest_year['Population'])*100, 2)
        ts_search_year['rate_of_search'] <- round( (ts_search_year['count'] / ts_search_year['Population'])*100, 2)
        
        ts_arrest_year <- ts_arrest_year %>% select('Year','rate_of_arrest')
        ts_search_year <- ts_search_year %>% select('Year','rate_of_search')
        
        ts_arrest_year <- ts(ts_arrest_year$rate_of_arrest, freq = 1, start = 2013, 1, end = 2015)
        ts_search_year <- ts(ts_search_year$rate_of_search, freq = 1, start = 2013, 1, end = 2015)
        
        assign(paste0("arrest_",tolower(i)),ts_arrest_year)
        assign(paste0("search_",tolower(i)),ts_search_year)
        
      }
      
    }
    
    # dynamically assigning default datewindow and axis label names
    if (input$arrest_plotby == 'Month and Year'){
      dateWindow = c("2015-01-01", "2015-12-01")
      arrest_plot_y_label = "Arrest Count"
      search_plot_y_label = "Search Count"
  
    }
    # dynamically assigning default datewindow and axis label names
    if (input$arrest_plotby == 'Year'){
      dateWindow = c("2013-01-01", "2015-04-01")
      arrest_plot_y_label = "Arrest Rate (%)"
      search_plot_y_label = "Search Rate (%)"
    }
    
    # return a list containing binded object of all races dataframe manipulated data
    # and variables for default date window and axis labels
    return (
      list(
        arrest_binded = cbind(arrest_asian, arrest_black, arrest_hispanic, arrest_white),
        search_binded = cbind(search_asian, search_black, search_hispanic, search_white),
        dateWindow = dateWindow,
        arrest_plot_y_label = arrest_plot_y_label,
        search_plot_y_label =  search_plot_y_label
      )
    )
  })
  
  
  
  # WEEKDAY-HOUR HEAT MAP OF VIOLATION
  
  # rendering dynamic UI to with value of unique violation of a state selected
  output$heatmap_violation_filter <- renderUI({
    selectInput(
      "heatmap_violation_select", label = "Select Violation", choices = unique(state_global_filter()$violation) ,
      selected = unique(state_global_filter()$violation)[2], multiple = FALSE
    )
  })
  
  
  heatmap_violation_data <- reactive({
    
    violation_day_time <- state_global_filter() %>%
      filter(violation == input$heatmap_violation_select) %>%
      group_by(hour = hour(stop_date_time), day = weekdays(stop_date_time)) %>%
      summarise(violation_count = n())

    return(violation_day_time)
  })
  
  
  # plotting heatmap using ggplot and plotly
  output$week_time_violation_heatmap <- renderPlotly({
    
    # manually creating weekdays names and hour numbers
    x_heatmap <- (0:23)
    y_heatmap <- weekdays(Sys.Date()+0:6)
    
    # creates all possible combinations of weekday - hour
    possibilities_comb <- expand.grid(hour=x_heatmap, day=y_heatmap)
    
    data_heatmap <- merge(possibilities_comb, heatmap_violation_data(), all.x = TRUE)
    
    data_heatmap$violation_count <- ifelse(is.na(data_heatmap$violation_count), 0, data_heatmap$violation_count) # substitute NA's to 0's
    
    # changing to factor with levels to make sure plot shows the values in correct order
    data_heatmap$hour <-  factor(data_heatmap$hour, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
    data_heatmap$day <- factor(data_heatmap$day, levels = c('Sunday', 'Saturday', 'Friday', 'Thursday', 'Wednesday', 'Tuesday', 'Monday'))
    
    data_heatmap$scaled_violation_count <- scale(data_heatmap$violation_count)
    
    # tootlip text
    data_heatmap <- data_heatmap %>%
      mutate(text = paste0("Day: ", day, "\n", "Hour: ", hour, "\n", "Count: ",round(violation_count,2)))
    
    # plotting heatmap with  ggplot and plotly
    heatmap_plot <- ggplot(data_heatmap, aes(hour, day, text=text)) + 
      geom_tile(aes(fill = violation_count)) +
      scale_fill_gradient(low="white", high="red") +
      labs(
        title="Hour vs Day of Week" ,
        y = "Day of Week", x="Hour of Day (hr)", fill="Count")+
      theme_minimal(base_size = 11)
    
    ggplotly(heatmap_plot, tooltip="text") %>% config(displayModeBar = FALSE)
  })
  
  
  
  # BUTTERFLY PLOT OF AGE DISTRIBUTION BY VIOLATION
  
  # reactive component for age group visualisation
  r_stoppages_age_distribution <- reactive({
    
    age_data <- state_global_filter() %>%
      select(driver_age, driver_gender, violation)
    
    # Age groups intervals formation
    
    dm = min(age_data$driver_age) - 1
    age_data$group <- cut(
      age_data$driver_age,
      c(dm, dm+5, dm+10, dm+15, dm+20, dm+25, dm+30, dm+35, dm+40, dm+45,  max(age_data$driver_age))
    )
    levels(age_data$group) = c(
      paste(toString(dm), toString(dm+5), sep="-"),
      paste( toString(dm+5) , toString(dm+10)  ,sep="-"),
      paste( toString(dm+10) , toString(dm+15)  ,sep="-"),
      paste( toString(dm+15) ,  toString(dm+20) ,sep="-"),
      paste( toString(dm+20) , toString(dm+25)  ,sep="-"),
      paste( toString(dm+25) , toString(dm+30)  ,sep="-"),
      paste( toString(dm+30) ,  toString(dm+35) ,sep="-"),
      paste( toString(dm+35) , toString(dm+40)  ,sep="-"),
      paste( toString(dm+40) , toString(dm+45)  ,sep="-"),
      paste( toString(dm+45) ,"", sep="+")
    )
    
    # filtering data according to the violation selected
    age_data <- age_data %>% filter(violation==input$heatmap_violation_select)
    age_group_rate <- age_data %>% filter(violation==input$heatmap_violation_select) %>% group_by(group, driver_gender) %>% summarize(freq = n())
    pop <- age_data %>% group_by(driver_gender) %>% summarise(pop = n())
    age_group_rate <- merge(age_group_rate, pop)
    
    # calculating rate (%)
    age_group_rate <- age_group_rate %>% filter(driver_gender=='F' | driver_gender=='M') %>%
      mutate(rate_stopping=case_when(driver_gender=='M'~round(freq/pop*100,2),
                                     TRUE~-round(freq/pop*100,2)),
             signal=case_when(driver_gender=='M'~1, TRUE~-1)
      )
    
    return(age_group_rate)
  })
  
  # custom manipulated barplot converted to butterfly plot using ggplotly
  output$stoppages_age_distribution <- renderPlotly({
    
    gg_age_group_rate<- ggplot(r_stoppages_age_distribution())+
      geom_bar(aes(x=group,y=rate_stopping,fill=driver_gender),stat='identity')+
      # geom_text(aes(x=group, y=rate_stopping+signal*2, label=paste(abs(rate_stopping),"%")), size=3)+
      coord_flip()+
      scale_fill_manual(name='',values=c('#ff9da7','#8ed3c7'))+
      scale_y_continuous(breaks=seq(-100,100,10),
                         labels=function(x){paste(abs(x),'%')})+
      labs(x='Age Group',y='Stoppage (%)', fill="Gender",
           title='Age Group vs Gender'
           # subtitle=paste('Total resident population in 2019:', format(sum(r_stoppages_age_distribution()$pop),big.mark='.'))
      )+
      theme_minimal(base_size = 11)
    
    ggplotly(gg_age_group_rate) %>% config(displayModeBar = FALSE)
    
  })
  
  
  
  
  # STACKED VIOLATION BAR CHART
  
  # plotting stacked bar chart of rate of violation by each race
  output$violation_stacked_bar <- renderPlotly({
  
      gg_plot <- ggplot( state_global_filter() %>% filter(driver_race %in% c('Asian','Black','Hispanic','White')), 
              aes(x = violation)) + 
        geom_bar(aes(y = round(..count../sum(..count..)*100, 2), fill = driver_race, text=paste('Rate:', round(..count../sum(..count..)*100, 2), "%\nRace:", fill))) + 
        scale_fill_manual(values = c("rgba(123,50,148,0.8)", "rgba(215,25,28,0.8)", "rgba(253,174,97, 0.8)", "rgba(44,123,182,0.8)")) +
        coord_flip() +
        ylab("Rate (%)") + 
        xlab("Violation") +
        labs(fill = "Driver Race") +
        ggtitle("Violation Rate by Race") +
        theme_minimal()
      
    ggplotly (gg_plot, tooltip=c("text")) %>% config(displayModeBar = FALSE) 
  })
  
  
  # POPULATION PROPORTION TREEMAP
  
  r_race_treemap_data <- reactive({
    treemap_data <- population_dataset %>% filter(Year==input$pop_by_year & Geography==input$us_state_dashboard)
    treemap_data$Share <- round(treemap_data$Share*100, 2)
    race_labels <- c(input$us_state_dashboard, c(treemap_data$Race))
    state_total_pop <- sum(treemap_data$Share)
    values <- c(state_total_pop, c(treemap_data$Share))
    
    return (list(
      labels = race_labels,
      state = input$us_state_dashboard,
      values = values
    ))
    
  })
  
  # plotting treemap using plot_ly()
  output$race_treemap <- renderPlotly({
    
    parents = c("", rep(r_race_treemap_data()$state, length(r_race_treemap_data()$labels)-1))
    
    plot_ly(
      type='treemap',
      labels=r_race_treemap_data()$labels,
      parents=parents,
      values= r_race_treemap_data()$values,
      textinfo="label+value",
      domain=list(column=0),
      marker = list(colors=c("red","rgba(44,123,182,0.9)", "rgba(123,50,148,0.8)","rgba(215,25,28,0.8)"))
    ) %>%
      layout(paper_bgcolor='transparent')  %>% config(displayModeBar = FALSE)
    
  })
  
  
  # plotting the arrest rate viz using dygraph with dyRangeSelector interation
  output$rate_of_arrest <- renderDygraph({
    
    dygraph(arrest_filter_data()$arrest_binded, main=arrest_filter_data()$arrest_plot_y_label, group = "race_search_arrest") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = arrest_filter_data()$arrest_plot_y_label) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(
        colors = c(pal$Asian, pal$Black, pal$Hispanic, pal$White),
        drawPoints = TRUE,
        pointSize = 2,
        drawGapEdgePoints = TRUE,
        gridLineColor = "lightblue",
        strokeWidth = 1,
        animatedZooms = TRUE,
        axisLabelFontSize = 11,
        gridLineWidth = 0.25
      ) %>%
      dyLegend(width = 600) %>%
      dyRangeSelector(height = 25, strokeColor = "",
                      fillColor = "rgba(69, 84, 88, 0.4)",
                      dateWindow = arrest_filter_data()$dateWindow)
  })
  
  # plotting the search rate viz using dygraph with dyRangeSelector interation
  output$rate_of_search <- renderDygraph({
    
    dygraph(arrest_filter_data()$search_binded, main=arrest_filter_data()$search_plot_y_label, group = "race_search_arrest") %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = arrest_filter_data()$search_plot_y_label) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(
        colors=c(pal$Asian, pal$Black, pal$Hispanic, pal$White),
        drawPoints = TRUE,
        pointSize = 2,
        drawGapEdgePoints = TRUE,
        gridLineColor = "lightblue",
        strokeWidth = 1,
        animatedZooms = TRUE,
        axisLabelFontSize = 11,
        gridLineWidth = 0.25
      ) %>%
      
      dyLegend(width = 600) %>%
      dyRangeSelector(height = 25, fillColor = "rgba(69, 84, 88, 0.4)", strokeColor = "", dateWindow = arrest_filter_data()$dateWindow)
    
  })
  
  
  # Narrations
  output$diversity_text <- renderUI({ 
    how_many_times = round(r_race_treemap_data()$values[2] / r_race_treemap_data()$values[4], 2)
    how_many_wa = round(r_race_treemap_data()$values[2] / r_race_treemap_data()$values[3], 2)
    
    HTML(str_glue(
      '<div>
          <p> In the year <span style="color: #4b9cdb"><b>{input$pop_by_year}</b></span>, 
          <ul>
            <li>The Population of White people were almost <span style="color: #4b9cdb"><b>{how_many_times}</b></span> times that of Black people.</li>
            <br><li> When compared to Asian people it was <span style="color: #4b9cdb"><b>{how_many_wa}</b></span> times more.</li>
          </ul>
          </p>
          </div>'
    ) )
    
  })
  
  output$heatmap_narration <- renderUI({ 
   
    HTML(str_glue(
      '<h3>Violations</h3><br>',
      '<p>The age group of <b>19 years to 30 years</b> have the highest violation rate.</p> <p>"Equipment" violation are more evident after or at <b>midnight (11PM-1AM)</b>.Unexpectedly <b>"Equipment" 
      violation rate</b> among female and male are either equal or female have been stopped more.</p>
      <p>For speed-related violation, the contribution stands equal among both age-group genders.</p>',
      
      '<p>For <b>Hartford city</b>, the age group of <b>58+</b> violated most <b>speed-related violations</b>. This can be both slow/fast driving.</p>',
      
      '<p><b>Seat Belt:</b> During the daytime, drivers were more stopped as compared to night time. This is in relevance to the real world. Similar results for
      <b>cell phone</b> violation too.</p>',
      
      '<p><small><small><i>Feel free to obeserve more interesting results.<i></small></small></p>'
    ) )
    
  })
  
  output$sync_plot_narration <- renderUI({ 
    
    
    HTML(str_glue(
      
      '<h3>Arrest and Search</h3><br>',
      '<p>According to US Census Bureau, Hispanic or Latino are people of Cuban, Puerto Rican, South or Central American, or other Spanish culture or origin.
        They can belong to any race, in short, they have been distinguished based on the location of birth among the mentioned countries/region.
      </p>',
      '<p>
        Since Hispanic can belong to both Black or White Race, we may think of it as a somewhat aggregated average of both races results. The primary focus of
        results from Arrest Rate and Search Rate visualisation is to compare White Alone and Black Alone race, as depicted.
      </p>',
      
    ) )
    
  })
  
  
  
  
  
  
  
  
  #  EXPLORE TAB
  
  hartford_lat_lng <- hartford[complete.cases(hartford), ]
  vermont_lat_lng <- vermont[complete.cases(vermont), ]
  
  # constructing custom label variables
  colors <- c(pal$Black, pal$White, pal$Hispanic, pal$Asian)
  labels <- c("Black", "White", "Hispanic", "Asian")
  sizes <- c(15, 15, 15, 15)
  shapes <- c("circle", "circle", "circle", "circle")
  borders <- c(pal$Black, pal$White, pal$Hispanic, pal$Asian)
  
  
  # constructing custom label function
  addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
    
    make_shapes <- function(colors, sizes, borders, shapes) {
      shapes <- gsub("circle", "50%", shapes)
      shapes <- gsub("square", "0%", shapes)
      paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
    }
    make_labels <- function(sizes, labels) {
      paste0("<div style='display: inline-block;height: ", 
             sizes, "px;margin-top: 4px;line-height: ", 
             sizes, "px;'>", labels, "</div>")
    }
    
    legend_colors <- make_shapes(colors, sizes, borders, shapes)
    legend_labels <- make_labels(sizes, labels)
    return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, title = "Race", position="bottomright" ))
  }
  
  # dynamically creating state selection dropdown for interaction with map
  output$selectinputelement <- renderUI({
    selectInput(
      "us_state", label = "US State/County", choices = c("hartford","vermont"),
      selected = "hartford", multiple = FALSE
    )
  })
  
  # returning data and variables according to the input state choosen
  state_dataset=reactive({
    
    if (input$us_state == 'hartford'){
      dataset <- hartford_lat_lng
      lng = -72.685097
      lat = 41.763710
      zoom_level = 13
    }
    else if (input$us_state == 'vermont'){
      dataset <- vermont_lat_lng
      lng = -72.577843
      lat = 44.558804
      zoom_level = 8
    }
    return(list(
      dataset =  dataset,
      state_lng = lng,
      state_lat = lat,
      state_zoom = zoom_level
    )
    
    )
    
  })
  
  
  
  # to be animated on interaction date slider of the latest year
  output$animated_time_slider <- renderUI({
    sliderInput("sliderdate", "",
                min = as.Date("2015-01-01","%Y-%m-%d"), max = as.Date("2015-12-01","%Y-%m-%d"), 
                value = as.Date("2015-01-01"),
                timeFormat="%Y-%m-%d",
                animate= animationOptions(
                  interval = 300, 
                  playButton = NULL,
                  pauseButton = NULL
                )
    )
  })
  
  
  
  # on load default leaflet map output  with clustering enabled
  output$leaflet_map <- renderLeaflet({
    
    leaflet() %>% clearMarkers() %>%
      clearShapes() %>%
      addProviderTiles('CartoDB') %>%
      setView(state_dataset()$state_lng, state_dataset()$state_lat , zoom = state_dataset()$state_zoom) %>%
      addCircleMarkers(
        data = state_dataset()$dataset %>% filter(driver_race == 'Black') ,
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = pal$Black,
        clusterOptions = markerClusterOptions(
          iconCreateFunction=JS("function (cluster) {
                    var childCount = cluster.getChildCount();
                    var size = childCount / 10;
                    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_black_cluster_color', iconSize: new L.Point(40, 40) });
                  }")
        ),
        group = "race_black"
      ) %>%
      addCircleMarkers(
        data = state_dataset()$dataset %>% filter(driver_race == 'White'),
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = pal$White,
        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_white_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
        group = "race_white"
      ) %>%
      addCircleMarkers(
        data = state_dataset()$dataset %>% filter(driver_race == 'Hispanic'),
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = pal$Hispanic,
        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_hispanic_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
        group = "race_hispanic"
      ) %>%
      addCircleMarkers(
        data = state_dataset()$dataset %>% filter(driver_race == 'Asian'),
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE, 
        fillOpacity = 0.8,
        color = pal$Asian,
        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_asian_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
        group = "race_asian"
      ) %>%
      # layer control to filter out the race user want to observe
      addLayersControl(
        overlayGroups = c("race_black", "race_white",'race_hispanic', 'race_asian'),
        options = layersControlOptions(
          collapsed = FALSE
        )   
      ) %>%
      addLegendCustom(colors, labels, sizes, shapes, borders) %>%
      addMiniMap(tiles = "CartoDB") # minimap
    
  }) 
  
  observeEvent(input$leaflet_map_groups,{
    proxy <- leafletProxy("leaflet_map")
    proxy %>% clearControls()
    # label
    proxy %>% addLegendCustom(colors, labels, sizes, shapes, borders)
  })
  
  # observe event on "Play single Day" button click
  observeEvent(input$my_button1 , {
    
    proxy <- leafletProxy("leaflet_map")
    proxy %>% clearMarkers() %>%
      clearShapes() 
    
    proxy %>% setView(state_dataset()$state_lng, state_dataset()$state_lat , zoom = state_dataset()$state_zoom)
    
    
    # observe event on slider position change/drag
    observeEvent(input$sliderdate, {
      
      # clearing map and adding markers according to the dates with slider animation
      sliderdate <- input$sliderdate
      proxy <- leafletProxy("leaflet_map")
      proxy %>% clearMarkers() %>%
        clearShapes() 
      
      proxy %>% clearMarkerClusters()  %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Black') %>% filter(stop_date==sliderdate),
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          stroke = FALSE, 
          fillOpacity = 0.7,
          color = pal$Black,
          group = "race_black"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'White') %>% filter(stop_date==sliderdate),
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          stroke = FALSE, 
          fillOpacity = 0.7,
          color = pal$White,
          group = "race_white"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Hispanic') %>% filter(stop_date==sliderdate),
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          stroke = FALSE, 
          fillOpacity = 0.7,
          color = pal$Hispanic,
          group = "race_hispanic"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Asian') %>% filter(stop_date==sliderdate),
          lng = ~lng,
          lat = ~lat,
          radius = 5,
          stroke = FALSE, 
          fillOpacity = 0.7,
          color = pal$Asian,
          group = "race_asian"
        )
      
    })
    
  })
  
  
  # observe event on "Aggregate stoppages" button click with clustering enabled
  observeEvent(input$my_button2,{
    output$leaflet_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles('CartoDB') %>%
        setView(state_dataset()$state_lng, state_dataset()$state_lat, zoom = state_dataset()$state_zoom) %>%
        
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Black'),
          lng = ~lng,
          lat = ~lat,
          radius = 4,
          stroke = FALSE, 
          fillOpacity = 0.8,
          color = pal$Black,
          clusterOptions = markerClusterOptions(
            iconCreateFunction=JS("function (cluster) {
                    var childCount = cluster.getChildCount();
                    var size = childCount / 10;
                    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_black_cluster_color', iconSize: new L.Point(40, 40) });
                  }")
          ),
          group = "race_black"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'White'),
          lng = ~lng,
          lat = ~lat,
          radius = 4,
          stroke = FALSE, 
          fillOpacity = 0.8,
          color = pal$White,
          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_white_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
          group = "race_white"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Hispanic'),
          lng = ~lng,
          lat = ~lat,
          radius = 4,
          stroke = FALSE, 
          fillOpacity = 0.8,
          color = pal$Hispanic,
          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_hispanic_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
          group = "race_hispanic"
        ) %>%
        addCircleMarkers(
          data = state_dataset()$dataset %>% filter(driver_race == 'Asian'),
          lng = ~lng,
          lat = ~lat,
          radius = 4,
          stroke = FALSE, 
          fillOpacity = 0.8,
          color = pal$Asian,
          clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {
                var childCount = cluster.getChildCount();
                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster race_asian_cluster_color', iconSize: new L.Point(40, 40) });
              }")),
          group = "race_asian"
        ) %>%
        # legend
        addLegendCustom(colors, labels, sizes, shapes, borders) %>%
        
        addLayersControl(
          overlayGroups = c("race_black", "race_white",'race_hispanic', 'race_asian'),
          options = layersControlOptions(
            collapsed = FALSE
          )     
        ) %>%
        addMiniMap(tiles = "CartoDB")
    }) 
  })

})
