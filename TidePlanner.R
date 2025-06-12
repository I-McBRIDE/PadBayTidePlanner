# Packages
library(shiny)
library(tidyverse)
library(reactable)
library(plotly)
library(rootSolve)
library(shinythemes)

# only loads the data if it doesn't already exist in the environment 
if(!exists('tides')) {
  tides <- read.csv('NOAAtides.csv')
  sun <- read.csv('NOAAsun.csv')
}

# this creates a date-time column for the tide data
tides <- tides %>%
  mutate(DateTime = paste(Date, Time, sep = ' '),
         DateTime = as.POSIXct(DateTime, c('%Y-%m-%d %H:%M'), tz = 'GMT'),
         DateIndex = as.numeric(DateTime))

#the UI
ui <- fluidPage(
  
  # Basic pre-built theme for the UI
  theme = shinytheme('cerulean'),
  
  # App header (shows up in all tabs)
  h1('2025 Padilla Field Work and Tide Planner'),
  h4('NOAA Tide Predictions for Padilla Bay (Swinomish Channel Entrance)'),
  hr(),
  
  # Creates a navigation bar (essentially main tabs)
  navbarPage('Contents:',
     
    # 1st tab                 
    tabPanel('Planner',
      
      # Date input and text outputs for low tide and sunrise/sunset       
      fluidRow(
        column(4, dateInput('date', 'Input Census Date', value = Sys.Date())),
        column(8, htmlOutput('lowTide'),
                  htmlOutput('sunrise'),
                  htmlOutput('sunset'))
      ),
      
      # Header for tide predictions
      hr(),
      h2('Workable Tide Window'),
  
      # Inputs for the three site elevations
      fluidRow(
        column(4, numericInput('shallowSite', 'Shallow Site (ft MLLW)', 
                                value = 1, min = -4, max = 10, step = 0.5),
                  numericInput('midSite', 'Mid Site (ft MLLW)', 
                                value = 0, min = -4, max = 10, step = 0.5),
                  numericInput('deepSite', 'Deep Site (ft MLLW)', 
                                value = -1, min = -4, max = 10, step = 0.5)),
        
        # Table outputs for the tide time tables
        column(8, 
          tabsetPanel(
            tabPanel('Cylce 1', reactableOutput('timeTable1')),
            tabPanel('Cycle 2', reactableOutput('timeTable2'))))
      ),
  
      # Plotly output for the tide predictions
      fluidRow(
        column(12, plotlyOutput('tideView'))
      ),
    ),
    
    # 2nd tab
    tabPanel('Tide Chart',
             
    # Table output for the tide chart  
    fluidRow(
        column(12, reactableOutput('tideChart'))
      )
    ),
    
    # 3rd tab
    tabPanel('Outlook',
      
      # Date input for the tide prediction outlook        
      fluidRow(
        column(4, dateRangeInput('month', 'Input Date Range', 
                                 start = floor_date(Sys.Date(), unit = 'month'), 
                                 end = ceiling_date(Sys.Date(), unit = 'month') - days(1)))
      ),
      
      hr(),
      
      # Plotly output for the tide prediction outlook       
      fluidRow(
        column(12, plotlyOutput('tideViewAll'))
      ),
      
      hr(),
      
      # Table output for the outlook tide chart             
      fluidRow(
        column(12, reactableOutput('tideChartAll'))
      )
    ),
    
    # 4th tab
    tabPanel('About',
      
      # About 1st tab
      h4('The Planner'),
      p('This is the real meat and potatoes of the app: creating a user interface for planning field work at intertidal sites in Padilla Bay, Washington. This section gives you the option of selecting the date on which you plan to conduct work. Subsequently, you will be provided with a plot for the tide prediction on that date. You may then select the elevation for up to three sites: a deep, mid-level, and shallow site. You can then refer to the timetable, which gives the essential information on timing. The ebb time refers to the period when the site will become exposed, and the flood time refers to when the site will flood or become unworkable, and the duration indicates how much time you have to work on that site. Under some circumstances, at very shallow sites, there may be two workable tide windows. You can refer to the “Cycle 2” timetable for information on the second tide if this is the case. It’s essential to use this timetable wisely, as getting caught in a flood of too much gear can be hazardous. The fine folks at Padilla Bay can assist you in determining the likely elevation of your sites if you are unsure.'),
      hr(),
      
      # About 2nd tab
      h4('The Tide Chart'),
      p('This section is quite simple, and in the days before Shiny apps, it was what we used to plan intertidal fieldwork. It simply prints out the tide chart in 15-minute intervals for the date you selected in the “Tide Planner” section. It may be handy in some situations to take a look at it.'),
      hr(),
      
      # About 3rd tab
      h4('The Outlook'),
      p('If you’re the sort of person who is interested in long-term planning, then you can use this last section to accomplish just that. Here, you can select a date range, ranging from two days to a month. You will then be provided with a plot of the tide predictions as well as a tide chart given in 15-minute intervals. Each page of the tide chart corresponds to a different day, and you have the option of sorting by column to check the low tide quickly.'),
      hr(),
      
      # Data
      h4('Data Source'),
      p('Data was sourced from the NOAA National Weather Service. Sunrise/Sunset data was provided by the Earth System Research Laboratories, and Tide data by the Center for Operational Oceanographic Products and Services.  For more information, follow the links below.'),
      p(tags$a(href = 'https://gml.noaa.gov/grad/solcalc/', 'Sunrise/Sunset & Moonrise/Moonset Calculator for anywhere in the U.S.')),
      p(tags$a(href = 'https://tidesandcurrents.noaa.gov/tide_predictions.html', 'National Ocean Service (NOS) Tide Predictor'))
      
    )
  )
)


# Server
server <- function(input, output) {

  # Creates a filtered tide data frame based on the date selected
  dateSelect <- reactive({
    tides %>%
      filter(Date == input$date)
  })
  
  # Creates a filtered sunrise/sunset data frame based on the date selected
  dateSelectSun <- reactive({
    sun %>%
      filter(Date == input$date)
  })
  
  # Creates a filtered tide data frame based on the date range selected
  dateSelectAll <- reactive({
    start <- input$month[1]
    end <- input$month[2]
    tides %>%
      filter(Date >= start & Date <= end)
  })
  
  # Function for calculating the low tide. Interpolates and find the minimum
  lowFunc <- function() {
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    optimize(f, interval =  range(df$DateIndex))
  }
  
  # Function for finding the times when a tide crosses a given elevation (var)
  timeFunc <- function(var) {
    df <- dateSelect()
    # Interpolate the 15min interval tide data
    f <- approxfun(df$DateIndex, df$Pred)
    # Function that shifts the tide predictions so the x-axis is at the given elevation
    g <- function(x) {f(x) - var}
    # Finds the time when the line  crosses 'zero' or the given elevation
    h <- uniroot.all(g, interval = range(df$DateIndex))
    # Produces the time as a string
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  }
  
  # Function for finding the duration when a tide exposes a given elevation (var)
  durFunc1 <- function(var) {
    df <- dateSelect()
    # Interpolate the 15min interval tide data
    f <- approxfun(df$DateIndex, df$Pred)
    # Function that shifts the tide predictions so the x-axis is at the given elevation
    g <- function(x) {f(x) - var}
    # Finds the time when the line  crosses 'zero' or the given elevation
    h <- uniroot.all(g, interval = range(df$DateIndex))
    # Subtracts the first time the line crosses 'zero' from the second time 
    i <- h[2] - h[1]
    # Produces the time as a string
    strftime(as.character(as.POSIXct(i, tz = 'GMT')), format = '%H:%M')
  }
  
  # Function for finding the duration when a tide exposes a given elevation (var)
  # Same as previous function, but used when there is a second tide cycle
  durFunc2 <- function(var) {
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - var}
    h <- uniroot.all(g, interval = range(df$DateIndex))
    # Subtracts the third time the line crosses 'zero' from the fourth time 
    i <- h[4] - h[3]
    strftime(as.character(as.POSIXct(i, tz = 'GMT')), format = '%H:%M')
  }
  
  # Creates a dataframe of tide timing using the functions listed previously
  # uses the elevation inputs UI selector
  times1 <- reactive({
    data.frame(ebb = c(timeFunc(input$shallowSite)[1], 
                       timeFunc(input$midSite)[1], 
                       timeFunc(input$deepSite)[1]),
               flood = c(timeFunc(input$shallowSite)[2], 
                         timeFunc(input$midSite)[2], 
                         timeFunc(input$deepSite)[2]),
               duration = c(durFunc1(input$shallowSite), 
                            durFunc1(input$midSite), 
                            durFunc1(input$deepSite)))
  })
  
  # Creates a dataframe of tide timing using the functions listed previously
  # this data frame is for cases where there is a second tide cycle
  times2 <- reactive({
    data.frame(ebb = c(timeFunc(input$shallowSite)[3],
                       timeFunc(input$midSite)[3],
                       timeFunc(input$deepSite)[3]),
               flood = c(timeFunc(input$shallowSite)[4],
                         timeFunc(input$midSite)[4],
                         timeFunc(input$deepSite)[4]),
               duration = c(durFunc2(input$shallowSite),
                            durFunc2(input$midSite),
                            durFunc2(input$deepSite)))
  })
  
  # Text output for the low tide level and time uses to low tide function above 
  output$lowTide <- renderText({
    HTML(paste0('The low tide is ',
                '<b>', round(as.numeric(lowFunc()[2]), digits = 3), '</b>', 
                ' ft MLLW at ',
                '<b>', strftime(as.character(as.POSIXct(as.numeric(lowFunc()[1]), tz = 'GMT')), format = '%H:%M'), '</b>'))
  })
  
  # Text output for the sunrise
  output$sunrise <- renderText({
    HTML(paste0('Sunrise is at ',
                '<b>', dateSelectSun()$Sunrise, '</b>'))
  })
  
  # Text output for the sunset
  output$sunset <- renderText({
    HTML(paste0('Sunset is at ',
                '<b>', dateSelectSun()$Sunset, '</b>'))
  })
  
  # Table output using the firt cycle time table dataframe
  output$timeTable1 <- renderReactable({
    reactable(times1(), 
              bordered = T, 
              highlight = T, 
              sortable = F,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = '#f7f7f8')),
              columns = list(
                ebb = colDef(name = 'Ebb Time', na = 'Unworkable',
                             style = function(val) {if (is.na(val)) list(color = "red")}),
                flood = colDef(name = 'Flood Time', na = 'Unworkable',
                               style = function(val) {if (is.na(val)) list(color = "red")}),
                duration = colDef(name = 'Duration', na = 'Unworkable',
                                  style = function(val) {if (is.na(val)) list(color = "red")})))
  })
  
  # Table output using the second cycle time table dataframe
  output$timeTable2 <- renderReactable({
    reactable(times2(), 
              bordered = T, 
              highlight = T, 
              sortable = F,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = '#f7f7f8')),
              columns = list(
                ebb = colDef(name = 'Ebb Time', na = 'Only One Cycle',
                             style = function(val) {if (is.na(val)) list(color = "red")}),
                flood = colDef(name = 'Flood Time', na = 'Only One Cycle',
                               style = function(val) {if (is.na(val)) list(color = "red")}),
                duration = colDef(name = 'Duration', na = 'Only One Cycle',
                                  style = function(val) {if (is.na(val)) list(color = "red")})))
  })
  
  # Plotly output for the tide predictions using the date selected data frame
  output$tideView <- renderPlotly({
    plot <- ggplot(dateSelect())+
      geom_line(aes(x = DateTime, y = Pred)) +
      # These next few lines are added based on the elevations selected for the three sites 
      geom_hline(aes(yintercept = input$shallowSite, linetype = 'Shallow'),
                 linewidth = 0.25) +
      geom_hline(aes(yintercept = input$midSite, linetype = 'Mid'),
                 linewidth = 0.25) +
      geom_hline(aes(yintercept = input$deepSite, linetype = 'Deep'), 
                 linewidth = 0.25) +
      theme_bw() +
      labs(y = 'Height in Feet (MLLW)', x = 'Time (24hr)') +
      guides(linetype = guide_legend(title = 'Site')) +
      scale_x_datetime(date_breaks = '2 hour', date_labels = format('%H:%M'))
    ggplotly(plot)
  })
  
  # Table output for the tide prediction based on the date selected data frame
  output$tideChart <- renderReactable({
    reactable(dateSelect()[1:4], 
              bordered = T, 
              highlight = T, 
              sortable = F,
              pagination = F,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = '#f7f7f8')),
              columns = list(Date = colDef(name = 'Date'),
                             Day = colDef(name = 'Day'),
                             Time = colDef(name = 'Time (24hr)'),
                             Pred = colDef(name = 'Prediction (ft MLLW)')))
  })
  
  # Plotly output for the tide predictions using the date range selected data frame
  output$tideViewAll <- renderPlotly({
    plot <- ggplot(dateSelectAll()) +
      geom_line(aes(x = DateTime, y = Pred)) +
      theme_bw() +
      labs(y = 'Height in Feet (MLLW)', x = 'Date/Time (24hr)')
    ggplotly(plot)
  })
  
  # Table output for the tide prediction based on the date range selected data frame
  output$tideChartAll <- renderReactable({
    reactable(dateSelectAll()[1:4],
              defaultPageSize = 96,
              paginationType = 'jump',
              bordered = T, 
              highlight = T, 
              sortable = T,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(background = '#f7f7f8')),
              columns = list(Date = colDef(name = 'Date'),
                             Day = colDef(name = 'Day'),
                             Time = colDef(name = 'Time (24hr)'),
                             Pred = colDef(name = 'Prediction (ft MLLW)')))
  })
  
}


shinyApp(ui, server)
  