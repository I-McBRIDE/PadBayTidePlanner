
library(shiny)
library(tidyverse)
library(reactable)
library(plotly)
library(rootSolve)

if(!exists('tides')) {
  tides <- read.csv('NOAAtides.csv')
}

tides <- tides %>%
  mutate(DateTime = paste(Date, Time, sep = ' '),
         DateTime = as.POSIXct(DateTime, c('%Y-%m-%d %H:%M'), tz = 'GMT'),
         DateIndex = as.numeric(DateTime))


ui <- fluidPage(
  
  h1('2025 Padilla Field Work and Tide Planner'),
  h4('NOAA Tide Predictions for Padilla Bay (Swinomish Channel Entrance)'),
  hr(),

  navbarPage('Contents:',
             
    tabPanel('Planner',
             
      fluidRow(
        column(4, dateInput('date', 'Input Census Date', value = Sys.Date())),
        column(8, htmlOutput('lowTide'))
      ),
      
      hr(),
      h2('Workable Tide Window'),
  
      fluidRow(
        column(4, numericInput('shallowSite', 'Shallow Site (ft MLLW)', 
                                value = 1, min = -4, max = 10, step = 0.5),
                  numericInput('midSite', 'Mid Site (ft MLLW)', 
                                value = 0, min = -4, max = 10, step = 0.5),
                  numericInput('deepSite', 'Deep Site (ft MLLW)', 
                                value = -1, min = -4, max = 10, step = 0.5)),
        column(8, reactableOutput('timeTable'))
      ),
  
      fluidRow(
        column(12, plotlyOutput('tideView'))
      ),
    ),
    
    tabPanel('Tide Chart',
             
      fluidRow(
        column(12, reactableOutput('tideChart'))
      )
    ),
    
    tabPanel('Outlook',
       
      fluidRow(
        column(4, dateRangeInput('month', 'Input Date Range', 
                                 start = floor_date(Sys.Date(), unit = 'month'), 
                                 end = ceiling_date(Sys.Date(), unit = 'month') - days(1)))
      ),
      
      hr(),
             
      fluidRow(
        column(12, plotlyOutput('tideViewAll'))
      ),
      
      hr(),
                   
      fluidRow(
        column(12, reactableOutput('tideChartAll'))
      )
    )
  )
)


server <- function(input, output) {

  dateSelect <- reactive({
    tides %>%
      filter(Date == input$date)
  })
  
  dateSelectAll <- reactive({
    start <- input$month[1]
    end <- input$month[2]
    tides %>%
      filter(Date >= start & Date <= end)
  })
  
  lowFunc <- function() {
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    optimize(f, interval =  range(df$DateIndex))
  }
  
  timeFunc <- function(var) {
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - var}
    h <- uniroot.all(g, interval = range(df$DateIndex))
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  }
  
  durFunc <- function(var) {
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - var}
    h <- uniroot.all(g, interval = range(df$DateIndex))
    i <- h[2] - h[1]
    strftime(as.character(as.POSIXct(i, tz = 'GMT')), format = '%H:%M')
  }
  
  times <- reactive({
    data.frame(ebb = c(timeFunc(input$shallowSite)[1], 
                       timeFunc(input$midSite)[1], 
                       timeFunc(input$deepSite)[1]),
               flood = c(timeFunc(input$shallowSite)[2], 
                         timeFunc(input$midSite)[2], 
                         timeFunc(input$deepSite)[2]),
               duration = c(durFunc(input$shallowSite), 
                            durFunc(input$midSite), 
                            durFunc(input$deepSite)))
  })
  
  output$lowTide <- renderText({
    HTML(paste0('The low tide is ',
                '<b>', round(as.numeric(lowFunc()[2]), digits = 3), '</b>', 
                ' ft MLLW at ',
                '<b>', strftime(as.character(as.POSIXct(as.numeric(lowFunc()[1]), tz = 'GMT')), format = '%H:%M'), '</b>'))
  })
  
  output$timeTable <- renderReactable({
    reactable(times(), 
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
  
  output$tideView <- renderPlotly({
    plot <- ggplot(dateSelect())+
      geom_line(aes(x = DateTime, y = Pred)) +
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
  
  output$tideViewAll <- renderPlotly({
    plot <- ggplot(dateSelectAll()) +
      geom_line(aes(x = DateTime, y = Pred)) +
      theme_bw() +
      labs(y = 'Height in Feet (MLLW)', x = 'Date/Time (24hr)')
    ggplotly(plot)
  })
  
  output$tideChartAll <- renderReactable({
    reactable(dateSelectAll()[1:4],
              bordered = T, 
              highlight = T, 
              sortable = F,
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
  