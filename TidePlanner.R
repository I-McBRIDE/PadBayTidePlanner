
library(shiny)
library(tidyverse)
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

  fluidRow(
    column(4, dateInput('date', 'Input Census Date', value = Sys.Date())),
    column(8, textOutput('lowTide'))
  ),
  
  h2('Workable Tide Window'),
  
  fluidRow(
    column(4, numericInput('shallowSite', 'Shallow Site (ft MLLW)', 
                            value = 1, min = -4, max = 10, step = 0.5),
              numericInput('midSite', 'Mid Site (ft MLLW)', 
                            value = 0, min = -4, max = 10, step = 0.5),
              numericInput('deepSite', 'Deep Site (ft MLLW)', 
                            value = -1, min = -4, max = 10, step = 0.5)),
    column(8, tableOutput('timeTable'))
  ),
  
  fluidRow(
    column(12, plotlyOutput('tideView'))
  ),
  
)


server <- function(input, output) {

  dateSelect <- reactive({tides %>%
      filter(Date == input$date)})
  
  lowTime <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- optimize(f, interval =  range(df$DateIndex))[1]
    strftime(as.character(as.POSIXct(as.numeric(g), tz = 'GMT')), format = '%H:%M')
  })
  
  lowLevel <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- optimize(f, interval =  range(df$DateIndex))[2]
    round(as.numeric(g), digits = 3)
  })
  
  shallowEbb <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$shallowSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[1]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  shallowFlood <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$shallowSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[2]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  shallowDuration <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$shallowSite}
    h1 <- uniroot.all(g, interval = range(df$DateIndex))[1]
    h2 <- uniroot.all(g, interval = range(df$DateIndex))[2]
    diff <- h2 - h1
    strftime(as.character(as.POSIXct(diff, tz = 'GMT')), format = '%H:%M')
  })
  
  midEbb <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$midSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[1]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  midFlood <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$midSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[2]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  midDuration <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$midSite}
    h1 <- uniroot.all(g, interval = range(df$DateIndex))[1]
    h2 <- uniroot.all(g, interval = range(df$DateIndex))[2]
    diff <- h2 - h1
    strftime(as.character(as.POSIXct(diff, tz = 'GMT')), format = '%H:%M')
  })
  
  deepEbb <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$deepSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[1]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  deepFlood <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$deepSite}
    h <- uniroot.all(g, interval = range(df$DateIndex))[2]
    strftime(as.character(as.POSIXct(h, tz = 'GMT')), format = '%H:%M')
  })
  
  deepDuration <- reactive({
    df <- dateSelect()
    f <- approxfun(df$DateIndex, df$Pred)
    g <- function(x) {f(x) - input$deepSite}
    h1 <- uniroot.all(g, interval = range(df$DateIndex))[1]
    h2 <- uniroot.all(g, interval = range(df$DateIndex))[2]
    diff <- h2 - h1
    strftime(as.character(as.POSIXct(diff, tz = 'GMT')), format = '%H:%M')
  })
  
  times <- reactive({
    data.frame(Ebb = c(shallowEbb(), midEbb(), deepEbb()),
               Flood = c(shallowFlood(), midFlood(), deepFlood()),
               Duration = c(shallowDuration(), midDuration(), deepDuration()))
  })
  
  output$lowTide <- renderText(c('The low tide is', lowLevel(), 'ft MLLW at', lowTime()))
  
  output$timeTable <- renderTable(times())
  
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
        labs(y = 'Height in Feet (MLLW)', x = 'Time') +
        scale_x_datetime(date_breaks = '2 hour', date_labels = format('%H:%M'))
      ggplotly(plot)
  })

  
}


shinyApp(ui, server)
  