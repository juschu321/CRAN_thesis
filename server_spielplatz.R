library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)


server <- function(input, output, session) {
  
  selected_ctvs <- reactive({
    selected_ctvs <- input$ctvs_select
    selected_ctvs
  })
  
  selected_packages <- reactive({
    selected_packages <- input$ctvs_packages
    selected_packages
  })
  
  aggregated_timeseries_data <- reactive({
    selected_ctvs <- input$ctvs_select
    selected_packages <- input$packages_select
    selected_from_ctv <- input$year[1]
    selected_to_ctv <- input$year[2]
    selected_from_p <- input$year[3]
    selected_to_p <- input$year[4]
  })
  
  #eigentlich macht  Filter nicht mehr viel Sinn, weil zwei versch. Datensätze pro Monat
  filtered_data <- reactive({
    if (selected_ctvs == TRUE){
      filtered_data <- tutti_timeseries_monthly_ctv %>%
        filter (ctv == selected_ctv) %>%
        filter (selected_from_ctv == selected_from_ctv) %>%
        filter (seleted_to_ctv == selected_to_ctv)}
    else { #packages
      filtered_data <- tutti_timeseries_monthly %>%
        filter (package == selected_packages) %>%
        filter (selected_from_p == selected_from_p) %>%
        filter (selected_to_p == selected_to_p)
    })
  
  #da schon monthly => nicht mehr nötig
  #aggregated_data <- aggregate_timeseries_data(filtered_data = filtered_data)
  #aggregated_data})
  
  #check
  output$ctvs_select <- renderText({
    selected_ctvs()
  })
  
  
  output$plot <- renderPlot({
    data = aggregated_timeseries_data()
    ggplot(data) +
      geom_line(aes (month, total, color= data$ctv)) +
      scale_x_date(
        date_breaks = "1 year",
        date_minor_breaks = "1 month",
        date_labels = "%Y - %m"
      )
    
  })
  
  output$plot_packages <- renderPlot({
    data = aggregated_timeseries_data_p()
    ggplot(data) +
      geom_line(aes (month, total, color= data$package))+
      scale_x_date(
        date_breaks = "1 year",
        date_minor_breaks = "1 month",
        date_labels = "%Y - %m"
      )
  })
  
  
  
  output$value <- renderPrint({
    input$checkboxGroup
  })
  
  }
  
  