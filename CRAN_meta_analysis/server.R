library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)


server <- function(input, output, session) {
   
   aggregated_timeseries_data <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      filtered_data <-
         filter_timeseries_data(
            selected_from = selected_from,
            selected_to = selected_to,
            selected_ctv = selected_ctvs
         )
      
      aggregated_data <- aggregate_timeseries_data(filtered_data = filtered_data)
      aggregated_data
   })
   
   selected_ctvs <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_ctvs
   })
   
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
   
   #packages filter (does not work yet with the plot!)
   
   aggregated_timeseries_data_p <- reactive({
      selected_packages <- input$packages_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      filtered_data_p <-
         filter_timeseries_data(
            selected_from = selected_from,
            selected_to = selected_to,
            selected_packages = selected_packages
         )
      
      aggregated_data_p <- aggregate_timeseries_data_p(filtered_data_p = filtered_data_p)
      aggregated_data_p
   })
   
   selected_packages <- reactive({
      selected_packages <- input$ctvs_packages
      selected_packages
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

