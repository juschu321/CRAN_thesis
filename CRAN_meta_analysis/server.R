library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)


server <- function(input, output, session) {
   #### CTV LEVEL ####
   time_series_monthly_ctv <- reactive({
      selected_ctvs <- input$ctvs_select
      selected_from <- input$year[1]
      selected_to <- input$year[2]
      
      filtered_data <- tutti_time_monthly_ctv %>%
         filter (ctv == selected_ctvs) %>%
         filter (month >= selected_from, month <= selected_to)
   })
   
   output$ctv_plot <- renderPlot({
      data = time_series_monthly_ctv()
      ggplot(data) +
         geom_line(aes (month, total, color= data$ctv)) +
         scale_x_date(
            date_breaks = "1 year",
            date_minor_breaks = "1 month",
            date_labels = "%Y - %m"
         )
      
   })
   
   #### PACKAGE LEVEL ####
   time_series_monthly_pkg <- reactive({
      selected_pkgs <- input$packages_select
      selected_from <- input$date_selection_package[1]
      selected_to <- input$date_selection_package[2]
      
      filtered_data <- tutti_time_monthly_package %>%
         filter (package == selected_pkgs) %>%
         filter (month >= selected_from, month <= selected_to)
   })
 
   output$pkg_plot <- renderPlot({
      data = time_series_monthly_pkg()
      ggplot(data) +
         geom_line(aes (month, total, color= data$package)) +
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

