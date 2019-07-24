library(shiny)
library(shinydashboard)
library(ggplot2)

#3 different tabs to work with

body <- dashboardBody(
  fluidRow(tabBox(
  width = 12,
  
  ####tab ctv####
  tabPanel(
    "ctv",
    box(
      title = "filter",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      selectizeInput(
        'ctvs_select',
        'ctvs to select',
        choices = ctvs,
        multiple = TRUE,
        options = list(maxItems = 40),
        selected = "Psychometrics"
      ),
      selectInput(
        'core_select',
        "the core-packages are installed with R Studio at the beginning; with this option you can exclude these core-packages (core = TRUE) from the analysis",
        c("FALSE", "TRUE")
      ),
      selectInput(
        'subcategory_select',
        "*only apperas when Psychometrics is selected: you can specify on subcategories",
        c("All")
      )
    ),
    
   
    
    fluidRow(
      box(
        title = "plot: linechart",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("ctv_plot")
      ),
      box(
        title = "plot: linechart",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("downloads_per_ctv")
      )
      
    ),
    
    fluidRow(
      box(
        title = "date range",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        sliderInput(
          'year',
          "time span",
          min = as.Date("2012-10-01"),
          max = Sys.Date(),
          value = c(as.Date("2016-02-25"), Sys.Date()),
          timeFormat = "%F"
        )
        
      )
    )
  ),
  
  
  ####tab package#####
  tabPanel(
    "package",
    box(
      title = "filter",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      selectizeInput(
        'packages_select',
        'packages to select',
        choices = packages,
        multiple = TRUE,
        options = list(maxItems = 100),
        selected = "dplyr"
      ),
      selectInput(
        'core_select',
        "the core-packages are installed with R Studio at the beginning; with this option you can exclude these core-packages (core = TRUE) from the analysis",
        c("FALSE", "TRUE")
      ),
      selectInput(
        'subcategory_select',
        "*only apperas when Psychometrics is selected: you can specify on subcategories",
        c("All")
      )
      ),
    fluidRow(
      box(
        title = "plot: dependencies",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        "plot dependencies"
      ),
      box(
        title = "plot: linechart",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("pkg_plot")
      )
      
    ),
    
    fluidRow(
      box(
        title = "date range",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        sliderInput(
          'date_selection_package',
          "time span",
          min = as.Date("2012-10-01"),
          max = Sys.Date(),
          value = c(as.Date("2016-02-25"), Sys.Date()),
          timeFormat = "%F"
        )
        
      )
    ),
    
    
    fluidRow(
      box(
        title = "dependencies",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        checkboxGroupInput(
          'checkGroup',
          label = "specify type of dependencies",
          choices = list(
            "imports" = 1,
            "depends" = 2,
            "suggests" = 3
          ),
          selected = 0
        )
      )
      
    
  )),
  
  #####tab update data + ui#####
  tabPanel("update data")
)))


# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(dashboardHeader(title = "CRAN"),
                    dashboardSidebar(collapsed = TRUE),
                    body)