library(shiny)
library(shinydashboard)
ui<-dashboardPage(
  
  dashboardHeader(title="Lending Club Data Dashboard", titleWidth = 350),
  
  dashboardSidebar(
    
    sidebarMenu(
        dateRangeInput("date_range", start="2007-01-01", end="2016-01-01", format='yyyy-mm-dd', label='Issued Date'),
        column(6),
        actionButton("date_update", label="Update",icon=icon('hand-o-up')),
        menuItem("Overall Performance", tabName = "Overall"),
        column(2),
        tags$strong("Numeric Attributes"),
        menuItem("Univariate", tabName = "Univariate"),
        menuItem("Bivariate", tabName = "Bivariate"),
        tags$br(),
        column(4),
        tags$strong("As of Now")
        )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("Overall", 
              fluidRow(
                valueBoxOutput("count"),
                valueBoxOutput("current"),
                valueBoxOutput("late"),
                valueBoxOutput("default"),
                valueBoxOutput("fully_paid")
              ),
              fluidRow(
                box(
                  width=8, status="info", solidHeader=TRUE,
                  title="Distribution by Issued Date",
                  plotOutput("overall_dist_date")
                ),
                box(
                  width=4,status="info", solidHeader=TRUE,
                  title="Top Default Vintage",
                  plotOutput("vintages_dist")
                )
              )
      ),
      tabItem("Univariate",
               fluidRow(
                 column(4, selectInput("uni_sel",label="Variable", choices=numeric_variable_name))
               ),
              fluidRow(
                 column(4,actionButton("uni_update", label="Update"))
              ),
              tags$br(),
              fluidRow(
                  valueBoxOutput("uni_mean"), 
                  valueBoxOutput("uni_min"),
                  valueBoxOutput("uni_max")  
               ),
              fluidRow(
              box(plotOutput("uni_histogram"),
                  width=6, status="info", solidHeader=TRUE, 
                  title="Variable Histogram"),
              box(width=6, status="info", solidHeader=TRUE, 
                title="Univariate Plot",
                plotOutput("uni_plot")
              )
              )
      ),
      tabItem("Bivariate",
              fluidRow(
                column(4,selectInput("bi_sel_1",label="Variable X", 
                                     choices=numeric_variable_name
                )),
                column(4,selectInput("bi_sel_2",label="Variable Y", 
                                     choices=numeric_variable_name
                ))),
              fluidRow(
                column(6,actionButton("bi_update",label="Update"))
              ),
              tags$br(),
              tags$br(),
              fluidRow(
                box(
                  width=12, status="info", solidHeader=TRUE, 
                  title="Bivariate Heat Map",
                  plotOutput("bi_heat")
                )
              )
      )
    )
  )
)