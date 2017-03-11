library(shiny)

library(shinydashboard)
library(plotly)
library(ggplot2)
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader( title =  "Re-Interview Process Statistics"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidPage(
      
      # Application title
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel( width = 0
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Home",
        
                     h3("Go ahead and explore various Tabs."),
                     br(),br(),
                     h4("First tab summarises the data"),
                     h4("Second Tab allows you to perform Univariate Analysis"),
                     h4("Third Tab allows you to perform Bivariate Analysis "),
                     h4("Fourth Tab helps in performing Chi Sq test")
                     
                     ),
            
            tabPanel( "Structure and summary",
                      uiOutput('date', inline = T),
                      dataTableOutput('head'),
                      verbatimTextOutput('str'),
                      radioButtons('rad','Choose Categorical or Continuous to see Summary',
                                   c("Continuous" , "Categorical" )),
                      conditionalPanel('input.rad == "Continuous"',verbatimTextOutput('summary')),
                      conditionalPanel('input.rad == "Categorical"',
                                       uiOutput('cat'), tableOutput('cattbl')
                                       )
            ),
            tabPanel("Distribution of Joining Date and Interview Date (Univariate)" , 
                     uiOutput('uniSlide'),
                     uiOutput('uniColor'),
                     fluidRow(plotlyOutput("uniplt"),
                     plotlyOutput('unibox') )
            ),
            tabPanel('Distribution of Joining Date and Interview Date (Bivariate)',
                     uiOutput('biSelect'),
                     uiOutput('biColor'),
                     plotlyOutput('biPlt')
                     ),
            tabPanel('Chi Square Test',
                     fluidRow( "Categorical variables",
                       uiOutput('statFac'),
                       verbatimTextOutput('statFacR'),
                       h6("p-value less than 0.05 indicate the variables are dependent on each other")
                       
                     
                     )
                     
                     )
          )
        )
      )
    )
  )
)

