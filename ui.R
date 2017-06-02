library(shiny)

initTabs = function() {
    tabsetPanel(
                type="tabs",
                # input$tabpanelId is the name of the current tab.
                id='tabpanelId',
                # The argument to *Ouput(x) is the name of an R object
                # (defined in server.R as output$x) of type *.
                tabPanel("Summary", 
                         fluidRow(
                                  column(12,
                                         plotOutput("scoreVsYear"))
                                  ),
                         fluidRow(
                                  column(12,
                                         plotOutput("viewsVsYear"))
                                  ),
                         fluidRow(
                                  column(12,
                                         plotOutput("epsVsYear"))
                                  )
                         #plotOutput("summaryView"),
                         #plotOutput("summaryView")
                         ), 
                tabPanel("Genres", textOutput("genresView")), 
                tabPanel("Sources", textOutput("sourcesView")), 
                tabPanel("Types", textOutput("typesView"))
                )
}

initYearInput = function() {
    dateRangeInput(
                   'yearRangeInputWidget',
                   start='2000-01-01',
                   end='2010-01-01',
                   format='yyyy'
                   )
}

# Define UI for random distribution application 
fluidPage(
    
  # Application title
  titlePanel("Tabsets"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  #sidebarLayout(
    #sidebarPanel(
      #radioButtons("dist", "Distribution type:",
                   #c("Normal" = "norm",
                     #"Uniform" = "unif",
                     #"Log-normal" = "lnorm",
                     #"Exponential" = "exp")),
      #br(),
      #
      #sliderInput("n", 
                  #"Number of observations:", 
                   #value = 500,
                   #min = 1, 
                   #max = 1000)
    #),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
              initTabs()
    )
  #)
)
