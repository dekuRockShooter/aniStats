library(shiny)
source('views/summaryView.R')
source('constants.R')
source('main5.R')

initTabs = function() {
    tabsetPanel(
                type="tabs",
                # input$tabpanelId is the name of the current tab.
                id='tabpanelId',
                # The argument to *Ouput(x) is the name of an R object
                # (defined in server.R as output$x) of type *.
                tabPanel(
                         "Summary", 
                         #summary.getView(),
                         fluidRow(
                                  column(10, plotOutput("plot1_1")),
                                  column(
                                         2,
                                         checkboxInput(
                                                       'plot1_1MinCheckbox',
                                                       'Min',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_1Q25Checkbox',
                                                       '25th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_1MeanCheckbox',
                                                       'Mean',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_1Q75Checkbox',
                                                       '75th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_1MaxCheckbox',
                                                       'Max',
                                                       FALSE
                                                       )
                                         )
                                  ),
                         fluidRow(
                                  column(10, plotOutput("plot1_2")),
                                  column(
                                         2,
                                         checkboxInput(
                                                       'plot1_2MinCheckbox',
                                                       'Min',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_2Q25Checkbox',
                                                       '25th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_2MeanCheckbox',
                                                       'Mean',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_2Q75Checkbox',
                                                       '75th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_2MaxCheckbox',
                                                       'Max',
                                                       FALSE
                                                       )
                                         )
                                  ),
                         fluidRow(
                                  column(10, plotOutput("plot1_3")),
                                  column(
                                         2,
                                         checkboxInput(
                                                       'plot1_3MinCheckbox',
                                                       'Min',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_3Q25Checkbox',
                                                       '25th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_3MeanCheckbox',
                                                       'Mean',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_3Q75Checkbox',
                                                       '75th percentile',
                                                       FALSE
                                                       ),
                                         checkboxInput(
                                                       'plot1_3MaxCheckbox',
                                                       'Max',
                                                       FALSE
                                                       )
                                         )
                                  ),
                         fluidRow(column(12, plotOutput("plot1_4"))),
                         fluidRow(column(12, plotOutput("plot1_5"))),
                         value=TAB_ID_SUMMARY
                         ), 
                tabPanel(
                         "Genres",
                         #textOutput("genresView"),
                         #category.getView(),
                         fluidRow(column(12, plotOutput("plot2_1"))),
                         fluidRow(column(12, plotOutput("plot2_2"))),
                         fluidRow(column(12, plotOutput("plot2_3"))),
                         fluidRow(column(12, plotOutput("plot2_4"))),
                         fluidRow(column(12, plotOutput("plot2_5"))),
                         fluidRow(column(12, plotOutput("plot2_6"))),
                         fluidRow(column(12, plotOutput("plot2_7"))),
                         value=TAB_ID_GENRES
                         ),
                tabPanel(
                         "Sources",
                         fluidRow(column(12, plotOutput("plot3_1"))),
                         fluidRow(column(12, plotOutput("plot3_2"))),
                         fluidRow(column(12, plotOutput("plot3_3"))),
                         fluidRow(column(12, plotOutput("plot3_4"))),
                         fluidRow(column(12, plotOutput("plot3_5"))),
                         fluidRow(column(12, plotOutput("plot3_6"))),
                         fluidRow(column(12, plotOutput("plot3_7"))),
                         value=TAB_ID_SOURCES
                         ),
                tabPanel(
                         "Types",
                         fluidRow(column(12, plotOutput("plot4_1"))),
                         fluidRow(column(12, plotOutput("plot4_2"))),
                         fluidRow(column(12, plotOutput("plot4_3"))),
                         fluidRow(column(12, plotOutput("plot4_4"))),
                         fluidRow(column(12, plotOutput("plot4_5"))),
                         fluidRow(column(12, plotOutput("plot4_6"))),
                         fluidRow(column(12, plotOutput("plot4_7"))),
                         value=TAB_ID_TYPES
                         )
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
  selectInput(
              inputId='studioSelectId',
              label='Select studio',
              choices=c('All', sort(levels(globalDS$studio))),
              selected='All'
              ),
  actionButton('changeDataActionButton', 'Done'),
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
