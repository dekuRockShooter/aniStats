library(shiny)
source('views/summaryView.R')
source('constants.R')
source('main5.R')
source('tabs.R')

initTabs = function() {
    tabsetPanel(
                type="tabs",
                # input$tabpanelId is the name of the current tab.
                id='tabpanelId',
                # The argument to *Ouput(x) is the name of an R object
                # (defined in server.R as output$x) of type *.
                createSummaryTab('Summary', TAB_ID_SUMMARY),
                createCatTab('Genres', TAB_ID_GENRES),
                createCatTab('Sources', TAB_ID_SOURCES),
                createCatTab('Types', TAB_ID_TYPES),
                createCatTab('Studios', TAB_ID_STUDIOS)
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
  fluidRow(
           column(2,
                  selectInput(
                              inputId='studioSelectId',
                              label='Select studio',
                              choices=c('All studios',
                                        sort(levels(globalDS$studio))),
                              selected='All studios'
                              )
                  ),
           column(2,
                  selectInput(
                              inputId='genreSelectId',
                              label='Select genre',
                              choices=c('Any genre',
                                        sort(names(globalDS)[6 : 44])),
                              selected='Any genre'
                              )
                  ),
           column(2,
                  selectInput(
                              inputId='sourceSelectId',
                              label='Select source',
                              choices=c('Any source',
                                        sort(levels(globalDS$source))),
                              selected='Any source'
                              )
                  ),
           column(2,
                  selectInput(
                              inputId='typeSelectId',
                              label='Select type',
                              choices=c('Any type',
                                        sort(levels(globalDS$type))),
                              selected='Any type'
                              )
                  )
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
          initTabs()
  #)
)
