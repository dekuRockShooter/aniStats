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
          initTabs()
  #)
)
