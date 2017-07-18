library(shiny)
source('views/summaryView.R')
source('constants.R')
source('main5.R')
source('tabs.R')
source('optionsPanel.R')

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

initPredictionsTabs = function() {
    tabsetPanel(
                type="tabs",
                id='predictionsTabsetPanelId',
                createPredictionsTab('Predictions', TAB_ID_PREDICTIONS)
                #createPlotsTab('Plots', TAB_ID_PLOTS)
                )
}

# Define UI for random distribution application 
fluidPage(
          tags$head(
                    tags$link(
                              rel="stylesheet",
                              type="text/css",
                              href="hover.css"
                              )
                    ),
          # Application title
          #titlePanel("Tabsets"),
          navbarPage(
                     'navbar',
                     tabPanel(
                              'Stats',
                              createOptionsPanel(),
                              initTabs()
                              ),
                     tabPanel(
                              'Predictions',
                              initPredictionsTabs()
                              )
                     )
)
