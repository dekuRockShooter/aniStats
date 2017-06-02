# This is a view for the 'summary' tab.  It shows plots and statistics
# for the entire data.
library(shiny)

summary.getView = function() {
    fluidPage(
              fluidRow(column(12, plotOutput("scoreVsYear"))),
              fluidRow(column(12, plotOutput("viewsVsYear"))),
              fluidRow(column(12, plotOutput("epsVsYear")))
              )
}
