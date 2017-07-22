# This file contains one function for creating an options panel.
# The panel contains several widgets to modify the dataset for
# which data is displayed.  A single action (submit) button is
# provided so that it can be listened to as a signal that the
# options have been finalized and the data should be queried.
#
# The following is a list of the widgets that this panel provides.
#
# Each entry in the list is formatted as:
#   <element ID>:
#       <element type>
#       <default value>
#       <additional comments>
#
#   studioSelectId:
#       select menu
#       'All studios'
#       The items are all studios from 'globalDS' sorted lexicographically
#       in ascending order.
#       Access with input$studioSelectId
#
#   genreSelectId:
#       select menu
#       'All genres'
#       The items are all genres from 'globalDS' sorted lexicographically
#       in ascending order.
#       Access with input$genreSelectId.
#
#   sourceSelectId:
#       select menu
#       'All sources'
#       The items are all levels from globalDS$source sorted
#       lexicographically in ascending order.
#       Access with input$sourceSelectId.
#
#   typeSelectId:
#       select menu
#       'All types'
#       The items are all levels from globalDS$type sorted
#       lexicographically in ascending order.
#       Access with input$typeSelectId.
#
#   fromYearSelectSelectId:
#       select menu
#       '1990'
#       The items are all years from 1990 to 2016 in that order
#       Access with input$fromYearSelectId.
#
#   toYearSelectSelectId:
#       select menu
#       '2016'
#       The items are all years from 1990 to 2016 in that order
#       Access with input$toYearSelectId.
#
#   changeDataActionButton:
#       actionButton
#       Default label is 'Done'
#       This button can be used to get all data from each widget
#       collectively at the same time.  It should be used as the
#       first argument to an eventReactive() call, with the second
#       argument being a block of code that serves as a reactive by
#       accessing the data from the other widgets.
#       Access with input$changeDataActionButton.
#
# In addition, space is provided to show a message to the user.  The
# HTML that shows the message needs to be implemented by the server
# using the renderUI function, and assigned to output$warning_space.
# This can be any HTML structure (or nothing--something like <p></p>).
#
# Returns:
#   A div with fluidRow elements containing the above widgets.
source('main5.R')

# Create a fluidRow with widgets for changing options.  See the file
# description for details on the elements that are built.
createOptionsPanel = function() {
    studioSelect =
        column(
               3,
               selectInput(
                           inputId='studioSelectId',
                           label='Select studio',
                           choices=c('All studios',
                                     sort(levels(globalDS$studio))),
                           selected='All studios'
                           )
               )
    genreSelect =
        column(
               3,
               selectInput(
                           inputId='genreSelectId',
                           label='Select genre',
                           choices=c('Any genre',
                                     sort(names(globalDS)[GENRE_COLS])),
                           selected='Any genre'
                           )
               )
    sourceSelect =
        column(
               3,
               selectInput(
                           inputId='sourceSelectId',
                           label='Select source',
                           choices=c('Any source',
                                     sort(levels(globalDS$source))),
                           selected='Any source'
                           )
               )
    typeSelect =
        column(
               3,
               selectInput(
                           inputId='typeSelectId',
                           label='Select type',
                           choices=c('Any type',
                                     sort(levels(globalDS$type))),
                           selected='Any type'
                           )
               )
    fromYearSelect =
        column(
               5,
               selectInput(
                           inputId='fromYearSelectId',
                           label='Select start year',
                           choices=1990 : 2016,
                           selected='1990'
                           )
               )
    toYearSelect =
        column(
               5,
               selectInput(
                           inputId='toYearSelectId',
                           label='Select end year',
                           choices=1990 : 2016,
                           selected='2016'
                           )
               )
    doneButton =
        column(
               2,
               actionButton('changeDataActionButton', 'Done')
               )

    row1 = fluidRow(
                    studioSelect,
                    genreSelect,
                    typeSelect,
                    sourceSelect
                    )
    row2 = fluidRow(
                    fromYearSelect,
                    toYearSelect,
                    doneButton
                    )
    row3 = fluidRow(htmlOutput('warning_space'))

    panel = div(row1, row2, row3)
}
