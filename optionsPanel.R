# This file contains one function for creating an options panel.
# The panel contains several widgets to modify the dataset for
# which data is displayed.  A single action (submit) button is
# provided so that it can be listened to as a signal that the
# options have been finalized and the data should be queried.
#
# The default values for the select menus are All studios, 'Any genre',
# 'Any source', 'Any genre', and 'Any studio'.  The other items are
# the levels (or names for genres) of the category, sorted in increasing
# order.
#
# The IDs of all elements are formatted as:
#
#   studioSelectId: the ID of the studio select menu.
#       Access with input$studioSelectId.
#
#   genreSelectId: the ID of the genre select menu.
#       Access with input$genreSelectId.
#
#   sourceSelectId: the ID of the source select menu.
#       Access with input$sourceSelectId.
#
#   typeSelectId: the ID of the type select menu.
#       Access with input$typeSelectId.
#
#   changeDataActionButton: the ID of the action (submit) button.
#       This would probably be most useful as the first argument
#       to an eventReactive() call.  The second argument being a
#       block of code that accesses the widget's values and acts
#       as a reactive.
#       Access with input$changeDataActionButton.
source('main5.R')

# Create a fluidRow with widgets for changing options.  See the file
# description for details on the elements that are built.
createOptionsPanel = function() {
    fluidRow(
             column(
                    2,
                    selectInput(
                                inputId='studioSelectId',
                                label='Select studio',
                                choices=c('All studios',
                                          sort(levels(globalDS$studio))),
                                selected='All studios'
                                )
                    ),
             column(
                    2,
                    selectInput(
                                inputId='genreSelectId',
                                label='Select genre',
                                choices=c('Any genre',
                                          sort(names(globalDS)[6 : 44])),
                                selected='Any genre'
                                )
                    ),
             column(
                    2,
                    selectInput(
                                inputId='sourceSelectId',
                                label='Select source',
                                choices=c('Any source',
                                          sort(levels(globalDS$source))),
                                selected='Any source'
                                )
                    ),
             column(
                    2,
                    selectInput(
                                inputId='typeSelectId',
                                label='Select type',
                                choices=c('Any type',
                                          sort(levels(globalDS$type))),
                                selected='Any type'
                                )
                    ),
             column(
                    4,
                    actionButton('changeDataActionButton', 'Done')
                    )
             )
}
