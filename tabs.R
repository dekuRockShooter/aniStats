# This file is for creating the tabs.  There are two functions:
#   createCatTab: create a tab to show data for a categorical variable
#   createSummaryTab: create a tab to show a summary of all the data
#
# These functions return tabPanel object and are meant to be used as:
#
#  tabsetPanel(
#              createSummaryTab('Summary', summaryTabId),
#              createCatTab('Category', catTabId)
#             )
#
# Each tab consists of several plots and widgets.  The ids of each element
# depend on the arguments passed (name, tabIdx).  The ids correspond to names
# in the Shiny 'output' and 'input' list.  The ids are formatted as:
#
#   plots: 'plotx_y', x = tabIdx, y = row in which the plot is displayed.
#       Access with output[['plotx_y']].
#
#   brushes: 'brushx_y', x = tabIdx, y = row in which the plot is displayed.
#       This is the brush for 'plotx_y'.  All plots except plot 4 have a brush.
#       Access with input[['brushx_y']].
#
#   double clicks: 'dblclickx_y', x = tabIdx, y = row in which the plot is
#       displayed.  This is the double click for 'plotx_y'.  All plots
#       except plot 4 support double click.
#       Access with input[['dblclickx_y']].
#
#   select menus: 'selectx_y', x = tabIdx, y = row in which the plot is
#       displayed.  This is the select menu for 'plotx_y'.  Only plot 3
#       has a select menu.
#       Access with input[['selectx_y']].
#
#   checkboxes: 'checkboxx_y_z', x = tabIdx, y = row in which the plot is
#       displayed, z = position from the top where the checkbox is located
#       (1 is topmost, 5 is bottomost).  Only plots 1, 2, and 3 of tabs
#       created by createSummaryTab contain checkboxes.
#       Access with input[['selectx_y_z']].
source('main5.R')

# Create a tabPanel for one of the categorical variables tabs.  See the
# file description for details.
#
# name: The name to display on the tab.
# tabIdx: The id of the tab.  This is one of the TAB_ID_* constants in
#   constants.R, except TAB_ID_SUMMARY.
#
# Returns a tabPanel.
createCatTab = function(name, tabIdx) {
    plotPrefix = paste('plot', tabIdx, '_', sep='')
    brushPrefix = paste('brush', tabIdx, '_', sep='')
    dblClkPrefix = paste('dblclick', tabIdx, '_', sep='')
    # Ids for plots.  Format is 'plotx_y', x=tabIdx, y=rowIdx.
    plotIds = sapply(1 : 7, function(idx) paste(plotPrefix, idx, sep=''))
    # Ids for brushes  Format is 'brushx_y', x=tabIdx, y=rowIdx.
    brushIds = sapply(1 : 7, function(idx) paste(brushPrefix, idx, sep=''))
    # Ids for double clicks  Format is 'dblclickx_y', x=tabIdx, y=rowIdx.
    dblClkIds = sapply(1 : 7, function(idx) paste(dblClkPrefix, idx, sep=''))
    # Id for select menu.  Format is 'selectx_3', x=tabIdx.  Only plot 3 has
    # a select menu.
    selectId = paste('select', tabIdx, '_3', sep='')
    selectLabel = NULL
    selectItems = NULL
    # Array of help text for each plot.  The array is chosen in accordance to
    # tabIdx.
    help_text = NULL
    # Initialize the select menu items.
    if (tabIdx == 2) {
        selectLabel = 'Select genre'
        selectItems = sort(names(globalDS) [GENRE_COLS], decreasing=FALSE)
        help_text = HELP_TEXT$genres
    } else if (tabIdx == 3) {
        selectLabel = 'Select source'
        selectItems = sort(levels(globalDS$source), decreasing=FALSE)
        help_text = HELP_TEXT$source
    } else if (tabIdx == 4) {
        selectLabel='Select type'
        selectItems = sort(levels(globalDS$type), decreasing=FALSE)
        help_text = HELP_TEXT$type
    } else if (tabIdx == 5) {
        selectLabel='Select studio'
        selectItems = sort(levels(globalDS$studio), decreasing=FALSE)
        help_text = HELP_TEXT$studio
    }

    # Create columns common to most rows.  These have plots with support for
    # brushes and double clicks, and some have a hoverable options menu.
    createCol = function(rowIdx, colSize) {
        checkboxDiv = NULL
        optionsDiv = NULL
        # Plot common to all rows.
        plotOut = plotOutput(
                             # plot id.
                             plotIds[rowIdx],
                             brush=brushOpts(
                                             id=brushIds[rowIdx],
                                             resetOnNew=TRUE
                                             ),
                             dblclick=dblClkIds[rowIdx]
                             )

        # Rows 3 has an options menu that appears when the plot
        # is hovered over.
        if (rowIdx == 3) {
            checkboxDiv = div(
                              class='percentile_checkbox',
                              selectInput(
                                          inputId=selectId,
                                          label=selectLabel,
                                          choices=selectItems,
                                          selected=selectItems[1]
                                          )
                              )
            optionsDiv = div(
                             class='hover_options',
                             checkboxDiv,
                             tags$p('Options')
                             )
            #plotOut = div(class='hover_plot', plotOut, optionsDiv)
        }

        # This gives all plots a tooltip that displays information on hover.
        helpTextDiv = div(class='plot_help_text', tags$p(help_text[rowIdx]))
        helpDiv = div(class='hover_help', helpTextDiv, tags$p('?'))

        plotOut = div(class='hover_plot', plotOut, optionsDiv, helpDiv)

        column(
               colSize,
               plotOut
               )
    }

    tabPanel(
             name,
             fluidRow(createCol(1, 12)),
             fluidRow(createCol(2, 12)),
             fluidRow(createCol(3, 12)),
             fluidRow(createCol(4, 12)),
             fluidRow(createCol(5, 12)),
             fluidRow(createCol(6, 12)),
             fluidRow(createCol(7, 12)),
             value=tabIdx
             )
}

# Create a summary tabPanel.  See the file description for details.
#
# name: The name to display on the tab.
# tabIdx: The id of the tab.  Currently, only TAB_ID_SUMMARY is supported.
#
# Returns a tabPanel.
createSummaryTab = function(name, tabIdx) {
    plotPrefix = paste('plot', tabIdx, '_', sep='')
    brushPrefix = paste('brush', tabIdx, '_', sep='')
    dblClkPrefix = paste('dblclick', tabIdx, '_', sep='')
    # Ids for plots.  Format is 'plotx_y', x=tabIdx, y=rowIdx.
    plotIds = sapply(1 : 7, function(idx) paste(plotPrefix, idx, sep=''))
    # Ids for brushes  Format is 'brushx_y', x=tabIdx, y=rowIdx.
    brushIds = sapply(1 : 7, function(idx) paste(brushPrefix, idx, sep=''))
    # Ids for double clicks  Format is 'dblclickx_y', x=tabIdx, y=rowIdx.
    dblClkIds = sapply(1 : 7, function(idx) paste(dblClkPrefix, idx, sep=''))
    # Ids for checkboxes  Format is 'checkboxx_y', x=rowIdx, y=checkboxIdx.
    cbIds = lapply(1 : 3, # Row indeces.  First three plots have checkboxes.
                   function(rowIdx) {
                       idPrefix = paste(
                                        'checkbox', tabIdx, '_',
                                        rowIdx, '_', sep=''
                                        )
                       sapply(1 : 5, # Checkbox indeces.
                              function(cbIdx) {
                                  paste(idPrefix, cbIdx, sep='')
                              })
                   })
    # The rows are plots ordered from top to bottom (first row is the topmost
    # plot, last row is the bottomost plot).  The columns are checkboxes
    # ordered from top to bottom (first column is the topmost checkbox, last
    # column is the bottomost checkbox).
    cbIds = do.call(rbind, cbIds)

    # Create columns common to most rows.  These have plots with support for
    # brushes and double clicks, and some have a hoverable options menu.
    createCol = function(rowIdx, colSize) {
        checkboxDiv = NULL
        optionsDiv = NULL
        # Plot common to all rows.
        plotOut = plotOutput(
                             # plot id.
                             plotIds[rowIdx],
                             brush=brushOpts(
                                             id=brushIds[rowIdx],
                                             resetOnNew=TRUE
                                             ),
                             dblclick=dblClkIds[rowIdx]
                             )

        # Rows 1, 2, and 3 have an options menu that appears when the plot
        # is hovered over.  This creates an HTML tree of divs as below.  The
        # dectection of hovers and visibility of each div is handled by the
        # hover.css file.  When plot (outermost div) is hovered, the icon
        # is displayed.  When the icon (second div) is hovered, the checkboxes
        # are displayed (innermost div).
        #
        #     <div class='hover_plot'>
        #       plotOut
        #       <div class='hover_options'>
        #         <div class='percentile_checkbox'>
        #           checkboxes
        #         </div>
        #       optionIcon
        #       </div>
        #     </div>
        if ((rowIdx > 0) && (rowIdx < 4)) {
            checkboxDiv = createCheckboxCol(cbIds[rowIdx, ])
            optionsDiv = div(
                             class='hover_options',
                             checkboxDiv,
                             tags$p('Options')
                             )
        }
        helpTextDiv = div(
                          class='plot_help_text',
                          tags$p(HELP_TEXT$summary[rowIdx])
                          )
        helpDiv = div(class='hover_help', helpTextDiv, tags$p('?'))
        plotOut = div(class='hover_plot', plotOut, optionsDiv, helpDiv)

        column(
               colSize,
               plotOut
               )
    }

    # Convenience function for creating a div that displays the
    # checkboxes.  'cbIds' is a row of this.cbIds.
    createCheckboxCol = function(cbIds) {
        div(
            class='percentile_checkbox',
            checkboxInput(cbIds[1], 'Min', FALSE),
            checkboxInput(cbIds[2], '25th percentile', FALSE),
            checkboxInput(cbIds[3], 'Mean', FALSE),
            checkboxInput(cbIds[4], '75th percentile', FALSE),
            checkboxInput(cbIds[5], 'Max', FALSE)
            )
    }

    tabPanel(
             name, 
             fluidRow(createCol(1, 12)),
             fluidRow(createCol(2, 12)),
             fluidRow(createCol(3, 12)),
             fluidRow(createCol(4, 12)),
             fluidRow(createCol(5, 12)),
             fluidRow(createCol(6, 12)),
             fluidRow(createCol(7, 12)),
             value=tabIdx
             )
}

# Create a tabPanel that shows a table.
#
# The tab contains a table with id 'table_predictions' (defined in server.R
# using output$table_predictions = renderTable(...) or something similar).
# Two select menus are also available.  The first one has id 'predictions_
# year_select' and is meant to choose the year for which predictions are
# shown.  The values are 1990 to 2016, as strings.  The second select menu
# has id 'predictions_season_select' and is meant to select the season for
# which to show predictions.  Values are 'Winter', 'Spring', 'Summer', and
# 'Fall'.  A button is also available that signals that the selections are
# final and that data should be queried.  This button has id 'predictionDate
# ActionButton', and should be used in an eventReactive:
#
#    eventReactive(output$predictionDateActionButton, {
#        # do something with input$predictions_season_select and
#        # input$predictions_year_select.
#    })
#
# The select menus and button are contained in a bootstrap row.
#
createPredictionsTab = function(name, tabIdx) {
    tableOut = htmlOutput('table_predictions')
    yearSelect = selectInput(
                inputId='predictions_year_select',
                label='year',
                choices=1990:2016,
                selected=2016
                )
    seasonSelect = selectInput(
                               inputId='predictions_season_select',
                               label='season',
                               choices=c('Winter', 'Spring', 'Summer', 'Fall'),
                               selected='Winter'
                               )
    doneButton = actionButton('predictionDateActionButton', 'Done')

    optionsRow = fluidRow(
                          column(3, yearSelect),
                          column(3, seasonSelect),
                          column(2, doneButton)
                          )
    tabPanel(
             name,
             optionsRow,
             tableOut,
             value=tabIdx
             )
}

# Create a tab to show performance figures of the classifier.
#
# The tab has images that show various performance metrics.  The
# id's of each figure are 'acc_vs_time', 'tp_vs_tn', 'fp_vs_stucnt',
# and 'fn_vs_stucnt'.  These should be elements of the Shiny 'output'
# list.  Each figure is wrapped in a div element with class
# 'performance_image'.
#
# The return value is a tabPanel.
createPerformanceTab = function(name, tabIdx) {
    imgIds = c(
               'acc_vs_time',
               'tp_vs_tn',
               'fp_vs_stucnt',
               'fn_vs_stucnt'
               )

    imgDivs = list()
    for (imgId in imgIds) {
        imgDiv = tags$div(class='performance_image', plotOutput(imgId))
        imgDivs[[length(imgDivs) + 1]] = imgDiv
    }

    tabPanel(
             name,
             htmlOutput('table_studio_acc'),
             imgDivs,
             value=tabIdx
             )
}
