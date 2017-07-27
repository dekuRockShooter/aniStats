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
# Each tab consists of several plots and widgets.  Each plot is accessed
# with plotOutput(p).  Here, p is the ID of the plot, and corresponds to
# output$p = renderPlot(...), which is assumed to have already been
# created in server.R.  The ID, p, of a plot is formatted as follows:
#
#   'plotx_y', x is a value in tabsEnum, y is a value in plotsEnum.
#   Concretely, these IDs are constructed as
#   paste0('plot', tabEnum, '_', plotEnum), where tabEnum and plotEnum are
#   enums in tabsEnum and plotsEnum, respectively.
#
# output[['plotx_y']] is the associated plot.
#
# Some plots can also be brushed (click and dragged) and double clicked.
# This corresponds to the call plotOuput(p, brush=b, dblclick=d).  The
# ID of a brush (b) and double click (d) are:
#
#   'brushx_y', x is a value in tabsEnum, y is a value in plotsEnum,
#   'dblclickx_y', x is a value in tabsEnum, y is a value in plotsEnum.
#
# The IDs of a brush and double click for a specific plot with ID plotx_y
# have the same values for x and y.  Thus, if plot plot2_1 has a brush and
# double click, their IDs are brush2_1 and dblclick2_1, respectively.
# These IDs can be used in reactives by using input$brush2_1 and
# input$dblclick2_1.  More generally, input$brushx_y and dblclickx_y can
# be used to react to brushes and double clicks on plotx_y.
#
# Some plots also have select menus that are accessed by hovering over the
# plot.  The IDs of these are formatted as:
#
#   'selectx_y', x is a value in tabsEnum, y is a value in plotsEnum.
#
# Like with brushes and double clicks, the ID of a select menu for a
# specific plot with ID plotx_y is selectx_y, where x and y are the same
# in both IDs.  The ID of a select menu can be used as a reactive source
# with input$selectx_y.
#
# Some plots have checkboxes that are accessed by hovering over the
# plot.  The IDs of these are formatted as:
#
#   'checkboxX_Y_Z', X is a value in tabsEnum, y is a value in plotsEnum,
#   Z is an integer in [1, 5].  Z denotes the purpose of the checkbox:
#   1 for min, 2 for 25th percentile, 3 for mean, 4 for 75th percentile,
#   and 5 for max.
#
# Like with select menus, the ID of a checkbox for a specific plot with ID
# plotX_Y is checkboxX_Y_Z, where X and Y are the same in both IDs.  The ID
# of a checkbox can be used as a reactive source with input$checkboxX_Y_Z.
source('main5.R')

# Create a tabPanel for one of the categorical variables tabs.  See the
# file description for details.
#
# name: The name to display on the tab.
# tabIdx: The id of the tab.  This is one of the values of tabsEnum,
#   tabsEnum$SUMMARY.
#
# Returns a tabPanel.
createCatTab = function(name, tabIdx) {
    plots = c(
              plotsEnum$PROP_BARCHART,
              plotsEnum$SCORE_VS_CATEGORY,
              plotsEnum$CATEGORY_PERF_VS_TIME,
              plotsEnum$CATEGORY_PROP_HEATMAP,
              plotsEnum$CATEGORY_SCORE_VS_VIEWS,
              plotsEnum$CATEGORY_SCORE_VS_PROP,
              plotsEnum$CATEGORY_SCORE_SLOPE_VS_PROP_SLOPE
              )
    plotPrefix = paste('plot', tabIdx, '_', sep='')
    plotIds = sapply(plots, function(val) paste(plotPrefix, val, sep=''))

    brushPrefix = paste('brush', tabIdx, '_', sep='')
    brushIds = sapply(plots, function(val) paste(brushPrefix, val, sep=''))

    dblClkPrefix = paste('dblclick', tabIdx, '_', sep='')
    dblClkIds = sapply(plots, function(val) paste(dblClkPrefix, val, sep=''))

    selectId = paste('select', tabIdx, '_', plotsEnum$CATEGORY_PERF_VS_TIME,
                     sep='')
    selectLabel = NULL
    selectItems = NULL
    # Array of help text for each plot.  The array is chosen in accordance to
    # tabIdx.
    help_text = NULL
    # Initialize the select menu items.
    if (tabIdx == tabsEnum$GENRES) {
        selectLabel = 'Select genre'
        selectItems = sort(names(GLOBAL_DS) [GENRE_COLS], decreasing=FALSE)
        help_text = HELP_TEXT$genres
    } else if (tabIdx == tabsEnum$SOURCES) {
        selectLabel = 'Select source'
        selectItems = sort(levels(GLOBAL_DS$source), decreasing=FALSE)
        help_text = HELP_TEXT$source
    } else if (tabIdx == tabsEnum$TYPES) {
        selectLabel='Select type'
        selectItems = sort(levels(GLOBAL_DS$type), decreasing=FALSE)
        help_text = HELP_TEXT$type
    } else if (tabIdx == tabsEnum$STUDIOS) {
        selectLabel='Select studio'
        selectItems = sort(levels(GLOBAL_DS$studio), decreasing=FALSE)
        help_text = HELP_TEXT$studio
    }
    # TODO: else throw error.

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
             fluidRow(createCol(1, 12)), tags$hr(),
             fluidRow(createCol(2, 12)), tags$hr(),
             fluidRow(createCol(3, 12)), tags$hr(),
             fluidRow(createCol(4, 12)), tags$hr(),
             fluidRow(createCol(5, 12)), tags$hr(),
             fluidRow(createCol(6, 12)), tags$hr(),
             fluidRow(createCol(7, 12)),
             value=tabIdx
             )
}

# Create a summary tabPanel.  See the file description for details.
#
# name: The name to display on the tab.
# tabIdx: The id of the tab.  Currently, only tabsEnum$SUMMARY is supported.
#
# Returns a tabPanel.
createSummaryTab = function(name, tabIdx) {
    plots = c(
              plotsEnum$SCORE_VS_TIME,
              plotsEnum$VIEWS_VS_TIME,
              plotsEnum$EPS_VS_TIME,
              plotsEnum$TYPEPROP_VS_TIME,
              plotsEnum$SOURCEPROP_VS_TIME,
              plotsEnum$PERFORMANCE_VS_TIME,
              plotsEnum$SCORE_PERC_VS_SHOW_PERC
              )

    plotPrefix = paste('plot', tabIdx, '_', sep='')
    plotIds = sapply(plots, function(idx) paste(plotPrefix, idx, sep=''))

    brushPrefix = paste('brush', tabIdx, '_', sep='')
    brushIds = sapply(plots, function(idx) paste(brushPrefix, idx, sep=''))

    dblClkPrefix = paste('dblclick', tabIdx, '_', sep='')
    dblClkIds = sapply(plots, function(idx) paste(dblClkPrefix, idx, sep=''))

    # Ids for checkboxes  Format is 'checkboxx_y', x=rowIdx, y=checkboxIdx.
    cbIds = lapply(plots[1 : 3], # First three plots have checkboxes.
                   function(val) {
                       idPrefix = paste(
                                        'checkbox', tabIdx, '_',
                                        val, '_', sep=''
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
        #         optionIcon
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
             fluidRow(createCol(1, 12)), tags$hr(),
             fluidRow(createCol(2, 12)), tags$hr(),
             fluidRow(createCol(3, 12)), tags$hr(),
             fluidRow(createCol(4, 12)), tags$hr(),
             fluidRow(createCol(5, 12)), tags$hr(),
             fluidRow(createCol(6, 12)), tags$hr(),
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
             optionsRow,tags$hr(),
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
        imgDiv = tags$div(
                          class='performance_image',
                          tags$hr(),
                          tags$p(PLOT_DESC[[imgId]]),
                          plotOutput(imgId)
                          )
        imgDivs[[length(imgDivs) + 1]] = imgDiv
    }

    tabPanel(
             name,
             tags$p(PERF_TABLE_DESC),
             tags$br(), tags$hr(),
             htmlOutput('table_studio_acc'),
             imgDivs,
             value=tabIdx
             )
}

# Create a tab to show an About page.  This returns a tabPanel with id
# 'name' and value 'tabIdx'.  The <p> tags are all in CSS class 'about_text'
# and the <ul> element is in CSS class 'about_list'.
createAboutTab = function(name, tabIdx) {
    # Create <p> tags with the predefined text.
    aboutText = lapply(ABOUT_TEXT, function(text)
                       tags$p(class='about_text', text))
    # Create <li> tags with the predefined text.
    questionsListElements = lapply(ABOUT_LIST, function(text) tags$li(text))
    questionsList = tags$ul(class='about_list', questionsListElements)
    mainDiv = tags$div(
                       class='about',
                       aboutText[[1]],
                       questionsList, # The list follows the first <p>.
                       aboutText[2 : length(aboutText)]
                       )

    tabPanel(
             name,
             mainDiv,
             value=tabIdx
             )
}

# Create the main content of the Stats page.
# This returns a tabsetPanel.
initStatsTabs = function() {
    tabsetPanel(
                type="tabs",
                # input$tabpanelId is the name of the current tab.
                id='tabpanelId',
                # The argument to *Ouput(x) is the name of an R object
                # (defined in server.R as output$x) of type *.
                createSummaryTab('Summary', tabsEnum$SUMMARY),
                createCatTab('Genres', tabsEnum$GENRES),
                createCatTab('Sources', tabsEnum$SOURCES),
                createCatTab('Types', tabsEnum$TYPES),
                createCatTab('Studios', tabsEnum$STUDIOS)
                )
}

# Create the main content of the Predictions page.
# This returns a tabsetPanel.
initPredictionsTabs = function() {
    tabsetPanel(
                type="tabs",
                id='predictionsTabsetPanelId',
                createPredictionsTab(
                                     'Predictions',
                                     tabsEnum$PREDICTIONS
                                     ),
                createPerformanceTab(
                                     'Performance',
                                     tabsEnum$PERFORMANCE
                                     )
                )
}

# Create the main content of the About page.
# This returns a tabsetPanel.
initAboutTab = function() {
    tabsetPanel(
                type="tabs",
                id='aboutTabsetPanelId',
                createAboutTab('About', 1)
                )
}
