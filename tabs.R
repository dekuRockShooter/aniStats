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
    # Initialize the select menu items.
    if (tabIdx == 2) {
        selectLabel = 'Select genre'
        selectItems = sort(names(globalDS) [6 : 44], decreasing=FALSE)
    } else if (tabIdx == 3) {
        selectLabel = 'Select source'
        selectItems = sort(levels(globalDS$source), decreasing=FALSE)
    } else if (tabIdx == 4) {
        selectLabel='Select type'
        selectItems = sort(levels(globalDS$type), decreasing=FALSE)
    } else if (tabIdx == 5) {
        selectLabel='Select studio'
        selectItems = sort(levels(globalDS$studio), decreasing=FALSE)
    }

    # Create columns common to most rows.  These have plots with support for
    # brushes and double clicks.
    createCol = function(rowIdx, colSize) {
        column(
               colSize,
               plotOutput(
                          # plot id.
                          plotIds[rowIdx],
                          brush=brushOpts(
                                          id=brushIds[rowIdx],
                                          resetOnNew=TRUE
                                          ),
                          dblclick=dblClkIds[rowIdx]
                          )
               )
    }

    tabPanel(
             name,
             fluidRow(createCol(1, 12)),
             fluidRow(createCol(2, 12)),
             fluidRow(
                      createCol(3, 10),
                      column(2,
                             selectInput(
                                         inputId=selectId,
                                         label=selectLabel,
                                         choices=selectItems,
                                         selected=selectItems[1]
                                         )
                             )
                      ),
             fluidRow(column(12, plotOutput(plotIds[4]))),
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
    # brushes and double clicks.
    createCol = function(rowIdx, colSize) {
        column(
               colSize,
               plotOutput(
                          # plot id.
                          plotIds[rowIdx],
                          brush=brushOpts(
                                          id=brushIds[rowIdx],
                                          resetOnNew=TRUE
                                          ),
                          dblclick=dblClkIds[rowIdx]
                          )
               )
    }

    # Convenience function for creating a column that displays the
    # checkboxes.  'cbIds' is a row of this.cbIds.
    createCheckboxCol = function(cbIds) {
        column(
               2,
               checkboxInput(cbIds[1], 'Min', FALSE),
               checkboxInput(cbIds[2], '25th percentile', FALSE),
               checkboxInput(cbIds[3], 'Mean', FALSE),
               checkboxInput(cbIds[4], '75th percentile', FALSE),
               checkboxInput(cbIds[5], 'Max', FALSE)
               )
    }

    tabPanel(
             name, 
             fluidRow(createCol(1, 10), createCheckboxCol(cbIds[1, ])),
             fluidRow(createCol(2, 10), createCheckboxCol(cbIds[2, ])),
             fluidRow(createCol(3, 10), createCheckboxCol(cbIds[3, ])),
             fluidRow(column(12, plotOutput(plotIds[4]))),
             fluidRow(column(12, plotOutput(plotIds[5]))),
             fluidRow(createCol(6, 12)),
             fluidRow(createCol(7, 12)),
             value=tabIdx
             )
}
