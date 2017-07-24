# This file defines all data related to reactive expressions.
# Any code that needs to use a reactive expression should use
# the functions and variables in this file instead of creating
# standalone reactives in seperate files.
#
# All functions return a reactive expression.  They all start
# with 'getReactive'.
#
# All variables are reactive expressions, and they all start
# with 'reactive'.
#
# Variables:
#  reactiveDataChange: react to user input for changing the data set
#
# Functions (that return reactive expressions):
#  getReactiveShowQuantiles: react to user input for plotting quantiles
#  getReactiveClassChange: react to user input for changing plots
#  getReactiveGlobalPerf: react to data set changes to compute statistics
#  getReactivePredictions:

# Update the underlying data set based on given parameters.
# This reactive returns the data set being computed on.  It
# listens to the changeDataActionButton and updates the data
# set based on values of the widgets in the options panel.
reactiveDataChange = eventReactive(
    input$changeDataActionButton, {
       studio = input$studioSelectId
       genre = input$genreSelectId
       src = input$sourceSelectId
       type = input$typeSelectId
       fromYear = as.integer(input$fromYearSelectId)
       toYear = as.integer(input$toYearSelectId)
       category = ''
       label = NULL
       localDS = globalDS

       # These if-blocks gradually decrease the data set
       # based on the parameters.  The current implementation
       # is fine at the moment, but could possibly become
       # slow since theoretically, only one pass through
       # the global data set should be enough.

       # Filter data set by studio.
       if (studio != 'All studios') {
           localDS = localDS[localDS$studio == studio, ]
       }
       else {
           studio = NULL
       }
       # Filter data set by genre.
       if (genre != 'All genres') {
           wgenre = which(names(localDS) == genre)
           localDS = localDS[localDS[[wgenre]] == 1, ]
           category = wgenre
           label = 1
       }
       # Filter data set by source.
       if (src != 'All sources') {
           localDS = localDS[localDS$source == src, ]
           category = 'source'
           label = src
       }
       # Filter data set by type.
       if (type != 'All types') {
           localDS = localDS[localDS$type == type, ]
           category = 'type'
           label = type
       }
       # Filter data set by year.
       localDS = localDS[
                         !(localDS$year < fromYear) &
                             !(localDS$year > toYear),
                         ]

       # The filtered data might yield an empty set.  In
       # this case, keep showing the current data (locData)
       # but signal that the filtered data is empty.
       if ((nrow(localDS) == 0)) {
           locData$badQry = TRUE
           return(locData)
       }

       if (nrow(localDS) != nrow(globalDS)) {
           localDS = init_data(localDS, globalDS, studio,
                               category, label)
       }
       else {
           localDS = globalData
       }

       localDS$badQry = FALSE
       locData <<- localDS
       return(localDS)
    },
    ignoreNULL = FALSE
)

# Create reactives to listen to checkbox-like selections.
#
# This reactive is a conductor.
#
# Arguments:
#   tabId:
#     One of the following variables: TAB_ID_SUMMARY.
#
#   whichPlot:
#     One of the following variables: PLOT_SCORE_VS_TIME,
#     PLOT_EPS_VS_TIME, PLOT_VIEWS_VS_TIME.
#
# Sources:
#   input$checkboxX_Y_1 (min)
#   input$checkboxX_Y_2 (25th percentile)
#   input$checkboxX_Y_3 (mean)
#   input$checkboxX_Y_4 (75th percentile)
#   input$checkboxX_Y_5 (max)
#   X is the value of 'tabId'
#   Y is the value of 'whichPlot'
#
#   Sources can be anything for which input$checkboxX_Y_Z is TRUE
#   or FALSE.  When a source's value is TRUE, the value in parenthesis
#   is to be computed.
#
# Returns:
#   An integer vector with values in S = {1, 2, 4, 5, 6}.  These values
#   correspond to indeces in the vector returned by summary().  When
#   input$checkboxX_Y_Z is true, the returned vector contains S[Z].
#   This allows the following:
#
#       reactiveQuantiles = getReactiveShowQuantiles()
#       x = summary(y)[reactiveQuantiles()]
#       or
#       x = data$genres$qscore_timeline[, reactiveQuantiles()]
getReactiveShowQuantiles = function(tabId, whichPlot) {
    # Create checkbox IDs of the format specified in
    # tabs.R.
    idPrefix = paste('checkbox', tabId, '_',
                     whichPlot, '_', sep='')
    cbIds = sapply(1 : 5, # Checkbox indeces.
                   function(cbIdx) {
                       paste(idPrefix, cbIdx, sep='')
                   })
    reactive({
        wquantile = integer(0)
        if (input[[cbIds[1]]] == TRUE) {
            wquantile = c(wquantile, 1)
        }
        if (input[[cbIds[2]]] == TRUE) {
            wquantile = c(wquantile, 2)
        }
        if (input[[cbIds[3]]] == TRUE) {
            wquantile = c(wquantile, 4)
        }
        if (input[[cbIds[4]]] == TRUE) {
            wquantile = c(wquantile, 5)
        }
        if (input[[cbIds[5]]] == TRUE) {
            wquantile = c(wquantile, 6)
        }
        return(wquantile)
    })
}

# Create a reactive that listens to selections for changing
# plots.  The source of this reactive depends on the value
# of 'tabId'.  
#
# This reactive is a conductor.
#
# Arguments:
#   tabId:
#     One of the following variables: TAB_ID_GENRES, TAB_ID_SOURCES,
#     TAB_ID_STUDIOS, TAB_ID_TYPES.
#
# Sources:
#   input$select2_3 (if tabIdx == TAB_ID_GENRES)
#   input$select3_3 (if tabIdx == TAB_ID_SOURCES)
#   input$select4_3 (if tabIdx == TAB_ID_TYPES)
#   input$select5_3 (if tabIdx == TAB_ID_STUDIOS)
#
#   Sources can be anything for which input$selectX_Y is one of
#   levels in the corresponding factor (for example, select3_3
#   can be some widget for which input$select3_3 is one of the
#   elements in levels(globalDS$sources)).
#
# Returns:
#   The index of the source's value in sort(levels(globalDS$x)),
#   if tabIdx is anything except TAB_ID_GENRES (x is either source,
#   type, or studio, depending on the value of 'tabIdx').  When
#   'tabIdx' is TAB_ID_GENRES, the return value is the index of
#   the source's value in sort(names(globalDS)[GENRE_COLS]).
getReactiveClassChange = function(tabId) {
    # For the input$select* accesses, x is the ID of a select
    # menu formatted as specified in tabs.R.

    # Each reactive listens to changes on a selection menu
    # for a specific tab.
    if (tabId == TAB_ID_GENRES) {
        classes = sort(names(globalDS)[GENRE_COLS],
                       decreasing=FALSE)
        reactive({
            class = input$select2_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_SOURCES) {
        classes = sort(levels(globalDS$source),
                       decreasing=FALSE)
        reactive({
            class = input$select3_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_TYPES) {
        classes = sort(levels(globalDS$type),
                       decreasing=FALSE)
        reactive({
            class = input$select4_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_STUDIOS) {
        classes = sort(levels(globalDS$studio),
                       decreasing=FALSE)
        reactive({
            class = input$select5_3
            idx = which(classes == class)
            return(idx)
        })
    }
}

# Create a reactive that listens to data changes.  Whenever
# the data set changes, this reactive computes the medians
# of a given variable in 'globalDS' for different years.
#
# This reactive is a conductor.
#
# Arguments:
#   rv:
#     One of the following strings: 'score', 'views', 'eps'.  The
#     medians of the given value will be computed.
#
# Sources:
#   reactiveDataChange
#
# Returns:
#   A numeric vector with medians for 'rv' computed for each year
#   in the new data set.  The medians are calculated using 'globalDS'.
#   Concretely, if v is the returned vector, v[j] is the median 'rv'
#   for rows in 'globalDS' with a year value of data$years[j], where
#   'data' is the new data set returned by the source.
getReactiveGlobalPerf = function(rv) {
    if (rv == 'score') {
        rv = globalDS$score
    } else if (rv == 'views') {
        rv = globalDS$tot_watched
    } else if (rv == 'eps') {
        rv = globalDS$tot_eps
    }

    reactive({
        data = reactiveDataChange()
        gblPerf = sapply(
                         data$years,
                         function(year) {
                             median(
                                    rv[globalDS$year == year],
                                    na.rm=TRUE
                                    )
                         })
        return(gblPerf)
    })
}

# Create a reactive that listens to changes in a data range.
# This reactive only reacts to clicks on input$predictionsDate
# ActionButton, whereupon it uses input$predictions_year_select
# and input$predictions_season_select to return the appropriate
# data.
#
# This reactive is a conductor.
#
# Sources:
#   input$predictionsDateActionButton
#   input$predictions_year_select
#   input$predictions_season_select
#
# Returns:
#   An integer vector of row indeces in 'globalDS' that have a
#   value of year in the range specified by the source.  The
#   rows are further limited to those with a value of 'TV' for
#   the 'type' column.
getReactivePredictions = function() {
    eventReactive(
        input$predictionDateActionButton, {
            year = input[['predictions_year_select']]
            year = as.integer(year)
            season = input[['predictions_season_select']]
            months = NULL
            indeces = NULL

            if (season != 'Winter') {
              if (season == 'Spring') months = c(3, 4, 5)
              else if (season == 'Summer') months = c(6, 7, 8)
              else months = c(9, 10, 11)
              indeces = which(
                              (globalDS$type == 'TV') &
                                  (globalDS$year == year) &
                                  (globalDS$month %in% months)
                              )
            }
            else {
              indeces = which(
                              (globalDS$type == 'TV') &
                                  (
                                   (
                                    (globalDS$year == year) &
                                        (globalDS$month %in% c(1, 2))
                                    ) |
                                   (
                                    (globalDS$year == (year - 1)) &
                                        (globalDS$month == 12)
                                    )
                                   )
                              )
            }

            return(indeces)
        },
        ignoreNULL = FALSE
        )
}

