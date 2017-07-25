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
#  getReactivePredictions: react to date range changes


# Listen to changes in the options panel and update the
# underlying data set based.  This reactive only reacts
# to clicks on input$changeDataActionButton, whereupon
# it uses the other sources to update and return the
# new data set.
#
# This reactive is a conductor.
#
# Sources:
#  input$changeDataActionButton
#  input$studioSelectId
#  input$genreSelectId
#  input$sourceSelectId
#  input$typeSelectId
#  input$fromYearSelectId
#  input$toYearSelectId
#
# Returns:
#  The new data set (as returned by init_data()).  The
#  value of 'locData' is also changed to refer to the new
#  data set.
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
       localDS = GLOBAL_DS

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

       if (nrow(localDS) != nrow(GLOBAL_DS)) {
           localDS = init_data(localDS, GLOBAL_DS, studio,
                               category, label)
       }
       else {
           localDS = GLOBAL_DATA
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
#   elements in levels(GLOBAL_DS$sources)).
#
# Returns:
#   The index of the source's value in sort(levels(GLOBAL_DS$x)),
#   if tabIdx is anything except TAB_ID_GENRES (x is either source,
#   type, or studio, depending on the value of 'tabIdx').  When
#   'tabIdx' is TAB_ID_GENRES, the return value is the index of
#   the source's value in sort(names(GLOBAL_DS)[GENRE_COLS]).
getReactiveClassChange = function(tabId) {
    # For the input$select* accesses, x is the ID of a select
    # menu formatted as specified in tabs.R.

    # Each reactive listens to changes on a selection menu
    # for a specific tab.
    if (tabId == TAB_ID_GENRES) {
        classes = sort(names(GLOBAL_DS)[GENRE_COLS],
                       decreasing=FALSE)
        reactive({
            class = input$select2_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_SOURCES) {
        classes = sort(levels(GLOBAL_DS$source),
                       decreasing=FALSE)
        reactive({
            class = input$select3_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_TYPES) {
        classes = sort(levels(GLOBAL_DS$type),
                       decreasing=FALSE)
        reactive({
            class = input$select4_3
            idx = which(classes == class)
            return(idx)
        })
    } else if (tabId == TAB_ID_STUDIOS) {
        classes = sort(levels(GLOBAL_DS$studio),
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
# of a given variable in 'GLOBAL_DS' for different years.
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
#   in the new data set.  The medians are calculated using 'GLOBAL_DS'.
#   Concretely, if v is the returned vector, v[j] is the median 'rv'
#   for rows in 'GLOBAL_DS' with a year value of data$years[j], where
#   'data' is the new data set returned by the source.
getReactiveGlobalPerf = function(rv) {
    if (rv == 'score') {
        rv = GLOBAL_DS$score
    } else if (rv == 'views') {
        rv = GLOBAL_DS$tot_watched
    } else if (rv == 'eps') {
        rv = GLOBAL_DS$tot_eps
    }

    reactive({
        data = reactiveDataChange()
        gblPerf = sapply(
                         data$years,
                         function(year) {
                             median(
                                    rv[GLOBAL_DS$year == year],
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
#   An integer vector of row indeces in 'GLOBAL_DS' that have a
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
                              (GLOBAL_DS$type == 'TV') &
                                  (GLOBAL_DS$year == year) &
                                  (GLOBAL_DS$month %in% months)
                              )
            }
            else {
              indeces = which(
                              (GLOBAL_DS$type == 'TV') &
                                  (
                                   (
                                    (GLOBAL_DS$year == year) &
                                        (GLOBAL_DS$month %in% c(1, 2))
                                    ) |
                                   (
                                    (GLOBAL_DS$year == (year - 1)) &
                                        (GLOBAL_DS$month == 12)
                                    )
                                   )
                              )
            }

            return(indeces)
        },
        ignoreNULL = FALSE
        )
}

# Create a reactive source for zooming in and out of plots.
# The source has two variables: x and y.  Both are two-element
# vectors, such that the first element is the min value in the
# x (y) direction, and the second element is the max value in
# the x (y) direction.  Thus, the following use case is possible:
#
#    reactiveRanges = getReactiveZoom(brushId, dblClickId)
#    xlim = reactiveRanges$x
#    ylim = reactiveRanges$y
#    plot(t, z, xlim=xlim[1] : xlim[2], ylim=ylim[1] : ylim[2])
#
# Arguments:
#  brushId:
#    a string formatted as specified in tabs.R.
#
#  dblClickId:
#    a string formatted as specified in tabs.R.
#
# Endpoints:
#  output$x, such that x = renderPlot(...)
getReactiveZoom = function(brushId, dblClickId) {
    # Taken from the zoom example on the Shiny Server gallery
    # page.
    #
    # These blocks of code implement zoom in and out.  Zooming
    # in means drawing a brush (click and drag), and zooming
    # out means double clicking.
    #
    # Together, these functions are meant to be used as:
    #   r = getReactivePlotLimitsChange()
    #   observeBrush(bid, r) # Listen to brushes and update r.
    #   observeDblClick(did, r) # Listen to dblclk and update r.
    #   ...
    #   plot(..., ylim=r$y, xlim=r$x) # Redraw on brush or dblclk.
    #   ...

    # This reactive is a source.  It is meant to change the limits of
    # a plot.  It has two variables: x and y.   Both are intended to
    # be two-element vectors, such that the first element is the min
    # value in the x (y) direction, and the second element is the max
    # value in the x (y) direction.  Thus, the following can be done:
    #
    #   reactiveLimits = getReactivePlotLimitsChange()
    #   ...
    #   output[[endpoint1]] = renderPlot({
    #     xlim = reactiveLimits$x
    #     ylim = reactiveLimits$y
    #     plot(x, y, xlim=xlim[1] : xlim[2], ylim=ylim[1] : ylim[2])
    #   })
    #
    # Endpoints:
    #  output$x, such that x = renderPlot(...)
    getReactivePlotLimitsChange = function() {
        reactiveValues(x=NULL, y=NULL)
    }

    # Create an observer for a brush action.  When the given
    # brush is acted on, the reactivePlotLimitChange (r) will be
    # updated such that r$x and r$y are the x and y limits
    # of the rectangle, respectively.  This is meant to be used
    # as:
    #   r = getReactivePlotLimitListener()
    #   observeBrush(bid, r)
    # brushId must be formatted as specified in tabs.R.
    observeBrush = function(brushId, reactivePlotLimitChange) {
        observeEvent(input[[brushId]], {
                         brush = input[[brushId]]
                         reactivePlotLimitChange$x <<-
                             c(brush$xmin, brush$xmax)
                         reactivePlotLimitChange$y <<-
                             c(brush$ymin, brush$ymax)
        })
    }

    # Create an observer for a double click.  When the given
    # double click is acted on, the reactivePlotLimitChange (r)
    # will be updated such that r$x and r$y are both NULL.
    # This is meant to be used as:
    #   r = getReactivePlotLimitListener()
    #   observeDblClick(did, r)
    # dblClickId must be formatted as specified in tabs.R.
    observeDblClick = function(dblClickId, reactivePlotLimitChange) {
        observeEvent(input[[dblClickId]], {
                         reactivePlotLimitChange$x <<- NULL
                         reactivePlotLimitChange$y <<- NULL
        })
    }

    reactiveLimits = getReactivePlotLimitsChange()
    # Listen to brushes and update reactiveLimits.
    observeBrush(brushId, reactiveLimits)
    # Listen to dblclk and update reactiveLimits.
    observeDblClick(dblClickId, reactiveLimits)

    return(reactiveLimits)
}
