library(shiny)
library(lattice)
source('main5.R')
source('constants.R')

# Initialization of data common to all users.
# Executed once throughout lifetime.


shinyServer(
            function(input, output) {
                # Initialization of data specific to the current user.
                # Executed once for every user.
                #dataset = D[(D$studio == cur_studio), ]

                # The render* functions are executed everytime a widget
                # that it is observing changes.  These widgets are stored
                # in input$x.  If a function does not use the input list,
                # then it is executed only once. 

                # Update the underlying data set based on given parameters.
                # This reactive returns the data set being computed on.  It
                # listens to the changeDataActionButton and updates the data
                # set based on values of the widgets in the options panel.
                reactiveDataChange = eventReactive(
                    input$changeDataActionButton,
                    {
                        studio = input$studioSelectId
                        genre = input$genreSelectId
                        src = input$sourceSelectId
                        type = input$typeSelectId
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
                        if (genre != 'Any genre') {
                            wgenre = which(names(localDS) == genre)
                            localDS = localDS[localDS[[wgenre]] == 1, ]
                        }
                        # Filter data set by source.
                        if (src != 'Any source') {
                            localDS = localDS[localDS$source == src, ]
                        }
                        # Filter data set by type.
                        if (type != 'Any type') {
                            localDS = localDS[localDS$type == type, ]
                        }

                        if (nrow(localDS) != nrow(globalDS)) {
                            localDS = init_data(localDS, globalDS, studio)
                        }
                        else {
                            localDS = globalData
                        }
                        return(localDS)
                    },
                    ignoreNULL = FALSE
                    )

                # Create reactives to listen to checkbox selections.  The
                # checkboxes are to show percentiles of some plots on the
                # Summary tab.  For r = getReactiveShowQuantiles(x), r is
                # the reactive for plot x.  r() returns a vector of all
                # checked boxes.  The indeces correspond to elements in
                # a call to summary(), so that if r() = 1,4,6, then this
                # means min, mean, and max.
                getReactiveShowQuantiles = function(tabId, rowIdx) {
                    # Create checkbox IDs of the format specified in
                    # tabs.R.
                    idPrefix = paste('checkbox', tabId, '_',
                                     rowIdx, '_', sep='')
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

                # Get a reactive that listens to select menu selections in
                # the score, prop vs. time plots.  These plots have a select
                # menu to change the data to be plotted.  The return value
                # of the reactives is the index of the selected item ordered
                # by increasing order of 1) genre names if tabId ==
                # TAB_ID_GENRES, 2) levels if tabId == TAB_ID_SOURCES or
                # TAB_ID_TYPES.
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

                # Create a reactive that listens to data changes and returns
                # a statistic for the global data.  The statistic is evaluated
                # using the years in the changed data.  The statistic is the
                # median, and the values are the global scores, views, or
                # episodes, depending on the value of 'rv'.
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

                getReactivePredictions = function() {
                    eventReactive(
                        input$predictionDateActionButton,
                        {
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
                                                     (globalDS$year == year) &
                                                         (globalDS$month %in% c(1, 2))
                                                     ) |
                                                (
                                                 (globalDS$year == (year - 1)) &
                                                     (globalDS$month == 12)
                                                 )
                                                )
                            }

                            return(indeces)
                        },
                    ignoreNULL = FALSE
                    )
                }

                # Create a renderPlot function.  This can be used as:
                #   output$x = createSummaryPlot(p, b, d).
                # brushId and dblClickId must be formatted according to the
                # specs in tabs.R.  plotId is the number of the plot; that is,
                # the y in plotx_y (see tabs.R).
                createSummaryPlot = function(plotId, brushId, dblClickId) {
                    # Create observers for double clicks and brushes if
                    # so desired.
                    if (!(is.null(dblClickId) || is.null(brushId))) {
                        ranges = getReactivePlotLimitsChange()
                        observeDblClick(dblClickId, ranges)
                        observeBrush(brushId, ranges)
                    }
                    if (plotId == 1) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('score')
                        reactiveShowQuantiles = getReactiveShowQuantiles(1, 1)

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = ranges$y
                            xlim = ranges$x
                            if (is.null(ylim)) {
                                ylim=c(min(curType$qscore_timeline, na.rm=TRUE),
                                       max(curType$qscore_timeline, na.rm=TRUE))
                            }

                            # Get the quantiles to show in the plot.
                            # This needs no reactive due to the if-block.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantiles()
                            if (length(wquantiles) > 0) {
                                timeline_mat =
                                    as.matrix(
                                              curType$qscore_timeline[,
                                                                      wquantiles
                                                                      ]
                                              )
                            }

                            score_vs_year(
                                          data$years,
                                          globalPerf, # median
                                          curType$qscore_timeline[, 3], # median
                                          title='Mean score throughout time',
                                          ylab='Mean score',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    } else if (plotId == 2) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('views')
                        reactiveShowQuantiles = getReactiveShowQuantiles(1, 2)

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = ranges$y
                            xlim = ranges$x
                            if (is.null(ylim)) {
                                ylim=c(min(curType$qview_timeline, na.rm=TRUE),
                                       max(curType$qview_timeline, na.rm=TRUE))
                            }

                            # Get the quantiles to show in the plot.
                            # This needs no reactive due to the if-block.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantiles()
                            if (length(wquantiles) > 0) {
                                timeline_mat =
                                    as.matrix(
                                              curType$qview_timeline[,
                                                                     wquantiles
                                                                     ]
                                              )
                            }

                            score_vs_year(
                                          data$years,
                                          globalPerf,
                                          curType$qview_timeline[, 3],
                                          title='Mean views throughout time',
                                          ylab='Mean views',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    }
                    else if (plotId == 3) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('eps')
                        reactiveShowQuantiles = getReactiveShowQuantiles(1, 3)

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = ranges$y
                            xlim = ranges$x
                            if (is.null(ylim)) {
                                ylim=c(min(curType$qeps_timeline, na.rm=TRUE),
                                       max(curType$qeps_timeline, na.rm=TRUE))
                            }

                            # Get the quantiles to show in the plot.
                            # This needs no reactive due to the if-block.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantiles()
                            if (length(wquantiles) > 0) {
                                timeline_mat =
                                    as.matrix(
                                              curType$qeps_timeline[, wquantiles]
                                              )
                            }

                            score_vs_year(
                                          data$years,
                                          globalPerf,
                                          curType$qeps_timeline[, 3],
                                          title='Mean episodes throughout time',
                                          ylab='Mean episodes',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    } else if (plotId == 4) {
                        renderPlot({
                            data = reactiveDataChange()
                            numtype_vs_year(
                                            data$years,
                                            t(data$types$prop_mat),
                                            data$types$class_names
                                            )
                        })
                    } else if (plotId == 5) {
                        renderPlot({
                            data = reactiveDataChange()
                            numtype_vs_year(
                                            data$years,
                                            t(data$sources$prop_mat),
                                            data$sources$class_names
                                            )
                        })
                    } else if (plotId == 6) {
                        renderPlot({
                            data = reactiveDataChange()
                            ylim = ranges$y
                            xlim = ranges$x
                            dominance_vs_year(
                                              data$years,
                                              data$overall$perf_mat,
                                              ylim=ylim,
                                              xlim=xlim
                                              )

                        })
                    } else if (plotId == 7) {
                        renderPlot({
                            data = reactiveDataChange()
                            ylim = ranges$y
                            xlim = ranges$x
                            score_vs_dominance(
                                               data$years,
                                               data$overall$perf_mat,
                                               ylim=ylim,
                                               xlim=xlim
                                               )
                        })
                    }
                }

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

                # Create a new reactiveValues object.  The values of this
                # reactiveValues are independent of the values of other
                # objects instantiated by this call.  By running:
                #   r = getReactivePlotLimitsChange()
                # r$x and r$y can be used as variables to listen to in a
                # render*() function.  If one piece of code modifies one of
                # these variables, then another piece of code that reads
                # it will be notified.  This is meant to be used to listen
                # to zoom in and out actions, where r$x is the new xlim
                # and r$y is the new ylim.
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

                # Create plot output objects for the categorical variables
                # tabs.  This function is meant to be used as:
                #
                #   output$x = createCatPlot(pid, tid)
                #
                # It returns a renderPlot() object that renders the given
                # plot for the given tab.
                #
                # plotId is the y part of plotx_y (see tabs.R).
                #
                # tabId is a TAB_ID_* constant (see constants.R) except
                # TAB_ID_SUMMARY.
                #
                # If the plot has support for brushing and double clicking,
                # then the IDs of those actions can be given with brushId
                # and dblClickId.  These must be formatted according to the
                # specs in tabs.R.
                createCatPlot = function(plotId, tabId,
                                         brushId, dblClickId) {
                    if (tabId == TAB_ID_GENRES) category = 'genres'
                    else if (tabId == TAB_ID_SOURCES) category = 'sources'
                    else if (tabId == TAB_ID_TYPES) category = 'types'
                    else if (tabId == TAB_ID_STUDIOS) category = 'studios'

                    if (!(is.null(dblClickId) || is.null(brushId))) {
                        ranges = getReactivePlotLimitsChange()
                        observeDblClick(dblClickId, ranges)
                        observeBrush(brushId, ranges)
                    }

                    if (plotId == 1) {
                        # Show props vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            w = order(curType$tot_props, decreasing=TRUE)
                            xlim = ranges$x
                            if (!is.null(xlim)) {
                                xlim = floor(xlim) + 1
                                # Lower bound of 1.
                                if (xlim[1] < 1) {
                                    xlim[1] = 1
                                }
                                # Upper bound of the number of classes.
                                if (xlim[2] > length(curType$tot_props)) {
                                    xlim[2] = length(curType$tot_props)
                                }
                                xlim = xlim[1] : xlim[2]
                                w = w[xlim]
                            }
                            gprop_vs_genre(
                                           curType$tot_props[w],
                                           curType$class_colors[w]
                                           #xlim=xlim
                                           )
                        })
                    } else if (plotId == 2) {
                        # Show score vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            w = order(curType$score_meds, decreasing=TRUE)
                            xlim = ranges$x
                            if (!is.null(xlim)) {
                                xlim = floor(xlim) + 1
                                # Lower bound of 1.
                                if (xlim[1] < 1) {
                                    xlim[1] = 1
                                }
                                # Upper bound of the number of classes.
                                if (xlim[2] > length(curType$score_meds)) {
                                    xlim[2] = length(curType$score_meds)
                                }
                                xlim = xlim[1] : xlim[2]
                                w = w[xlim]
                            }
                            mean_gscore_vs_genre(
                                                 curType$score_meds[w],
                                                 curType$class_colors[w],
                                                 globalMedScore
                                                 )
                        })
                    } else if (plotId == 3) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        reactiveClassChange = getReactiveClassChange(tabId)

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            classIdx = reactiveClassChange()
                            ylim = ranges$y
                            xlim = ranges$x
                            gprop_vs_year(
                                          data$years,
                                          curType$prop_mat[, classIdx],
                                          curType$score_mat[, classIdx],
                                          max(curType$prop_mat, na.rm=TRUE),
                                          range(curType$score_mat, na.rm=TRUE),
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    } else if (plotId == 4) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        renderPlot({
                            data = reactiveDataChange()
                            curType = data[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            gprop_palette = colorRampPalette(c('black',
                                                               'white'))(n=128)
                            image(
                                  x=data$years,
                                  y=1:length(curType$class_names),
                                  curType$prop_mat,
                                  col=gprop_palette
                                  )
                            axis(
                                 side=2,
                                 at=1:length(curType$class_names),
                                 labels=curType$class_names,
                                 las=2
                                 )
                        })
                    } else if (plotId == 5) {
                        # Show score vs. views for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = ranges$y
                            xlim = ranges$x
                            gscore_vs_gview(
                                            curType$view_meds,
                                            curType$score_meds,
                                            curType$class_colors,
                                            globalMedScore,
                                            globalMedViews,
                                            curType$class_names,
                                            ylim=ylim,
                                            xlim=xlim
                                            )
                        })
                    } else if (plotId == 6) {
                        # Show score vs. props for all categorical variables
                        # (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = ranges$y
                            xlim = ranges$x
                            gscore_vs_gprop(
                                            curType$tot_props,
                                            curType$score_meds,
                                            globalMedScore,
                                            curType$class_colors,
                                            curType$class_names,
                                            ylim=ylim,
                                            xlim=xlim
                                            )
                        })
                    } else if (plotId == 7) {
                        # Show score slope vs. prop slope for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = ranges$y
                            xlim = ranges$x
                            gscore_slope_vs_gprop_slope(
                                                        curType$prop_slopes,
                                                        curType$score_slopes,
                                                        curType$class_colors,
                                                        curType$class_names,
                                                        ylim=ylim,
                                                        xlim=xlim
                                                        )
                        })
                    }
                }

                # For the moment, this reactive is not necessary.
                #reactiveTabChange <- reactive({
                    #tabId = as.integer(input$tabpanelId)
                    #curType = NULL
                    ## This 'if' is probably not necessary.
                    #if (tabId == TAB_ID_SUMMARY) curType = data$overall
                    #else if (tabId == TAB_ID_GENRES) curType = data$genres
                    #else if (tabId == TAB_ID_SOURCES) curType = data$sources
                    #else if (tabId == TAB_ID_TYPES) curType = data$types
#
                    #retList = list()
                    #retList$curType = curType
                    #return(retList)
                #})

                # Create an HTML table to display all shows and their
                # predictions.  This uses renderUI instead of renderTable
                # because the rows need to be formatted according to
                # the predictions.  Each row needs to be in a specific
                # CSS class, and this is much simpler to do with renderUI.
                createPredictionsTable = function() {
                    getIndeces = getReactivePredictions()

                    renderUI({
                        indeces = getIndeces()
                        columns = which(
                                        names(globalDS) %in%
                                            c('name', 'score', 'studio',
                                              'predicted_correctly')
                                        )

                        table = globalDS[indeces, columns]
                        outcomes = table$predicted_correctly
                        scores = table$score
                        # Determine the predictions for each show.  There are
                        # four possibilities:
                        # outcome | score | prediction
                        #    1       < 7       0 (correctly predicted 0)
                        #    1       >= 7      1 (correctly predicted 1)
                        #    0       < 7       1 (incorrectly predicted 1)
                        #    0       >= 7      0 (incorrectly predicted 0)
                        yhats = ifelse(
                                       ((outcomes == 1) & !(scores < 7.0)) |
                                           ((outcomes == 0) & (scores < 7.0)),
                                       1,
                                       0
                                       )

                        # Build an HTML table.  The rows are shows.  If
                        # a show's score was correctly predicted, then
                        # it goes into the 'correct_row' CSS class.
                        # Otherwise, the row goes into the 'wrong_row'
                        # CSS class.
                        rows = list()
                        for (rowIdx in 1 : nrow(table)) {
                            cells = list()

                            # The row cells are the columns of 'table'
                            # except the 'predicted_correctly' column,
                            # which is used to determine the CSS class
                            # that the row belongs to.  The last cell
                            # after this loop is the prediction made.
                            # by the classifier.
                            for (colIdx in 1 : (ncol(table) - 1)) {
                                cell = tags$td(table[rowIdx, colIdx])
                                cells[[length(cells) + 1]] = cell
                            }
                            cell = tags$td(yhats[rowIdx])
                            cells[[length(cells) + 1]] = cell

                            # Determine the row's class.
                            row = NULL
                            if (table[rowIdx, ncol(table)] == 1) {
                                row = tags$tr(class='correct_row', cells)
                            }
                            else {
                                row = tags$tr(class='wrong_row', cells)
                            }
                            rows[[length(rows) + 1]] = row
                        }

                        # Get the total number of shows and the number of
                        # shows predicted correctly and incorrectly.  These
                        # numbers are shown to the user.
                        nwrong = length(which(outcomes == 0))
                        nrows = length(rows)
                        res1 = paste('Number of shows:', nrows)
                        res2 = paste(
                                     'Number predicted correctly:',
                                     length(which(outcomes == 1))
                                     )
                        res3 = paste('Number predicted incorrectly:', nwrong)
                        res4 = paste(
                                     'Total error: ',
                                     round(100.0 * nwrong / nrows, 2), '%',
                                     sep=''
                                     )
                        header = tags$tr(
                                         tags$th('Name'),
                                         tags$th('Studio'),
                                         tags$th('Actual score'),
                                         tags$th('Prediction')
                                         )
                        htmlTable = tags$table(
                                               class='predictions_table',
                                               tags$thead(header),
                                               tags$tbody(rows)
                                               )
                        element = tags$div(
                                           res1,
                                           tags$br(),
                                           res2,
                                           tags$br(),
                                           res3,
                                           tags$br(),
                                           res4,
                                           tags$br(),
                                           tags$br(),
                                           htmlTable
                                           )

                        return(element)
                    })
                }

                # Create a table that shows the classifier performance
                # for each studio.
                createStudioPredictionsTable = function() {

                    renderUI({
                        rows = list()
                        for (idx in perfSortedIndeces) {
                            # This creates the cells: one for the studio
                            # name, one for the number of its shows predicted
                            # correctly, one for the total number of shows it
                            # made, and another for the ratio of the previous
                            # two.
                            studioNameCell = tags$td(studioNames[idx])
                            studioAccCell = tags$td(studioAcc[idx])
                            studioCountCell = tags$td(studioCount[idx])
                            studioPerfCell = tags$td(studioPerf[idx])

                            row = tags$tr(
                                          studioNameCell,
                                          studioAccCell,
                                          studioCountCell,
                                          studioPerfCell
                                          )
                            rows[[length(rows) + 1]] = row
                        }

                        header = tags$tr(
                                         tags$th('Studio'),
                                         tags$th('Shows predicted correctly'),
                                         tags$th('Total shows animated'),
                                         tags$th('Proportion of shows predicted correctly')
                                         )
                        htmlTable = tags$table(
                                               class='studio_acc_table',
                                               tags$thead(header),
                                               tags$tbody(rows)
                                               )

                        return(htmlTable)
                    })
                }


                # Create plots for the summary tab.
                plotSuffix = paste('plot', 1, '_', sep='')
                brushSuffix = paste('brush', 1, '_', sep='')
                dblClkSuffix = paste('dblclick', 1, '_', sep='')
                sapply(1 : 7,
                       function(plotId) {
                           pid = paste(plotSuffix, plotId, sep='')

                           # These plots don't listen to double clicks
                           # or brushes.
                           if ((plotId == 4) || (plotId == 5)) {
                               output[[pid]] =
                                   createSummaryPlot(plotId, NULL, NULL)
                           }
                           # These plots listen to double clicks and
                           # brushes.
                           else {
                               bid = paste(brushSuffix, plotId, sep='')
                               dcid = paste(dblClkSuffix, plotId, sep='')
                               output[[pid]] =
                                   createSummaryPlot(plotId, bid, dcid)
                           }
                       })

                # Create plots for the other tabs.
                sapply(2 : 5,
                       function(tabId) {
                           plotSuffix = paste('plot', tabId, '_', sep='')
                           brushSuffix = paste('brush', tabId, '_', sep='')
                           dblClkSuffix = paste('dblclick', tabId, '_', sep='')
                           sapply(1 : 7,
                                  function(plotId) {
                                      pid = paste(plotSuffix, plotId, sep='')
                                      if (plotId == 4) {
                                          output[[pid]] =
                                              createCatPlot(plotId, tabId,
                                                            NULL, NULL)
                                      }
                                      else {
                                          bid = paste(
                                                      brushSuffix,
                                                      plotId,
                                                      sep=''
                                                      )
                                          dcid = paste(
                                                       dblClkSuffix,
                                                       plotId,
                                                       sep=''
                                                       )
                                          output[[pid]] =
                                              createCatPlot(
                                                            plotId,
                                                            tabId,
                                                            bid,
                                                            dcid
                                                            )
                                      }
                                  })
                       })
                output[['table_predictions']] = createPredictionsTable()
                output[['table_studio_acc']] = createStudioPredictionsTable()

                # These are output elements for the images in the 'Performance'
                # tab of the 'Predictions' page.
                output[['acc_vs_time']] = renderImage(deleteFile=FALSE, {
                    list(src='www/acc_vs_time.png')
                })
                output[['tp_vs_tn']] = renderImage(deleteFile=FALSE, {
                    list(src='www/tp_vs_tn.png')
                })
                output[['fp_vs_stucnt']] = renderImage(deleteFile=FALSE, {
                    list(src='www/fp_vs_stucnt.png')
                })
                output[['fn_vs_stucnt']] = renderImage(deleteFile=FALSE, {
                    list(src='www/fn_vs_stucnt.png')
                })

                rm(plotSuffix)
                rm(brushSuffix)
                rm(dblClkSuffix)
            }
)
