library(shiny)
library(lattice)
source('main5.R')
source('constants.R')

# Initialization of data common to all users.
# Executed once throughout lifetime.


shinyServer(
            function(input, output) {
                # This allows the reactives in reactives.R to use 'input'.
                # This may need to rewritten as source(...)$value, but
                # no issues have popped up yet (https://stackoverflow.com/questions/30534674/displaying-true-when-shiny-files-are-split-into-different-folders).
                source('reactives.R', local=TRUE)

                # This is the current data being displayed.  It is the
                # exact same object that is returned by reactiveDataChange().
                # Its purpose is to provide a backup for those cases when
                # the new data set is empty.  When this happens, 'locData'
                # can be used to keep displaying the data instead of showing
                # nothing or unexpectadly changing the data to 'GLOBAL_DATA'.
                locData = GLOBAL_DATA

                # Initialization of data specific to the current user.
                # Executed once for every user.
                #dataset = D[(D$studio == cur_studio), ]

                # The render* functions are executed everytime a widget
                # that it is observing changes.  These widgets are stored
                # in input$x.  If a function does not use the input list,
                # then it is executed only once.


                # Create a renderPlot function.  This can be used as:
                #   output$x = createSummaryPlot(p, b, d).
                # brushId and dblClickId must be formatted according to the
                # specs in tabs.R.  plotId is the number of the plot; that is,
                # the y in plotx_y (see tabs.R).
                createSummaryPlot = function(plotId, brushId, dblClickId) {
                    # Create observers for double clicks and brushes if
                    # so desired.
                    if (!(is.null(dblClickId) || is.null(brushId))) {
                        reactiveRanges = getReactiveZoom(brushId, dblClickId)
                    }
                    if (plotId == plotsEnum$SCORE_VS_TIME) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('score')
                        reactiveShowQuantiles =
                            getReactiveShowQuantiles(
                                tabsEnum$SUMMARY,
                                plotId
                                )

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
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
                                          title='Median score throughout time',
                                          ylab='Median score',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    } else if (plotId == plotsEnum$VIEWS_VS_TIME) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('views')
                        reactiveShowQuantiles =
                            getReactiveShowQuantiles(
                                tabsEnum$SUMMARY,
                                plotId
                                )

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
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
                                          title='Median views throughout time',
                                          ylab='Median views',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    }
                    else if (plotId == plotsEnum$EPS_VS_TIME) {
                        reactiveGlobalPerf = getReactiveGlobalPerf('eps')
                        reactiveShowQuantiles =
                            getReactiveShowQuantiles(
                                tabsEnum$SUMMARY,
                                plotId
                                )

                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            globalPerf = reactiveGlobalPerf()

                            # This needs no reactive due to the if-block.
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
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
                                          title='Median episodes throughout time',
                                          ylab='Median episodes',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim,
                                          xlim=xlim
                                          )
                        })
                    } else if (plotId == plotsEnum$TYPEPROP_VS_TIME) {
                        renderPlot({
                            data = reactiveDataChange()
                            numtype_vs_year(
                                            data$years,
                                            t(data$types$prop_mat),
                                            data$types$class_names
                                            )
                        })
                    } else if (plotId == plotsEnum$SOURCEPROP_VS_TIME) {
                        renderPlot({
                            data = reactiveDataChange()
                            numtype_vs_year(
                                            data$years,
                                            t(data$sources$prop_mat),
                                            data$sources$class_names
                                            )
                        })
                    } else if (plotId == plotsEnum$PERFORMANCE_VS_TIME) {
                        renderPlot({
                            data = reactiveDataChange()
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
                            dominance_vs_year(
                                              data$years,
                                              data$overall$perf_mat,
                                              ylim=ylim,
                                              xlim=xlim
                                              )

                        })
                    } else if (plotId == plotsEnum$SCORE_PERC_VS_SHOW_PERC) {
                        renderPlot({
                            data = reactiveDataChange()
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
                            score_vs_dominance(
                                               data$years,
                                               data$overall$perf_mat,
                                               ylim=ylim,
                                               xlim=xlim
                                               )
                        })
                    }
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
                # tabId is a value of tabsEnum, except tabsEnum$SUMMARY.
                #
                # If the plot has support for brushing and double clicking,
                # then the IDs of those actions can be given with brushId
                # and dblClickId.  These must be formatted according to the
                # specs in tabs.R.
                createCatPlot = function(plotId, tabId,
                                         brushId, dblClickId) {
                    if (tabId == tabsEnum$GENRES)
                        category = 'genres'
                    else if (tabId == tabsEnum$SOURCES)
                        category = 'sources'
                    else if (tabId == tabsEnum$TYPES)
                        category = 'types'
                    else if (tabId == tabsEnum$STUDIOS)
                        category = 'studios'

                    if (!(is.null(dblClickId) || is.null(brushId))) {
                        reactiveRanges = getReactiveZoom(brushId, dblClickId)
                    }

                    if (plotId == plotsEnum$PROP_BARCHART) {
                        # Show props vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            w = order(curType$tot_props, decreasing=TRUE)
                            xlim = reactiveRanges$x
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
                    } else if (plotId == plotsEnum$SCORE_VS_CATEGORY) {
                        # Show score vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            w = order(curType$score_meds, decreasing=TRUE)
                            xlim = reactiveRanges$x
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
                                                 GLOBAL_MED_SCORES
                                                 )
                        })
                    } else if (plotId == plotsEnum$CATEGORY_PERF_VS_TIME) {
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
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
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
                    } else if (plotId == plotsEnum$CATEGORY_PROP_HEATMAP) {
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
                    } else if (plotId == plotsEnum$CATEGORY_SCORE_VS_VIEWS) {
                        # Show score vs. views for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
                            gscore_vs_gview(
                                            curType$view_meds,
                                            curType$score_meds,
                                            curType$class_colors,
                                            GLOBAL_MED_SCORES,
                                            GLOBAL_MED_VIEWS,
                                            curType$class_names,
                                            ylim=ylim,
                                            xlim=xlim
                                            )
                        })
                    } else if (plotId == plotsEnum$CATEGORY_SCORE_VS_PROP) {
                        # Show score vs. props for all categorical variables
                        # (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
                            gscore_vs_gprop(
                                            curType$tot_props,
                                            curType$score_meds,
                                            GLOBAL_MED_SCORES,
                                            curType$class_colors,
                                            curType$class_names,
                                            ylim=ylim,
                                            xlim=xlim
                                            )
                        })
                    } else if (plotId ==
                               plotsEnum$CATEGORY_SCORE_SLOPE_VS_PROP_SLOPE) {
                        # Show score slope vs. prop slope for all categorical
                        # variables (genres, source, type, and studio).

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            if (is.null(curType)) {
                                return()
                            }
                            ylim = reactiveRanges$y
                            xlim = reactiveRanges$x
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
                                        names(GLOBAL_DS) %in%
                                            c('name', 'score', 'studio',
                                              'yhats', 'predicted_correctly')
                                        )

                        table = GLOBAL_DS[indeces, columns]
                        outcomes = table$predicted_correctly

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
                            for (colIdx in 1 : (ncol(table) - 2)) {
                                cell = tags$td(table[rowIdx, colIdx])
                                cells[[length(cells) + 1]] = cell
                            }
                            cell = tags$td(table$yhats[rowIdx])
                            cells[[length(cells) + 1]] = cell

                            # Determine the row's class.
                            row = NULL
                            if (table[rowIdx, ncol(table) - 1] == 1) {
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
                        for (idx in GLOBAL_PERF_SORTED_INDECES) {
                            # This creates the cells: one for the studio
                            # name, one for the number of its shows predicted
                            # correctly, one for the total number of shows it
                            # made, and another for the ratio of the previous
                            # two.
                            studioNameCell = tags$td(GLOBAL_STUDIO_NAMES[idx])
                            studioAccCell = tags$td(GLOBAL_STUDIO_ACC[idx])
                            studioCountCell = tags$td(GLOBAL_STUDIO_COUNT[idx])
                            studioPerfCell = tags$td(GLOBAL_STUDIO_PERF[idx])

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
                plotSuffix = paste('plot', tabsEnum$SUMMARY, '_', sep='')
                brushSuffix = paste('brush', tabsEnum$SUMMARY, '_', sep='')
                dblClkSuffix = paste('dblclick', tabsEnum$SUMMARY, '_', sep='')
                plots = c(
                          plotsEnum$SCORE_VS_TIME,
                          plotsEnum$VIEWS_VS_TIME,
                          plotsEnum$EPS_VS_TIME,
                          plotsEnum$TYPEPROP_VS_TIME,
                          plotsEnum$SOURCEPROP_VS_TIME,
                          plotsEnum$PERFORMANCE_VS_TIME,
                          plotsEnum$SCORE_PERC_VS_SHOW_PERC
                          )
                sapply(plots,
                       function(plotEnum) {
                           pid = paste(plotSuffix, plotEnum, sep='')

                           # These plots don't listen to double clicks
                           # or brushes.
                           if ((plotEnum == plotsEnum$TYPEPROP_VS_TIME) ||
                               (plotEnum == plotsEnum$SOURCEPROP_VS_TIME)) {
                               output[[pid]] =
                                   createSummaryPlot(plotEnum, NULL, NULL)
                           }
                           # These plots listen to double clicks and
                           # brushes.
                           else {
                               bid = paste(brushSuffix, plotEnum, sep='')
                               dcid = paste(dblClkSuffix, plotEnum, sep='')
                               output[[pid]] =
                                   createSummaryPlot(plotEnum, bid, dcid)
                           }
                       })

                # Create plots for the other tabs.
                plots = c(
                          plotsEnum$PROP_BARCHART,
                          plotsEnum$SCORE_VS_CATEGORY,
                          plotsEnum$CATEGORY_PERF_VS_TIME,
                          plotsEnum$CATEGORY_PROP_HEATMAP,
                          plotsEnum$CATEGORY_SCORE_VS_VIEWS,
                          plotsEnum$CATEGORY_SCORE_VS_PROP,
                          plotsEnum$CATEGORY_SCORE_SLOPE_VS_PROP_SLOPE
                          )
                tabs = c(
                         tabsEnum$GENRES,
                         tabsEnum$TYPES,
                         tabsEnum$SOURCES,
                         tabsEnum$STUDIOS
                         )
                sapply(tabs,
                       function(tabEnum) {
                           plotSuffix = paste('plot', tabEnum, '_', sep='')
                           brushSuffix = paste('brush', tabEnum, '_', sep='')
                           dblClkSuffix = paste('dblclick', tabEnum, '_', sep='')
                           sapply(plots,
                                  function(plotEnum) {
                                      pid = paste(plotSuffix, plotEnum, sep='')
                                      if (plotEnum ==
                                          plotsEnum$CATEGORY_PROP_HEATMAP) {
                                          output[[pid]] =
                                              createCatPlot(plotEnum, tabEnum,
                                                            NULL, NULL)
                                      }
                                      else {
                                          bid = paste(
                                                      brushSuffix,
                                                      plotEnum,
                                                      sep=''
                                                      )
                                          dcid = paste(
                                                       dblClkSuffix,
                                                       plotEnum,
                                                       sep=''
                                                       )
                                          output[[pid]] =
                                              createCatPlot(
                                                            plotEnum,
                                                            tabEnum,
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

                # This UI output displays a warning message when as error
                # occurs.  It listens to data set changes (reactiveDataChange)
                # and displays a message depending on the badQry field.
                output[['warning_space']] = renderUI({
                    data = reactiveDataChange()
                    msg = NULL

                    if (data$badQry == TRUE) {
                        msg = tags$p(
                                     id='warningText',
                                     'Warning: No results found.  The data has not changed!'
                                     )
                    }
                    else {
                        msg = tags$p()
                    }

                    return(msg)
                })

                rm(plotSuffix)
                rm(brushSuffix)
                rm(dblClkSuffix)
                rm(plots)
                rm(tabs)
            }
)
