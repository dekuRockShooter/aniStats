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
                data = globalData

                # The render* functions are executed everytime a widget
                # that it is observing changes.  These widgets are stored
                # in input$x.  If a function does not use the input list,
                # then it is executed only once. 

                scoreVsYear <- renderPlot({
                    score_vs_year(data$years,
                                  globalTimeline$scores,
                                  title='Mean score throughout time',
                                  ylab='Mean score',
                                  timeline_mat=data$overall$qscore_timeline,
                                  main_col=3,
                                  ylim=c(min(data$overall$qscore_timeline, na.rm=TRUE),
                                         max(data$overall$qscore_timeline, na.rm=TRUE))
                                  )
                })

                viewsVsYear <- renderPlot({
                    score_vs_year(data$years,
                                  globalTimeline$scores,
                                  title='Mean views throughout time',
                                  ylab='Mean views',
                                  timeline_mat=data$overall$qview_timeline,
                                  main_col=3,
                                  ylim=c(min(data$overall$qview_timeline, na.rm=TRUE),
                                         max(data$overall$qview_timeline, na.rm=TRUE))
                                  )
                })

                epsVsYear <- renderPlot({
                    score_vs_year(data$years,
                                  globalTimeline$scores,
                                  title='Mean episodes throughout time',
                                  ylab='Mean episodes',
                                  timeline_mat=data$overall$qeps_timeline,
                                  main_col=3,
                                  ylim=c(min(data$overall$qeps_timeline, na.rm=TRUE),
                                         max(data$overall$qeps_timeline, na.rm=TRUE))
                                  )
                })

                typePropVsYear <- renderPlot({
                    numtype_vs_year(data$years,
                                    t(data$types$prop_mat),
                                    data$types$class_names)
                })

                sourcePropVsYear <- renderPlot({
                    numtype_vs_year(data$years,
                                    t(data$sources$prop_mat),
                                    data$sources$class_names)
                })

                reactiveDataChange = eventReactive(
                    input$changeDataActionButton,
                    {
                        studio = input$studioSelectId
                        if (studio == 'All') {
                            data <<- init_data(globalDS, globalDS, NULL)
                        }
                        else {
                            studioDS = globalDS[globalDS$studio == studio, ]
                            data <<- init_data(studioDS, globalDS, studio)
                        }
                        data
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
                getReactiveShowQuantiles = function(plotId) {
                    qnames = c('Min', 'Q25', 'Mean', 'Q75', 'Max')
                    nameSuffix = 'Checkbox'
                    checkboxIds = NULL
                    #if (plotId == 1) {
                    namePrefix = paste('plot1_', plotId, sep='')
                    checkboxIds = sapply(qnames,
                                         function(q) {
                                             paste(namePrefix,
                                                   q,
                                                   nameSuffix,
                                                   sep='')
                                         })
                    reactive({
                        wquantile = integer(0)
                        if (input[[checkboxIds[1]]] == TRUE) {
                            wquantile = c(wquantile, 1)
                        }
                        if (input[[checkboxIds[2]]] == TRUE) {
                            wquantile = c(wquantile, 2)
                        }
                        if (input[[checkboxIds[3]]] == TRUE) {
                            wquantile = c(wquantile, 4)
                        }
                        if (input[[checkboxIds[4]]] == TRUE) {
                            wquantile = c(wquantile, 5)
                        }
                        if (input[[checkboxIds[5]]] == TRUE) {
                            wquantile = c(wquantile, 6)
                        }
                        return(wquantile)
                    })
                }

                reactiveShowQuantilesPlot1_1 = getReactiveShowQuantiles(1)
                reactiveShowQuantilesPlot1_2 = getReactiveShowQuantiles(2)
                reactiveShowQuantilesPlot1_3 = getReactiveShowQuantiles(3)

                # Get a reactive that listens to select menu selections in
                # the score, prop vs. time plots.  These plots have a select
                # menu to change the data to be plotted.  The return value
                # of the reactives is the index of the selected item ordered
                # by increasing order of 1) genre names if tabId ==
                # TAB_ID_GENRES, 2) levels if tabId == TAB_ID_SOURCES or
                # TAB_ID_TYPES.
                getReactiveClassChange = function(tabId) {
                    # Each reactive listens to changes on a selection menu
                    # for a specific tab.
                    if (tabId == TAB_ID_GENRES) {
                        reactive({
                            class = input$plot2_3SelectId
                            classes = sort(names(globalDS)[6 : 44],
                                           decreasing=FALSE)
                            idx = which(classes == class)

                            return(idx)
                        })
                    } else if (tabId == TAB_ID_SOURCES) {
                        reactive({
                            class = input$plot3_3SelectId
                            classes = sort(levels(globalDS$source),
                                           decreasing=FALSE)
                            idx = which(classes == class)

                            return(idx)
                        })
                    } else if (tabId == TAB_ID_TYPES) {
                        reactive({
                            class = input$plot4_3SelectId
                            classes = sort(levels(globalDS$type),
                                           decreasing=FALSE)
                            idx = which(classes == class)

                            return(idx)
                        })
                    }
                }

                createSummaryPlot = function(plotId) {
                    if (plotId == 1) {
                        renderPlot({
                            data = reactiveDataChange()
                            # TODO: this block needs to be in a reactive.
                            curType = data$overall
                            ylim=c(min(curType$qscore_timeline, na.rm=TRUE),
                                   max(curType$qscore_timeline, na.rm=TRUE))
                            globalPerf = lapply(
                                                data$years,
                                                get_quantiles_tmp,
                                                na.omit(globalDS$score),
                                                globalDS$year
                                                )
                            globalPerf = do.call(rbind, globalPerf)[, 3]

                            # Get the quantiles to show in the plot.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantilesPlot1_1()
                            if (length(wquantiles) > 0) {
                                timeline_mat =
                                    as.matrix(
                                              curType$qscore_timeline[, wquantiles]
                                              )
                            }

                            score_vs_year(
                                          data$years,
                                          globalPerf, # median
                                          curType$qscore_timeline[, 3], # median
                                          title='Mean score throughout time',
                                          ylab='Mean score',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim
                                          )
                        })
                    } else if (plotId == 2) {
                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            ylim=c(min(curType$qview_timeline, na.rm=TRUE),
                                   max(curType$qview_timeline, na.rm=TRUE))
                            globalPerf = lapply(
                                                data$years,
                                                get_quantiles_tmp,
                                                na.omit(globalDS$tot_watched),
                                                globalDS$year
                                                )
                            globalPerf = do.call(rbind, globalPerf)[, 3]

                            # Get the quantiles to show in the plot.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantilesPlot1_2()
                            if (length(wquantiles) > 0) {
                                timeline_mat =
                                    as.matrix(
                                              curType$qview_timeline[, wquantiles]
                                              )
                            }

                            score_vs_year(
                                          data$years,
                                          globalPerf,
                                          curType$qview_timeline[, 3],
                                          title='Mean views throughout time',
                                          ylab='Mean views',
                                          timeline_mat=timeline_mat,
                                          ylim=ylim
                                          )
                        })
                    }
                    else if (plotId == 3) {
                        renderPlot({
                            data = reactiveDataChange()
                            curType = data$overall
                            ylim=c(min(curType$qeps_timeline, na.rm=TRUE),
                                   max(curType$qeps_timeline, na.rm=TRUE))
                            globalPerf = lapply(
                                                data$years,
                                                get_quantiles_tmp,
                                                na.omit(globalDS$tot_eps),
                                                globalDS$year
                                                )
                            globalPerf = do.call(rbind, globalPerf)[, 3]

                            # Get the quantiles to show in the plot.
                            timeline_mat = NULL
                            wquantiles = reactiveShowQuantilesPlot1_3()
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
                                          ylim=ylim
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
                            dominance_vs_year(
                                              data$years,
                                              data$overall$perf_mat
                                              )

                        })
                    } else if (plotId == 7) {
                        renderPlot({
                            data = reactiveDataChange()
                            score_vs_dominance(
                                               data$years,
                                               data$overall$perf_mat
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
                createCatPlot = function(plotId, tabId) {
                    if (tabId == TAB_ID_GENRES) category = 'genres'
                    else if (tabId == TAB_ID_SOURCES) category = 'sources'
                    else if (tabId == TAB_ID_TYPES) category = 'types'

                    if (plotId == 1) {
                        # Show props vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).
                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            w = order(curType$tot_props, decreasing=TRUE)
                            gprop_vs_genre(curType$tot_props[w],
                                           curType$class_colors[w])
                        })
                    } else if (plotId == 2) {
                        # Show score vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).
                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            w = order(curType$score_meds, decreasing=TRUE)
                            mean_gscore_vs_genre(curType$score_meds[w],
                                                 curType$class_colors[w],
                                                 globalMedScore)
                        })
                    } else if (plotId == 3) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        reactiveClassChange = getReactiveClassChange(tabId)

                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            classIdx = reactiveClassChange()
                            gprop_vs_year(
                                          data$years,
                                          curType$prop_mat[, classIdx],
                                          curType$score_mat[, classIdx],
                                          max(curType$prop_mat, na.rm=TRUE),
                                          range(curType$score_mat, na.rm=TRUE)
                                          )
                        })
                    } else if (plotId == 4) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        renderPlot({
                            curType = reactiveDataChange()[[category]]
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
                            gscore_vs_gview(
                                            curType$view_meds,
                                            curType$score_meds,
                                            curType$class_colors,
                                            globalMedScore,
                                            globalMedViews,
                                            curType$class_names
                                            )
                        })
                    } else if (plotId == 6) {
                        # Show score vs. props for all categorical variables
                        # (genres, source, type, and studio).
                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            gscore_vs_gprop(
                                            curType$tot_props,
                                            curType$score_meds,
                                            globalMedScore,
                                            curType$class_colors,
                                            curType$class_names
                                            )
                        })
                    } else if (plotId == 7) {
                        # Show score slope vs. prop slope for all categorical
                        # variables (genres, source, type, and studio).
                        renderPlot({
                            curType = reactiveDataChange()[[category]]
                            gscore_slope_vs_gprop_slope(
                                                        curType$prop_slopes,
                                                        curType$score_slopes,
                                                        curType$class_colors,
                                                        curType$class_names
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


                output$testView <- renderPlot({
                    plot(1, 1)
                })

                # Create plots for the summary tab.
                outputSuffix = paste('plot', 1, '_', sep='')
                sapply(1 : 7,
                       function(plotId) {
                           outputName = paste(
                                              outputSuffix,
                                              plotId,
                                              sep=''
                                              )
                           output[[outputName]] =
                           createSummaryPlot(plotId)
                       })

                # Create plots for the other tabs.
                sapply(2 : 4,
                       function(tabId) {
                           outputSuffix = paste('plot', tabId, '_', sep='')
                           sapply(1 : 7,
                                  function(plotId) {
                                      outputName = paste(
                                                         outputSuffix,
                                                         plotId,
                                                         sep=''
                                                         )
                                      output[[outputName]] =
                                          createCatPlot(plotId, tabId)
                                  })
                       })
            }
)
