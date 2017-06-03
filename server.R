library(shiny)
library(lattice)
source('main5.R')
source('constants.R')

# Initialization of data common to all users.
# Executed once throughout lifetime.

A = init_anime()
globalNames = A$name
globalDS = A[, -1]
rm(A)
sapply(6 : 44,
       function(idx)
           globalDS[, idx] <<- as.integer(globalDS[, idx]) - 1)
# Not the current year
defaultYears = (max(globalDS$year) - 10) : (max(globalDS$year) - 1)
globalData = init_data(globalDS, globalDS, NULL)
globalMedScore = median(globalDS$score)
globalMedViews = median(globalDS$tot_watched)
globalTimeline = list()
globalTimeline$scores = lapply(globalData$years,
                               get_quantiles_tmp,
                               na.omit(globalDS$score),
                               globalDS$year)
globalTimeline$views = lapply(globalData$years,
                               get_quantiles_tmp,
                               na.omit(globalDS$tot_watched),
                               globalDS$year)
globalTimeline$eps = lapply(globalData$years,
                               get_quantiles_tmp,
                               na.omit(globalDS$tot_eps),
                               globalDS$year)
globalTimeline$scores = do.call(rbind, globalTimeline$scores)[, 3]
globalTimeline$views = do.call(rbind, globalTimeline$views)[, 3]
globalTimeline$eps = do.call(rbind, globalTimeline$eps)[, 3]

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

                # Create plot output objects for the categorical variables
                # tabs.  This function is meant to be used as:
                #
                #   output$x = createCatPlot(pid, tid)
                #
                # It returns a renderPlot() object that renders the given
                # plot for the given tab.
                createCatPlot = function(plotId, tabId) {
                    if (tabId == TAB_ID_GENRES) curType = data$genres
                    else if (tabId == TAB_ID_SOURCES) curType = data$sources
                    else if (tabId == TAB_ID_TYPES) curType = data$types

                    if (plotId == 1) {
                        # Show props vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).
                        renderPlot({
                            w = order(curType$tot_props, decreasing=TRUE)
                            gprop_vs_genre(curType$tot_props[w],
                                           curType$class_colors[w])
                        })
                    } else if (plotId == 2) {
                        # Show score vs. class barplot for all categorical
                        # variables (genres, source, type, and studio).
                        renderPlot({
                            w = order(curType$score_meds, decreasing=TRUE)
                            mean_gscore_vs_genre(curType$score_meds[w],
                                                 curType$class_colors[w],
                                                 globalMedScore)
                        })
                    } else if (plotId == 3) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        renderPlot({
                            gprop_vs_year(
                                          data$years,
                                          curType$prop_mat[, 1],
                                          curType$score_mat[, 1],
                                          max(curType$prop_mat, na.rm=TRUE),
                                          range(curType$score_mat, na.rm=TRUE)
                                          )
                        })
                    } else if (plotId == 4) {
                        # Show score,prop vs. year for all categorical variables
                        # (genres, source, type, and studio).
                        renderPlot({
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
                        renderPlot({
                            # Show score vs. views for all categorical
                            # variables (genres, source, type, and studio).
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

                # Create plots for the Summary tab.
                output[[paste('plot', 1, '_', 1, sep='')]] = scoreVsYear
                output[[paste('plot', 1, '_', 2, sep='')]] = viewsVsYear
                output[[paste('plot', 1, '_', 3, sep='')]] = epsVsYear
                output[[paste('plot', 1, '_', 4, sep='')]] = typePropVsYear
                output[[paste('plot', 1, '_', 5, sep='')]] = sourcePropVsYear

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
