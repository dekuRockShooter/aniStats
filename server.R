library(shiny)
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

                # Reactive expression to generate the requested distribution.
                # This is called whenever the inputs change. The output
                # functions defined below then all use the value computed from
                # this expression
                #data <- reactive({
                    #dist <- switch(input$dist,
                                   #norm = rnorm,
                                   #unif = runif,
                                   #lnorm = rlnorm,
                                   #exp = rexp,
                                   #rnorm)
#
                    #dist(input$n)
                #})

                # The render* functions are executed everytime a widget
                # that it is observing changes.  These widgets are stored
                # in input$x.  If a function does not use the input list,
                # then it is executed only once. 

                output$scoreVsYear <- renderPlot({
                    score_vs_year(data$years,
                                  globalTimeline$scores,
                                  title='Mean score throughout time',
                                  ylab='Mean score',
                                  timeline_mat=data$overall$qscore_timeline,
                                  main_col=3,
                                  ylim=c(min(data$overall$qscore_timeline, na.rm=TRUE),
                                         max(data$overall$qscore_timeline, na.rm=TRUE))
                                  )

                    #dist <- input$dist
                    #n <- input$n

                    #hist(data(), 
                         #main=paste(input$tabpanelId, '(', n, ')', sep=''))
                })

                output$viewsVsYear <- renderPlot({
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

                output$epsVsYear <- renderPlot({
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

                output$typePropVsYear <- renderPlot({
                    numtype_vs_year(data$years,
                                    t(data$types$prop_mat),
                                    data$types$class_names)
                })

                output$sourcePropVsYear <- renderPlot({
                    numtype_vs_year(data$years,
                                    t(data$sources$prop_mat),
                                    data$sources$class_names)
                })

                # Generate a plot of the data. Also uses the inputs to build
                # the plot label. Note that the dependencies on both the inputs
                # and the data reactive expression are both tracked, and
                # all expressions are called in the sequence implied by the
                # dependency graph
                output$summaryView <- renderPlot({
                    #dist <- input$dist
                    #n <- input$n
#
                    #hist(data(), 
                         #main=paste(input$tabpanelId, '(', n, ')', sep=''))
                    plot(1, 1)
                })

                output$barplot <- renderPrint({
                    #summary(data())
                    input$tabpanelId
                })

                output$genresView <- renderPrint({
                    #summary(data())
                    input$tabpanelId
                })

                output$genresView <- renderPrint({
                    #summary(data())
                    input$tabpanelId
                })

                output$sourcesView <- renderPrint({
                    #summary(data())
                    input$tabpanelId
                })

                # Show props vs. class barplot for all categorical
                # variables (genres, source, type, and studio).
                output$propsVsYear <- renderPlot({
                    curType = NULL
                    tabId = input$tabpanelId
                    # This 'if' is probably not necessary.
                    #if (tabId == TAB_ID_SUMMARY) curType = data$overall
                    if (tabId == TAB_ID_GENRES) curType = data$genres
                    else if (tabId == TAB_ID_SOURCES) curType = data$sources
                    else if (tabId == TAB_ID_TYPES) curType = data$types

                    w = order(curType$tot_props, decreasing=TRUE)
                    gprop_vs_genre(curType$tot_props[w],
                                   curType$class_colors[w])
                })

                # Show score vs. class barplot for all categorical
                # variables (genres, source, type, and studio).
                output$scoreVsClass <- renderPlot({
                    curType = NULL
                    tabId = input$tabpanelId
                    if (tabId == TAB_ID_GENRES) curType = data$genres
                    else if (tabId == TAB_ID_SOURCES) curType = data$sources
                    else if (tabId == TAB_ID_TYPES) curType = data$types

                    w = order(curType$score_meds, decreasing=TRUE)
                    mean_gscore_vs_genre(curType$score_meds[w],
                                         curType$class_colors[w],
                                         globalMedScore)
                })

                # Show score,prop vs. year for all categorical variables
                # (genres, source, type, and studio).
                output$scorePropVsYear <- renderPlot({
                    curType = NULL
                    tabId = input$tabpanelId
                    if (tabId == TAB_ID_GENRES) curType = data$genres
                    else if (tabId == TAB_ID_SOURCES) curType = data$sources
                    else if (tabId == TAB_ID_TYPES) curType = data$types

                    gprop_vs_year(
                                  data$years,
                                  curType$prop_mat[, 1],
                                  curType$score_mat[, 1],
                                  max(curType$prop_mat, na.rm=TRUE),
                                  range(curType$score_mat, na.rm=TRUE)
                                  )
                })

                output$typesView <- renderPrint({
                    #summary(data())
                    input$tabpanelId
                })

                # Generate an HTML table view of the data
                #output$table <- renderTable({
                #data.frame(x=data())
                #})

                output$testView <- renderPlot({
                    plot(1, 1)
                })
            }


)
