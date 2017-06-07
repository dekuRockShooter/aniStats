# Functions: #   init_anime: initialize the main data set 
library(lattice)
source('util.R')
source('variables.R')


QMIN_IDX = 1
Q25_IDX = 2
Q50_IDX = 3
QMEAN_IDX = 4
Q75_IDX = 5
QMAX_IDX = 6
PERFMAT_PROP = 1
PERFMAT_QPROP = 2
PERFMAT_QSCORE = 3
category_enum = list(
                     CATEGORY_TYPE=1,
                     CATEGORY_SOURCE=2,
                     CATEGORY_GENRE=3,
                     CATEGORY_STUDIO=4
                     )

# Initialize the main data set.  The Anime data set in its entirety is
# loaded and extended with new variables  These include:
#   Throughput
#   genres_bits
#   num_genres
#   score_class
#   noisy_score
init_anime = function() {
    #par(fg=rgb(0.6, 0.6, 0.6),
        #bg=rgb(0, 0, 0),
        #col.axis=rgb(0.6, 0.6, 0.6),
        #col.lab=rgb(0.6, 0.6, 0.6),
        #mfrow=c(2, 2))

    Anime = read.table("anime_mach.csv",
                     header=FALSE,
                     skip=1,
                     na.strings="?",
                     sep='`',
                     comment.char='',
                     quote=''
                     #colClasses=colClasses
                     )
    header = read.csv("anime_mach.csv",
                      header=TRUE,
                      nrow=1
                      )
    colnames(Anime) = colnames(header)
    Anime = na.omit(Anime)
    Anime$studio = factor(Anime$studio)
    Anime = data.frame(
                       Anime,
                       num_genres=get_num_genres(Anime)
                       )
}

get_overall_data = function(data, glo_data, cur_studio) {
    D = glo_data
    years = min(data$year) : max(data$year)
    overall = list()

    get_quantiles = function(year, vec) {
        as.numeric(summary(vec[data$year == year]))
    }

    # Get timeline data for scores, views, and episodes.
    data_score = as.numeric(na.omit(data$score))
    data_tot_watched = as.integer(na.omit(data$tot_watched))
    data_tot_eps = as.integer(na.omit(data$tot_eps))

    qscore_timeline = lapply(years, get_quantiles, data_score)
    qview_timeline = lapply(years, get_quantiles, data_tot_watched)
    qeps_timeline = lapply(years, get_quantiles, data_tot_eps)
    qscore_timeline = as.matrix(do.call(rbind, qscore_timeline))
    qview_timeline = as.matrix(do.call(rbind, qview_timeline))
    qeps_timeline = as.matrix(do.call(rbind, qeps_timeline))

    # Get performance percentiles for each year.  The rows are the years.
    # Column 1 is the proportion of shows made by the studio.  Column 2
    # is the proportions percentile.  Column 3 is the score percentile.
    year_idx = 0
    D_year = as.integer(D$year)
    D_studio = as.character(D$studio)
    gloDS_year = as.integer(globalDS$year)
    perf_mat = lapply(years,
                function(year) {
                    year_idx <<- year_idx + 1
                    wyear = which(D_year == year)
                    ss = NULL
                    studios = NULL
                    studio_counts = NULL
                    if (is.null(cur_studio)) {
                        # Pull these out to reduce repetitive calculations.
                        studios = D_year # All rows
                        studio_counts = table(studios)
                        wyear = 1 : nrow(D) # This extra array can be removed.

                        cur_studio_count = studio_counts[as.character(year)]
                        cur_studio_count = as.integer(cur_studio_count)
                        yr = year - min(gloDS_year) + 1 # 1-based index
                        # Median of studio median scores for each year.
                        sm = apply(studioMedScoresMat, 1,
                                   function(row) median(row, na.rm=TRUE))
                        # Number of studios for each year.
                        sw = apply(studioMedScoresMat, 1,
                                   function(row) length(na.omit(row)))
                        # Number of years worse than yr /
                        # Number of years
                        ss = sum(sm < sm[yr], na.rm=TRUE)/sum(sw > 0)
                    }
                    else {
                        studios = D_studio[wyear]
                        studio_counts = table(studios)
                        cur_studio_count = studio_counts[cur_studio]
                        cur_studio_count = as.integer(cur_studio_count)
                        yr = year - min(gloDS_year) + 1 # 1-based index
                        ss = sum(studioMedScoresMat[yr, ] < 
                                 qscore_timeline[year_idx, Q50_IDX],
                                 na.rm=TRUE)/
                        length(na.omit(studioMedScoresMat[yr, ]))
                    }
                    studio_counts = as.integer(studio_counts)
                    # Proportion percentile.
                    a = sum(studio_counts[!(studio_counts > cur_studio_count)],
                            na.rm=TRUE)
                    # proportion.
                    l = cur_studio_count/length(studios)
                    # Mean score percentile.
                    #ss = sum(D$score[wyear] <
                             #qscore_timeline[year_idx, QMEAN_IDX])/
                    c(l, a/sum(studio_counts), ss)
                })
    perf_mat = do.call(rbind, perf_mat)
    perf_mat[perf_mat < 1e-8] = NaN

    overall$qscore_timeline = qscore_timeline
    overall$qview_timeline = qview_timeline
    overall$qeps_timeline = qeps_timeline
    overall$perf_mat = perf_mat
    return(overall)
}

get_type_data = function(data, glo_data, category) {
    D = glo_data
    years = min(data$year) : max(data$year)
    data_year = as.integer(data$year)

    get_category_metric = function(year, cat_vec, categories, metric='count',
                                   metric_vec=NULL) {
        wcur_year = which(data_year == year)
        cur_cat_vec = cat_vec[wcur_year]
        cur_metric_vec = NULL
        if (!is.null(metric_vec)) {
            cur_metric_vec = na.omit(metric_vec[wcur_year])
        }
        n = length(wcur_year)
        # Get the metric of shows from each type.
        type_prop = sapply(categories,
                           function(cat) {
                               if (metric == 'count') {
                                   sum(cur_cat_vec == cat)
                               } else if (metric == 'median') {
                                   median(cur_metric_vec[cur_cat_vec == cat])
                               } else if (metric == 'mean') {
                                   mean(cur_metric_vec[cur_cat_vec == cat])
                               }
                           })
        return(type_prop)
    }

    class_names = NULL
    count_mat = NULL
    score_mat = NULL
    view_mat = NULL
    vec = NULL

    # Initialization of some matrices is different for genres that the
    # other categories.  The first if-block either initializes the
    # matrices for genres, or if the category is not genres, deffers
    # initialization to the second if-block.
    is_genre = TRUE
    if (category == category_enum$CATEGORY_TYPE) {
        vec = data$type
        is_genre = FALSE
    } else if (category == category_enum$CATEGORY_SOURCE) {
        vec = data$source
        is_genre = FALSE
    } else if (category == category_enum$CATEGORY_STUDIO) {
        vec = data$studio
        is_genre = FALSE
    # Matrix initialization genre.
    } else if (category == category_enum$CATEGORY_GENRE) {
        # Get timeline matrices for genres and their slopes.
        score_mat = get_genre_freqs(data, years, 'median', col_idx=46)
        count_mat = get_genre_freqs(data, years, 'count')
        view_mat = get_genre_freqs(data, years, 'median', col_idx=45)
        class_names = sort(names(data)[6 : 44], decreasing=FALSE)
    }
    # Matrix initialization for all categories except genre.
    if (is_genre == FALSE) {
        class_names = as.character(sort(levels(vec), decreasing=FALSE))
        vec = as.character(vec)
        count_mat = lapply(years, get_category_metric, vec, class_names)
        score_mat = lapply(years, get_category_metric, vec, class_names,
                           'median', as.numeric(data$score))
        view_mat = lapply(years, get_category_metric, vec, class_names,
                          'median', as.integer(data$tot_watched))
        count_mat = as.matrix(do.call(rbind, count_mat))
        score_mat = as.matrix(do.call(rbind, score_mat))
        view_mat = as.matrix(do.call(rbind, view_mat))
    }

    # The following code is common to all categories, and creates further
    # data structures.

    # Proportions of each genre relative to specific years.
    prop_mat = lapply(1 : nrow(count_mat),
                      function(idx) {
                          count_mat[idx, ]/sum(count_mat[idx, ])
                      })
    prop_mat = as.matrix(do.call(rbind, prop_mat))
    # This line causes barplots to render incorrectly.
    #prop_mat[prop_mat < 1e-10] = NaN

    score_slopes = get_trendline(score_mat, byrow=FALSE, onlysig=FALSE)
    prop_slopes = get_trendline(prop_mat, byrow=FALSE, onlysig=FALSE)

    core_classes = get_core_genres(count_mat, 0.5)

    # median(X) for each class.
    score_meds = apply(score_mat, 2, function(col) median(col, na.rm=TRUE))
    view_meds = apply(view_mat, 2, function(col) median(col, na.rm=TRUE))

    # Proportions of each genre relative to all years.
    tot_props = get_genre_props(count_mat)
    tot_props[tot_props < 1e-8] = NaN # Set as NaN to ignore 0's when graphing.

    names(tot_props) = class_names
    names(score_meds) = class_names

    score_med = median(score_meds, na.rm=TRUE)
    view_med = median(view_meds, na.rm=TRUE)
    props_med = median(tot_props, na.rm=TRUE)
    default_color = '#303030'
    class_colors = sapply(1 : length(class_names),
                          function(class_idx) {
                              if (is.na(score_meds[class_idx]) |
                                  is.na(tot_props[class_idx])) {
                                  return('#303030')
                              }
                              cur_color = default_color
                              if (class_idx %in% core_classes) {
                                  cur_color = 'red'
                              } else if (score_meds[class_idx] > score_med) {
                                  if (tot_props[class_idx] < props_med) {
                                      cur_color = 'blue'
                                  } else if (view_meds[class_idx] < view_med) {
                                      cur_color = 'green'
                                  }
                              }
                              return(cur_color)
                          })

    ret_list = list()
    ret_list$core_classes = core_classes # The most important classes.
    ret_list$class_names = class_names # Class names in increasing order.
    ret_list$count_mat = count_mat # Mj,k = num of class k shows in year j.
    ret_list$score_mat = score_mat # Mj,k = med score of class k in year j.
    ret_list$view_mat = view_mat # Mj,k = med views of class k in year j.
    ret_list$prop_mat = prop_mat # Mj,k = prop of class k shows in year j.
    ret_list$score_slopes = score_slopes # Vj = slope of class j score
    ret_list$prop_slopes = prop_slopes # Vj = slope of class j prop
    ret_list$score_meds = score_meds # Vj = class j med score for all years
    ret_list$view_meds = view_meds # Vj = class j med view for all years
    ret_list$tot_props = tot_props # Vj = class j prop for all years
    ret_list$class_colors = class_colors # Vj = color of class j (in plots)

    return(ret_list)
}


# 'data' is a data frame of the data of interest, for example, of all shows
# made by a specific studio.
init_data = function(data, glo_data, studio) {
    D = glo_data
    genres = list()
    types = list()
    sources = list()
    overall = get_overall_data(data, glo_data, studio)
    types = get_type_data(data, glo_data, category_enum$CATEGORY_TYPE)
    sources = get_type_data(data, glo_data, category_enum$CATEGORY_SOURCE)
    genres = get_type_data(data, glo_data, category_enum$CATEGORY_GENRE)
    studios = NULL
    if (is.null(studio)) {
        studios = get_type_data(data, glo_data, category_enum$CATEGORY_STUDIO)
    }

    years = min(data$year) : max(data$year)

    ret_list = list(
                    overall=overall,
                    sources=sources,
                    types=types,
                    genres=genres,
                    studios=studios,
                    #gcolors=gcolors,
                    years=years,
                    studio=studio
                    )
    return(ret_list)
}


###############################################################################
# GENRE PROPORTION VS. GENRE
###############################################################################
# w = order(gprops, decreasing=TRUE)
# prop_vs_genre(gprops[w], gcolors[w])
gprop_vs_genre = function(gprops, gcolors) {
    barplot(
            100*gprops, space=0, las=2, col=gcolors,
            ylab='Frequency of genre (%)',
            main='Genres by frequency'
            )
}
#w = order(gprops, decreasing=TRUE)
#gprop_vs_genre(gprops[w], gcolors[w])

###############################################################################
# MEAN GENRE SCORE VS. GENRE
###############################################################################
# w = order(score_means, decreasing=TRUE)
# mean_gscore_vs_genre(score_means[w], gcolors[w])
mean_gscore_vs_genre = function(mean_gscores, gcolors, global_med_score) {
    #barplot(sort(score_means, decreasing=TRUE), space=0, las=2,
            #col=gcolors[order(score_means, decreasing=TRUE)])
    barplot(mean_gscores, space=0, las=2, col=gcolors,
            ylab='Mean score',
            main='Genres by mean score')
    abline(h=median(mean_gscores, na.rm=TRUE), lty='dashed', col='black')
    abline(h=global_med_score, lty='dotted', col='black')
}
#w = order(score_means, decreasing=TRUE)
#mean_gscore_vs_genre(score_means[w], gcolors[w], median(D$score))


###############################################################################
# GENRE PROPORTION VS. YEAR (SHIFT IN FOCUS)
###############################################################################
# genre proportion vs. year (shift in focus)

# gprop_vs_year(1990:2000, gprop_mat[, 4], data$gscore_mat[, 4])
gprop_vs_year = function(years, gprops, gscores, max_prop, minmax_scores,
                         ylim=NULL, xlim=NULL) {
    #gprops[gprops < 1e-12] = NaN
    if (is.null(ylim)) ylim = c(0.0, max_prop)
    plot(years, gprops, type='b', ylim=ylim, xlim=xlim,
         xlab='years',
         ylab='Frequency of genre',
         main='Genre frequency throughout time')
    axis(side=4,
         at=seq(0.0, max_prop, length=8),
         labels=round(seq(minmax_scores[1], minmax_scores[2], length=8), 1))
    b1 = max_prop/(minmax_scores[2] - minmax_scores[1])
    b0 = -b1 * minmax_scores[1]
    lines(years, b0 + b1*gscores, lty='dashed', type='b', col='#303030',
          ylab='Mean score')
}
#gprop_vs_year(years, gprop_mat[, 1], data$gscore_mat[, 1], max(gprop_mat))

###############################################################################
# GENRE SCORE VS. GENRE VIEWS (POPULAR GENRES)
###############################################################################
# gscore_vs_gview(view_means, score_means, gcolors,
#                 median(D$score), median(D$tot_watched))
gscore_vs_gview = function(mean_gviews, mean_gscores, gcolors,
                           global_med_score, global_med_views,
                           glabels, ylim=NULL, xlim=NULL) {
    plot(
         mean_gviews, mean_gscores, type='n',
         ylab='Mean score (quality)',
         xlab='Mean views (popularity)',
         main='Genre popularity and quality',
         ylim=ylim,
         xlim=xlim
         )
    abline(h=median(mean_gscores, na.rm=TRUE), lty='dashed')
    abline(v=median(mean_gviews, na.rm=TRUE), lty='dashed')
    abline(h=global_med_score, lty='dashed', col='#303030')
    abline(v=global_med_views, lty='dashed', col='#303030')
    text(mean_gviews, mean_gscores, labels=glabels, col=gcolors)
}
#gscore_vs_gview(view_means, score_means, gcolors,
                #median(D$score), median(D$tot_watched),
                #names(D)[6 : 44])

###############################################################################
# GENRE SCORE VS. GENRE PROPORTION
###############################################################################
# score vs. proportion of total genres
# gscore_vs_gprop(gprops, score_means, median(D$score), gcolors,
#                 names(D)[6 : 44])
gscore_vs_gprop = function(gprops, mean_gscores, global_med_score, gcolors,
                           glabels, ylim=NULL, xlim=NULL) {
    plot(
         gprops, mean_gscores, type='n',
         main='Genre quantity and quality',
         xlab='Frequency of genre (quantity)',
         ylab='Mean score (quality)',
         ylim=ylim,
         xlim=xlim
         )
    abline(h=median(mean_gscores, na.rm=TRUE), lty='dashed')
    abline(v=median(gprops, na.rm=TRUE), lty='dashed')
    abline(h=global_med_score, lty='dashed', col='#303030')
    #abline(v=median(get_genre_props(freq_mat for D)), lty='dashed', col='#303030')
    text(gprops, mean_gscores, labels=glabels, col=gcolors)
}
#gscore_vs_gprop(gprops, score_means, median(D$score), gcolors,
                #names(D)[6 : 44])

###############################################################################
# GENRE SCORE SLOPE VS. GENRE PROPORTION SLOPE
###############################################################################
# score vs. proportion of total genres
# gscore_slope_vs_gprop_slope(data$gcount_slopes, data$gscore_slopes,
#                             gcolors, names(D)[6 : 44])
# score slope vs. count slope (forecast) (quality vs. quanitity)
gscore_slope_vs_gprop_slope = function(gcount_slopes, gscore_slopes,
                                       gcolors, glabels,
                                       ylim=NULL, xlim=NULL) {
    plot(
         gcount_slopes, gscore_slopes, type='n',
         main='Forecast of genre quality and quantity',
         xlab='Change in frequency',
         ylab='Change in mean score',
         ylim=ylim,
         xlim=xlim
         )
    abline(h=0, lty='dashed')
    abline(v=0, lty='dashed')
    text(gcount_slopes, gscore_slopes, labels=glabels, col=gcolors)
}
#gscore_slope_vs_gprop_slope(data$gcount_slopes, data$gscore_slopes,
                            #gcolors, names(D)[6 : 44])

###############################################################################
# SCORE VS. YEAR, VIEWS VS. YEAR, EPS VS. YEAR
###############################################################################
# score vs. year
# score_vs_year(years, data$med_score_timeline,
#               get_timeline('median', D$score[D$year%in%years],
#                            D$year[D$year%in%years]),
#               data$q_timeline, c(min(data$q_timeline, max(data$q_timeline))))
score_vs_year = function(years, global_timeline, local_timeline, title, ylab,
                         timeline_mat=NULL, ylim=NULL, xlim=NULL) {
    if (is.null(ylim)) ylim = range(timeline, na.rm=TRUE)
    plot(years, local_timeline, type='b', ylim=ylim, xlim=xlim, col='red',
         main=title,
         xlab='years',
         ylab=ylab)
    lines(years, global_timeline, type='b', col='#303030')

    if (is.null(timeline_mat)) {
        return(NULL)
    }
    apply(timeline_mat, 2,
          function(col) lines(years, col, lty='dashed'))
}
#score_vs_year(years, data$mean_score_timeline,
              #get_timeline('median', D$score[D$year%in%years],
                           #D$year[D$year%in%years]),
              #data$q_timeline, c(min(data$q_timeline), max(data$q_timeline))
              #)


###############################################################################
# NUMBER OF TYPE VS. YEAR
###############################################################################
# numtype_vs_year(2000:2010, dataset)
numtype_vs_year = function(years, cat_freq_mat, categories) {
    barplot(cat_freq_mat, names.arg=years, col=rainbow(length(categories)),
            legend.text=categories, args.legend=list(x='topleft'),
            border=NA,
            main='Proportions of anime types',
            xlab='year')
}
#numtype_vs_year(years, dataset)

###############################################################################
# PROPORTION OF TOTAL SHOWS VS. YEAR
###############################################################################
dominance_vs_year = function(years, perf_mat, ylim=NULL, xlim=NULL) {
    if (is.null(ylim)) {
        ymax = max(perf_mat, na.rm=TRUE)
        ylim = c(0, ymax)
    }
    plot(years, perf_mat[, 1], type='b', xlim=xlim, ylim=ylim,
         main='Performance throughout time',
         xlab='years',
         ylab='Proportion of studios that did worse')
    lines(years, perf_mat[, 2], type='b', lty='dashed', col='red')
    lines(years, perf_mat[, 3], type='b', lty='dashed', col='blue')
}
#dominance_vs_year(years, totprop, qscore_timeline)

###############################################################################
# SCORE VS. DOMINANCE
###############################################################################
score_vs_dominance = function(years, perf_mat, ylim=NULL, xlim=NULL) {
    cur_year = as.integer(format(Sys.time(), '%Y'))
    ycolors = sapply(years,
                     function(year) {
                         if (year > (cur_year - 6)) 'yellow'
                         else if (year > (cur_year - 11)) 'brown'
                         else '#a0a0a0'
                     })
    plot(perf_mat[, 2], perf_mat[, 3], type='n',
         main='Performance throughout time: from a different angle',
         xlab='Proportion of studios that produced fewer anime',
         ylab='Proportion of studios with lower mean scores',
         ylim=ylim,
         xlim=xlim
         )
    abline(v=median(perf_mat[, 2], na.rm=TRUE), lty='dashed')
    abline(h=median(perf_mat[, 3], na.rm=TRUE), lty='dashed')
    text(perf_mat[, 2], perf_mat[, 3], labels=years,
         col=ycolors)
    #lines(years, data$mean_score_timeline, type='b', col='blue')
}
#score_vs_dominance(totprop, data$mean_score_timeline)


########################################################################
# DATA
#
# The data computed for each type can be done so for any given range of
# years (the default is ten years prior to the current year).  The years
# are consecutive.
#
# The data computed for each type can be done so for a subset of the
# data.  The subsets allowed are: all shows within a range of years, and
# all shows within a range of years and with type A = class Aj.  The
# default is to compute data for shows within 10 years of the current
# year.  This can change by specifying a different contiguous range, and
# possibly a specific type.  For example: compute data using the shows
# from 2000 to 2004 that have type = TV.  Or compute data using the same
# years, but with source = Manga.  Or compute data for shows from 1994
# to 2004 and that have the romance genre present.
########################################################################
# Data initialization
#A = init_anime()
#D = A[, -1]
#sapply(6 : 44, function(idx) D[, idx] <<- as.integer(D[, idx]) - 1)
#cur_studio = 'Gainax'
#default_years = (max(D$year) - 10) : (max(D$year) - 1) # Not the current year
#dataset = D[(D$studio == cur_studio), ]
#data = init_data(D, D, NULL)

#pdf(file='tmp.pdf')
#par(fg=rgb(0.6, 0.6, 0.6),
    #bg=rgb(0, 0, 0),
    #col.axis=rgb(0.6, 0.6, 0.6),
    #col.lab=rgb(0.6, 0.6, 0.6),
    #mfrow=c(1, 1))
#glb_med_score = median(D$score)
#glb_med_view = median(D$tot_watched)
#
#########################################################################
## TYPES SECTION
##
## These are the common elements of each type (genre, source, and type).
## The only thing that changes is the value of cur_type, which denotes
## the currently selected type.
#########################################################################
#cur_type = data$genre
#
#w = order(cur_type$tot_props, decreasing=TRUE)
#gprop_vs_genre(cur_type$tot_props[w], cur_type$class_colors[w])
#w = order(cur_type$score_meds, decreasing=TRUE)
#mean_gscore_vs_genre(cur_type$score_meds[w], cur_type$class_colors[w], glb_med_score)
#gprop_vs_year(data$years, cur_type$prop_mat[, 4], cur_type$score_mat[, 4],
              #max(cur_type$prop_mat, na.rm=TRUE),
              #range(cur_type$score_mat, na.rm=TRUE))
#
#gprop_palette = colorRampPalette(c('black', 'white'))(n=128)
#image(x=data$years, y=1:length(cur_type$class_names),
      #cur_type$prop_mat, col=gprop_palette)
#axis(side=2, at=1:length(cur_type$class_names),
     #labels=cur_type$class_names, las=2)
##levelplot(data$gprop_mat, region=TRUE, col.regions=gprop_palette)
#
#gscore_vs_gview(cur_type$view_meds, cur_type$score_meds, cur_type$class_colors,
                #glb_med_score, glb_med_view, cur_type$class_names)
#gscore_vs_gprop(cur_type$tot_props, cur_type$score_meds,
                #glb_med_score, cur_type$class_colors,
                #cur_type$class_names)
#gscore_slope_vs_gprop_slope(
                            #get_trendline(cur_type$prop_mat,
                                          #byrow=FALSE,
                                          #onlysig=FALSE),
                            #cur_type$score_slopes,
                            #cur_type$class_colors, cur_type$class_names)
#
#########################################################################
## OVERALL SECTION
#########################################################################
get_quantiles_tmp = function(year, vec, data_years) {
    as.numeric(summary(vec[data_years == year]))
}
#
#tmln = lapply(data$years, get_quantiles_tmp, na.omit(D$score))
#tmln = do.call(rbind, tmln)[, 3]
#score_vs_year(data$years, tmln,
              ##get_timeline('median', D$score[D$year%in%data$years],
                           ##D$year[D$year%in%data$years]),
              #title='Mean score throughout time',
              #ylab='Mean score',
              #timeline_mat=data$overall$qscore_timeline,
              #main_col=3,
              #ylim=c(min(data$overall$qscore_timeline, na.rm=TRUE),
                     #max(data$overall$qscore_timeline, na.rm=TRUE))
              #)
#tmln = lapply(data$years, get_quantiles_tmp, na.omit(D$tot_watched))
#tmln = do.call(rbind, tmln)[, 3]
#score_vs_year(data$years, tmln,
              ##get_timeline('median', D$tot_watched[D$year%in%data$years],
                           ##D$year[D$year%in%data$years]),
              #title='Mean views throughout time',
              #ylab='Mean views',
              #data$overall$qview_timeline,
              #main_col=3,
              #c(min(data$overall$qview_timeline, na.rm=TRUE),
                #max(data$overall$qview_timeline, na.rm=TRUE))
              #)
#tmln = lapply(data$years, get_quantiles_tmp, na.omit(D$tot_eps))
#tmln = do.call(rbind, tmln)[, 3]
#score_vs_year(data$years, tmln,
              ##get_timeline('median', D$tot_eps[D$year%in%data$years],
                           ##D$year[D$year%in%data$years]),
              #title='Mean episodes throughout time',
              #ylab='Mean episodes',
              #data$overall$qeps_timeline,
              #main_col=3,
              #c(min(data$overall$qeps_timeline, na.rm=TRUE),
                #max(data$overall$qeps_timeline, na.rm=TRUE))
              #)
#numtype_vs_year(data$years, t(data$types$prop_mat), data$types$class_names)
                ##sort(levels(dataset$type), decreasing=FALSE))
#numtype_vs_year(data$years, t(data$sources$prop_mat), data$sources$class_names)
                ##sort(levels(dataset$source), decreasing=FALSE))
##dominance_vs_year(data$years, data$overall$perf_mat)
##score_vs_dominance(data$years, data$overall$perf_mat)
#dev.off()

# Core genres
#data$core_genres
# Hidden gems
#data$hidden_genres
# Rare gems
#data$rare_genres
# Type counts
#types = sort(levels(data$type), decreasing=FALSE)
#type_counts = lapply(types, function(type) sum(dataset$type == type))
# Source counts
#sources = sort(levels(data$source), decreasing=FALSE)
#type_counts = lapply(sources, function(src) sum(dataset$sources == src))

##############################################################################
# PLOTS.
##############################################################################
# Red: core genres
# Blue: rare gems--genres that are not made frequently but have high scores
# Green: hidden gems--genres with low views but high scores
# Black: not special
# * score vs. year (quality)
# * views vs. year (popularity)
# * eps vs. year (length)
# * proportion of total shows vs. year (dominance)
# best genres vs. year (strengths)
# worst genres vs. year (weaknesses)
# * score vs. views (hidden gems)
# * max_score - min_score vs. year (consistency)
# * score slope vs. count slope (forecast) (quality vs. quanitity)
# * score vs. proportion of total genres (quality vs. quantity) (priorities)
# number of times in top x% best vs. count (quality vs. quantity)
# * genre proportion vs. year (shift in focus)
# number of times above mean vs. number of time below mean
# * number of OVAs vs. year (interest in OVA)
# * number of TVs vs. year (interest in TV)
# * number of Movies vs. year (interest in Movie)

#A = init_anime()
#globalNames = A$name
#globalDS = A[, -1]
#rm(A)
#sapply(6 : 44,
       #function(idx)
           #globalDS[, idx] <<- as.integer(globalDS[, idx]) - 1)
#
## Matrix of median scores for each studio (columns) per year (rows).
#years = min(globalDS$year) : max(globalDS$year)
#gloStudio = as.character(globalDS$studio)
#gloYear = as.integer(globalDS$year)
#gloScore = as.numeric(globalDS$score)
#studioMedScoresMat = lapply(
                            #sort(as.character(levels(globalDS$studio)),
                                 #decreasing=FALSE),
                            #function(studio) {
                                #locScore = gloScore[gloStudio == studio]
                                #locYear = gloYear[gloStudio == studio]
                                #sapply(years,
                                       #function(year) {
                                           #median(locScore[
                                                  #(locYear == year)
                                                  #], na.rm=TRUE)
                                       #})
                            #})
#rm(years); rm(gloStudio); rm(gloYear); rm(gloScore)
#studioMedScoresMat = do.call(cbind, studioMedScoresMat)

# Not the current year
#defaultYears = (max(globalDS$year) - 10) : (max(globalDS$year) - 1)
globalData = init_data(globalDS, globalDS, NULL)
#globalMedScore = median(globalDS$score)
#globalMedViews = median(globalDS$tot_watched)

# I don't remember what these variables were meant for.
#globalTimeline = list()
#globalTimeline$scores = lapply(globalData$years,
                               #get_quantiles_tmp,
                               #na.omit(globalDS$score),
                               #globalDS$year)
#globalTimeline$views = lapply(globalData$years,
                               #get_quantiles_tmp,
                               #na.omit(globalDS$tot_watched),
                               #globalDS$year)
#globalTimeline$eps = lapply(globalData$years,
                               #get_quantiles_tmp,
                               #na.omit(globalDS$tot_eps),
                               #globalDS$year)
#globalTimeline$scores = do.call(rbind, globalTimeline$scores)[, 3]
#globalTimeline$views = do.call(rbind, globalTimeline$views)[, 3]
#globalTimeline$eps = do.call(rbind, globalTimeline$eps)[, 3]
