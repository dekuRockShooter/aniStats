# Functions:
#   binToDec: convert a binary string to a decimal integer 
#   get_genreset_prop: calculate class proportions of genre sets
#   set_genreset_prop: set class proportions of genre sets for a test set
#   yearwise_ks_test: perform a KS-test for data from consecutive years
#   get_timeline: compute a metric for data per time interval
#
# Under construction:
#   get_watched_prop: not implemented yet
#   set_watched_prop: not implemented yet
#   get_genreset_watched: not implemented yet
#   set_genreset_watched: not implemented yet
#   remove_constants: remove variables that have zero variance
#   get_production_prop: undergoing documentation

library(MASS)
library(nnet)
library(class)
library(boot)
library(randomForest)
library(gbm)
library(e1071)
library(glmnet)
library(foreach)
library(doParallel)


# Calculate class proportions of all genre sets.
#
# Args:
#   data:
#       A data frame returned by get_training_set(), or a superset of
#       one.
#
# Return:
#   An n x c matrix where n is the number of rows in 'data' and c is
#   the number of classes in data$score_class.  For a given row j, the
#   k'th column is the proportion of the number of times the k'th class
#   occurs in all rows that have the same genre subset as the j'th
#   observation.  For example, the entry (4, 5) = 0.3 means that out
#   of all rows with the same genre subset as row 4, 30% of them belong
#   to class 5.
get_genreset_prop = function(data) {
    genreset_prop_impl(data, data)
}

# Initialize class proportions of a test set.
#
# The proportions are calculated based on the training set.  For a
# given genre subset in 'new_data', its proportions are set to the
# same values as the matching genre subset in 'data'.  If a subset
# is unique to 'new_data', its proportions are set to zero.
#
# Return:
#   A matrix with the same meaning as in 'get_genreset_prop'.  The
#   difference is that the proportions are calculated based on the
#   training set 'data'.
set_genreset_prop = function(data, new_data) {
    genreset_prop_impl(data, new_data)
}

# Private function for set_genreset_prop and get_genreset_prop.
#
# Args:
#   data:
#       A data frame returned by get_training_set(), or a superset of
#       one.
#
#   new_data:
#       A data frame that has the variable 'genre_bits'.
genreset_prop_impl = function(data, new_data) {
    foreach(bits = new_data$genre_bits, .combine=rbind) %do% {
        w = data$score_class[data$genre_bits == bits]
        if (length(w) == 0) {
            s = rbind(rep(0.0, times=length(levels(DP$score_class))))
            return(s)
        }
        s = sapply(1 : length(levels(data$score_class)),
                   function(idx) {
                       sum(w == levels(data$score_class)[idx]) / length(w)
                   })
        s
    }
}

get_watched_prop = function(data) {
    watched_prop_impl(data, data)
}

set_watched_prop = function(data, new_data) {
    watched_prop_impl(data, new_data)
}

watched_prop_impl = function(data, new_data) {
    foreach(bits = new_data$genre_bits, .combine=rbind) %do% {
        w = data$score[data$genre_bits == bits]
        if (length(w) == 0) {
            return(7.0)
        }
        else if (length(w) == 1) {
            return(w[1])
        }
        abs(rnorm(1, mean=mean(w), sd=sd(w)))
    }
}

get_genreset_watched = function(data) {
    genreset_watched_impl(data, data)
}

set_genreset_watched = function(data, new_data) {
    genreset_watched_impl(data, new_data)
}

genreset_watched_impl = function(data, new_data) {
    foreach(bits = new_data$genre_bits, .combine=rbind) %do% {
        w = data$tot_watched[data$genre_bits == bits]
        if (length(w) == 0) { # Probably never happens.
            return(7.0)
        }
        else if (length(w) == 1) {
            return(w[1])
        }
        abs(rnorm(1, mean=mean(w), sd=sd(w)))
    }
}

# Convert a binary string to a decimal integer.
# 
# The source of this function is stackoverflow.
#
# Args:
#   x (str) : string of 0's and 1's.
#
# Return:
#   The decimal representation of x.
binToDec = function(x) {
    sum(2^(which(rev(unlist(strsplit(as.character(x), '')) == 1))-1))
}

remove_constants = function(data, from=6, to=44) {
    s = sapply(from : to, function(idx) sum(data[, idx] == 'Y'))
    s = which((s == 0) | (s == nrow(data))) + from - 1
    if (length(s) > 0) data = data[, -c(s)]
    data$score_class = factor(data$score_class)
    data
}

yearwise_ks_test = function(data, variable, year) {
    k = ks.test(variable(data[data$year == year, ]),
                variable(data[data$year == (year - 1), ]))
    k$p.value
}

get_production_prop = function(data, max_studio) {
    s = sapply(1 : max(data$studio),
               function(s) sum(data$studio == s)/nrow(data))
    qs = quantile(s[s > 1e-8])
    wq1 = which(!(s < qs[1]) & !(s > qs[2]))
    wq2 = which((s > qs[2]) & !(s > qs[3]))
    wq3 = which((s > qs[3]) & !(s > qs[4]))
    wq4 = which((s > qs[4]) & !(s > qs[5]))
    list(raw=s, q1=wq1, q2=wq2, q3=wq3, q4=wq4)
}

# Calculate a statistic for a range of times.  The statistic is computed
# for each subset of the vector that contains the elements measured at a
# specific time.
#
# Args:
#   statistic:
#       This defines the statistic to compute.  It is computed on the
#       subset of 'vec' that was measured at time 't'.
#
#       'mean': calculate the mean of each subset.
#       'sd': calculate the standard deviation of each subset.
#       'median': calculate the median of each subset.
#
#   vec:
#       A vector of observations.  The statistic is computed for all
#       observations in 'vec' that have the time value 't' (if times[j]
#       = t, then vec[j] is assumed to have occured at time t).
#
#   times:
#       A vector of times.  'vec' and 'times' should be related such that
#       vec[j] was measured at time[j].
#
# Return:
#   A vector with length equal to the number of distinct times in 'times'.
#   The vector is sorted in increasing order.  Element j is the value of
#   the statistic calculated using all observations measured at time j.
#
# Example:
#   df$ms = runif(100, 1, 10)
#   df$speed = runif(100, 5, 25)
#   # For all unique increasing times in df$ms, find the mean speed.
#   tm = get_timeline('mean', df$speed, df$ms)
get_timeline = function(statistic, vec, times) {
    sapply(sort(unique(times), decreasing=FALSE),
           function(cur_time) {
               w = which(times == cur_time)
               s = NULL
               if (statistic == 'mean') s = mean(vec[w], na.rm=TRUE)
               else if (statistic == 'sd') s = sd(vec[w], na.rm=TRUE)
               else if (statistic == 'median') s = median(vec[w], na.rm=TRUE)
               s
           })
}

# Create a matrix that stores timeline data for all genres.  The matrix
# is y by g dimensional, where y is the length of 'years' and g is the
# number of genres.  Each row vector represents values for a specific
# year, where the first row correponds to years[1], the second row to
# years[2], and so on.  The k'th column of a row gives the value of the
# statistic for the k'th genre for the corresponding year.
#
# Args:
#   data:
#       A data frame that contains the genres, a column named 'year',
#       and a column named 'score'.  The values of the genre columns
#       should be in {0, 1}.
#
#   years:
#       A list of unique years stored in ascending order.
#
#   metric:
#       A string defining the statistic to compute.
#
#       'mean': calculate the mean score for a genre at a year
#       'count': count the number of ones present in a genre for a year
#
# Return:
#    A matrix with length(years) rows and one column per genre.  Entry
#   (j, k) is the value of the metric for genre k at year j.
get_genre_freqs = function(data, years, metric, col_idx=-1) {
    freq_mat = lapply(years,
                      function(year) {
                          D = data[data$year == year, ]
                          if (col_idx > -1) {
                              col = D[, col_idx]
                          }
                          else {
                              col = D$score
                          }
                          sapply(GENRE_COLS,
                                 function(genre_idx) {
                                     if (metric == 'count') {
                                         sum(D[, genre_idx])
                                     } else if(metric == 'mean') {
                                         mean(col[D[, genre_idx] == 1],
                                              na.rm=TRUE)
                                     } else if(metric == 'median') {
                                         median(col[D[, genre_idx] == 1],
                                                na.rm=TRUE)
                                     }
                                 })
                      })
    freq_mat = do.call(rbind, freq_mat)
    return(freq_mat)
}

# Calculate the slope of the regression line for a given vector.
#
# Args:
#   mat:
#       A matrix of numerical data.
#
#   byrow:
#       A boolean that specifies if the regression line should be fit
#       on the row vectors (TRUE) or column vectors (FALSE).
#
#   onlysig:
#       A boolean that specifies if non-zero slopes should be returned
#       as non-zero regardless of their statistical significance.  If
#       this is TRUE, then the fitted line's slope is returned only if
#       it's p-value is significantly small.  Otherwise, 0.0 is
#       returned.  If this is FALSE, then the fitted line's slope is
#       always returned, regardless of statistical significance.
#
# Return:
#   A vector of slopes of the regression lines fit through either the
#   row vectors or column vectors, depending on that value of 'byrow'.
#   The j'th element is the slope of the line fit through the j'th row
#   or column.
get_trendline = function(mat, byrow=TRUE, onlysig=TRUE) {
    order = if (byrow) 1 else 2
    apply(mat,
          order,
          function(vec) {
              if (length(na.omit(vec)) < 2) return(0.0)
              line = lm(y ~ x, data=data.frame(y=vec, x=1:length(vec)))
              smry = summary(line)$coefficients['x', ]
              pval = smry[4]
              # Return the slope if it's not zero.
              if (onlysig) {
                  m = if (!is.na(pval) && (pval < 0.05)) smry[1] else 0.0
                  return(m)
              }
              m = if (!is.na(pval)) smry[1] else 0.0
              return(m)
          })
}

get_best_genres = function(mat) {
    apply(mat, 1, which.max)
}

get_genre_props = function(mat) {
    genre_freqs = apply(mat, 2, sum)
    tot_sum = sum(genre_freqs)
    return(genre_freqs/tot_sum)
}

get_core_genres = function(mat, alpha) {
    if ((alpha > 1.0) | (alpha < 0.0)) return(0.0)
    props = get_genre_props(mat)
    return(get_alpha_props(props, alpha))
}

get_props = function(vec, min_val=NULL, max_val=NULL) {
    if (is.null(min_val)) min_val = min(vec)
    if (is.null(max_val)) max_val = max(vec)
    vec_len = length(vec)
    sapply(min_val : max_val, function(s) sum(vec == s)/vec_len)
}

get_alpha_props = function(props, alpha) {
    #q = quantile(props, alpha)
    #return(which(!(props < q)))
    sorted_props = sort(props, decreasing=TRUE)
    s = 0.0
    m = 0
    while(s < alpha) {
        m = m + 1
        s = s + sorted_props[m]
    }
    alpha_props = which(!(props < sorted_props[m]))
    return(alpha_props)
}

# H = h(...)
# H is nrow(data) x 39 dimensional.  The rows in H correspond to those in
# data.  Those with years in [from_year, to_year] have new values for the
# genre columns.  T is nrow(test) x 39 dimensional.  The rows in T
# correspond to those in test.  The values are those of the last row of
# the score matrix for data plus the trendline slope.
transform_genres = function(data, from_year, to_year, test=NULL) {
    mat = matrix(0.0, nrow=nrow(data), ncol=39)
    test_mat = NULL
    if (!is.null(test)) test_mat = matrix(0.0, nrow=nrow(test), ncol=39)
    genre_cols = GENRE_COLS
    sapply(unique(data$studio),
           function(val) {
               wval = which(data$studio == val)
               #gmat = f(data[wval, ], from_year, to_year, 5)
               gmat = get_genre_freqs(data[wval, ],
                                      from_year : to_year,
                                      metric='mean')

               # Get data for test set.
               if (!is.null(test)) {
                   wval_test = which(test$studio == val)
                   if (length(wval_test) > 0) {
                       tl = get_trendline(gmat, byrow=FALSE, onlysig=TRUE)
                       #tl2 = get_trendline(G[13 : 16, ],
                                           #byrow=FALSE, onlysig=FALSE)
                       gmat_test = apply(gmat, 2,
                                         function(col) {
                                             last_val = col[length(col)]
                                             if (is.na(last_val)) {
                                                 mean(col, na.rm=TRUE) 
                                             } else {
                                                 last_val
                                             }
                                         }) + tl
                       sapply(wval_test,
                              function(idx) {
                                  gscores = ifelse(test[idx, genre_cols] == 0,
                                                   0.0,
                                                   gmat_test)
                                  test_mat[idx, ] <<- gscores
                              })
                   }
               }

               sapply(1 : nrow(gmat),
                      function(year_idx) {
                          year = from_year + year_idx - 1
                          wyear = which(data$year[wval] == year)
                          sapply(wval[wyear],
                                 function(idx) {
                                     gscores =
                                         ifelse(data[idx, genre_cols] == 0,
                                                0.0,
                                                gmat[year_idx, ])
                                     mat[idx, ] <<- gscores
                                 })
                      })
           })
    ret = if (is.null(test)) mat else list(data=mat, test=test_mat)
    return(ret)
}
