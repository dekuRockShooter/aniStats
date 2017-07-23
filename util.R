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
#   col_indeces:
#      A vector of columns of 'data' for which to calculate the metric
#      for.
#
# Return:
#    A matrix with length(years) rows and length(col_indeces) columns.
#    Entry (j, k) is the value of the metric for data[, col_indeces[k]]
#    at year j.
get_genre_freqs = function(data, years, col_indeces) {
    data_year = as.integer(data$year)
    # Convert the data frame to a matrix to improve performance.
    data = as.matrix(data[, col_indeces])
    col_indeces = 1 : length(col_indeces)
    r = range(data_year)
    # This list will hold the indeces of rows in 'data' that have
    # a common year.  w = year_indeces[[j]] is thus a vector of indeces
    # such that data$year[w] will all have the same year.  This is
    # equivalent to which(data$year == j'th year).  The reason for not
    # using the which() function is to improve runtime, since this
    # serves as a cache.
    year_indeces = lapply(r[1] : r[2], function(j) c())
    # This is a list of lists.  matching_indeces[[y]][[c]] is a vector
    # of indeces of rows in 'data' that have year == y'th year and
    # the c'th column == 1.  This is the same as which(data$year == y'th
    # year and data[, c] == 1).  These indeces are cached to reduce
    # computation time.
    matching_indeces = lapply(
                              r[1] : r[2],
                              function(j) {
                                  lapply(col_indeces, function(j) NULL)
                              })

    # This loop initializes year_indeces.  It passes once through the
    # rows of 'data', appending the row index to the appropriate vector.
    # When a row's year is y, the index (year_idx) of the vector in
    # 'year_indeces' is y - min_year = y - r[1] + 1 (this creates a
    # 1-based index starting from the smallest year in 'data').
    min_year = r[1] - 1
    for (row_idx in 1 : length(data_year)) {
        year_idx = data_year[row_idx] - min_year # y = data_year[row_idx]
        year_indeces[[year_idx]] = c(year_indeces[[year_idx]], row_idx)
    }
    rm(r)

    # This is the main function.  It is returned to the caller.
    # Data in 'matching_indeces' are initialized the first time
    # this is called.  Any subsequent calls will not recompute
    # the data, and will instead reference 'matching_indeces' to
    # get them.
    f = function(metric, col=NULL, col_idx=-1) {
    freq_mat = lapply(years,
                      function(year) {
                          y = year - min_year
                          yr_list = matching_indeces[[y]]
                          wyr = year_indeces[[y]]
                          wyr_len = length(wyr)
                          if (wyr_len == 0) {
                              return(numeric(length(col_indeces)))
                          }
                          # This is the data for the current year.  If 'wyr'
                          # only has one element, data[wyr, ] will be a vector,
                          # not a matrix.  Thus, D[, col_idx] in the loop
                          # below will give an error.  To avoid this, the
                          # vector is converted to a matrix.
                          D = data[wyr, ]
                          if (wyr_len == 1) D = cbind(rbind(data[wyr, ]))

                          sapply(col_indeces,
                                 function(col_idx) {
                                     if (metric == 'count') {
                                         sum(D[, col_idx])
                                     } else if(metric == 'mean') {
                                         mean(col[D[, col_idx] == 1],
                                              na.rm=TRUE)
                                     } else if(metric == 'median') {
                                         # w stores the indeces of rows whose
                                         # year is equal to 'year' and whose
                                         # col_idx'th column is 1.
                                         w = yr_list[[col_idx]]
                                         if (is.null(w)) {
                                             w = which(D[, col_idx] == 1)
                                             matching_indeces[[y]][[col_idx]] <<- w
                                         }
                                         med(col[w])
                                     }
                                 })
                      })
    freq_mat = do.call(rbind, freq_mat)
    return(freq_mat)
    }

    return(f)
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
    # This is a linear least squares implementation to find the regression
    # line through the columns of 'mat'.  This is much faster than R's
    # lm() function, since that is tailored to handle general matrices,
    # while this function takes advantage of the properties specific to
    # 'mat'.  For comparison, the call to lm() that performs the equivalent
    # code to this function is commented out at the end of sapply().
    # It's a simple one liner, but it is many times slower.

    # Linear least squares (LLS) for this problem results in solving for
    # z1 and z2 in the following matrix equation:
    #
    #   [length(x) sum(x)  ] [z1] [sum(y)  ]
    #   [                  ]*[  ]=[        ]
    #   [sum(x)    sum(x^2)] [z2] [sum(y*x)]
    #
    #  = Az = b

    # These variables are initialized assuming y has no NA values.
    # These are cached so that when y has no NA values, there is no
    # need to recompute these values.
    x = 1 : nrow(mat)
    A = matrix(nrow=2, ncol=2)
    A[1, 1] = length(x)
    A[1, 2] = sum(x)
    A[2, 1] = A[1, 2]
    A[2, 2] = sum(x^2)
    b = numeric(2)
    sapply(
           1 : ncol(mat),
           function(col_idx) {
               y = as.numeric(mat[, col_idx])
               y[y < 1e-10] = NA
               wna = which(is.na(y))

               # When y has NA values, they and the corresponding x values
               # are removed.  Since x serves as a fixed cache, t is set to
               # it in case elements need to be removed.  Also in this case,
               # A needs to be recomputed, since its entries depend on x.
               # Since A acts as a fixed cache, a is set to it in case it
               # needs to be recomputed based on t.
               t = x
               a = A
               if (length(wna) > 0) {
                   y = y[-wna]
                   # Need at least 2 variables for regression.
                   if (length(y) < 2) return(0.0)

                   # This is the behavior of lm().
                   t = x[-wna]
                   # This gives higher slopes since the points are closer
                   # to eachother.
                   #t = x[1 : length(y)]

                   a[1, 1] = length(t)
                   a[1, 2] = sum(t)
                   a[2, 1] = a[1, 2]
                   a[2, 2] = sum(t^2)
               }
               b[1] = sum(y)
               b[2] = sum(y * t) # t == x when y has no NAs.
               slope = solve(a, b)[2] # a == A when y has no NAs.

               return(slope)

               # This does the same as the above, but is too slow.
               #coef(lm(y ~ x, data.frame(y=y, x=x)))[2]
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
transform_genres = function(data, from_year, to_year, genre_cols, test=NULL) {
    mat = matrix(0.0, nrow=nrow(data), ncol=39)
    test_mat = NULL
    if (!is.null(test)) test_mat = matrix(0.0, nrow=nrow(test), ncol=39)
    sapply(unique(data$studio),
           function(val) {
               wval = which(data$studio == val)
               #gmat = f(data[wval, ], from_year, to_year, 5)
               gmat = get_genre_freqs(data[wval, ],
                                      from_year : to_year,
                                      metric='mean',
                                      genre_cols)

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


# This function computes the median of a vector.  This exists for
# for those times when the built in median() function is too slow.
# This reduces computation time considerably.
med = function(vec) {
    if (length(vec) < 2) return(vec[1])
    sorted_vec = sort.int(vec, na.last=NA)
    len = length(sorted_vec)
    mid = ceiling(length(sorted_vec) / 2)
    # x bitwAnd 1 == 1 iff. x is odd.  This is faster than using
    # x mod 2 == 1 to check if x is odd.
    if (bitwAnd(1, len) == 1) sorted_vec[mid]
    else (sorted_vec[mid] + sorted_vec[mid+1]) / 2.0
}

