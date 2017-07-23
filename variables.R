# Functions:
#   get_studio_month_score: create a variable of studio monthly mean scores
#   get_k: create variables that approximate a show's score
#   get_metrics: calculate statistics for an instance of a random variable
#   get_gcounts: compute the occurences of genres of random variable instances
#   get_score_class: create a factor for the class that a score falls in
#   get_num_genres: create a variable for the number of genres in a show
#   get_genre_bits: create a variable that encodes a genre set as an integer
#   get_studio_mean: create a variable for the mean scores of studios
#   get_throughput: create a factor for studio means
#
# Under construction:
#   set_noisy_genreset_srctw: create noise for a source's viewership
#   set_noisy_genreset_srcms: create noise for a source's mean score
#   set_noisy_genreset_stw: create noise for a studio's viewership
#   set_noisy_genreset_sms: create noise for a studio's mean score
#   set_noisy_genreset_tot_watched: create noise for a show's viewership
#   set_noisy_genreset_score: create noise for a show's score
#   set_studio_mean_score: ?
#   set_genre_mean_score: ?

source('util.R')


# Create a variable of the monthly mean scores of a studio.  The
# variable X is such that X[j] is the mean score of shows created by
# data$studio[j] that aired in data$month[j].  That is, for a given
# observation j in 'data', X[j] is the mean score of shows in 'data'
# created by j's studio that aired in the same month os j.
#
# When 'test_data' is given, then for a given observation j in
# 'test_data', X[j] is the mean score of shows in 'data' created by j's
# studio that aired in the same month as j.  In this case, the variable
# is created for the new, unknown data based on the information in the
# old, known data.
#
# Return:
#   A vector with contents described as above.  If 'test_data' is not
#   given, then the vector is of length nrow(data), otherwise, it is
#   of length nrow(test_data).
get_studio_month_score = function(data, test_data=NULL) {
    sspm_func = function(idx, train, test) {
        m = mean(train$score[(train$studio == test$studio[idx]) &
                 (train$month == test$month[idx])])
        if (is.na(m)) m = mean(train$score[train$month == train$month[idx]])
        m
    }
    if (is.null(test_data)) {
        sapply(1 : nrow(data), sspm_func, data, data)
    }
    else {
        sapply(1 : nrow(test_data), sspm_func, data, test_data)
    }
}

# Create variables that approximate a show's score.  The approximations
# are based on the given random variable, 'rv' and 'X'.  X is used to
# compute the approximations.
#
# Args:
#   X:
#       A vector that represents the random variable given by 'rv'.
#       This is such that X[j] is the value of the random variable
#       for the j'th observation in 'data'.
#
#   rv:
#       One of 'studio' or 'source'.  If 'studio', then X[j] is the
#       studio of the j'th observation in 'data'.  If 'source', then
#       X[j] is the source of the j'th observation in 'data'.
#
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A list with names k1, ..., k7, n_a, n_b, p_a, p_b, g_a, g_b, and
#   rho.  k1 through k7 are the new variables, while the other elements
#   are as described in the 'Statistics' section of get_training_set().
get_k = function(X, rv, data, max_studio=0) {
    n_a = lapply(X, get_metrics, 'a', data=data, rv)
    n_b = lapply(X, get_metrics, 'b', data=data, rv)
    n_a = do.call(rbind, n_a)
    n_b = do.call(rbind, n_b)

    p_a = n_a[, 'n'] / (n_a[, 'n'] + n_b[, 'n'])
    p_b = 1.0 - p_a

    # TODO: don't hardcode the 286.
    studios = if (max_studio == 0) 1 : 286 else 1 : max_studio
    g_a = lapply(studios, get_gcounts, 'a', data, rv)
    g_b = lapply(studios, get_gcounts, 'b', data, rv)
    g_a = do.call(rbind, g_a)
    g_b = do.call(rbind, g_b)
    gaj = sapply(1 : nrow(data),
                 function(idx) {
                     studio = X[idx]
                     # This makes the sum fast (array instead of list).
                     genres = as.integer(data[idx, GENRE_COLS])
                     sum(genres*(g_a[studio, ] + 1)/(g_b[studio, ] + 1))
                 })
    gbj = sapply(1 : nrow(data),
                 function(idx) {
                     studio = X[idx]
                     # This makes the sum fast (array instead of list).
                     genres = as.integer(data[idx, GENRE_COLS])
                     sum(genres*(g_b[studio, ] + 1)/(g_a[studio, ] + 1))
                 })
    rho = ((n_a[, 'n'] + 1) / (n_b[, 'n'] + 1))
    # Some shows might have zero genres which would result in 0/0, so this
    # makes such shows have one good genre and one bad genre.
    w = which((gaj == 0) & (gbj == 0))
    gaj[w] = 1
    gbj[w] = 1

    k1 = ((gaj*n_a[, 's']) + (gbj*n_b[, 's'])) / (gaj + gbj)
    k2 = ((p_a*gaj*n_a[, 's']) + (p_b*gbj*n_b[, 's'])) / (p_a*gaj + p_b*gbj)
    k3 = ((rho*gaj*n_a[, 's']) + ((1/rho)*gbj*n_b[, 's'])) /
        (rho*gaj + (1/rho)*gbj)
    k4 = ((rho^2)*gaj*n_a[, 's'] + (1/(rho^2))*gbj*n_b[, 's']) /
        ((rho^2)*gaj + (1/(rho^2))*gbj)
    k5 = ((n_a[, 'v']*gaj*n_a[, 's']) + (n_b[, 'v']*gbj*n_b[, 's'])) /
        (n_a[, 'v']*gaj + n_b[, 'v']*gbj)
    k6 = ((n_a[, 'r']*gaj*n_a[, 's']) + (n_b[, 'r']*gbj*n_b[, 's'])) /
        (n_a[, 'r']*gaj + n_b[, 'r']*gbj)
    k7 = ((n_a[, 'n']*gaj*n_a[, 's']) + (n_b[, 'n']*gbj*n_b[, 's'])) /
        (n_a[, 'n']*gaj + n_b[, 'n']*gbj)
    list(
         k1=k1,
         k2=k2,
         k3=k3,
         k4=k4,
         k5=k5,
         k6=k6,
         k7=k7,
         n_a=n_a,
         n_b=n_b,
         p_a=p_a,
         p_b=p_b,
         g_a=g_a,
         g_b=g_b,
         rho=rho
         )
}

# Calculate statistics for an instance of a random variable.  Given
# an instance, x,  of a random variable, this function computes
# statistics using all observations in 'data' that have the same
# value of x.  For example, if x = 1, then all observations in 'data'
# for which the random variable is 1 are used to gather statistics
# such as mean score, mean viewership, total count, and more.
#
# Args:
#   x:
#       An instance of the random variable denoted by 'rv'.
#
#   ab:
#       One of 'a' or 'b'.  If 'a', then only those observations that
#       have a value of x and a score greater than the mean score for
#       all observations with a value of x will be used to compute the
#       statistics.  In other words, given a set S of all observations
#       in 'data' for which the random variable is x, use only the
#       observations in S that have scores greater than the mean score
#       of the observations in S.  This gives statistics for the above
#       average instances of x.  If 'b', then only the observations
#       that have a value of x and score lower than the mean score of
#       S are used.  This gives statistics for the below average
#       instances of x.
#
#   rv:
#       One of 'studio' or 'source'.  If 'studio', then 'x' should
#       be a studio.  If 'source', then 'x' should be a source.
#
#   data:
#       A data frame returned by init_anime().
#
# Returns:
#   A cbind'ed vector c such that c['n'] is the number of observations
#   in 'data' that have a value of x for the random variable and below/
#   average scores (see the description of 'ab' for a precise definition
#   of average in this case), c['v'] is the mean tot_watched of these
#   observations, c['s'] is the mean score, c['sd'] is the standard
#   deviation of the scores, and c['r'] is the sum of squared
#   differences between the scores and the mean score.
get_metrics = function(x, ab, data, rv='studio') {
    X = rv
    if (X == 'studio') X = data$studio
    else if (X == 'source') X = data$source
    w = NULL
    sg = mean(data$score[X == x])
    if (ab == 'a') {
        w = which((X == x) & !(data$score < sg))
    }
    else {
        w = which((X == x) & (data$score < sg))
    }
    n = length(w)
    v = if (n == 0) median(data$tot_watched) else mean(data$tot_watched[w])
    s = if (n == 0) median(data$score) else mean(data$score[w])
    r = if (n == 0) 0.5 else sum((data$score[w] - sg)^2)
    sd = if (n < 2) 0.5 else sd(data$score[w])
    cbind(n=n, v=v, s=s, r=r, sd=sd)
}

# Compute the occurences of genres for a random variable instance.  For
# a given instance x of a random variable, this function computes the
# number of times each genre occurs in all observations in 'data' that
# have the same value of x for the random variable.
#
# Args:
#   x:
#       An instance of the random variable denoted by 'rv'.
#
#   ab:
#       One of 'a' or 'b'.  Let S be the set of observations in 'data'
#       for which the random variable is x.  If 'a', then only the
#       observations in S that have a score greater than the mean score
#       of S will be used.  This gives the number of times a genre
#       occured in above average shows.  If 'b', then only the
#       observations in S that have a score below the mean score
#       of S will be used.  This gives the number of times a genre
#       occured in below average shows.
#
#   rv:
#       One of 'studio' or 'source'.  If 'studio', then 'x' should
#       be a studio.  If 'source', then 'x' should be a source.
#
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] is the number of times the j'th genre
#   occured in the below/above average shows that have a random variable
#   instance of x.
get_gcounts = function(x, ab, data, rv='studio') {
    X = rv
    if (X == 'studio') X = data$studio
    else if (X == 'source') X = data$source
    sg = 0
    if(is.null(x)) sg = mean(data$score)
    else sg = mean(data$score[X == x])
    g = GENRE_COLS # genre indeces.
    
    if (ab == 'a') {
        if (!is.null(x)) {
            n = sapply(g, function(j) sum((data[, j] == 1) &
                                          !(data$score < sg) &
                                          (X == x)))
        } else {
            n = sapply(g, function(j) sum((data[, j] == 1) & !(data$score < sg)))
        }
    }
    else { 
        if (!is.null(x)) {
            n = sapply(g, function(j) sum((data[, j] == 1) &
                                          (data$score < sg) &
                                          (X == x)))
        } else {
            n = sapply(g, function(j) sum((data[, j] == 1) & (data$score < sg)))
        }
    }
}

# Create a factor for the class that a score falls in.  For an
# observation j in 'data', it is classified as M if its score is
# above the median score, otherwise, it is classified as L.
#
# Args:
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] = M if data$score[j] > median score,
#   otherwise, v[j] = L.
get_score_class = function(data, thresh=7.0) {
    thresh = if (thresh > 0.0) thresh else median(data$score)
    score_class = ifelse(data$score > thresh, 'M', 'L')
    score_class = as.factor(score_class)
    score_class
}

# Create a variable for the number of genres in a show.
#
# Args:
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] is the number of genres present in
#   the j'th observation of 'data'.
get_num_genres = function(data) {
    num_genres = rep(0, times=nrow(data))
    sapply(6 : 43,
           function(idx) {
               num_genres <<- num_genres + as.integer(data[, idx]) +
                   as.integer(data[, idx + 1]) - 2
           })
    num_genres
}

# Create a variable that encodes a genre set as a base-10 integer.
# Given an observation j, its set of genres is converted to a binary
# string, which is then converted to the equivalent decimal integer.
#
# Args:
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] is the decimal integer representation
#   of the j'th observation's genre set.
get_genre_bits = function(data) {
    genre_bit_str = rep('1', times=nrow(data))
    sapply(GENRE_COLS,
           function(idx) {
               genre_bit_str <<- paste(genre_bit_str,
                                       as.integer(data[, idx]) - 1,
                                       sep='')
           })
    genre_bits = sapply(genre_bit_str, binToDec)
    genre_bits
}

# Create a variable for the mean scores of studios.
#
# Args:
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] is the mean score of all shows that have
#   the same studio as data$studio[j],
get_studio_mean = function(data, test_data=NULL) {
    mean_score = sapply(1 : max(data$studio),
                        function(studio)
                            mean(data$score[data$studio == studio]))
    studio_mean = sapply(data$studio,
                                function(studio) mean_score[studio])
    studio_mean
}

# Create a factor that classifies studios based on their mean scores.
# The factor has levels L, M, and H.  If 's' is a vector such that s[j]
# is the mean score of the shows produced by studio j, then observations
# in class L were made by studios in the first quartile of 's'.
# Observations in class M were made by studios in the second or third
# quartiles of 's', and those in class H were made by studios in the
# fourth quartile of 's'.
#
# Args:
#   data:
#       A data frame returned by init_anime().
#
# Return:
#   A vector v such that v[j] is one of L, M, or H depending on which
#   quartile data$studio[j] is in.
get_throughput = function(data) {
    mean_score = sapply(1 : max(data$studio),
                        function(s) mean(data$score[data$studio == s]))
    #mean_score[is.nan(mean_score)] = 0.0
    q = quantile(mean_score, na.rm=TRUE)
    throughput = sapply(1 : nrow(data),
                        function(idx) {
                            studio = data$studio[idx]
                            if (mean_score[studio] >= q[4]) {
                                "H"
                            } else if ((mean_score[studio] < q[2])
                                     || (is.nan(mean_score[studio]))) {
                                "L"
                            } else {
                                "M"
                            }
                        })
    throughput
}

set_noisy_genreset_srctw = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    train_genre_bits = sort(unique(data$genre_bits))
    train_sources = sort(levels(data$source))
    srcstw = sapply(1 : nrow(test_data),
                 function(idx) {
                     test_gb = test_data$genre_bits[idx]
                     test_st = test_data$source[idx]
                     m = mean(data$tot_watched[(data$genre_bits == test_gb) &
                              (data$source %in% test_st)])
                     # At least one genre_bit and source combo exists.
                     if (!is.na(m)) {
                         s = sd(data$tot_watched[(data$genre_bits == test_gb) &
                                (data$source %in% test_st)])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                         #return(m)
                     }
                     if (test_gb %in% train_genre_bits) {
                         #return(median(data$tot_watched[
                                       #data$genre_bits %in% test_gb]))
                         m = mean(data$tot_watched[
                                       data$genre_bits %in% test_gb])
                         s = sd(data$tot_watched[
                                       data$genre_bits %in% test_gb])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else if (test_st %in% train_sources) {
                         #return(median(data$tot_watched[
                                       #data$source %in% test_st]))
                         m = mean(data$tot_watched[
                                       data$source %in% test_st])
                         s = sd(data$tot_watched[
                                       data$source %in% test_st])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else {
                         #return(median(data$tot_watched))
                         m = mean(data$tot_watched)
                         s = sd(data$tot_watched)
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     }
                 })
    srcstw
}

# If there are NA values, then one possible reason is that the levels
# of source in data and test_data are the same when they are actually
# different.  Run data$source = factor(data$source) for data and
# train_data before calling this.
set_noisy_genreset_srcms = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    train_genre_bits = sort(unique(data$genre_bits))
    train_sources = sort(levels(data$source))
    srcsms = sapply(1 : nrow(test_data),
                 function(idx) {
                     test_gb = test_data$genre_bits[idx]
                     test_st = test_data$source[idx]
                     m = mean(data$score[(data$genre_bits == test_gb) &
                              (data$source %in% test_st)])
                     # At least one genre_bit and source combo exists.
                     if (!is.na(m)) {
                         s = sd(data$score[(data$genre_bits == test_gb) &
                                (data$source %in% test_st)])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                         #return(m)
                     }
                     if (test_gb %in% train_genre_bits) {
                         #return(median(data$score[
                                       #data$genre_bits %in% test_gb]))
                         m = mean(data$score[ data$genre_bits %in% test_gb])
                         s = sd(data$score[ data$genre_bits %in% test_gb])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else if (test_st %in% train_sources) {
                         #return(median(data$score[data$source %in% test_st]))
                         m = mean(data$score[ data$source %in% test_st])
                         s = sd(data$score[ data$source %in% test_st])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else {
                         #return(median(data$score))
                         m = mean(data$score)
                         s = sd(data$score)
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     }
                 })
    srcsms
}

set_noisy_genreset_stw = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    train_genre_bits = sort(unique(data$genre_bits))
    train_studios = sort(levels(data$studio))
    stw = sapply(1 : nrow(test_data),
                 function(idx) {
                     test_gb = test_data$genre_bits[idx]
                     test_st = test_data$studio[idx]
                     m = mean(data$tot_watched[(data$genre_bits == test_gb) &
                              (data$studio %in% test_st)])
                     # At least one genre_bit and studio combo exists.
                     if (!is.na(m)) {
                         s = sd(data$tot_watched[(data$genre_bits == test_gb) &
                                (data$tot_watched %in% test_st)])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                         #return(m)
                     }
                     if (test_gb %in% train_genre_bits) {
                         #return(median(data$tot_watched[data$genre_bits %in%
                                       #test_gb]))
                         m = mean(data$tot_watched[data$genre_bits %in% test_gb])
                         s = sd(data$tot_watched[data$genre_bits %in% test_gb])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else if (test_st %in% train_studios) {
                         #return(median(data$tot_watched[data$studio %in%
                                       #test_st]))
                         m = mean(data$tot_watched[ data$studio %in% test_st])
                         s = sd(data$tot_watched[ data$studio %in% test_st])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else{
                         #return(median(data$tot_watched))
                         m = mean(data$tot_watched)
                         s = sd(data$tot_watched)
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     }
                 })
    stw
}

set_noisy_genreset_sms = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    train_genre_bits = sort(unique(data$genre_bits))
    train_studios = sort(levels(data$studio))
    sms = sapply(1 : nrow(test_data),
                 function(idx) {
                     test_gb = test_data$genre_bits[idx]
                     test_st = test_data$studio[idx]
                     m = mean(data$score[(data$genre_bits == test_gb) &
                              (data$studio %in% test_st)])
                     # At least one genre_bit and studio combo exists.
                     if (!is.na(m)) {
                         #return(m)
                         s = sd(data$score[(data$genre_bits == test_gb) &
                                (data$studio %in% test_st)])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     }
                     if (test_gb %in% train_genre_bits) {
                         #return(median(data$score[data$genre_bits %in%
                                       #test_gb]))
                         m = mean(data$score[data$genre_bits %in% test_gb])
                         s = sd(data$score[data$genre_bits %in% test_gb])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else if (test_st %in% train_studios) {
                         #return(median(data$score[data$studio %in%
                                       #test_st]))
                         m = mean(data$score[data$studio %in% test_st])
                         s = sd(data$score[data$studio %in% test_st])
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     } else{
                         #return(median(data$score))
                         m = mean(data$score)
                         s = sd(data$score)
                         if (is.na(s)) s = 0
                         return(rnorm(1, mean=m, sd=s))
                     }
                     #if (is.na(m)) median(data$score) else m
                 })
    sms
}

set_noisy_genreset_tot_watched = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    ntw = sapply(1 : nrow(test_data),
                 function(idx) {
                     m = mean(data$tot_watched[
                              data$genre_bits == test_data$genre_bits[idx]])
                     if (is.na(m)) median(data$tot_watched) else m
                 })
    ntw
}

set_noisy_genreset_score = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    ngs = sapply(1 : nrow(test_data),
                 function(idx) {
                     m = mean(data$score[data$genre_bits ==
                              test_data$genre_bits[idx]])
                     if (is.na(m)) median(data$score) else m
                 })
    ngs
}

set_studio_mean_score = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    sms = sapply(1 : nrow(test_data),
           function(idx) {
               m = mean(data$score[data$studio %in% test_data$studio[idx]])
               if (is.na(m)) median(data$score) else m
           })
    sms
}

set_genre_mean_score = function(data, test_data=NULL) {
    test_data = if (is.null(test_data)) data else test_data
    gm = sapply(GENRE_COLS, function(idx) mean(data$score[data[, idx] == 'Y']))
    gm = ifelse(test_data[, GENRE_COLS] == 'Y', gm[1 : 39], 0.0)
    gmr = NULL
    if (is.null(test_data)) {
        gmr = ifelse(gm > 0.0, test_data$score / gm, 0.0)
    } else {
        gmr = ifelse(gm > 0.0, test_data$ngsms / gm, 0.0)
    }
    gmr
}
