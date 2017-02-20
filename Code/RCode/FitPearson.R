library(dplyr)
library(stringr)
library(parallel)
library(reshape2)

## get the MakeLagDF function from FitLasso
source('FitLasso.R')

#' Shuffle a subset of indices in a vector
#'
#' @param vec vector, vector for which a subset of indices will be shuffled
#' @param inds vector, vector of indices to shuffle from \code{vec}
#'
#' @return vector, \code{vec} but with the indices in \code{inds} shuffled
ShuffleInds <- function(vec, inds){
    shuffleInds <- sample(inds, replace = FALSE, size = length(inds))
    vec[inds] <- vec[shuffleInds]
    return(vec)
}

#' Randomize Presence-Absence data
#'
#' Randomize the presence and absence of each species such that it is
#' present for the same number of timepoints, but autocorrelation and
#' correlation signal with other species should be removed.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#'
#' @return data.frame, randomized training data
RandomizeRows <- function(df){
    inds <- which(complete.cases(df))
    df <- df %>%
        apply(ShuffleInds, MARGIN = 2, inds = inds) %>%
        as.data.frame
    out <- MakeLagDF(df)
    return(out)
}

#' Perform one rep of permutation test
#'
#' Perform a single repetition of the permutation test, such that
#' the dataset is randomized and compared to the observed correlation
#' matrix once.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param abscormat matrix, correlation matrix of species and lag-1s of the
#' species. Correlations are all absolute-value transformed, so that a
#' two-sided permutation test is being performed.
#'
#' @return matrix, same size as \code{abscormat}, but contains 0s and 1s; 1 if
#' the randomized correlation was more extreme than observed empirically, 0 otw.
OneRandomization <- function(df, abscormat){
    out <- RandomizeRows(df)
    ## gracefully handle issues from randomization
    biggerCor <- tryCatch({
        randcormat <- cor(out$df, out$lagdf, method = "pearson")
        biggerCor <- (abs(randcormat) > abscormat) * 1
        return(biggerCor)
    }, warning = function(w) {
        ## randomizing and lagging the dataset may result
        ## in an edge case where the randomized df or lagdf
        ## are all 1s or 0s (especially possible for very
        ## common or rare sp). If this comes up, we
        ## re-randomize and don't use the problem
        ## randomization.
        biggerCor <- OneRandomization(df, abscormat)
        return(biggerCor)
    })
    return(biggerCor)
}

#' Helper function for parallelization
#'
#' Perform a portion of the permutation test reps on a single core. This
#' is a convenience function to be passed to \code{mclapply}.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param abscormat matrix, correlation matrix of species and lag-1s of the
#' species. Correlations are all absolute-value transformed, so that a
#' two-sided permutation test is being performed.
#' @param reps numeric, number of randomization repetitions to perform on this core.
#'
#' @return matrix, contains counts of the number of repetitions where the
#' randomized correlation was more extreme than observed empirically.
PearsonRandomizationParallel <- function(df, abscormat, reps){
    pvalmat <- matrix(0, nrow(abscormat), ncol(abscormat))
    for(i in 1:reps){
        tmpmat <- OneRandomization(df, abscormat)
        pvalmat <- pvalmat + tmpmat
    }
    return(pvalmat)
}

#' Convert matrix of values to adjlist
#'
#' Convert a matrix of values into a full adjacency list containing
#' all values (including 0s).
#'
#' @param mat matrix, a matrix to convert into adjlist form
#'
#' @return data.frame, an adjacency list containing the columns
#' 'Child' (row name), 'Parent' (column name), and 'value' (value
#' from the matrix).
AdjmatToAdjlist <- function(mat){
    adjlist <- reshape2::melt(mat)
    names(adjlist) <- c('Child', 'Parent', 'value')
    adjlist <- adjlist %>% mutate(Child = as.character(Child),
                                  Parent = as.character(Parent))
    return(adjlist)
}

#' Calculate p-values for correlation matrix using permutation test
#'
#' Perform a permutation test to identify statistically significant
#' correlations, multiple-test-corrected using the False Discovery Rate.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param cormat matrix, correlation matrix of species and lag-1s of the
#' species.
#' @param reps numeric, number of randomization repetitions to perform.
#' @param cores integer, number of computer cores to use in computation
#'
#' @return data.frame, adjacency list containing columns 'Child' (unlagged species),
#' 'Parent' (lagged species), and 'p.value' (corrected p-value based on the
#' randomization).
PearsonRandomization <- function(df, cormat, reps = 1e4, cores = 6){
    ## number of chunks to split randomization into, for speed purposes
    ## HARDCODED (but not worth exposing to the user)
    listsize <- 100
    ## take absolute value for a two-sided test
    abscormat <- abs(cormat)
    ## create list of df for mclapply to use
    listdf <- vector(mode = 'list', length = listsize)
    listdf <- lapply(listdf, function(x) return(df))
    subreps <- ceiling(reps/listsize)
    ## run portions of the randomization on different cores
    pvallist <- parallel::mclapply(listdf, PearsonRandomizationParallel,
                                   abscormat = abscormat, reps = subreps,
                                   mc.cores = cores)
    ## utility function to add together all matrices in the list
    add <- function(x) Reduce('+', x)
    pvals <- add(pvallist)
    ## convert to probability of randomly getting that extreme a value
    pvals <- pvals / reps

    ## convert to adjlist format
    adjpval <- AdjmatToAdjlist(pvals)
    
    ## adjust for FDR
    adjpval$p.value <- p.adjust(adjpval$value, method = 'fdr')
    ## keep significant rows
    adjpval <- adjpval %>% select(-value) %>% filter(p.value <= .05)
    return(adjpval)
}

#' Build final adjlist from results
#'
#' Build adjacency list of significant interactions, their correlations,
#' their p-values, and their signs.
#'
#' @param adjpval data.frame, adjacency list of significant interactions
#' containing 'Child' (unlagged species), 'Parent' (lagged species), and
#' 'p.value' (corrected p-value based on randomization test)
#' @param cormat matrix, correlation matrix of species and lag-1s of the
#' species.
#'
#' @return data.frame, adjacency list containing data from \code{adjpval},
#' with additional columns 'cor' (correlation value) and 'Sign' (sign of
#' the correlation).
BuildAdjlist <- function(adjpval, cormat){
    adjcor <- AdjmatToAdjlist(cormat) %>% rename(cor = value)
    adjpval <- adjpval %>% left_join(adjcor, by = c('Child', 'Parent'))
    adjpval <- adjpval %>% mutate(Sign = sign(cor))
    ## remove "lag." from the beginning of the Parent spnames
    rmlag <- function(x) return(str_replace(x, 'lag.', ''))
    adjpval <- adjpval %>% mutate(Parent = rmlag(Parent))
    return(adjpval)
}

#' Fit network based on Pearson correlation
#'
#' Build an adjacency list of interactions based on Pearson correlation
#' of data and the lag-1 of the data. Calculate p-values based on
#' two-sided permutation test.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param adjlistfname character, path to csv file name where results will
#' be written
#' @param reps numeric, number of randomization repetitions to perform.
#' @param cores integer, number of computer cores to use in computation
FitPearson <- function(df, adjlistfname, reps = 1e4, cores = 6){
    ## build lagged data frame
    out <- MakeLagDF(df, multiple = FALSE)
    newdf <- out$df
    lagdf <- out$lagdf

    ## all correlations between unlagged and lagged columns
    cormat <- cor(newdf, lagdf, method = "pearson")
    adjpval <- PearsonRandomization(df, cormat, reps=reps, cores = cores)

    ## build adjlist with all relevant information based on
    ## correlations and p-values
    adjlist <- BuildAdjlist(adjpval, cormat)

    ## write results to file
    write.csv(adjlist, adjlistfname, quote = FALSE, row.names = FALSE)
}

#' Fit Pearson correlation network for Comte dataset
#'
#' @param basefname character, beginning of file name that will be used
#' as the basis for Lasso output file names. Should be something like
#' '../../Results/Comte-training-final-2/Comte-training-final-2'
#' @param trainfname character, path to training data
#' @param reps numeric, number of randomization repetitions to perform.
#' @param cores integer, number of computer cores to use in computation
FitPearsonComte <- function(basefname, trainfname = '../../Data/Comte-training-final/Comte-all-training-final.txt', reps = 1e4, cores = 6){
    train <- read.table(trainfname, stringsAsFactors = FALSE, header = TRUE) %>%
        select(-OBJECTID, -Period)
    adjlistfname <- paste0(basefname, "-pearson-adjlist.csv")
    FitPearson(train, adjlistfname, reps = reps, cores = cores)
}

#' Fit Pearson correlation network for Tatoosh dataset
#'
#' @param path character, path to the folders where the models for each cross
#' validation set go
#' @param datafname character, path to full dataset
#' @param crossvalfname, character, path to cross validation information
#' @param reps numeric, number of randomization repetitions to perform.
#' @param cores integer, number of computer cores to use in computation
FitPearsonTatoosh <- function(path, datafname = '../../Data/tatoosh-control-exp-cv/tatoosh-control-exp-cv-all.txt', crossvalfname = '../../Data/tatoosh-control-exp-cv/tatoosh-control-exp-cv-crossvalidation.txt', reps = 1e4, cores = 6){
    alldata <- read.table(datafname, stringsAsFactors = FALSE, header = TRUE)
    crossval <- read.table(crossvalfname, stringsAsFactors = FALSE, header = TRUE)
    folds <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    for(fold in folds){
        print(fold)
        ## only want cross validation folders
        if(basename(fold) == 'Marginals') next
        ## get crossvalidation rep number
        basefname <- basename(fold)
        repnum <- str_split(basefname, '-')[[1]]
        repnum <- repnum[length(repnum) - 1]
        cvrep <- select_(crossval, .dots = c('site', str_c('rep_', repnum)))
        ## taken from FitLasso
        datasets <- SplitData(cvrep, alldata)
        adjlistfname <- str_c(fold, '/', basefname, '-pearson-adjlist.csv')
        FitPearson(datasets$train, adjlistfname, reps = reps, cores = cores)
    }
}
