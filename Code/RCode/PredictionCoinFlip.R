require(dplyr)

#' Calculate Probability of Observing Data Given a Coin Flip Weight
#'
#' @param dataPt a vector of 0s and 1s, representing presence/absence
#' of a species at a given time
#' @param weight a value between 0 and 1, representing the probability
#' of observing that species in the training set.
#'
#' @return a vector of length \code{length(dataPt)}, containing the
#' probabilities of observing the values in \code{dataPt}.
#'
#' @export
ProbCoinFlip <- function(dataPt, weight){
    ## weight is the weighted coin flip probability
    ## create object of same size as dataPt but filled with 0s
    probs <- dataPt*0
    probs[dataPt == 1] <- weight
    probs[dataPt != 1] <- 1-weight
    return(probs)
}

#' Fit the Coin Flip Model to Training Data
#'
#' @param trainFile A path to a training set, or a data frame containing
#' a training set.
#' @param comte a logical flag for if the Comte dataset is being used.
#' Setting \code{comte=TRUE} assumes that the dataset contains metadata
#' columns \code{OBJECTID} and \code{Period}. Setting \code{comte=FALSE}
#' assumes that the dataset contains metadata column \code{site}.
#'
#' @return a vector with as many values as feature columns in the training set.
#' The vector contains the coin flip weight; that is, the probability that
#' species is present at any given time.
#'
#' @export
GetCoinFlipWeights <- function(trainFile, comte = TRUE){
    if(length(trainFile) == 1) {
        train <- read.table(trainFile, header = TRUE, stringsAsFactors = FALSE)
    } else {
        train <- trainFile
    }
    
    train <- train %>% dplyr::filter(complete.cases(train))
    if(comte){
        train <- dplyr::select(train, -OBJECTID, -Period)
    } else {
        train <- dplyr::select(train, -site)
    }
    weights <- mapply(mean, train)
    return(weights)
}

PredictOutOfSampleCoinFlip <- function(trainFile, cvFile, comte = TRUE){
    weights <- GetCoinFlipWeights(trainFile, comte = comte)
    if(length(cvFile) == 1){
        cv <- read.table(cvFile, header = TRUE, stringsAsFactors = FALSE)
    } else {
        cv <- cvFile
    }
    
    cv <- cv %>% dplyr::filter(complete.cases(cv))
    if(comte){
        cv <- dplyr::select(cv, -OBJECTID, -Period)
    } else {
        cv <- dplyr::select(cv, -site)
    }
    ## this code relies on the column ordering to be the same, so
    ## we check for it here
    if(all(names(cv) == names(weights))){
        probs <- mapply(ProbCoinFlip, cv, weight = weights)
        return(probs)
    } else {
        stop('Columns in train and cv must be the same names and in the same order')
    }
}
