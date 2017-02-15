require(dplyr)

source('ParameterInference.R')
source('ParameterInferenceDisconnectedWeb.R')
source('PredictionCoinFlip.R')
source('PredictionAccuracy.R')

RemoveRows <- function(crossvalRep, alldata){
    ## remove rows which are used for cross validation
    ## replace them with a row of NAs to preserve the
    ## non-contiguity of the data points before and after the
    ## CV datapoints.
    paramdata <- numeric(0)
    for(i in 1:nrow(crossvalRep)){
        ## filter to the site of interest
        currSite <- crossvalRep$site[i]
        sitedata <- dplyr::filter(alldata, site == currSite)

        ## remove the row specified in the crossvalidation info
        toRemove <- crossvalRep[i,2]
        sitedata[toRemove,] <- rep(NA, ncol(alldata))

        ## append to paramdata
        if(length(paramdata) == 0){
            paramdata <- sitedata
        } else {
            ## append NA row in between sites
            paramdata <- rbind.data.frame(paramdata, rep(NA, ncol(alldata))) %>%
                dplyr::bind_rows(sitedata)
        }
    }
    paramdata <- as.data.frame(paramdata)
    return(paramdata)
}

GetPredictionRows <- function(crossvalRep, alldata){
    predictdata <- numeric(0)
    for(i in 1:nrow(crossvalRep)){
        ## filter to the site of interest
        currSite <- crossvalRep$site[i]
        sitedata <- dplyr::filter(alldata, site == currSite)

        ## extract the row to predict, and the previous row, which
        ## will be used to predict it
        toRemove <- crossvalRep[i,2]
        sitedata <- sitedata[(toRemove-1):toRemove,]

        ## append to predictdata
        if(length(predictdata) == 0){
            predictdata <- sitedata
        } else {
            ## append NA row in between sites
            predictdata <- rbind.data.frame(predictdata, rep(NA, ncol(alldata))) %>%
                dplyr::bind_rows(sitedata)
        }
    }
    predictdata <- as.data.frame(predictdata)
    return(predictdata)
}

PredictCoinFlip <- function(crossval, alldata){
    ############################################################################
    ## NOTE: named list? add a column for which CV set the predictions are from?
    ## Might be useful to know the variance in predictive power across CV sets
    ############################################################################
    probs <- vector(mode = 'list', length = ncol(crossval) - 1)
    ## iterate from 2 to skip the 'site' column
    for(i in 2:ncol(crossval)){
        ## take out cross-validation rows
        paramData <- RemoveRows(crossval[, c(1,i)], alldata)
        ## extract rows for prediction
        predictData <- GetPredictionRows(crossval[, c(1,i)], alldata)
        ## parameterize and predict based on subset of data
        ## from PredictionCoinFlip.R
        probs[[i-1]] <- PredictOutOfSampleCoinFlip(paramData, predictData,
                                                   comte = FALSE)
    }
    ############################################################################
    ## This is taken from PredictionAccuracy.R
    ## first we are joining across all prediction points in a CV set,
    ## and then again here, across all CV sets.
    ############################################################################
    ## convert to data.frame
    ## remove first element in Reduce to use it as the base case
    ## each element will be a matrix, not a data.frame so we should
    ## use rbind instead of bind_rows here
    probDF <- Reduce(rbind, probs[-1], probs[[1]]) %>%
        as.data.frame
    return(probDF)
}

PredictAdjlist <- function(adjlist, crossval, alldata, spfile = NA){
    ############################################################################
    ## NOTE: named list? add a column for which CV set the predictions are from?
    ## Might be useful to know the variance in predictive power across CV sets
    ############################################################################
    probs <- vector(mode = 'list', length = ncol(crossval) - 1)
    ## iterate from 2 to skip the 'site' column
    for(i in 2:ncol(crossval)){
        ## take out cross-validation rows
        paramData <- RemoveRows(crossval[, c(1,i)], alldata)
        ## parameterize based on subset of data
        ## from ParameterInference.R
        posteriors <- ParameterInference(spfile = spfile,
                                         adjfile = adjlist,
                                         datafile = paramData)
        predictData <- GetPredictionRows(crossval[, c(1,i)], alldata)
        ## from PredictionAccuracy.R
        probs[[i-1]] <- PredictOutOfSample(posteriors, predictData)
    }
    ############################################################################
    ## This is taken from PredictionAccuracy.R
    ## first we are joining across all prediction points in a CV set,
    ## and then again here, across all CV sets.
    ############################################################################
    ## convert to data.frame
    ## remove first element in Reduce to use it as the base case
    probDF <- Reduce(dplyr::bind_rows, probs[-1], probs[[1]]) %>%
        as.data.frame
    return(list(posteriors = posteriors, predictprobs = probDF))
}

PredictTatoosh <- function(pathToResults, pathToData){
    consensusfile <- list.files(pathToResults, pattern = ".*consensus.*.csv$",
                                full.names = TRUE)
    cvfile <- list.files(pathToData, pattern = ".*crossvalidation.*",
                         full.names = TRUE)
    disconfile <- list.files(pathToResults, '/..',
                             pattern = ".*disconnected-adjlist.*",
                             full.names = TRUE)
    alldatafile <- list.files(pathToData, pattern = ".*-all.txt$",
                              full.names = TRUE)
    spfile <- list.files(pathToData, pattern = ".*-spnames.*",
                         full.names = TRUE)

    consensusadj <- read.csv(consensusfile, stringsAsFactors = FALSE) %>%
        select(Child, Parent) %>%
            as.data.frame
    disconadj <- read.csv(disconfile, stringsAsFactors = FALSE) %>%
        select(Child, Parent) %>%
            as.data.frame
    crossval <- read.table(cvfile, header = TRUE, stringsAsFactors = FALSE)
    alldata <- read.table(alldatafile, header = TRUE, stringsAsFactors = FALSE)

    print('Predicting model...')
    model <- PredictAdjlist(consensusadj, crossval, alldata)
    print('Predicting disconnected network...')
    disconnected <- PredictAdjlist(disconadj, crossval, alldata, spfile = spfile)
    print('Predicting weighted coin flip...')
    coin <- PredictCoinFlip(crossval, alldata)
    predictions <- list(coin = coin,
                        disconnected = disconnected$predictprobs,
                        model = model$predictprobs)
    posteriors <- list(model = model$posteriors,
                       disconnected = disconnected$posteriors)
    return(list(posteriors = posteriors, predictions = predictions))
}

PredictTatooshOneCVSet <- function(pathToResults, pathToData, cvnum){
    ## cvnum is the cross validation set currently being considered
    consensusfile <- list.files(pathToResults, pattern = ".*consensus.*.csv$",
                                full.names = TRUE)
    cvfile <- list.files(pathToData, pattern = ".*crossvalidation.*",
                         full.names = TRUE)
    disconfile <- list.files(paste0(pathToResults, '/..'),
                             pattern = ".*disconnected-adjlist.*",
                             full.names = TRUE)
    alldatafile <- list.files(pathToData, pattern = ".*-all.txt$",
                              full.names = TRUE)
    spfile <- list.files(pathToData, pattern = ".*-spnames.*",
                         full.names = TRUE)

    consensusadj <- read.csv(consensusfile, stringsAsFactors = FALSE) %>%
        select(Child, Parent) %>%
            as.data.frame
    disconadj <- read.csv(disconfile, stringsAsFactors = FALSE) %>%
        select(Child, Parent) %>%
            as.data.frame
    crossval <- read.table(cvfile, header = TRUE, stringsAsFactors = FALSE)
    alldata <- read.table(alldatafile, header = TRUE, stringsAsFactors = FALSE)

    ## only keep the column for the site name and cv set of interest
    crossval <- crossval[, c('site', paste0('rep_', cvnum))]
    
    print('Predicting model...')
    model <- PredictAdjlist(consensusadj, crossval, alldata)
    print('Predicting disconnected network...')
    disconnected <- PredictAdjlist(disconadj, crossval, alldata, spfile = spfile)
    print('Predicting weighted coin flip...')
    coin <- PredictCoinFlip(crossval, alldata)
    predictions <- list(coin = coin,
                        disconnected = disconnected$predictprobs,
                        model = model$predictprobs)
    posteriors <- list(model = model$posteriors,
                       disconnected = disconnected$posteriors)
    return(list(posteriors = posteriors, predictions = predictions))

}
