require(dplyr)
source('ParameterInference.R')
source('ParameterInferenceDisconnectedWeb.R')
source('PredictionCoinFlip.R')

ProbPredict <- function(dataPt, alpha, beta){
    ## this function calculates the probability of drawing dataPt
    ## from a bernoulli distribution, where the p parameter for the
    ## bernoulli is drawn from a beta distribution with parameters
    ## alpha and beta.
    if(dataPt == 1){
        return(alpha/(alpha + beta))
    } else {
        return(beta/(alpha + beta))
    }
}

PredictOneSpp <- function(sppPosterior, obs){
    obsBefore <- as.data.frame(obs[1,])
    names(obsBefore) <- names(obs)
    obsAfter <- as.data.frame(obs[2,])
    names(obsAfter) <- names(obs)
    ## get the appropriate posterior distribution to draw from
    obsPosterior <- dplyr::left_join(obsBefore, sppPosterior$joint,
                                     by = sppPosterior$parents)

    #####################
    ## HARDCODED PRIOR ##
    #####################
    ## if there are NAs, the system state was not observed in the training
    ## set. This means that we should just use the prior distribution
    ## as the posterior. Note that the prior is hardcoded here as
    ## alpha = 1, beta = 1.
    if(!all(!is.na(obsPosterior))){
        obsPosterior$alpha <- 1
        obsPosterior$beta <- 1
    }
    
    ## analytically calculate the probability of predicting the value correctly
    predictedVal <- ProbPredict(obsAfter[,sppPosterior$child],
                                obsPosterior$alpha, obsPosterior$beta)

    return(predictedVal)
}

PredictOneTimept <- function(posteriors, obs){
    probs <- matrix(NA, 1, length(posteriors))
    probs <- as.data.frame(probs)
    names(probs) <- names(posteriors)
    for(i in names(posteriors)){
        sppPosterior <- posteriors[[i]]
        sppObs <- dplyr::select_(obs, .dots = c(sppPosterior$parents,
                                          sppPosterior$child))
        probs[[i]] <- PredictOneSpp(sppPosterior, sppObs)
    }
    return(probs)
}

PredictOutOfSample <- function(posteriors, predictionData){
    probs <- vector(mode = 'list', length = nrow(predictionData) - 1)
    k <- 1
    for(i in 1:(nrow(predictionData)-1)){
        if(i %% 100 == 0) print(i/nrow(predictionData))
        ## if there are NAs, we're at a border between plots in the dataset
        if(sum(is.na(predictionData[i:(i+1),])) == 0){
            probs[[k]] <- PredictOneTimept(posteriors,
                                           predictionData[i:(i+1),])
            k <- k + 1
        }
    }

    ## remove NULL values at the end of the list for Reduce
    probs <- probs[!sapply(probs, is.null)]
    ## convert to data.frame
    ## remove first element in Reduce to use it as the base case
    probDF <- Reduce(dplyr::bind_rows, probs[-1], probs[[1]])
    return(probDF)
}

PredictionAccuracy <- function(posteriorRData,
                               predictionFile =
                               '../../Data/Comte2015/Comte-CV/Comte-all-CV.txt'){
    load(posteriorRData)
    predictionData <- read.table(predictionFile, header = TRUE,
                                 stringsAsFactors = FALSE)
    accuracyDist <- PredictOutOfSample(posteriors, predictionData)
    return(accuracyDist)
}

PredictionModelAndNulls <- function(posteriorRData = "Comte-training-DBN-score-15376.6908-structure.RData", disconnectedRData = "Comte-training-DBN-Disconnected-predictions.RData", spFile = '../../Data/Comte2015/Comte-Species.txt', trainFile = '../../Data/Comte2015/Comte-training/Comte-all-training.txt', cvFile = '../../Data/Comte2015/Comte-CV/Comte-all-CV.txt'){
    print('Fitting Coin Flip Null...')
    coinFlipProbs <- PredictOutOfSampleCoinFlip(trainFile, cvFile)
    print('Done!')
    print('Fitting Disconnected Network Null...')
    disconnectedProbs <- PredictionAccuracy(disconnectedRData, predictionFile = cvFile)
    print('Done!')
    print('Fitting Model...')
    modelProbs <- PredictionAccuracy(posteriorRData, predictionFile = cvFile)
    print('Done!')
    predictions <- list(coin = coinFlipProbs, disconnected = disconnectedProbs,
                       model = modelProbs)
    postfname <- stringr::str_split(posteriorRData, 'parameters.RData')[[1]][1]
    postfname <- stringr::str_c(postfname, 'predictions.RData')
    save(predictions, file = postfname)
    return(predictions)
}
