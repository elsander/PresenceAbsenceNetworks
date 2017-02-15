require(hash)
require(dplyr)
require(tidyr)
require(stringr)

#' Make a Dictionary of Children and Parents
MakeNodeDict <- function(adjlist){
    nodeDict <- hash::hash()
    uniqNodes <- unique(c(adjlist$Child, adjlist$Parent))
    for(node in uniqNodes){
        parents <- adjlist %>% dplyr::filter(Child == node) %>%
            dplyr::select(Parent)
        if(nrow(parents) > 0){
            nodeDict[[node]] <- unlist(parents, use.names = FALSE)
        } else {
            nodeDict[[node]] <- numeric(0)
        }
    }
    return(nodeDict)
}

AdjlistNumsToNames <- function(adjlist, spnames){
    adjlistCopy <- matrix(NA, nrow(adjlist), ncol(adjlist))
    names(adjlistCopy) <- names(adjlist)
    for(i in 1:length(spnames)){
        adjlistCopy[which(adjlist == (i-1))] <- spnames[i]
    }
    adjlistCopy <- as.data.frame(adjlistCopy, stringsAsFactors = FALSE)
    names(adjlistCopy) <- names(adjlist)
    if(sum(is.na(adjlistCopy)) > 0) browser()
    return(adjlistCopy)
}

JointCredibleBeta <- function(alpha, beta, pct = .95){
    cutoff <- (1-pct)/2
    interval <- qbeta(c(cutoff, 1-cutoff), shape1 = alpha, shape2 = beta)
    names(interval) <- c('lower_credible', 'upper_credible')
    return(interval)
}

OneNodePosterior <- function(child, parents, dataset, alpha = 1, beta = 1){
    ## there's some weird syntax here to get select() to accept
    ## column names from a vector. See vignette('nse') for details on
    ## select_() and .dots
    datasetCopy <- dataset
    dataset <- dplyr::select_(datasetCopy, .dots = child)
    oldChild <- child
    child <- stringr::str_c(child, '_t') ## as opposed to t-1
    dataset <- dplyr::rename_(dataset, .dots = setNames(oldChild, child)) %>%
        dplyr::bind_cols(dplyr::select_(datasetCopy, .dots = parents))
    
    dataShifted <- dplyr::bind_cols(select_(dataset, .dots = child) %>% slice(-1),
                                    select_(dataset, .dots = parents) %>%
                                    slice(-nrow(dataset)))
    ## remove columns with NAs
    dataShifted <- dataShifted[complete.cases(dataShifted),]

    ## count number of each combination of parent and child values
    countTable <- dplyr::count_(dataShifted, c(child, parents))
        
    ## make table for calculating conditional probabilities
    tmp <- lapply(vector(mode = 'list', length = length(parents) + 1),
                  function(x) return(c(0,1)))
    jointTable <- do.call(expand.grid, tmp)
    names(jointTable) <- c(child, parents)
    jointTable <- dplyr::left_join(jointTable, countTable, by = c(child, parents))
    jointTable$n[is.na(jointTable$n)] <- 0
    jointTable <- tidyr::spread_(jointTable, child, 'n')
    ## more informative column names
    names(jointTable)[names(jointTable) == '1'] <- 'alpha'
    names(jointTable)[names(jointTable) == '0'] <- 'beta'
    ## incorporate prior into posterior distribution
    jointTable[,'alpha'] <- jointTable[,'alpha'] + alpha
    jointTable[,'beta'] <- jointTable[,'beta'] + beta
    ## calculate 95% credible intervals
    jointCred <- as.data.frame(t(mapply(FUN = JointCredibleBeta,
                                        alpha = jointTable[,'alpha'],
                                        beta = jointTable[,'beta'])))
    jointTable <- jointTable %>% dplyr::bind_cols(jointCred)

    return(list(child = oldChild, parents = parents,
                joint = jointTable))
}

AllNodePosterior <- function(dataset, nodeDict, alpha = 1, beta = 1){
    ## this will be a dictionary of lists
    posteriorDict <- hash::hash()
    for(node in hash::keys(nodeDict)){
        posteriorDict[[node]] <- OneNodePosterior(node, nodeDict[[node]],
                                                  dataset,
                                                  alpha = alpha, beta = beta)
    }
    return(posteriorDict)
}

ParameterInference <- function(spfile  = '../../Data/Comte2015/Comte-Species.txt',
                               adjfile = '../../Results/Comte-training/MCMC-score-15376.6908/Comte-training-DBN-score-15376.6908-adjlist.csv',
                               datafile = '../../Data/Comte2015/Comte-training/Comte-all-training.txt'){
    if(length(adjfile) == 1){
        adjlist <- read.csv(adjfile, header = TRUE, stringsAsFactors = FALSE)
    } else {
        adjlist <- adjfile
    }

    if(length(datafile) == 1){
        dataset <- read.table(datafile, header = TRUE)
    } else {
        dataset <- datafile
    }

    ## use spfile to fill in species names if they aren't already in the adjlist
    if(is.numeric(adjlist$Child)){
        if(length(spfile) == 1){
            spnames <- as.vector(as.matrix(read.table(spfile,
                                                      header = FALSE, sep = '\n')))
        } else {
            spnames <- spfile
        }
        adjlist <- AdjlistNumsToNames(adjlist, spnames)
    }

    ## make names standard in adjlist and column names
    adjlist$Parent <- make.names(adjlist$Parent)
    adjlist$Child <- make.names(adjlist$Child)
    
    nodeDict <- MakeNodeDict(adjlist)
    ## The actual workhorse function
    posteriorDict <- AllNodePosterior(dataset, nodeDict)
    ## print(posteriorDict)
    return(posteriorDict)
}

BestInFolderParameterInference <- function(path,
                                           spfile =
                                           '../../Data/Comte2015/Comte-Species.txt',
                                           datafile = '../../Data/Comte2015/Comte-training/Comte-all-training.txt'){
    fs <- list.files(path, pattern = '.*[0-9]-adjlist\\.csv$')
    bestScore <- 100000000
    bestf <- ''
    for(f in fs){
        fsplit <- stringr::str_split(f, '-')[[1]]
        score <- as.numeric(fsplit[length(fsplit)-1])
        if(score < bestScore){
            bestScore <- score
            bestf <- f
        }
    }
    print(paste('Best structure:', bestf))
    fpath <- file.path(path, bestf)
    posteriors <- ParameterInference(spfile = spfile,
                                     adjfile = fpath,
                                     datafile = datafile)

    ## save as .RData
    fsplit <- stringr::str_split(bestf, '-adjlist\\.csv')[[1]]
    fname <- file.path(path, stringr::str_c(fsplit[1], '-parameters.RData'))
    save(posteriors, file = fname)
    return(posteriors)
}
