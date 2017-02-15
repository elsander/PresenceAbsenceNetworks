require(dplyr)
source('ParameterInference.R')

FitDisconnectedPosterior <- function(datafile,
                                     outfname = '../../Results/banjo_DBN/Comte-training/Comte-training-DBN-Disconnected-adjlist.csv'){
    dataset <- read.table(datafile, header = TRUE)
    if('OBJECTID' %in% names(dataset)){
        dataset <- dataset %>% dplyr::select(-OBJECTID, -Period)
    } else {
        dataset <- dataset %>% dplyr::select(-site)
    }
    n <- ncol(dataset)
    ## create disconnected adjlist based on number of species
    adjlist <- data.frame(Child = 0:(n-1), Parent = 0:(n-1))
    write.table(adjlist, outfname, quote = FALSE, row.names = FALSE, sep = ',')
}

ParameterInferenceDisconnected <- function(spfile  = '../../Data/Comte2015/Comte-Species.txt', datafile = '../../Data/Comte2015/Comte-training/Comte-all-training.txt', outadjfname = '../../Results/banjo_DBN/Comte-training/Comte-training-DBN-Disconnected-adjlist.csv',
                                           writeoutadj = FALSE){
    ## only create the adjlist if we don't have it already
    if(writeoutadj){
        FitDisconnectedPosterior(datafile, outadjfname)
    }
    posteriors <- ParameterInference(spfile, outadjfname, datafile)
    ## adjbase <- basename(outadjfname)
    postfname <- stringr::str_split(outadjfname, '-adjlist.csv')[[1]][1]
    postfname <- stringr::str_c(postfname, '-parameters.RData')
    save(posteriors, file = postfname)
    return(list(posteriors = posteriors, fname = postfname))
}
