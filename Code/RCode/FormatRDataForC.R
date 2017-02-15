require(hash)
require(dplyr)

#' Convert Presence-Absence Vector to Single Number
#'
#' This actually uses the numeric data type rather than int, because R ints are
#' signed (max value is 2^31).
VecToInt <- function(vec, spnames){
    state <- 0
    N <- length(spnames)
    ## for each possible species in the posterior
    for(i in 1:N){
        ## if that species is part of the joint posterior
        if(spnames[i] %in% names(vec)){
            ## if that species is present
            if(vec[spnames[i]] == 1){
                ## subtract 1 because R indexes at 1
                state = state + 2^(i-1)
            }
        }
    }
    if(state > 2^32) warning("This state is too large to represent in the MCMC C code!")
    return(state)
}

################################################################################
## Format needed:
## Space-separated data frame with no header, with columns:
## - Species # (base 10)
## - System state (base 10, converted from binary presence/absence)
##   (note that only parent state should contain 1s)
## - alpha parameter for beta distribution posterior for the system state
## - beta parameter for beta distribution posterior for the system state

FormatRData <- function(rdatafile = "../../Results/Comte-training/MCMC-score-15376.6908/Comte-training-DBN-score-15376.6908-parameters.RData", trainingfname = "../../Data/Comte2015/Comte-training/Comte-all-training.txt", outfile = "../../Results/Comte-training/MCMC-score-15376.6908/FullMarginal/Comte-training-DBN-score-15376.6908-MCMCinfile.txt" ){

    ## this will load the object "posteriors"
    load(rdatafile)

    ## Species order is important because it determines the integer value for the state
    ## We can get this order from one of the original data files
    training <- read.table(trainingfname, header = TRUE, stringsAsFactors = FALSE)
    ## get species names, removing OBJECTID and Period
    spnames <- names(training)
    spnames <- spnames[-which(spnames %in% c('OBJECTID', 'Period'))]

    MCMCdf <- data.frame(Species = numeric(0),
                         State = numeric(0),
                         alpha = numeric(0),
                         beta = numeric(0))

    for(i in 1:length(spnames)){
        states <- apply(posteriors[[spnames[i]]]$joint, MARGIN = 1, FUN = VecToInt,
                        spnames = spnames)
        ## i-1 because R indexes from 1, C from 0
        tmpdf <- data.frame(Species = rep(i-1, length(states)),
                            State = states,
                            alpha = posteriors[[spnames[i]]]$joint$alpha,
                            beta = posteriors[[spnames[i]]]$joint$beta
                            )
        MCMCdf <- MCMCdf %>% dplyr::bind_rows(tmpdf)
    }

    write.table(MCMCdf, outfile, col.names = FALSE, row.names = FALSE, quote = FALSE)
}
