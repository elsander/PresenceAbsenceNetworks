require(dplyr)
require(hash)

#' Depth-First Search to find all of the necessary species for the MCMC
#' 
#' This makes it a bit more feasible to use the 32-bit bit-twiddling
#' approach to mariginalizing.
#'
#' @param node the focal species to DFS from
#' @param adjlist the network as an adjacency list, with columns Child and Parent
#'
#' @return a vector containing the focal species and all of its parents, their parents,
#' etc.
DFS <- function(node, adjlist, splist = NULL){
    ## initialize if appropriate
    if(is.null(splist)){
        splist <- node
    }

    ## recursively search through parents
    parents <- adjlist %>% dplyr::filter(Child == node) %>%
        dplyr::select(Parent) %>%
            as.matrix %>% as.vector
    for(parent in parents){
        if(parent %in% splist){
            next
        } else {
            ## mark that we've seen the parent
            splist <- c(splist, parent)
            ## find parents of the parent
            splist <- DFS(parent, adjlist, splist)
        }
    }
    return(splist)
}

#' Convert Presence-Absence Vector to Single Number
#'
#' This actually uses the numeric data type rather than int, because R ints are
#' signed (max value is 2^31).
#'
#' @param vec a vector of species state, where the vector item names are the
#' species names, and the entries in the vector are 0s and 1s corresponding to
#' the species presence or absence
#' @param spnames a vector of all species names in the network
#'
#' @return a number of type numeric, which condenses the state into a single number
#' (to be used in C for bitwise operations)
VecToIntTatoosh <- function(vec, spnames){
    state <- 0
    N <- length(spnames)
    names(vec) <- make.names(names(vec))
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

FormatRDataTatoosh <- function(rdatafile = "../../Results/tatoosh-empirical-struct/tatoosh-empirical-struct-consensus-parameters.RData", adjfile = "../../Results/tatoosh-empirical-struct/tatoosh-empirical-struct-consensus-adjlist.csv", outfile = "../../Results/tatoosh-empirical-struct/FullMarginal/tatoosh-empirical-struct-"){
    load(rdatafile)

    adjlist <- read.csv(adjfile, stringsAsFactors = FALSE)
    spnames <- unique(c(adjlist$Child, adjlist$Parent))

    ## create separate MCMCinfile for each species, to decrease the number
    ## of species to marginalize over
    for(spp in spnames){
        MCMCdf <- data.frame(Species = numeric(0),
                             State = numeric(0),
                             alpha = numeric(0),
                             beta = numeric(0))
        ## recursively find all parents, parent's parents, etc. for focal species
        splist <- DFS(spp, adjlist) %>% make.names
        ## add parameter info for each species in this list
        for(i in 1:length(splist)){
            states <- apply(posteriors[[splist[i]]]$joint,
                            MARGIN = 1,
                            FUN = VecToIntTatoosh,
                            spnames = splist)
            ## i-1 because R indexes from 1, C from 0
            tmpdf <- data.frame(Species = rep(i-1, length(states)),
                                State = states,
                                alpha = posteriors[[splist[i]]]$joint$alpha,
                                beta = posteriors[[splist[i]]]$joint$beta
                                )
            MCMCdf <- MCMCdf %>% dplyr::bind_rows(tmpdf)
        }
        dfname <- paste0(outfile, spp, '-MCMCinfile.txt')
        splistname <- paste0(outfile, spp, '-parentlist.txt')

        print(list(MCMCMdf = MCMCdf, splist = splist))
        write.table(MCMCdf, dfname, col.names = FALSE, row.names = FALSE, quote = FALSE)
        write.table(splist, splistname, col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
}
