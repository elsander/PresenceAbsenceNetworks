library(dplyr)
library(ggplot2)

#' Calculate precision and recall for one model network
#'
#' @param empirical a data frame of empirical interactions, with columns Child and Parent
#' @param consensus a data frame of predicted interactions, with columns Child and Parent
#' @param spnames a character vector of all species in the ecological system
#'
#' @return a list containing \code{precision}, the proportion of positives which
#' are true positives, and \code{recall}, the proportion of true positives captured
#' by the model.
GetPrecisionRecall <- function(consensus, empirical, spnames){
    matches <- inner_join(empirical, consensus, by = c("Child", "Parent"))
    truePos <- nrow(matches)
    fullPos <- nrow(consensus)
    fullNeg <- nrow(empirical)
    precision <- truePos/fullPos
    recall <- truePos/fullNeg
    return(list(precision = precision, recall = recall))
}

#' Build a single randomized network for p-value calculation
#'
#' @param consensus a data frame of predicted interactions, with columns Child and Parent
#' @param spnames a character vector of all species in the ecological system
#'
#' @return a data frame containing columns Child and Parent. The data frame will
#' maintain the number of children for each parent species as in \code{consensus},
#' but will randomize the identities of the children.
BuildRandomConsensus <- function(consensus, spnames){
    ## build a random DBN where each child species has the same number
    ## of parents
    randCon <- data.frame(Child = consensus$Child,
                          Parent = rep(NA, nrow(consensus)),
                          stringsAsFactors = FALSE)
    for(spname in unique(randCon$Child)){
        nparents <- randCon %>% filter(Child == spname) %>% nrow
        parents <- sample(spnames[spnames != spname], nparents, replace=FALSE)
        randCon$Parent[randCon$Child == spname] <- parents
    }
    return(randCon)
}

#' Plot distribution of randomized precision and recall
#'
#' @param precision a vector of precision values from randomized DBNs
#' @param recall a vector of recall values from randomized DBNs
#' @param truepr a list containing the empirical precision and recall
#'
#' @return a ggplot object containing kernels of the precision and recall,
#' with vertical lines marking the empirical values
PlotPrecisionRecall <- function(precision, recall, truepr){
    prdf <- data.frame(variable = c(rep('Precision', length(precision)),
                               rep('Recall', length(recall))),
                       value = c(precision, recall),
                       empirical = c(rep(truepr$precision, length(precision)),
                                     rep(truepr$recall, length(recall))))
    g <- ggplot(prdf, aes(x = value, fill = variable, colour = variable)) +
        theme_bw() + 
        geom_density(alpha = .2) + 
        ## facet_wrap(~ variable, scales = "free_x") +
        geom_vline(aes(xintercept = empirical))
    plot(g)
    return(g)
}

#' Calculate precision and recall p-values by randomizing
#'
#' Run a randomization to estimate the probability of getting precision
#' and recall values higher than observed from the model prediction.
#' Optionally draws a plot to file. Note that self-loops will be removed,
#' since they are not a very ecologically informative part of the model.
#' 
#' @param empirical a data frame of empirical interactions, with columns Child and Parent
#' @param consensus a data frame of predicted interactions, with columns Child and Parent
#' @param spnames a character vector of all species in the ecological system
#' @param n the number of randomizations to do (will be rounded up to the nearest 100)
#' @param plotfname the path to the file where plots will be drawn. If NA, no plots will
#' be drawn.
#'
#' @return a list containing: \code{precisionprob}, the probability of randomly obtaining
#' a higher precision than the one observed; and \code{recallprob}, the probability of
#' randomly obtaining a higher recall than the one observed. Note that the p-values
#' should be the same for precision and recall, based on the randomization method
#' that is currently being used.
RandomizePrecisionRecall <- function(empirical, consensus, spnames,
                                     n = 10000, plotfname = NA){
    ## filter out self-loops, since they're not very interesting
    empirical <- empirical %>% filter(Child != Parent)
    consensus <- consensus %>% filter(Child != Parent)
    truepr <- GetPrecisionRecall(consensus, empirical, spnames)

    listsize <- 100
    conList <- vector(mode = 'list', length = listsize)
    conList <- lapply(conList, function(x) return(consensus))
    reps <- ceiling(n/listsize)
    precision <- vector(mode = 'list', length = reps)
    recall <- vector(mode = 'list', length = reps)
    for(i in 1:reps){
        ## print(i/reps)
        randList <- lapply(conList, BuildRandomConsensus, spnames = spnames)
        pr <- lapply(randList, GetPrecisionRecall, empirical = empirical,
                     spnames = spnames)
        precision[[i]] <- unlist(lapply(pr, function(x) return(x$precision)))
        recall[[i]] <- unlist(lapply(pr, function(x) return(x$recall)))
    }
    precision <- unlist(precision)
    recall <- unlist(recall)

    precisionprob <- sum(precision >= truepr$precision)/length(precision)
    recallprob <- sum(recall >= truepr$recall)/length(recall)

    if(!is.na(plotfname)){
        pdf(plotfname)
        plots <- PlotPrecisionRecall(precision, recall, truepr)
        dev.off()
    }
    
    return(list(precision = truepr$precision,
                precisionprob = precisionprob,
                recall = truepr$recall,
                recallprob = recallprob))
}

#' Simple wrapper for RandomizePrecisionRecall that reads in files
PrecisionRecallScript <- function(empfname = '../../Data/tatoosh-empirical-struct-adjlist.csv', confname = '../../Results/tatoosh-control-exp-cv/tatoosh-control-exp-cv-consensus-adjlist.csv', spfname = '../../Data/tatoosh-control-exp-cv/tatoosh-control-exp-cv-spnames.txt', plotfname = NA, n = 10000){
    empirical <- read.csv(empfname, stringsAsFactors = FALSE)
    consensus <- read.csv(confname, stringsAsFactors = FALSE)
    spnames <- read.table(spfname, header = FALSE, stringsAsFactors = FALSE)

    out <- RandomizePrecisionRecall(empirical, consensus, spnames, n=n,
                                    plotfname = plotfname)
    return(list(precision = out$precision, precisionprob = out$precisionprob,
                recall = out$recall, recallprob = out$recallprob))
}
