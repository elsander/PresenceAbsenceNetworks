library(dplyr)
library(stringr)

#' Get sample period from strings in Periode1 and Periode2
#'
#' @param period character, period in the format "P1", "P2", etc
#'
#' @return numeric, period number
removeP <- function(period){
    period <- str_extract(period, '[0-9]')
    return(as.numeric(period))
}

#' Get the first presence-absence state from the transition string
#'
#' If transition string is "extirpated" or "persisted", returns 1 (species
#' was present in the first period). If transition string is "colonized"
#' or "unoccupied", returns 0 (species was absent in the first period).
#' Otherwise, returns NA.
#'
#' @param transition character, presence-absence transition between two
#' periods.
#'
#' @return numeric: 1 (present), 0 (absent), or NA
mutateFirst <- function(transition){
    if(transition == 'extirpated' || transition == 'persisted'){
        state <- 1
    } else {
        if(transition == 'colonized' || transition == 'unoccupied'){
            state <- 0
        } else {
            state <- NA
        }
    }
    return(state)
}

#' Get the second presence-absence state from the transition string
#'
#' If transition string is "colonized" or "persisted", returns 1 (species
#' was present in the second period). If transition string is "extirpated"
#' or "unoccupied", returns 0 (species was absent in the second period).
#' Otherwise, returns NA.
#'
#' @param transition character, presence-absence transition between two
#' periods.
#'
#' @return numeric: 1 (present), 0 (absent), or NA
mutateLast <- function(transition){
    if(transition == 'colonized' || transition == 'persisted'){
        state <- 1
    } else {
        if(transition == 'extirpated' || transition == 'unoccupied'){
            state <- 0
        } else {
            state <- NA
        }
    }
    return(state)
}

#######################################################################
#######################################################################
##                                                                   ##
## Script                                                            ##
##                                                                   ##
#######################################################################
#######################################################################

## we can construct the original presence absence based on the first or
## second period (we need both to get the first and last time periods),
## so we can create a copy to do the conversion twice
comte <- read.table('../../Data/Comte-data/data_Species_dynamics.txt',
                    sep = "", header = TRUE,
                    stringsAsFactors = FALSE)
comte <- bind_cols(select(comte, -Periode1),
                   transmute(comte, Periode1 = removeP(Periode1)))
comte <- bind_cols(select(comte, -Periode2),
                   transmute(comte, Periode2 = removeP(Periode2)))
comte <- as.data.frame(comte)
comte2 <- comte

## convert dataset of transitions to presence-absence dataset
tmp <- as.matrix(select(comte, -OBJECTID, -Periode1, -Periode2))
for(i in 1:nrow(tmp)){
    for(j in 1:ncol(tmp)){
        tmp[i,j] <- mutateFirst(tmp[i,j])
    }
}
tmp <- as.data.frame(tmp)
comte <- bind_cols(select(comte, OBJECTID, Periode1), tmp)
tmp <- as.matrix(select(comte2, -OBJECTID, -Periode1, -Periode2))
for(i in 1:nrow(tmp)){
    for(j in 1:ncol(tmp)){
        tmp[i,j] <- mutateLast(tmp[i,j])
    }
}
tmp <- as.data.frame(tmp)
comte2 <- bind_cols(select(comte2, OBJECTID, Periode2), tmp)
comte <- rename(comte, Period = Periode1)
comte2 <- rename(comte2, Period = Periode2)

## join presence and absence data from the before and after time periods
complete <- full_join(comte, comte2)
## arrange by site, then by period collected
complete <- arrange(complete, OBJECTID, Period)
write.table(complete, '../../Data/Comte-data/Comte-Presence-Absence.txt',
            quote = FALSE, row.names = FALSE)
