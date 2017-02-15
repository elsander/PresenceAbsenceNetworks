library(dplyr)
library(stringr)

addNAsComte <- function(comte, outfname){
    nNA <- length(unique(comte[,'OBJECTID']))
    comte2 <- as.data.frame(matrix(NA, nrow(comte) + nNA, ncol(comte)))
    comte2[1,] <- comte[1,]

    k <- 2
    for(i in 2:nrow(comte)){
        if(comte[i,1] != comte[i-1,1]){
            comte2[k,] <- rep(NA, ncol(comte))
            comte2[k+1,] <- comte[i,]
            k <- k + 2
        } else {
            comte2[k,] <- comte[i,]
            k <- k + 1
        }
    }
    names(comte2) <- names(comte)

    write.table(comte2, outfname, quote = FALSE, row.names = FALSE)
}

onePlotFiles <- function(comte, plots, setName){
    for(p in plots){
        onePlotData <- dplyr::filter(comte, OBJECTID == p) %>%
            dplyr::select(-OBJECTID, -Period)
        fname <- stringr::str_c('../../Data/Comte-',
                                setName, '/Comte-', setName,
                                '-', p, '.txt')
        write.table(onePlotData, fname, row.names = FALSE,
                    col.names = FALSE, quote = FALSE)
    }
}

subsamplePlots <- function(){
    set.seed(216584)

    comte <- read.table('../../Data/Comte-data/Comte-Presence-Absence.txt',
                        header = TRUE, stringsAsFactors = FALSE)

    ## Remove Barbus_barbus, because it is piscivorous, but little info
    ## was available for it in the literature
    comte <- comte[,-which(names(comte) == 'Bab')]

    allplots <- unique(comte$OBJECTID)
    ##shuffle plot order
    allplots <- sample(allplots, size = length(allplots), replace = FALSE)

    ##split into training and cross-validation sets (60/40)
    trainingplots <- allplots[1:476] ## 476
    cvplots <- allplots[477:length(allplots)] ## 315

    trainingSet <- filter(comte, OBJECTID %in% trainingplots)
    cvSet <- filter(comte, OBJECTID %in% cvplots)

    system('mkdir ../../Data/Comte-training-final/')
    system('mkdir ../../Data/Comte-CV-final/')
    
    ## add NAs and write to table
    addNAsComte(trainingSet,
                '../../Data/Comte-training-final/Comte-all-training-final.txt')
    addNAsComte(cvSet,
                '../../Data/Comte-CV-final/Comte-all-CV-final.txt')

    ## write data from individual sites to the appropriate folder
    onePlotFiles(comte, trainingplots, 'training-final')
    onePlotFiles(comte, cvplots, 'CV-final')
}
