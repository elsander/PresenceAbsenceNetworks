library(dplyr)
library(glmnet)
library(stringr)
library(boot)

#' Create lagged predictor data frame for lasso
#'
#' Create a data frame of lag-1 predictors and unlagged response variables
#' for lasso regression
#'
#' @param df data.frame, each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param multiple logical, flag for if multiple interactions should be
#' included in lagdf
#'
#' @return list containing two data.frames: \code{df}, which is the original
#' data frame, but with NA rows removed, and \code{lagdf}, which is the lag-1
#' of the original data frame, also with NA rows removed
MakeLagDF <- function(df, multiple = FALSE){
    ## create lagged data to use as predictors
    lagdf <- df
    names(lagdf) <- paste0('lag.', names(lagdf))

    ## lag lagdf by 1 with respect to df
    lagdf <- lagdf[1:(nrow(lagdf) - 1),]
    df <- df[2:nrow(df),]
    ## rows that have NAs for either should be removed from both dfs
    toRM <- complete.cases(lagdf) & complete.cases(df)
    lagdf <- lagdf[toRM,]
    df <- df[toRM,]

    ## add multiple interactions
    if(multiple){
        ## the dummy column makes it possible to use the
        ## dot syntax for model.matrix
        lagdf$dummy <- lagdf[,1]
        ## add higher order interactions and remove the intercept
        ## that model.matrix adds
        lagdf <- model.matrix(dummy ~ (.)^2, lagdf) %>%
            as.data.frame %>% select(-`(Intercept)`)
    }
    
    return(list(df = df, lagdf = lagdf))
}

#' Convert multiple interactions to single interactions
#'
#' Replace multiple interactions with single interactions
#' in an adjacency list, for easier network analysis
#'
#' @param adjlist data.frame which may contain multiple interactions,
#' containing three rows: Parent, the predictor variables; Child, the
#' dependent variable; and Sign, the sign of the Parent's affect on the
#' presence/absence of the Child
#'
#' @return data.frame, similar to adjlist, but parents of the form 'X:Y'
#' will be split into two rows with 'X' and 'Y'. Duplicate rows will be
#' merged, so that each Parent-Child combination appears at most once. If
#' splitting multiple interactions would create rows with the same Parent
#' and Child but conflicting signs, the sign will be set to NA.
SplitMultiple <- function(adjlist){
    ## test for empty data.frame
    if(nrow(adjlist) == 0){
        return(adjlist)
    }
    
    newadj <- adjlist %>% distinct %>% filter(!(str_detect(Parent, ":")))
    ## iterate over multiple interaction rows
    multadj <- filter(adjlist, str_detect(Parent, ":"))
    ## test for empty data.frame
    if(nrow(multadj) == 0){
        return(newadj)
    }
    for(i in 1:nrow(multadj)){
        parents <- str_split(multadj$Parent[i], ":")[[1]]
        child <- multadj$Child[i]
        intsign <- multadj$Sign[i]
        for(parent in parents){
            rowmatch <- filter(newadj, Parent == parent,
                               Child == child)
            if(nrow(rowmatch) > 0){
                ## avoid using NA in != statement, since it results in NA
                if(is.na(rowmatch$Sign[1])){
                    next
                } else {
                    ## if this parent/child edge is already present,
                    ## change the sign to NA if needed, otherwise continue
                    if(rowmatch$Sign[1] != intsign){
                        newadj$Sign[newadj$Parent == parent & newadj$Child == child] <- NA
                    } else {
                        next
                    }
                }
            } else {
                ## if the edge isn't present, add it
                newadj <- newadj %>% bind_rows(data.frame(Parent = parent,
                                                          Child = child,
                                                          Sign = intsign))
            }
        }
    }
    return(newadj)
}

#' Add predictors kept by the lasso to an adjlist
#'
#' Create an adjlist with all non-zero predictors in the lasso,
#' and 
#'
#' @param fitcoef matrix, row names are predictors (parent species)
#' and values in the column are beta values
#'
#' @param spname character, name of the child species whose presence/absence
#' was predicted by the model
#' 
#' @param adjlistfname character, name of the file to which interactions
#' will be appended
#'
#' @param multiple logical, flag for if multiple interactions were used in
#' fitting the lasso
AddToAdjlist <- function(fitcoef, spname, adjlistfname, multiple = FALSE){
    fitcoef <- as.matrix(fitcoef)

    ## remove intercept
    fitcoef <- fitcoef[-which(rownames(fitcoef) == '(Intercept)'),,drop = FALSE]
    ## remove 'lag.' from names
    fitnames <- str_replace_all(rownames(fitcoef), "lag.", "")
    rownames(fitcoef) <- fitnames
    
    ## which values in the matrix are nonzero?
    inds <- which(fitcoef != 0)
    ## create adjlist from remaining beta values
    adjlist <- data.frame(Parent = rownames(fitcoef)[inds],
                          Child = rep(spname, length(inds)),
                          Sign = sign(fitcoef[inds]))

    if(multiple){
        multfname <- str_c(adjlistfname, '.multint')
        ## write network with multiple interactions for reference
        write.table(adjlist, multfname, append = TRUE,
                    col.names = !file.exists(adjlistfname),
                    row.names = FALSE, quote = FALSE, sep = ',')
        ## split multiple interactions into single interactions
        adjlist <- SplitMultiple(adjlist)
    }

    ## write adjlist for network analysis
    write.table(adjlist, adjlistfname, append = TRUE,
                col.names = !file.exists(adjlistfname),
                row.names = FALSE, quote = FALSE, sep = ',')
}

#' Fit a Lasso regression to every species in a data frame
#'
#' Fit a Lasso regression to species in a data frame, predicting
#' with lag-1s of all species in the data frame. Write non-zero
#' coefficients to an adjacency list, and save lasso output to RData
#' file.
#'
#' @param df data.frame, training data for model to be fit on.
#' Each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param adjlistfname character, file path where adjacency list links will
#' be saved
#' @param lassofname character, file path where lasso RData will be saved
#' @param multiple logical, flag for if multiple interactions should be
#' included in lagdf
#'
#' @return list, each element of the list contains lasso results to predict one
#' species. The name of each element is the species name of the response
#' variable. The sublist contains two elements: \code{fit}, a glmnet object
#' containing the lasso fit; and \code{cvfit}, a glmnet object containing
#' the 10-fold cross-validation fit to the same data (used to determine
#' optimal lambda).
FitLasso <- function(df, adjlistfname, lassofname, multiple = FALSE){
    out <- MakeLagDF(df, multiple)

    ## convert each column to factor for glmnet, then
    ## covnert list object back to data.frame
    df <- out$df %>% lapply(as.factor) %>% as.data.frame

    ## remove species with fewer than 8 observations in each class,
    ## to address warnings from glmnet
    ## convert to matrix for glmnet
    ## lagdf <- as.matrix(RemoveSparseSpp(out$lagdf))
    lagdf <- as.matrix(out$lagdf)
    
    ## run LASSO for each species
    lasso <- list()
    for(i in 1:ncol(df)){
        fit <- glmnet(x = lagdf, y = df[,i], family = "binomial")
        cvfit <- cv.glmnet(lagdf, df[,i], family = "binomial", type.measure = "class")
        lmin <- cvfit$lambda.min
        fitcoef <- as.matrix(coef(fit, s = lmin, exact = TRUE))
        AddToAdjlist(fitcoef, names(df)[i], adjlistfname, multiple = multiple)
        lasso[[names(df)[i]]] <- list(fit = fit, cvfit = cvfit)
    }

    save(lasso, file = lassofname)
    return(lasso)
}
##TODO: fix function so that tests pass after adding SplitMultiple function ^^^^

#' Convert glm prediction to probability of correctly observing data
#'
#' @param pred numeric, prediction from glm output
#' @param obs numeric, observed presence/absence value
#'
#' @return numeric, probability of correctly observing \code{obs} given
#' model prediction \code{obs}. If \code{obs} is not 1 or 0, NA is returned.
PredToProb <- function(pred, obs){
    if(obs == 1) return(pred)
    if(obs == 0) return(1 - pred)
    ## return NA for unexpected input
    return(NA)
}

#' Calculate probabilities of predicting out of sample data given the models
#'
#' @param lasso list, list of lasso fit and lasso cross validation fit objects
#' for different species (returned from \code{FitLasso})
#' @param cv data.frame of cross validation data,
#' each column is a species in the network, and each
#' row is an observation at one timepoint. Data taken from different sites
#' are separated by a row of NAs, so that adjacenct rows can be assumed to
#' be adjacent timepoints at the same site.
#' @param multiple logical, flag for if multiple interactions should be
#' included in lagdf
#'
#' @return data.frame of size \code{dim(cv)}, containing probabilities of
#' correctly predicting the data in \code{cv} given the lasso models
PredictLasso <- function(lasso, cv, multiple = FALSE){
    out <- MakeLagDF(cv, multiple)
    cvdf <- out$df
    cvlag <- as.matrix(out$lagdf)

    ## preallocate prediction data.frame
    predprobs <- matrix(NA, nrow = nrow(cvdf), ncol = ncol(cvdf)) %>%
        as.data.frame
    ## check that column naming will work as expected
    if(length(names(lasso)) != ncol(cvdf)){
        stop('differing dimensions between lasso models and cross validation data')
    }
    names(predprobs) <- names(lasso)

    ## validate that cv data frames contain only 1s and 0s
    if(any(cvdf != 0 & cvdf != 1)){
        stop("cvdf is not only 1s and 0s")
    }
    if(any(cvlag != 0 & cvlag != 1)){
        stop("cvlag is not only 1s and 0s")
    }
    
    ## make predictions for each point based on the appropriate lasso model
    for(sp in names(lasso)){
        lmin <- lasso[[sp]]$cvfit$lambda.min
        ## predict all of the points for that species, and take the inverse
        ## logit to convert back into probabilities
        predictions <- predict(lasso[[sp]]$fit, newx = cvlag, type = "link",
                               s = lmin) %>% inv.logit
        ## Those predictions should be values between 0 and 1, and represent
        ## the probability the species will be present in the next time step,
        ## as predicted by the model. We want to know the probability of predicting
        ## the data given the model. Those probabilities are given by `predictions`
        ## for all cases where the species is present, and `1-predictions` for
        ## cases where the species is absent.
        tmp <- data.frame(pred = as.vector(predictions), obs = cvdf[[sp]])
        tmp$prob <- 1 - tmp$pred
        tmp$prob[tmp$obs == 1] <- tmp$pred[tmp$obs == 1]
        ## add probabilities to final data.frame
        predprobs[[sp]] <- tmp$prob
    }
    if(sum(is.na(predprobs)) > 0){
        warning('Some prediction probabilities are missing')
    }
    return(predprobs)
}

#' Fit and predict Lasso models for Comte dataset
#'
#' Fits one Lasso model for each species in the dataset, cross-validates
#' the model to find the optimal lambda, and calculated probabilities of
#' observing a cross validation dataset given the Lasso models for each
#' species. Writes to file an adjacency list of non-zero regression terms.
#' Additionally, lasso models are saved in an RData file, as is the data.frame
#' of cross validation predictions.
#'
#' @param basefname character, beginning of file name that will be used
#' as the basis for Lasso output file names. Should be something like
#' '../../Results/Comte-training-final-2/Comte-training-final-2'
#' @param multiple logical, flag for if multiple interactions should be
#' included in lagdf
#' @param trainfname character, path to training data
#' @param cvfname character, path to cross validation data
#' @param suffix character, suffix to append to file names. Used to
#' distinguish different versions of the model.
FitLassoComte <- function(basefname, multiple = FALSE, trainfname = '../../Data/Comte2015/Comte-training-final/Comte-all-training-final.txt', cvfname = '../../Data/Comte2015/Comte-CV-final/Comte-all-CV-final.txt', suffix = '-rmsp'){
    train <- read.table(trainfname, stringsAsFactors = FALSE, header = TRUE) %>%
        select(-OBJECTID, -Period)
    cv <- read.table(cvfname, stringsAsFactors = FALSE, header = TRUE) %>%
        select(-OBJECTID, -Period)
    adjlistfname <- paste0(basefname, "-lasso-adjlist", suffix, ".csv")
    lassofname <- paste0(basefname, "-lasso-parameters", suffix, ".RData")
    lasso <- FitLasso(train, adjlistfname, lassofname, multiple = multiple)
    predictions <- PredictLasso(lasso, cv, multiple = multiple)
    save(predictions, file = paste0(basefname, "-lasso-predictions", suffix, ".RData"))
}

#' Split data into training and cv based on the cv set number
#'
#' @param crossvalRep data.frame, taken from tatoosh crossvalidation file.
#' Should contain two columns: site, and rep_##, the cross validation rep
#' of interest, which contains the index of the row that has been left out
#' of the training set for cross-validation
#' @param alldata data.frame, full tatoosh data set, containing columns for site and
#' for species presence/absence
#'
#' @return list containing two elements: train, a data.frame containing training
#' data, and cv, a data.frame containing cross validation data
SplitData <- function(crossvalRep, alldata){
    cvdata <- numeric(0)
    traindata <- numeric(0)
    for(i in 1:nrow(crossvalRep)){
        ## filter to the site of interest
        currSite <- crossvalRep$site[i]
        sitedata <- dplyr::filter(alldata, site == currSite)

        ## get index of the row specified in the crossvalidation info
        cvind <- crossvalRep[i,2]

        ## add the cvind row to cvdata, and add all other rows to traindata
        ## this has a side effect of putting the sites in reverse order,
        ## but the order of sites doesn't matter
        cvdata <- sitedata %>% slice(cvind) %>% bind_rows(cvdata)
        traindata <- sitedata %>% slice(-cvind) %>% bind_rows(traindata)
    }
    ## remove site column from datasets
    traindata <- traindata %>% select(-site)
    cvdata <- cvdata %>% select(-site)
    return(list(train = traindata, cv = cvdata))
}

#' Fit and predict Lasso models for Tatoosh dataset
#'
#' Fits Lasso models and makes predictions as in \code{FitLassoComte}. This
#' function differs in that it fits and predicts separate models for each
#' leave-one-out cross validation set.
#'
#' @param path character, path to the folders where the models for each cross
#' validation set go
#' @param multiple logical, flag for if multiple interactions should be
#' included in lagdf
#' @param datafname character, path to full dataset
#' @param crossvalfname, character, path to cross validation information
#' @param suffix character, suffix to append to file names. Used to
#' distinguish different versions of the model.
FitLassoTatoosh <- function(path, multiple = FALSE, datafname = '../../Data/tatoosh-control-exp-cv/tatoosh-control-exp-cv-lasso.txt', crossvalfname = '../../Data/tatoosh-control-exp-cv/tatoosh-control-exp-cv-crossvalidation.txt', suffix = '-rmsp'){
    alldata <- read.table(datafname, stringsAsFactors = FALSE, header = TRUE)
    crossval <- read.table(crossvalfname, stringsAsFactors = FALSE, header = TRUE)
    folds <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    for(fold in folds){
        print(fold)
        ## only want cross validation folders
        if(basename(fold) == 'Marginals') next
        ## get crossvalidation rep number
        basefname <- basename(fold)
        repnum <- str_split(basefname, '-')[[1]]
        repnum <- repnum[length(repnum) - 1]
        cvrep <- select_(crossval, .dots = c('site', str_c('rep_', repnum)))
        datasets <- SplitData(cvrep, alldata)
        adjlistfname <- str_c(fold, '/', basefname, '-lasso-adjlist', suffix, '.csv')
        lassofname <- str_c(fold, '/', basefname, '-lasso-parameters', suffix, '.RData')
        lasso <- FitLasso(datasets$train, adjlistfname, lassofname, multiple = multiple)
        predictions <- PredictLasso(lasso, datasets$cv, multiple = multiple)
        save(predictions, file = str_c(fold, '/', basefname,
                                       '-lasso-predictions', suffix, '.RData'))
    }
}

#' Quick and dirty script to fix output from a bug in an earlier code version.
#' Reads in multint tatoosh files and rewrites the multiple files properly. Also,
#' adds headers to the tatoosh multint files.
MultipleScript <- function(path){
    dirnames <- list.dirs(path)
    dirnames <- dirnames[str_detect(dirnames, 'control')]
    for(dirname in dirnames){
        fs <- list.files(dirname, full.names = TRUE)
        fname <- fs[str_detect(fs, 'multint')]
        if(length(fname) > 1) {
            stop(str_c('directory ', dirname, 'contains more than one match'))
        }
        multint <- read.csv(fname, stringsAsFactors = FALSE)
        names(multint) <- c('Parent', 'Child', 'Sign')
        multadj <- SplitMultiple(multint)
        write.table(multint, fname, row.names = FALSE, quote = FALSE, sep = ',')
        multfname <- str_replace(fname, '.multint', '')
        write.table(multadj, multfname, row.names = FALSE, quote = FALSE, sep = ',')
    }
}
