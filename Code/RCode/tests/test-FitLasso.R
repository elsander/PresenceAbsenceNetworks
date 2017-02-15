library(testthat)

setwd('..')
source('FitLasso.R')
setwd('tests')

## original data.frame of species
olddf <- data.frame(A = c(1,1,0,NA,0,0,0,1,0,1,0),
                    B = c(0,1,1,NA,0,1,1,0,1,0,1))
## expected data.frame from MakeLagDF
df <- data.frame(A = c(1,0,0,0,1,0,1,0),
                 B = c(1,1,1,1,0,1,0,1))
## expected lagged data.frame from MakeLagDF
lagdf <- data.frame(lag.A = c(1,1,0,0,0,1,0,1),
                    lag.B = c(0,1,0,1,1,0,1,0))
## lagdf with multiple interactions
lagdf2 <- lagdf
lagdf2$`lag.A:lag.B` <- c(0,1,0,0,0,0,0,0)

test_that("MakeLagDF makes expected data.frames", {
    out <- MakeLagDF(olddf)
    expect_equivalent(out$df, df)
    expect_equivalent(out$lagdf, lagdf)
    ## test that multiple interactions work
    expect_equivalent(MakeLagDF(olddf, multiple = TRUE)$lagdf, lagdf2)
})

spname <- 'example'
multAdj <- data.frame(Parent = c(LETTERS[1:3], 'A:B', 'A:D'),
                      Child = rep(spname, 5),
                      Sign = c(rep(1, 4), -1),
                      stringsAsFactors = FALSE)
multExpected <- data.frame(Parent = LETTERS[1:4],
                           Child = rep(spname, 4),
                           Sign = c(NA, 1, 1, -1),
                           stringsAsFactors = FALSE)
multAdj1 <- data.frame(Parent = c('Mytilus.californianus',
                                  'Amphipoda',
                                  'Mytilus.californianus:Lottia.paradigitalis',
                                  'Mytilus.trossulus:Balanus.glandula',
                                  'Mytilus.trossulus:Hallosaccion',
                                  'Mytilus.trossulus:Ulva',
                                  'Mytilus.trossulus:Rock',
                                  'Mytilus.trossulus:Amphipoda',
                                  'Mytilus.trossulus:Chironomidae',
                                  'Mytilus.trossulus:Sipunculid',
                                  'Balanus.glandula:Hallosaccion'),
                       Child = c('Mytilus.californianus',
                                 rep('Mytilus.trossulus', 10)),
                       Sign = c(1,1,-1,rep(1,8)),
                       stringsAsFactors = FALSE)
multExpected1 <- data.frame(Parent = c('Mytilus.californianus',
                                       'Amphipoda',
                                       'Mytilus.californianus',
                                       'Lottia.paradigitalis',
                                       'Mytilus.trossulus',
                                       'Balanus.glandula',
                                       'Hallosaccion',
                                       'Ulva',
                                       'Rock',
                                       'Chironomidae',
                                       'Sipunculid'),
                            Child = c('Mytilus.californianus',
                                      rep('Mytilus.trossulus',10)),
                            Sign = c(1,1,-1,-1,rep(1,7)),
                            stringsAsFactors = FALSE)
test_that("SplitMultiple updates adjlist rows as expected", {
    multOut <- SplitMultiple(multAdj)
    expect_equivalent(multOut, multExpected)
    multOut1 <- SplitMultiple(multAdj1)
    expect_equivalent(multOut1, multExpected1)
})

adjcoef <- data.frame(test = c(0,0,0,.4,.2,1.5,0,-.1))
rownames(adjcoef) <- c('(Intercept)', str_c('lag.', LETTERS[2:7]), 'lag.A:lag.B')
adjlistfname <- 'test-FitLasso-adjlist.csv'
multfname <- paste0(adjlistfname, '.multint')
expectedAdj1 <- data.frame(Parent = c(LETTERS[4:6], 'A:B'),
                          Child = rep(spname, 4),
                          Sign = c(1,1,1,-1),
                          stringsAsFactors = FALSE)
expectedAdj2 <- data.frame(Parent = c(LETTERS[4:6], 'A', 'B'),
                          Child = rep(spname, 5),
                          Sign = c(1,1,1,-1,-1),
                          stringsAsFactors = FALSE)

test_that("AddToAdjlist appends correct rows", {
    ## clear adjlist file
    if(file.exists(adjlistfname)) system(paste('rm', adjlistfname))
    AddToAdjlist(adjcoef, spname, adjlistfname, multiple = FALSE)
    adj <- read.csv(adjlistfname, stringsAsFactors = FALSE)

    expect_equal(adj, expectedAdj1)

    system(paste('rm', adjlistfname))
    AddToAdjlist(adjcoef, spname, adjlistfname, multiple = TRUE)
    adj <- read.csv(adjlistfname, stringsAsFactors = FALSE)
    adjmult <- read.csv(multfname, stringsAsFactors = FALSE)

    expect_equal(adj, expectedAdj2)
    expect_equal(adjmult, expectedAdj1)

    system(paste('rm', adjlistfname))
    system(paste('rm', multfname))
})

lassofname <- 'test-FitLasso-lasso-save.RData'
set.seed(444)
fitdf <- data.frame(A = sample(0:1, 100, replace = TRUE),
                    B = sample(0:1, 100, replace = TRUE))

test_that("FitLasso creates and saves a list of lasso objects", {
    lasso <- FitLasso(fitdf, adjlistfname, lassofname)
    expect_true(file.exists(adjlistfname))
    expect_true(file.exists(lassofname))
    expect_equal(length(lasso), 2)
    for(item in lasso){
        expect_equal(names(item), c('fit', 'cvfit'))
        expect_equal(class(item$fit), c('lognet', 'glmnet'))
        expect_equal(class(item$cvfit), 'cv.glmnet')
    }
    system(paste('rm', adjlistfname))
    system(paste('rm', lassofname))

    lasso <- FitLasso(fitdf, adjlistfname, lassofname, multiple = TRUE)
    expect_true(file.exists(adjlistfname))
    expect_true(file.exists(lassofname))
    expect_equal(length(lasso), 2)
    for(item in lasso){
        expect_equal(names(item), c('fit', 'cvfit'))
        expect_equal(class(item$fit), c('lognet', 'glmnet'))
        expect_equal(class(item$cvfit), 'cv.glmnet')
    }
    system(paste('rm', adjlistfname))
    system(paste('rm', lassofname))
    system(paste('rm', multfname))
})

test_that("PredToProb returns expected values", {
    expect_equal(PredToProb(.7, 1), .7)
    expect_equal(PredToProb(.7, 0), .3)
    expect_true(is.na(PredToProb(.7, 20)))
})

test_that("PredictLasso predicts values from a lasso object", {
    lasso <- FitLasso(fitdf, adjlistfname, lassofname)
    predprobs <- PredictLasso(lasso, olddf)
    expect_equal(dim(predprobs), dim(df))
    expect_equal(sum(is.na(predprobs)), 0)
    system(paste('rm', adjlistfname))
    system(paste('rm', lassofname))
    lasso <- FitLasso(fitdf, adjlistfname, lassofname, multiple = TRUE)
    predprobs <- PredictLasso(lasso, olddf, multiple = TRUE)
    expect_equal(dim(predprobs), dim(df))
    expect_equal(sum(is.na(predprobs)), 0)
    system(paste('rm', adjlistfname))
    system(paste('rm', lassofname))
    system(paste('rm', multfname))
})

crossvalRep <- data.frame(site = c('site_1', 'site_2'),
                          rep_3 = c(1,2))
## in theory this should be presence absence data, but it
## shouldn't matter to this function, and it's easier to test
## with distinct values in the data.frame
alldata <- data.frame(site = c(rep('site_1', 3), rep('site_2',3)),
                      A = 1:6, B = 6:1)
expectedTrain <- data.frame(A = c(4,6,2,3), B = c(3,1,5,4))
expectedCV <- data.frame(A = c(5,1), B = c(2,6))

test_that("SplitData returns correct data", {
    out <- SplitData(crossvalRep, alldata)
    expect_equal(out$train, expectedTrain)
    expect_equal(out$cv, expectedCV)
})
