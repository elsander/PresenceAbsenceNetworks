## NOTE: if any new mismatch issues arise with the FitPearson or
## PearsonRandomization tests, try reducing the number of cores
## down to 1. It's possible that the number of cores could result in
## different output sometimes, although so far this has only happened
## for the Comte test.

library(testthat)

setwd('..')
source('FitPearson.R')
setwd('tests')

vec <- letters[1:20]
inds <- c(5:9, 16)
expectedVec <- c(letters[1:4], "f", "h", "e", "g", letters[9:20])

test_that("ShuffleInds shuffles as expected", {
    set.seed(8372)
    outvec <- ShuffleInds(vec, inds)
    expect_equal(outvec, expectedVec)
})

df <- data.frame(sp1 = rep(c(1,1,0,1,NA), 3),
                 sp2 = rep(c(0,1,1,0,NA), 3),
                 sp3 = c(1,1,1,1,NA,0,1,1,1,NA,0,0,1,0,NA))
expectedDF <- data.frame(sp1 = c(rep(1, 7), 0, 0),
                         sp2 = c(0,1,1,1,0,0,0,1,1),
                         sp3 = c(1,0,1,1,1,0,1,1,0))
expectedLag <- data.frame(lag.sp1 = c(rep(1,6),0,1,0),
                          lag.sp2 = c(0,0,1,1,1,0,0,0,1),
                          lag.sp3 = c(1,1,0,1,1,1,0,1,1))

test_that("RandomizeRows randomizes as expected", {
    set.seed(8372)
    out <- RandomizeRows(df)
    expect_equivalent(out$df, expectedDF)
    expect_equivalent(out$lagdf, expectedLag)
})

tmp <- MakeLagDF(df, multiple = FALSE)
newdf <- tmp$df
lagdf <- tmp$lagdf
cormat <- cor(newdf, lagdf, method = "pearson")
abscormat <- abs(cormat)
expectedBigCor <- matrix(0, 3,3)
expectedBigCor[c(1,3), 3] <- 1
rownames(expectedBigCor) <- c('sp1', 'sp2', 'sp3')
colnames(expectedBigCor) <- c('lag.sp1', 'lag.sp2', 'lag.sp3')

test_that("OneRandomization returns expected matrix", {
    set.seed(8372)
    biggerCor <- OneRandomization(df, abscormat)
    expect_equal(biggerCor, expectedBigCor)
})

expectedPvalMat <- matrix(c(25,0,576,106,182,515,917,142,516), 3, 3)
rownames(expectedPvalMat) <- c('sp1', 'sp2', 'sp3')
colnames(expectedPvalMat) <- c('lag.sp1', 'lag.sp2', 'lag.sp3')

test_that("PearsonRandomizationParallel returns expected matrix", {
    set.seed(8372)
    ## this seed and number of reps will trigger the warning
    ## condition in the tryCatch of OneRandomization 7 times,
    ## so it incidentally serves as a test that the exception
    ## handling is working properly.
    pvalmat <- PearsonRandomizationParallel(df, abscormat, 1000)
    expect_equal(pvalmat, expectedPvalMat)
})

expectedAdjlist <- data.frame(Child = rep(c('sp1','sp2','sp3'), 3),
                              Parent = rep(c('lag.sp1','lag.sp2','lag.sp3'), each = 3),
                              value = c(25,0,576,106,182,515,917,142,516),
                              stringsAsFactors = FALSE)

test_that("AdjmatToAdjlist returns expected adjlist", {
    adjlist <- AdjmatToAdjlist(expectedPvalMat)
    expect_equal(adjlist, expectedAdjlist)
})

expectedPval <- data.frame(Child = 'sp2',
                              Parent = 'lag.sp1',
                              p.value = 0,
                              stringsAsFactors = FALSE)

test_that("PearsonRandomization returns expected p-value adjlist", {
    set.seed(8372)
    adjpval <- PearsonRandomization(df, cormat, reps = 1000)
    expect_equal(adjpval, expectedPval)
})

expectedFinal <- data.frame(Child = 'sp2',
                            Parent = 'sp1',
                            p.value = 0,
                            cor = 1,
                            Sign = 1,
                            stringsAsFactors = FALSE)

test_that("BuildAdjlist returns expected adjlist", {
    adjpval <- BuildAdjlist(expectedPval, cormat)
    expect_equal(adjpval, expectedFinal)
})

adjlistfname <- "test-FitPearson-adjlist.csv"
test_that("FitPearson writes expected csv to file", {
    set.seed(8372)
    FitPearson(df, adjlistfname, reps = 1000)
    adj <- read.csv(adjlistfname, stringsAsFactors = FALSE)
    expect_equal(adj, expectedFinal)
    system(str_c('rm ', adjlistfname))
})

expectedComte <- read.csv('test-FitPearson-Comte-example-pearson-adjlist.csv',
                          stringsAsFactors = FALSE)

test_that("FitPearsonComte writes expected csv to file", {
    set.seed(8372)
    FitPearsonComte('test-FitPearson-Comte',
                    trainfname = '../../../Data/Comte-training-final/Comte-all-training-final.txt',
                    reps = 100, cores = 1)
    comtefname <- 'test-FitPearson-Comte-pearson-adjlist.csv'
    adj <- read.csv(comtefname,
                    stringsAsFactors = FALSE)
    expect_equal(adj, expectedComte)
    system(str_c("rm ", comtefname))
})
