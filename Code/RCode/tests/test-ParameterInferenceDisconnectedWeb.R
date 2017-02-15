library(testthat)

setwd('..')
source('ParameterInferenceDisconnectedWeb.R')
setwd('tests')

context('Parameter Inference of Disconnected Null Model')

testdf1 <- data.frame(OBJECTID = runif(10),
                     Period = runif(10),
                     test1 = runif(10),
                     test2 = runif(10),
                     stringsAsFactors = FALSE)
testfile1 <- 'testfile-1-ParamInfDiscweb.txt'
write.table(testdf1, testfile1, quote = FALSE, row.names = FALSE)

testdf2 <- data.frame(site = runif(10),
                      test1 = runif(10),
                      test2 = runif(10),
                      stringsAsFactors = FALSE)
testfile2 <- 'testfile-2-ParamInfDiscweb.txt'
write.table(testdf2, testfile2, quote = FALSE, row.names = FALSE)

test_that("FitDisconnectedPosterior creates disconnected structures", {
    FitDisconnectedPosterior(testfile1, outfname = paste0(testfile1, '-out'))
    adjlist1 <- read.csv(paste0(testfile1, '-out'), header = TRUE,
                           stringsAsFactors = FALSE)
    FitDisconnectedPosterior(testfile2, outfname = paste0(testfile2, '-out'))
    adjlist2 <- read.csv(paste0(testfile2, '-out'), header = TRUE,
                           stringsAsFactors = FALSE)
    outdf <- data.frame(Child = 0:1, Parent = 0:1,
                        stringsAsFactors = FALSE)
    expect_equal(adjlist1, outdf)
    expect_equal(adjlist2, outdf)
    ## tear down
    system(paste0('rm ', testfile1, '-out'))
    system(paste0('rm ', testfile2, '-out'))
})

## tear down
system(paste('rm', testfile1))
system(paste('rm', testfile2))
