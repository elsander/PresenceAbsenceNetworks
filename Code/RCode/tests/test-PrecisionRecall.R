library(testthat)

setwd('..')
source('PrecisionRecall.R')
setwd('tests')

consensus <- data.frame('Child' = letters[1:5],
                        'Parent' = c('a', 'a', 'a', 'b', 'b'),
                        stringsAsFactors = FALSE)
empirical <- data.frame('Child' = rep(letters[1:4], times = 2),
                        'Parent' = c('a', 'a', 'a', 'c', 'c', 'd', 'd', 'd'),
                        stringsAsFactors = FALSE)
spnames <- letters[1:5]

test_that('GetPrecisionRecall calculates correctly', {
    out <- GetPrecisionRecall(consensus, empirical, spnames)
    expect_equal(out$precision, 0.6)
    expect_equal(out$recall, 0.375)
    ## try with diagonals removed
    emp2 <- empirical %>% filter(Child != Parent)
    con2 <- consensus %>% filter(Child != Parent)
    out <- GetPrecisionRecall(con2, emp2, spnames)
    expect_equal(out$precision, 0.5)
    expect_equal(out$recall, 1/3)
})

test_that('BuildRandomConsensus works with seed', {
    set.seed(848)
    out <- BuildRandomConsensus(consensus, spnames)
    expected <- data.frame('Child' = letters[1:5],
                           'Parent' = c('e', 'c', 'a', 'b', 'd'),
                           stringsAsFactors = FALSE)
    expect_equal(out, expected)
})

expectedAdj <- data.frame(Child = c('d', 'e'), Parent = c('b', 'b'),
                          stringsAsFactors = FALSE)

test_that('RemoveSpecies removes correct rows', {
    ## test the spnames set without 'a'
    outadj <- RemoveSpecies(consensus, spnames[-1])
    ## expect 'a' to be removed from the adjlist
    expect_equivalent(expectedAdj, outadj)
})

test_that('RandomizePrecisionRecall works with seed', {
    set.seed(192)
    ## test the case where precision and recall are high
    out <- RandomizePrecisionRecall(empirical, consensus, spnames, n = 100)
    expect_equal(out$precision, .5)
    expect_equal(out$recall, 1/3)
    expect_equal(out$precisionprob, .44)
    expect_equal(out$recallprob, .44)
})
