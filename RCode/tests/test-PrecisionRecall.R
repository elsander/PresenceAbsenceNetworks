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
})

test_that('BuildRandomConsensus works with seed', {
    set.seed(848)
    out <- BuildRandomConsensus(consensus, spnames)
    expected <- data.frame('Child' = letters[1:5],
                           'Parent' = c('e', 'c', 'a', 'b', 'd'),
                           stringsAsFactors = FALSE)
    expect_equal(out, expected)
})

test_that('RandomizePrecisionRecall works with seed', {
    set.seed(192)
    ## test the case where precision and recall are high
    out <- RandomizePrecisionRecall(empirical, consensus, spnames, n = 100)
    expect_equal(out$precisionprob, .12)
    expect_equal(out$recallprob, .12)
    ## precision and recall are no higher than random here
    set.seed(555)
    empirical$Child <- sample(empirical$Child, size = nrow(empirical))
    empirical$Parent <- sample(empirical$Parent, size = nrow(empirical))
    out <- RandomizePrecisionRecall(empirical, consensus, spnames, n = 1000)
    expect_equal(out$precisionprob, .57)
    expect_equal(out$recallprob, .57)
})
