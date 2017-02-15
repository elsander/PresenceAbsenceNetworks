library(testthat)
library(hash)

setwd('..')
source('ParameterInference.R')
setwd('tests')

context("Parameter Inference for Comte dataset")

adjlist <- data.frame(Parent = 0:5,
                      Child = c(0,0,2,3,1,0),
                      stringsAsFactors = FALSE)
spnames <- letters[1:6]
nameAdjlist <- data.frame(Parent = letters[1:6],
                          Child = c('a', 'a', 'c', 'd', 'b', 'a'),
                          stringsAsFactors = FALSE)
nodedict <- hash::hash(a = c('a', 'b', 'f'),
                       b = 'e',
                       c = 'c',
                       d = 'd',
                       e = numeric(0),
                       f = numeric(0))

test_that("MakeNodeDict works", {
    tmpdict <- MakeNodeDict(nameAdjlist)
    for(key in keys(tmpdict)){
        expect_equal(tmpdict[[key]], nodedict[[key]])
    }
})

test_that("AdjlistNumsToNames works", {
    expect_equal(AdjlistNumsToNames(adjlist, spnames), nameAdjlist)
})

test_that("JointCredibleBeta works", {
    expected1_1 <- c(lower_credible = .025, upper_credible = .975)
    expected5_10 <- c(lower_credible = 0.1275984, upper_credible = 0.5810353)
    expected.5 <- c(lower_credible = 0.25, upper_credible = 0.75)
    expect_equal(JointCredibleBeta(1,1,.95), expected1_1)
    expect_equal(JointCredibleBeta(5,10,.95), expected5_10, tolerance = 1e-7)
    expect_equal(JointCredibleBeta(1,1,.5), expected.5)
})

testData <- data.frame(A = c(0,0,0,1,1,NA,0,1,1),
                       B = c(0,0,1,0,0,NA,0,0,1))
Atest <- data.frame(A = c(0,1),
                    beta = c(3,1),
                    alpha = c(3,3),
                    lower_credible = c(0.1466328, 0.2924018),
                   upper_credible = c(0.8533672, 0.9915962))
Btest <- data.frame(A = c(0,0,1,1),
                    B = c(0,1,0,1),
                    beta = c(3,2,2,1),
                    alpha = c(2,1,2,1),
                    lower_credible = c(0.06758599, 0.01257912, 0.09429932, 0.02500000),
                    upper_credible = c(0.8058796, 0.8418861, 0.9057007, 0.97500000))

test_that("OneNodePosterior works", {
    Aout <- OneNodePosterior('A', 'A', testData)
    Bout <- OneNodePosterior('B', c('A', 'B'), testData)
    expect_equal(Aout$child, 'A')
    expect_equal(Bout$child, 'B')
    expect_equal(Aout$parents, 'A')
    expect_equal(Bout$parents, c('A', 'B'))
    expect_equal(as.data.frame(Aout$joint), Atest,
                 tolerance = 1e-7, check.attributes = FALSE)
    expect_equal(as.data.frame(Bout$joint), Btest,
                 tolerance = 1e-7, check.attributes = FALSE)
})

ABnodedict <- hash::hash(A = 'A', B = c('A', 'B'))

test_that("AllNodePosterior works", {
    allout <- AllNodePosterior(testData, ABnodedict)
    expect_equal(length(keys(allout)), 2)
    expect_equal(as.data.frame(allout[['A']]$joint), Atest,
                 tolerance = 1e-7, check.attributes = FALSE)
    expect_equal(as.data.frame(allout[['B']]$joint), Btest,
                 tolerance = 1e-7, check.attributes = FALSE)
})
