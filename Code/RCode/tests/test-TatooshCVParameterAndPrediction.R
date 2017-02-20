library(testthat)

setwd('..')
source('TatooshCVParameterAndPrediction.R')
setwd('tests')

context("Tatoosh parameter fitting and prediction")

crossval <- data.frame(site = letters[1:3],
                       rep_1 = rep(2,3),
                       rep_2 = rep(3,3))
alldata <- data.frame(Mytilus = c(1,1,1,1,NA,0,0,0,0,NA,1,0,1,0),
                      Corallina = c(0,0,0,0,NA,1,1,1,1,NA,0,1,0,1),
                      Pollicipes = c(0,0,0,0,NA,0,0,0,0,NA,0,1,0,1),
                      site = c('a','a','a','a',NA,'b','b','b','b',NA,'c','c','c','c'),
                      stringsAsFactors = FALSE)
adjlist <- as.data.frame(rbind(c('Mytilus', 'Mytilus'),
                               c('Corallina', 'Corallina'),
                               c('Pollicipes', 'Pollicipes'),
                               c('Corallina', 'Mytilus'),
                               c('Pollicipes', 'Mytilus')),
                         stringsAsFactors = FALSE)
names(adjlist) <- c('Child', 'Parent')



test_that("RemoveRows removes the correct rows", {
    out <- RemoveRows(crossval[,1:2], alldata)
    expected <- alldata
    expected[c(2,7,12),] <- rep(NA, 4)
    expect_equal(out, expected)
})

test_that("GetPredictionRows returns data for prediction", {
    out <- GetPredictionRows(crossval[,1:2], alldata)
    expected <- rbind(alldata[1:2,],
                      rep(NA,4),
                      alldata[6:7,],
                      rep(NA,4),
                      alldata[11:12,])
    expect_equal(out, expected, check.attributes = FALSE)
})

test_that("PredictCoinFlip creates data frame of predictions", {
    out <- PredictCoinFlip(crossval, alldata)
    expected <- data.frame(Mytilus = c(0.5555556, 0.5555556, 0.4444444, 0.4444444,
                                       0.5555556, 0.4444444, 0.4444444, 0.4444444,
                                       0.5555556, 0.5555556, 0.5555556, 0.4444444),
                           Corallina = c(0.5555556, 0.5555556, 0.4444444, 0.4444444,
                                         0.5555556, 0.4444444, 0.4444444, 0.4444444,
                                         0.5555556, 0.5555556, 0.5555556, 0.4444444),
                           Pollicipes = c(0.8888889, 0.8888889, 0.8888889, 0.8888889,
                                          0.8888889, 0.1111111, 0.7777778, 0.7777778,
                                          0.7777778, 0.7777778, 0.2222222, 0.7777778))
    expect_equal(out, expected, tolerance = 1e-7)
})

test_that("PredictAdjlist creates data frame of predictions", {
    out <- PredictAdjlist(adjlist, crossval, alldata)
    expected <- data.frame(Corallina = c(.5, .6666667, .5, .5, .6666667, .3333333),
                           Mytilus = c(.5, .6666667, .5, .5, .6666667, .3333333),
                           Pollicipes = c(.5, .6666667, .5, .5, .6666667, .5))
    corallina <- data.frame(Corallina = c(0,0,1,1),
                            Mytilus = c(0,1,0,1),
                            beta = c(1,2,1,1),
                            alpha = c(1,2,2,1),
                            lower_credible = c(0.025,0.09429932,0.15811388,0.025),
                            upper_credible = c(0.975,0.9057007, 0.9874209,0.975))
    expect_equal(out$predictprobs, expected, tolerance = 1e-4)
    expect_equal(length(out$posteriors), 3)
    expect_equal(out$posteriors$Corallina$child, "Corallina")
    expect_equal(out$posteriors$Corallina$parents,
                 c("Corallina", "Mytilus"))
    expect_true("data.frame" %in% class(out$posteriors$Corallina$joint))
    expect_equal(as.data.frame(out$posteriors$Corallina$joint), corallina,
                 check.attributes = FALSE, tolerance = 1e-7)
})
