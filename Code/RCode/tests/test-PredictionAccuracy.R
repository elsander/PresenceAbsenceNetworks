library(testthat)
library(hash)

setwd('..')
source('PredictionAccuracy.R')
setwd('tests')

context('Prediction Accuracy of Model')

test_that("ProbPredict works", {
    expect_equal(ProbPredict(1,2,2), .5)
    expect_equal(ProbPredict(0,2,2), .5)
    expect_equal(ProbPredict(1,10,20), 1/3)
})

testData <- data.frame(A = c(0,0,0,1,1,NA,0,1,1,1),
                       B = c(0,0,1,0,0,NA,0,0,1,1))
Atest <- data.frame(A = c(0,1),
                    beta = c(3,1),
                    alpha = c(3,3),
                    lower_credible = c(0.1466328, 0.2924018),
                   upper_credible = c(0.8533672, 0.9915962))
Btest <- data.frame(A = c(0,0,1),
                    B = c(0,1,0),
                    beta = c(3,2,2),
                    alpha = c(2,1,2),
                    lower_credible = c(0.06758599, 0.01257912, 0.09429932),
                    upper_credible = c(0.8058796, 0.8418861, 0.9057007))
Apost <- list(child = 'A', parents = 'A', joint = Atest)
Bpost <- list(child = 'B', parents = c('A', 'B'), joint = Btest)
posttest <- hash::hash(A = Apost, B = Bpost)

test_that("PredictOneSpp works", {
    expect_equal(PredictOneSpp(Apost, testData[1:2,]), .5)
    expect_equal(PredictOneSpp(Bpost, testData[1:2,]), 3/5)
    ## unobserved state should default to alpha, beta = 1
    expect_equal(PredictOneSpp(Bpost, testData[9:10,]), .5)
})

test_that("PredictOneTimept works", {

})
