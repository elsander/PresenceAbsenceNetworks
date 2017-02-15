library(testthat)
library(dplyr)

setwd('..')
source('PredictionCoinFlip.R')
setwd('tests')

context('Prediction Accuracy of Coin Flip Null Model')

test_that("ProbCoinFlip works for vectors", {
    weight <- .3
    expect_equal(ProbCoinFlip(1, weight), weight)
    expect_equal(ProbCoinFlip(0, weight), 1-weight)
    expected <- c(weight, weight, 1-weight)
    expect_equal(ProbCoinFlip(c(1,1,0), weight), expected)
})

train <- data.frame(OBJECTID = LETTERS[1:6],
                    Period = 1:6,
                    a = c(0,0,NA,0,0,0),
                    b = c(1,1,NA,1,1,1),
                    c = c(0,0,NA,1,1,1),
                    stringsAsFactors = FALSE)
trainfile <- 'test-train-coinflip.txt'
write.table(train, trainfile, quote = FALSE, row.names = FALSE)
train2 <- train %>% dplyr::select(-OBJECTID) %>%
    dplyr::rename(site = Period) %>% as.data.frame

test_that("GetCoinFlipWeights works", {
    weights <- c(a = 0, b = 1, c = .6)
    expect_equal(GetCoinFlipWeights(train, comte = TRUE), weights)
    expect_equal(GetCoinFlipWeights(trainfile, comte = TRUE), weights)
    expect_equal(GetCoinFlipWeights(train2, comte = FALSE), weights)
})

predictions <- cbind(c(1,1,1,1,1),
                     c(1,1,1,1,1),
                     c(.4,.4,.6,.6,.6))
colnames(predictions) <- letters[1:3]
cv <- data.frame(site = 'test', a = 1, b = 0, c = 1)
cvpred <- c(a = 0, b = 0, c = .6)

test_that("PredictOutOfSampleCoinFlip works", {
    expect_equal(PredictOutOfSampleCoinFlip(train, train, comte = TRUE), predictions)
    expect_equal(PredictOutOfSampleCoinFlip(train, trainfile, comte = TRUE), predictions)
    expect_equal(PredictOutOfSampleCoinFlip(train2, train2, comte = FALSE), predictions)
    expect_equal(PredictOutOfSampleCoinFlip(train2, cv, comte = FALSE), cvpred)
})

system(paste("rm", trainfile))
