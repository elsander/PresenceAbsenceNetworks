require(testthat)

setwd('..')
source('FormatRDataForC.R')
setwd('tests')

spnames <- c(letters[1:26], LETTERS[1:26])

## single species in joint
vec1 <- 1
names(vec1) <- 'a'
## 0 state
vec2 <- c(0,0,0,0)
names(vec2) <- c('b', 'z', 'A', 'q')
vec3 <- c(0,1,1)
## mixed 0s and 1s, with names out of order
names(vec3) <- c('c', 'd', 'a')
## state too large for MCMC code
vec4 <- c(1,1,1)
names(vec4) <- c('Z', 'A', 'a')

test_that("VecToInt returns correct state", {
    expect_equal(VecToInt(vec1, spnames), 1)
    expect_equal(VecToInt(vec2, spnames), 0)
    expect_equal(VecToInt(vec3, spnames), 9)
    expect_warning(VecToInt(vec4, spnames),
                   "This state is too large to represent in the MCMC C code!")
})
