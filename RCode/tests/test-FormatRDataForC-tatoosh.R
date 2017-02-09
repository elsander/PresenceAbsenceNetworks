require(testthat)

setwd('..')
source('FormatRDataForC-tatoosh.R')
setwd('tests')

adjlist <- data.frame(Child = c('C', 'C', 'B', 'D'),
                      Parent = c('A', 'B', 'C', 'D'))

test_that("DFS finds the species list", {
    expect_equal(DFS('A', adjlist), 'A')
    expect_equal(DFS('B', adjlist), c('B', 'C', 'A'))
    expect_equal(DFS('C', adjlist), c('C', 'A', 'B'))
    expect_equal(DFS('D', adjlist), 'D')
})

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

test_that("VecToIntTatoosh returns correct state", {
    expect_equal(VecToIntTatoosh(vec1, spnames), 1)
    expect_equal(VecToIntTatoosh(vec2, spnames), 0)
    expect_equal(VecToIntTatoosh(vec3, spnames), 9)
    expect_warning(VecToIntTatoosh(vec4, spnames),
                   "This state is too large to represent in the MCMC C code!")
})

spnames[1] <- 'a.test'
names(vec1) <- 'a-test'

## VecToIntTatoosh only runs make.names() on the column names, and
## assumes that spnames already contains valid names. This is an issue
## that should probably be refactored away if any code changes need to be
## made.
test_that("VecToIntTatoosh handles names correctly", {
    expect_equal(VecToIntTatoosh(vec1, spnames), 1)
})
