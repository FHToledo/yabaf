library(testthat)

data(stg12025, package = "yabaf")
b <- Breeder(stg12025)
l <- ACBD(b)

test_that("Breeder abstract class test", {
  expect_true(is(b, "Breeder")) 
})

test_that("ACBD concrete class test", {
  expect_true(is(l, "ACBD")) 
})

test_that("ACBD concrete class sigma check", {
  expect_equal(round(l$report$fit$sigma, 4), 0.1493) ## see reference
})

