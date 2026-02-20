library(testthat)

data(ks1988, package = "yabaf")
b <- Breeder(ks1988)
p <- Pedigree(b)

test_that("Breeder abstract class test", {
    expect_true(is(b, "Breeder")) 
})

test_that("Pedigree concrete class test", {
    expect_true(is(p, "Pedigree")) 
})

test_that("Pedigree concrete class dimension check", {
    expect_equal(p$structure$size, 8) ## see reference
})
