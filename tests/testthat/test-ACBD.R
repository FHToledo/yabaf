library(testthat)

data(wheat, package = "yabaf")
b <- Breeder(wheat)
l <- ACBD(b)

test_that("Breeder abstract class test", {
    expect_true(is(b, "Breeder")) 
})

test_that("ACBD concrete class test", {
    expect_true(is(l, "ACBD")) 
})

test_that("ACBD anova terms", {
    expect_true(all(c("(Intercept)", "role", "at(role, 'check'):treat", "residual (MS)") %in% rownames(a$report$anova)))
})

test_that("ACBD variance components", {
    expect_true(all(c("block", "at(role, 'test'):treat", "row:column!R", "row:column!row!cor", "row:column!column!cor") %in% rownames(a$report$variance)))
})

test_that("ACBD blues and blups", {
    expect_true(all(unique(a$report$means$treat) %in% levels(a$data$treat)))
})
