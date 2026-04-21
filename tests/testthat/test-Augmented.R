library(testthat)

data("burgueno.unreplicated", package = "agridat")

dat <- burgueno.unreplicated
names(dat)[names(dat) == "gen"] <- "treat"
names(dat)[names(dat) == "col"] <- "column"
names(dat)[names(dat) == "yield"] <- "response"
dat$role <- ifelse(dat$treat == "G000", "check", "test")

b <- Breeder(dat)
a <- Augmented(b)

test_that("Breeder abstract class test", {
  expect_true(is(b, "Breeder"))
})

test_that("Augmented concrete class test", {
  expect_true(is(a, "Augmented"))
})

test_that("Augmented anova terms", {
  expect_true(all(c("(Intercept)", "role", "at(role, 'check'):treat", "residual (MS)") %in% rownames(a$report$anova)))
})

test_that("Augmented variance components", {
  expect_true(all(c("at(role, 'test'):treat", "row:column!R", "row:column!row!cor", "row:column!column!cor") %in% rownames(a$report$variance)))
})

test_that("Augmented adjusted means", {
  expect_true(all(unique(a$report$means$treat) %in% levels(a$data$treat)))
})
