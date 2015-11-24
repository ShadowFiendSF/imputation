
library(testthat)
library(imputation)

context("weighted mean") 

test_that("weighted mean works", {
  set.seed(359)
  a <- rnorm(10)
  b <- runif(10)
  
  a2 <- rnorm(1000)
  b2 <- runif(1000)
  
  ### first check errors
#   expect_equal(weighted_mean(a, runif(9)), -1)
#   expect_equal(weighted_mean(a, runif(11)), -1)
#   expect_equal(weighted_mean(rnorm(9), b), -1)
#   expect_equal(weighted_mean(rnorm(11), b), -1)
  
  ### next check results
  expect_equal(weighted_mean(a,b), weighted.mean(a,b))
  expect_equal(weighted_mean(a2,b2), weighted.mean(a2,b2))
  
})

context("sort indices")

test_that("sort_indices == order", {
  set.seed(359)
  a <- rnorm(10)
  b <- rnorm(1000)
  
  # expected to equal (order(x) - 1) -- using C++ indices [0, n-1] vs R: [1, n]
  expect_equal(sort_indices(a), order(a) - 1)
  expect_equal(sort_indices(b), order(b) - 1)
  
})

context("kern_wt") 

test_that("kern_wt returns same as kernlab", {
  set.seed(1356)
  x1 <- rnorm(10)
  x2 <- rnorm(1000)
  kern <- kernlab::rbfdot(sigma=1)
  kern2 <- kernlab::rbfdot(sigma=sd(x1))
  kern3<- kernlab::rbfdot(sigma=sd(x2))
  
  expect_equal(kernlab::kernelMatrix(kern, c(0, x1))[1, -1, drop= TRUE],
               imputation:::kern_wt(sigma= 1, x= x1))
  expect_equal(kernlab::kernelMatrix(kern, c(0, x2))[1, -1, drop= TRUE],
               imputation:::kern_wt(sigma= 1, x= x2))
  expect_equal(kernlab::kernelMatrix(kern2, c(0, x))[1, -1, drop= TRUE],
               imputation:::kern_wt(sigma= sd(x1), x= x))
  expect_equal(kernlab::kernelMatrix(kern3, c(0, x))[1, -1, drop= TRUE],
               imputation:::kern_wt(sigma= sd(x2), x= x))
})


context("impute_fn_knn")

test_that("impute_fn_knn returns values accurately", {
  set.seed(1356)
  val1 <- rnorm(10)
  dist1 <- c(rep(1,5), rep(10,5))
  
  # case 1: first n values used
  expect_equal(imputation:::impute_fn_knn(val1, dist1, k= 5, sigma=1),
               imputation:::weighted_mean(val1[1:5], dist1[1:5]))
  expect_equal(imputation:::impute_fn_knn(val1, dist1, k= 3, sigma=1),
               imputation:::weighted_mean(val1[1:3], dist1[1:3]))
  
  # case 2: handling NAs -- which should never happen
  expect_equal(imputation:::impute_fn_knn(c(val1, NA), c(dist1, 100), k= 3, sigma=1),
               imputation:::weighted_mean(val1[1:3], dist1[1:3])) # here, the NA is ignored due to dist
  # here, the NA is used and returns NA
  expect_true(is.na(imputation:::impute_fn_knn(c(val1, NA), c(dist1, .5), k= 3, sigma=1)))
  
  # case 3: all values used
  expect_equal(imputation:::impute_fn_knn(val1, dist1, k= length(val1), sigma=1),
               imputation:::weighted_mean(val1, kern_wt(sigma= 1, dist1)))
})
