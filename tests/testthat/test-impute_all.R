
library(testthat)
library(imputation)

context("impute_fn_knn_all.nonPar")

test_that("function returns accurately -- no missing", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  rownames(x1) <- 1:nrow(x1)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.nonPar(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, verbose= FALSE)
  
  # no missing
  expect_equal(sum(is.na(xm_imp1)), 0)
})


test_that("equivalence b/w high-rownames / low-rownames", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  x1a <- x1
  rownames(x1) <- 500:(500 + nrow(x1) - 1)
  rownames(x1a) <- 1:nrow(x1a)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  imp_pre1a <- impute_prelim(x1a, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.nonPar(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, verbose= FALSE)
  
  xm_imp1a <- impute_fn_knn_all.nonPar(
    x_missing= imp_pre1a$x_missing,
    x_complete= x1a, k=3, q=2, 
    sigma= 1, verbose= FALSE)
  
  dimnames(xm_imp1) <- dimnames(xm_imp1a) <- NULL
  expect_equal(xm_imp1, xm_imp1a)
  
})



# context("impute_fn_knn_all.Par")



