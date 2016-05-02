
library(testthat)
library(imputation)

context("impute_fn_knn_all.nonPar")

test_that("function returns no missing", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  rownames(x1) <- 1:nrow(x1)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.nonPar(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, verbose= FALSE)
  
  orig_miss <- imp_pre1$x_missing[which(is.na(imp_pre1$x_missing))]
  imputed <- xm_imp1[which(is.na(imp_pre1$x_missing))]
  
  # no missing
  expect_equal(sum(is.na(xm_imp1)), 0)
  expect_equal(length(orig_miss), length(imputed))
  
})

test_that("function returns accurately ", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  rownames(x1) <- 1:nrow(x1)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.nonPar(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, verbose= FALSE)
  
  # all values w/in bounds
  orig_miss <- apply(imp_pre1$x_missing, 2, function(j) {as.vector(which(is.na(j)))})
  
  bounded <- vector(mode= "logical", length= ncol(xm_imp1))
  for (j in 1:(length(orig_miss) - 1)) {
    min_j <- min(x1[,j], na.rm=T)
    max_j <- max(x1[,j], na.rm=T)
    
    if (all(xm_imp1[orig_miss[[j]], j] >= min_j) &
          all(xm_imp1[orig_miss[[j]], j] <= max_j)) {
      bounded[j] <- TRUE
    } else {
      bounded[j] <- FALSE
    }
  }
  
  expect_true(all(bounded)) ## all imputed values w/in range bounded by their neighbors
  
})


test_that("equiv: high-rownames / low-rownames -- rownames handled accurately", {
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



context("impute_fn_knn_all.Par")

test_that("function returns no missing", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  rownames(x1) <- 1:nrow(x1)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.Par(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, leave_cores = round(parallel::detectCores() / 2, 0))
  
  orig_miss <- imp_pre1$x_missing[which(is.na(imp_pre1$x_missing))]
  imputed <- xm_imp1[which(is.na(imp_pre1$x_missing))]
  
  # no missing
  expect_equal(sum(is.na(xm_imp1)), 0)
  expect_equal(length(orig_miss), length(imputed))
  
})

test_that("function returns accurately ", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  rownames(x1) <- 1:nrow(x1)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.Par(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, leave_cores = round(parallel::detectCores() / 2, 0))
  
  # all values w/in bounds
  orig_miss <- apply(imp_pre1$x_missing, 2, function(j) {as.vector(which(is.na(j)))})
  
  bounded <- vector(mode= "logical", length= ncol(xm_imp1))
  for (j in 1:(length(orig_miss) - 1)) {
    min_j <- min(x1[,j], na.rm=T)
    max_j <- max(x1[,j], na.rm=T)
    
    if (all(xm_imp1[orig_miss[[j]], j] >= min_j) &
        all(xm_imp1[orig_miss[[j]], j] <= max_j)) {
      bounded[j] <- TRUE
    } else {
      bounded[j] <- FALSE
    }
  }
  
  expect_true(all(bounded)) ## all imputed values w/in range bounded by their neighbors
  
})


test_that("equiv: high-rownames / low-rownames -- rownames handled accurately", {
  set.seed(34)
  x1 <- matrix(rnorm(300), 30, 10)
  x1[x1 > 1.25] <- NA
  x1a <- x1
  rownames(x1) <- 500:(500 + nrow(x1) - 1)
  rownames(x1a) <- 1:nrow(x1a)
  
  imp_pre1 <- impute_prelim(x1, parallel = FALSE)
  imp_pre1a <- impute_prelim(x1a, parallel = FALSE)
  
  xm_imp1 <- impute_fn_knn_all.Par(
    x_missing= imp_pre1$x_missing,
    x_complete= x1, k=3, q=2, 
    sigma= 1, leave_cores = round(parallel::detectCores() / 2, 0))
  
  xm_imp1a <- impute_fn_knn_all.Par(
    x_missing= imp_pre1a$x_missing,
    x_complete= x1a, k=3, q=2, 
    sigma= 1, leave_cores = round(parallel::detectCores() / 2, 0))
  
  dimnames(xm_imp1) <- dimnames(xm_imp1a) <- NULL
  expect_equal(xm_imp1, xm_imp1a)
  
})


