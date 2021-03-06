# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title Calculate \eqn{L_q} distance of two vectors
#' @description Calculate \eqn{L_q} distance of two vectors
#' @param x A numeric vector. Missing values are allowed.
#' @param y A numeric vector. Missing values are allowed.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a scalar
dist_q <- function(x, y, q) {
    .Call('imputation_dist_q', PACKAGE = 'imputation', x, y, q)
}

#' @title Calculate \eqn{L_q} distance
#' @description Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
#' vector.
#' @param x A numeric matrix Missing values are allowed.
#' @param ref An integer specifying the reference row.
#' @param q An integer specifying the which norm to take the L-q distance of.
#' @return a numeric vector of length \code{nrow(x) - 1}
dist_q_matrix <- function(x_ref, x_rest, q) {
    .Call('imputation_dist_q_matrix', PACKAGE = 'imputation', x_ref, x_rest, q)
}

#' @title Sort vector and return indexes
#' @description Sort the vector by the values
#' Return the indexes of the sorted vector according to original
#' @param values The vector that should be sorted
sort_indices <- function(values) {
  .Call('imputation_sort_indices', PACKAGE = 'imputation', values) 
}

weighted_mean <- function(x, w) {
    .Call('imputation_weighted_mean', PACKAGE = 'imputation', x, w)
}

sort_indices <- function(values) {
    .Call('imputation_sort_indices', PACKAGE = 'imputation', values)
}

kern_wt <- function(sigma, x) {
    .Call('imputation_kern_wt', PACKAGE = 'imputation', sigma, x)
}

int_rownames <- function(x) {
    .Call('imputation_int_rownames', PACKAGE = 'imputation', x)
}

rowname_match <- function(rowname_vec, rowID) {
    .Call('imputation_rowname_match', PACKAGE = 'imputation', rowname_vec, rowID)
}

which_na <- function(x) {
    .Call('imputation_which_na', PACKAGE = 'imputation', x)
}

