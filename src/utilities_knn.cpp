
#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//-----------------------------------------------------------------------------
NumericMatrix row_erase (NumericMatrix& x, int& rowID) {
  NumericMatrix x2(Dimension(x.nrow()-1, x.ncol()));
  int iter = 0; // possibly make this a pointer?
  for (int i = 0; i < x.nrow(); i++) {
    if (i != rowID) {
      x2.row(iter) = x.row(i);
      iter++;
    }
  }
  return x2;
}

NumericMatrix row_erase (NumericMatrix& x, IntegerVector& rowID) {
  rowID = rowID.sort();

  NumericMatrix x2(Dimension(x.nrow()- rowID.size(), x.ncol()));
  int iter = 0;
  int del = 1; // to count deleted elements
  for (int i = 0; i < x.nrow(); i++) {
    if (i != rowID[del - 1]) {
      x2.row(iter) = x.row(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}
//-----------------------------------------------------------------------------
NumericMatrix col_erase (NumericMatrix& x, int& colID) {
  NumericMatrix x2(Dimension(x.nrow(), x.ncol() - 1));
  int iter = 0; // possibly make this a pointer?
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID) {
      x2.row(iter) = x.row(i);
      iter++;
    }
  }
  return x2;
}


NumericMatrix col_erase (NumericMatrix& x, IntegerVector& colID) {
  colID = colID.sort();

  NumericMatrix x2(Dimension(x.nrow(), x.ncol()- colID.size()));
  int iter = 0;
  int del = 1;
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID[del - 1]) {
      x2.column(iter) = x.column(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}

//-----------------------------------------------------------------------------
// [[Rcpp::export]]
IntegerVector int_rownames(const NumericMatrix x) {
    List dimnames = x.attr("dimnames");
    CharacterVector rownames = dimnames[0];
    IntegerVector out(rownames.size());

    std::transform(rownames.begin(), rownames.end(), out.begin(), atoi);

    return out;
}

// take a vector of rownames (as in above function) and a comparison integer
// return the index in rownames_vec that matches the comparison integer
// [[Rcpp::export]]
int rowname_match(const IntegerVector& rowname_vec, const int& rowID) {
  int matchID = -1;
  for (int ic = 0; ic < rowname_vec.size(); ic++) {
      if (rowname_vec[ic] == rowID) {
        matchID = ic;
        break;
      }
  }
  return matchID;
}
