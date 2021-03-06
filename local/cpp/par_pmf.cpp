#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector par_pmf(DataFrame df, 
                      NumericMatrix Ut, 
                      NumericMatrix Vt) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v, sum(3);
  sum[0] = sum[1] = sum[2] = 0;
  int count = user.size();
  int n = df.nrows();
  int i, j, r;
  double aux;
  for(int k = 0; k < n; k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    u = Ut.row(i-1);
    v = Vt.row(j-1);
    aux = 0;
    for(int l = 0; l < u.size(); l++){
      aux += u[l]*v[l];
    }
    sum[0] += (r-aux)*(r-aux);
  }
  sum[0] = sum[0]/count;
  for(int k = 0; k < Ut.rows(); k++){
    u = Ut.row(k);
    for(int l = 0; l < u.size(); l++){ 
      sum[1] += u[l]*u[l];
    }
  }
  for(int k = 0; k < Vt.rows(); k++){
    v = Vt.row(k);
    for(int l = 0; l < v.size(); l++){ 
      sum[2] += v[l]*v[l];
    }
  }
  return sum;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
dim = 2
U <- matrix(rnorm(N*dim), nrow = dim)
V <- matrix(rnorm(M*dim), nrow = dim)
par_pmf(train, t(U), t(V))
*/
