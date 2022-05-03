#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double loss_cpmf_aux(DataFrame df,
                 NumericMatrix Yt,
                 NumericMatrix Vt,
                 NumericMatrix Wt,
                 NumericMatrix Ut,
                 double u_lam,
                 double v_lam,
                 double w_lam) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v;
  int i, j;
  double sum = 0, sum_u = 0, sum_v = 0, r, sum_w= 0;
  double aux;
  for(int k = 0; k < df.rows(); k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    u = Ut.row(i-1);
    v = Vt.row(j-1);
    aux = 0;
    for(int l = 0; l < u.size(); l++){
      aux += u[l]*v[l];
    }
    aux = 1/(1+exp(-aux));
    sum += (r-aux)*(r-aux);
  }
//  for(int k = 0; k < Yt.size(); k++){
//    sum_u += Yt[k]*Yt[k];
//    sum_v += Vt[k]*Vt[k];
//    sum_w += Wt[k]*Wt[k];
//  }
//  sum_u *= u_lam;
//  sum_v *= v_lam;
//  sum_w *= w_lam;
//  sum += (sum_u + sum_v + sum_w);
  sum /= 2;
  return sum;
}


/*** R
dim = 2
Y <- matrix(rnorm(N*dim), nrow = dim)
V <- matrix(rnorm(M*dim), nrow = dim)
W <- matrix(rnorm(M*dim), nrow = dim)
Ut <- user_cpmf(read, t(Y), t(W))
loss_cpmf_aux(train, t(Y), t(V), t(W), Ut, 1, 1, 1)
*/
