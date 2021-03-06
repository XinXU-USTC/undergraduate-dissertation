#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double loss_pmf_aux(DataFrame df,
            NumericMatrix Ut,
            NumericMatrix Vt,
            double u_lam,
            double v_lam) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v;
  int i, j, r;
  double sum = 0, sum_u = 0, sum_v = 0;
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
    sum += (r-aux)*(r-aux);
  }
//  for(int k = 0; k < Ut.size(); k++){
//    sum_u += Ut[k]*Ut[k];
//    sum_v += Vt[k]*Vt[k];
//  }
//  sum_u *= u_lam;
//  sum_v *= v_lam;
//  sum += (sum_u + sum_v);
  sum /= 2;
  return sum;
}


