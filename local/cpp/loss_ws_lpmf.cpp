#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double loss_ws_lpmf_aux(DataFrame df,
                     NumericMatrix Ut,
                     NumericMatrix Vt,
                     NumericMatrix FUt,
                     NumericMatrix FVt,
                     NumericMatrix WUt,
                     NumericMatrix WVt,
                     double u_lam,
                     double v_lam,
                     double wu_lam,
                     double wv_lam) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v;
  int i, j;
  double sum = 0, r;
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
  sum /= 2;
  return sum;
}

