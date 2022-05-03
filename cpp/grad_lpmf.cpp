#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List grad_lpmf(DataFrame df,
               NumericMatrix Ut,
               NumericMatrix Vt,
               double u_lam,
               double v_lam,
               int batch,
               int num) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v, du, dv;
  NumericMatrix grad_Ut =  clone(Ut), grad_Vt =  clone(Vt);
  grad_Ut = u_lam * grad_Ut*num/batch;
  grad_Vt = v_lam * grad_Vt*num/batch;
  int i, j;
  double aux, g, r;
  for(int k = 0; k < df.rows(); k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    u = Ut.row(i-1);
    v = Vt.row(j-1);
    //du = grad_Ut.row(i-1);
    //dv = grad_Vt.row(j-1);
    aux = 0;
    for(int l = 0; l < u.size(); l++){
      aux += u[l]*v[l];
    }
    g = 1/(1+exp(-aux));
    aux = g*(1-g)*(g-r);
    grad_Ut.row(i-1) = aux*v + grad_Ut.row(i-1);
    grad_Vt.row(j-1) = aux*u + grad_Vt.row(j-1);
  }
  return List::create(
    Named("Ut") = grad_Ut,
    Named("Vt") = grad_Vt
  );
}


