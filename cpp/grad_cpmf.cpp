#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List grad_cpmf(DataFrame df,
               NumericMatrix Yt,
               NumericMatrix Vt,
               NumericMatrix Wt,
               NumericMatrix Ut,
               List read,
               double u_lam,
               double v_lam,
               double w_lam,
               int batch,
               int num) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  IntegerVector user_idx = as<IntegerVector>(read["user_idx"]);
  IntegerVector user_num = as<IntegerVector>(read["user_num"]);
  List user_book = as<List>(read["user_book"]);
  NumericVector u, v;
  NumericMatrix grad_Vt =  clone(Vt), grad_Wt = clone(Wt), grad_Yt =  clone(Yt);
  grad_Yt = u_lam * grad_Yt*num/batch;
  grad_Vt = v_lam * grad_Vt*num/batch;
  grad_Wt = w_lam * grad_Wt*num/batch;
  int i, j, idx;
  IntegerVector b;
  double aux, g, r;
  for(int k = 0; k < df.rows(); k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    u = Ut.row(i-1);
    v = Vt.row(j-1);
    auto it = std::find(v.begin(), v.end(), i);
    // If element was found
    idx = it - v.begin();
    b = user_book[idx];
    aux = 0;
    for(int l = 0; l < u.size(); l++){
      aux += u[l]*v[l];
    }
    g = 1/(1+exp(-aux));
    aux = g*(1-g)*(g-r);
    grad_Yt.row(i-1) = aux*v + grad_Yt.row(i-1);
    grad_Vt.row(j-1) = aux*u + grad_Vt.row(j-1);
    for(int l = 0; l < b.size(); l++){
      grad_Wt.row(b[l]-1) = aux*v/user_num[idx] + grad_Wt.row(b[l]-1);
    }
  }
  return List::create(
    Named("Yt") = grad_Yt,
    Named("Vt") = grad_Vt,
    Named("Wt") = grad_Wt
  );
}


