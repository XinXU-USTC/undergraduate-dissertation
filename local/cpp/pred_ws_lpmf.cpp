#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List pred_ws_lpmf(DataFrame df,
               NumericMatrix FUt,
               NumericMatrix FVt,
               NumericMatrix WUt,
               NumericMatrix WVt) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  int dim = WUt.rows();
  NumericVector fu, fv, u(dim), v(dim);
  NumericVector pred(rate.size());
  int i, j;
  double sum = 0, sum1 = 0, r;
  double aux;
  for(int k = 0; k < df.rows(); k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    fu = FUt.row(i-1);
    fv = FVt.row(j-1);
    aux = 0;
    for(int l = 0; l < dim; l++){
      u[l] = fu[0]*WUt(l,0);
      if(fu[1] != 0){
        u[l] += WUt(l, fu[1]);
      }
      v[l] = fv[0]*WVt(l,0);
      if(fv[1] != 0){
        v[l] += WVt(l, fv[1]);
      }
      aux += u[l]*v[l];
    }
    pred[k] = 1/(1+exp(-aux));
    pred[k] = floor(pred[k]*9)/9;
    sum += (r-pred[k])*(r-pred[k]);
    if(r-pred[k] > 0){
      sum1 += (r-pred[k]);
    }else{
      sum1 += (pred[k]-r);
    }
    
  }
  
  return List::create(
    Named("prediction") = pred,
    Named("rmse") = sqrt(sum/rate.size()),
    Named("mae") = sum1/rate.size()
  );
}


