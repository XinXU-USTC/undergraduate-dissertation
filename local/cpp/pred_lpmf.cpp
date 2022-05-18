#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List pred_lpmf(DataFrame df,
               NumericMatrix Ut,
               NumericMatrix Vt) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v;
  NumericVector pred(rate.size());
  int i, j;
  double sum = 0, sum1 = 0, r;
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
    pred[k] = 1/(1+exp(-aux));
    //int rrr = floor(pred[k]*20);
    //if(rrr % 2 == 0){
    //  if(rrr != 0){
    //    rrr = rrr - 1;
    //  }else{
    //    rrr = 1;
    //  }
    //}
    //pred[k] = rrr/20;
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


/*** R
dim = 2
U <- matrix(rnorm(N*dim), nrow = dim)
V <- matrix(rnorm(M*dim), nrow = dim)
pred_lpmf(test, t(U), t(V))$mae
*/
