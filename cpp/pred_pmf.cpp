#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List pred_pmf(DataFrame df,
              NumericMatrix Ut,
              NumericMatrix Vt) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector u, v;
  IntegerVector pred(rate.size());
  int i, j, r;
  double sum = 0, sum1 = 0;
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
    pred[k] = floor(aux);
    if(pred[k] > 10){
      pred[k] = 10;
    }
    if(pred[k] < 1){
      pred[k] = 1;
    }
    sum += (r-aux)*(r-aux);
    if(r-aux > 0){
      sum1 += (r-aux);
    }else{
      sum1 += (aux-r);
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
pred_pmf(test, t(U), t(V))$rmae
*/
