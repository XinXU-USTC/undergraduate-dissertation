#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
List pred_bookavg(DataFrame df,
                  NumericVector book_avg,
                  CharacterVector book_idx) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  NumericVector pred(rate.size());
  book_avg.names() = book_idx;
  int j;
  double sum = 0, sum1 = 0, r;
  for(int k = 0; k < df.rows(); k++){
    j = book[k];
    r = rate[k];
    auto it = std::find(book_idx.begin(), book_idx.end(), j);
    int index = it - book_idx.begin();
    pred[k] = book_avg[index];
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

*/
