#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericMatrix user_cpmf(List read,
                        NumericMatrix Yt,
                        NumericMatrix Wt) {
  IntegerVector user_idx = as<IntegerVector>(read["user_idx"]);
  IntegerVector user_num = as<IntegerVector>(read["user_num"]);
  List user_book = as<List>(read["user_book"]);
  NumericMatrix Ut = clone(Yt);
  int user, num;
  IntegerVector book;
  NumericVector sum(Wt.row(0).size());
  sum = 0;
  for(int i = 0; i < user_idx.size(); i++){
    user = user_idx[i];
    book = user_book[i];
    num = user_num[i];
    for(int j = 0; j < book.size(); j++){
      sum = sum + Wt.row(book[j]-1);
    }
    sum = sum/num;
    Ut.row(user-1) = Yt.row(user-1) + sum;
  }
  return Ut;
}


/*** R
dim = 2
Y <- matrix(rnorm(N*dim), nrow = dim)
W <- matrix(rnorm(M*dim), nrow = dim)
aaa <- user_cpmf(read, t(Y), t(W))
*/
