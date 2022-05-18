#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List grad_ws_lpmf(DataFrame df,
               NumericMatrix Ut,
               NumericMatrix Vt,
               NumericMatrix FUt,
               NumericMatrix FVt,
               NumericMatrix WUt,
               NumericMatrix WVt,
               double u_lam,
               double v_lam,
               double wu_lam,
               double wv_lam,
               int batch,
               int num) {
  NumericVector user = as<NumericVector>(df["User.ID"]);
  NumericVector book = as<NumericVector>(df["ISBN"]);
  NumericVector rate = as<NumericVector>(df["Book.Rating"]);
  int dim = transpose(Ut).rows();
  NumericVector u(dim), v(dim), du, dv, fu, fv;
  NumericMatrix grad_Ut =  clone(Ut), grad_Vt =  clone(Vt);
  NumericMatrix  grad_WUt = clone(WUt), grad_WVt = clone(WVt);
  NumericMatrix aux_Ut = clone(Ut), aux_Vt = clone(Vt);
  //printf("here 26\n");
  for(int k = 0; k < Ut.rows(); k++){
    fu = FUt.row(k);
    for(int l = 0; l < dim; l++){
      u[l] = fu[0]*WUt(l,0);
      if(fu[1] != 0){
        u[l] += WUt(l, fu[1]);
      }
    }
    aux_Ut.row(k) = Ut.row(k) - u;
  }
  //printf("here 37\n");
  for(int k = 0; k < Vt.rows(); k++){
    fv = FVt.row(k);
    for(int l = 0; l < dim; l++){
      v[l] = fv[0]*WVt(l,0);
      if(fv[1]!=0){
        v[l] += WVt(l, fv[1]);
      }
    }
    aux_Vt.row(k) = Vt.row(k) - v;
  }
  //printf("here 48\n");
  grad_Ut = u_lam * aux_Ut*num/batch;
  grad_Vt = v_lam * aux_Vt*num/batch;
  int i, j;
  double aux, g, r;
  //printf("%d", Ut.rows());
  for(int k = 0; k < df.rows(); k++){
    i = user[k];
    j = book[k];
    r = rate[k];
    u = Ut.row(i-1);
    //printf("here 58\n");
    v = Vt.row(j-1);
    //printf("here 60\n");
    aux = 0;
    for(int l = 0; l < u.size(); l++){
      aux += u[l]*v[l];
    }
    g = 1/(1+exp(-aux));
    aux = g*(1-g)*(g-r);
    grad_Ut.row(i-1) = aux*v + grad_Ut.row(i-1);
    grad_Vt.row(j-1) = aux*u + grad_Vt.row(j-1);
  }
  grad_WUt = wu_lam*WUt;
  grad_WVt = wv_lam*WVt;
  //printf("here 70\n");
  for(int i = 0; i < Ut.rows(); i++){
    fu = FUt.row(i);
    for(int k = 0; k < grad_WUt.rows(); k++){
      grad_WUt(k, 0) = grad_WUt(k, 0) - u_lam*aux_Ut(i, k);
      grad_WUt(k, fu[1]) = grad_WUt(k, fu[1]) - u_lam*aux_Ut(i, k);
    }
  }
  //printf("here 78\n");
  for(int i = 0; i < Vt.rows(); i++){
    fv = FVt.row(i);
    for(int k = 0; k < grad_WVt.rows(); k++){
      grad_WVt(k, 0) = grad_WVt(k, 0) - v_lam*aux_Vt(i, k);
      grad_WVt(k, fv[1]) = grad_WVt(k, fv[1]) - v_lam*aux_Vt(i, k);
    }
  }
  //printf("here 86\n");
  //for(int k = 0; k < Ut.rows(); k++){
  //  fu = FUt.row(k);
  //  grad_WUt(k, 0) = grad_WUt(k, 0) - u_lam*aux_Ut(k, 0);
    //double aaa = grad_WUt(k, fu[1]) - u_lam*aux_Ut(k, fu[1]);
    //if(k > 10 & k < 20){
    //  printf("%f\n", aaa);
    //}
    //grad_WUt(k, fu[1]) = grad_WUt(k, fu[1]) - u_lam*aux_Ut(k, fu[1]);
    //if(fu[1] != 0){
      //printf("%d\n", k);
      //grad_WUt(k, fu[1]) = grad_WUt(k, fu[1]) - u_lam*aux_Ut(k, fu[1]);
   // }
  //}
  //for(int k = 0; k < Vt.rows(); k++){
  //  fv = FVt.row(k);
  //  grad_WVt(k, 0) -= u_lam*aux_Vt(k, 0);
  //  if(fv[1] != 0){
  //    grad_WVt(k, fv[1]) -= u_lam*aux_Vt(k, fv[1]);
  //  }
  //}
  
  grad_WUt = grad_WUt*num/batch;
  grad_WVt = grad_WVt*num/batch;
  
  
  return List::create(
    Named("Ut") = grad_Ut,
    Named("Vt") = grad_Vt,
    Named("WUt") = grad_WUt,
    Named("WVt") = grad_WVt
  );
}
/*** R
dim = 2
a <- max(FU[2, ])
b <- max(FV[2, ])
U <- matrix(0, nrow = dim, ncol = N)
V <- matrix(0, nrow = dim, ncol = M)
WU <- matrix(0, nrow = 1+a, ncol = dim)
WV <- matrix(0, nrow = 1+b, ncol = dim)
aaa <- grad_ws_lpmf(train, t(U), t(V), t(FU), t(FV), t(WU), t(WV), 1, 1, 1e-1, 1e-1, 3e4, nrow(train))
*/

