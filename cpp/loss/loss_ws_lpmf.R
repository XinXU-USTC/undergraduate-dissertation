loss_ws_lpmf <- function(df, Ut, Vt, FUt, FVt, WUt, WVt, u_lam, v_lam, wu_lam, wv_lam) 
{
  sum <- loss_ws_lpmf_aux(df, Ut, Vt, FUt, FVt, WUt, WVt, u_lam, v_lam, wu_lam, wv_lam) 
  sum_wu <- sum(WUt^2)*wu_lam/2
  sum_wv <- sum(WVt^2)*wv_lam/2 
  #sum_u <- t(ifelse(FUt[,2] == 0, 0, WUt[ ,1+FUt[,2]]) + WUt[,1])
  sum_u <- t(WUt[ ,1+FUt[,2]] + WUt[,1])
  sum_u[which(FUt[, 2] == 0), ] = sum_u[which(FUt[, 2] == 0), ] / 2
  #browser()
  sum_u <- sum((Ut - sum_u)^2)*u_lam/2
  #sum_v <- t(ifelse(FVt[,2] == 0, 0, WVt[ ,1+FVt[,2]]) + WVt[,1])
  sum_v <- t(WVt[ ,1+FVt[,2]] + WVt[,1])
  sum_v[which(FVt[, 2] == 0), ] = sum_v[which(FVt[, 2] == 0), ] / 2
  sum_v <- sum((Vt - sum_v)^2)*v_lam/2
  sum <- sum + sum_wu + sum_wv + sum_u + sum_v
  return(sum)
}
