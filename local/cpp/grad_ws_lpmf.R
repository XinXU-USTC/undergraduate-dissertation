grad_ws_lpmf <- function(data, Ut, Vt, FUt, FVt, WUt, WVt, u_lam, v_lam, wu_lam, wv_lam, batch, num){
  grad <- grad_ws_lpmf_aux(data, Ut, Vt, FUt, FVt, WUt, WVt, u_lam, v_lam, wu_lam, wv_lam, batch, num)
  grad_WUt <- wu_lam*WUt
  grad_WVt <- wv_lam*WVt
  aux_ut <- t(WUt[ ,1+FUt[,2]] + WUt[,1])
  aux_ut[which(FUt[, 2] == 0), ] = aux_ut[which(FUt[, 2] == 0), ] / 2
  aux_ut <- aux_ut - Ut
  grad_WUt[, 1] <- grad_WUt[, 1] + u_lam*aux_ut[, 1]
  grad_WUt[1:row(Ut), FUt[, 2]] <- grad_WUt[1:row(Ut), FUt[, 2]] + u_lam*aux_ut[1:row(Ut), FUt[, 2]]
  grad_WUt <- grad_WUt*num/batch
  grad_WVt <- grad_WVt*num/batch
  return(list(Ut = grad$Ut, Vt = grad$Vt, WUt = grad_WUt, WVt = grad_WVt ))
}

