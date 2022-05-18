loss_cpmf <- function(df, Yt, Vt, Wt, Ut, u_lam, v_lam, w_lam){
    sum <- loss_cpmf_aux(df, Yt, Vt, Wt, Ut, u_lam, v_lam, w_lam)
    sum_u <- sum(Ut^2)/2*u_lam
    sum_v <- sum(Vt^2)/2*v_lam
    sum_w <- sum(Wt^2)/2*w_lam
    sum <- sum + sum_u + sum_v + sum_w
    return(sum)
}
