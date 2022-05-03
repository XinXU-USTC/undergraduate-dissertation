loss_pmf <- function(df, Ut, Vt, u_lam, v_lam) {
    sum <- loss_pmf_aux(df, Ut, Vt, u_lam, v_lam)
    sum_u <- sum(Ut^2)/2*u_lam
    sum_v <- sum(Vt^2)/2*v_lam
    sum <- sum + sum_u + sum_v
    return(sum)
}

