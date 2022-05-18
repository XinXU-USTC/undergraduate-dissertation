loss_lpmf <- function(df, Ut, Vt, u_lam, v_lam) 
{
    sum <- loss_lpmf_aux(df, Ut, Vt, u_lam, v_lam)
    sum_u <- sum(Ut^2)*u_lam/2
    sum_v <- sum(Vt^2)*v_lam/2 
    sum <- sum + sum_u + sum_v
    return(sum)
}
