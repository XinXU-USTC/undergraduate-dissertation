cpmf_mm <- function(data,
                    dim,
                    init,
                    eps = 1e-6,
                    max_iter = 1e2,
                    mu = 0.9,
                    t = 5e-3,
                    batch = 5e4,
                    verbose = FALSE){
  
  # initialize parameters
  sig2 <- init[1]
  sig2u <- init[2]
  sig2v <- init[3]
  sig2w <- init[4]
  
  # initialize U, V
#  Y <- matrix(rnorm(N*dim, sd = sqrt(sig2u)), nrow = dim)
#  V <- matrix(rnorm(M*dim, sd = sqrt(sig2v)), nrow = dim)
#  W <- matrix(rnorm(M*dim, sd = sqrt(sig2w)), nrow = dim)
  
  Y <- matrix(0, nrow = dim, ncol = N)
  V <- matrix(0, nrow = dim, ncol = M)
  W <- matrix(0, nrow = dim, ncol = M)


  min.Y <- Y
  min.V <- V
  min.W <- W
  
  # define variables to record the process
  U <- t(user_cpmf(read, t(Y), t(W)))
  aux <- pred_cpmf(data, t(U), t(V))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  tr.loss <- loss_cpmf(data, t(Y), t(V), t(W), t(U), sig2/sig2u, sig2/sig2v, sig2/sig2w)
  min.mae <- tr.mae
  min.rmse <- aux$rmse
  tr.dlt <- NULL #relative change of the loss

  
  V_Y <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = N)
  V_V <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = M)
  V_W <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = M)
  
  for(epoch in 1:max_iter){
    
    # calculate the number of batches
    num_bat <- ceiling(nrow(data)/batch)
    bat_idx <- rep((1:(num_bat-1)), batch)
    red <- nrow(data) %% batch
    bat_idx <- c(bat_idx, rep(num_bat, red))
    bat_idx <- sample(bat_idx)
    
    # for each batch
    for(iter in 1:num_bat){
      idx <- which(bat_idx == iter)
      df <- data[idx, ]
      
      # calculate gradient
      U <- t(user_cpmf(read, t(Y), t(W)))
      grad <- grad_cpmf(df, t(Y), t(V), t(W), t(U), read, sig2/sig2u, sig2/sig2v, sig2/sig2w, batch, nrow(data))
      grad_Y <- t(grad$Yt)
      grad_V <- t(grad$Vt)
      grad_W <- t(grad$Wt)
      
      # calculate momentum
      V_Y <- mu*V_Y + t*grad_Y
      V_V <- mu*V_V + t*grad_V
      V_W <- mu*V_W + t*grad_W
      
      # update U, V
      Y <- Y - V_Y
      V <- V - V_V
      W <- W - V_W
      #upd_num <- upd_num + 1
      
      
    }# batch
    
    # record loss/mae/rmse/dlt
    U <- t(user_cpmf(read, t(Y), t(W)))
    loss <- loss_cpmf(data, t(Y), t(V), t(W), t(U), sig2/sig2u, sig2/sig2v, sig2/sig2w)
    aux <- pred_cpmf(test, t(U), t(V))
    tr.rmse <- c(tr.rmse, aux$rmse)
    tr.mae <- c(tr.mae, aux$mae)
    last_loss <- tr.loss[length(tr.loss)]
    tr.loss <- c(tr.loss, loss)
    dlt <- abs((loss - last_loss)/last_loss)
    tr.dlt <- c(tr.dlt, dlt)
    
    if(min.mae > aux$mae){
      min.mae <- aux$mae
      min.rmse <- aux$rmse
      min.Y <- Y
      min.V <- V
      min.W <- W
    }
    
    
    # print info
    if(verbose && epoch %% 2 == 1){
      print(Sys.time())
      cat(paste("Epoch", epoch, "\t loss:", loss, "\t dlt:", dlt))
      cat(paste("\nmae:", aux$mae, "\trmse:", aux$rmse, "\n"))
    }
    
    # check if convergent
    if(dlt < eps){
      break
    }
    
  }# epoch
  
  result <- list(Y = min.Y,
                 V = min.V,
                 W = min.W,
                 U = t(user_cpmf(read, t(min.Y), t(min.W))),
                 parameters = c(sig2, sig2u, sig2v, sig2w),
                 mae = min.mae,
                 rmse = min.rmse,
                 tr.mae = tr.mae,
                 tr.rmse = tr.rmse,
                 tr.loss = tr.loss,
                 tr.delta = tr.dlt)
  dir <- "~/nPMF/result/cpmf/"
  save(result, file = paste0(dir ,"mm-",dim, ".Rda"))
  return(result)
}