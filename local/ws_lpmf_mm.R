ws_lpmf_mm <- function(data,
                    dim,
                    init,
                    FU,
                    FV,
                    eps = 1e-6,
                    max_iter = 2e2,
                    mu = 0.9,
                    t = 1e-2,
                    batch = 5e4,
                    verbose = FALSE){
  
  
  # initialize parameters
  sig2 <- init[1]
  sig2u <- init[2]
  sig2v <- init[3]
  sig2wu <- init[4]
  sig2wv <- init[5]
  a <- max(FU[2, ])
  b <- max(FV[2, ])
  
  # initialize U, V
  #  U <- matrix(rnorm(N*dim, sd = sqrt(sig2u)), nrow = dim)
  #  V <- matrix(rnorm(M*dim, sd = sqrt(sig2v)), nrow = dim)
  U <- matrix(0, nrow = dim, ncol = N)
  V <- matrix(0, nrow = dim, ncol = M)
  WU <- matrix(0, nrow = 1+a, ncol = dim)
  WV <- matrix(0, nrow = 1+b, ncol = dim)
  min.U <- U
  min.V <- V
  min.WU <- WU
  min.WV <- WV
  
  # define variables to record the process
  aux <- pred_ws_lpmf(test, t(FU), t(FV), t(WU), t(WV))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  min.mae <- aux$mae
  min.rmse <- aux$rmse
  tr.loss <- loss_ws_lpmf(data, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv)
  #tr.sig2 <- sig2
  #tr.sig2u <- sig2u
  #tr.sig2v <- sig2v
  tr.dlt <- NULL #relative change of the loss
  #upd_num <- 0
  #browser()
  V_U <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = N)
  V_V <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = M)
  V_WU <- matrix(rnorm(1+a, sd = 0.01), nrow = 1+a, ncol = dim)
  V_WV <- matrix(rnorm(1+b, sd = 0.01), nrow = 1+b, ncol = dim)
  #V_WU <- matrix(0, nrow = 1+a, ncol = dim)
  #V_WV <- matrix(0, nrow = 1+b, ncol = dim)
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
      #browser()
      grad <- grad_ws_lpmf(df, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv, batch, nrow(data))
      grad_U <- t(grad$Ut)
      grad_V <- t(grad$Vt)
      #grad_WU <- t(grad$WUt)
      #grad_WV <- t(grad$WVt)
      
      # calculate momentum
      V_U <- mu*V_U + t*grad_U
      V_V <- mu*V_V + t*grad_V
      
      
      #V_WU <- mu*V_WU + t*grad_WU
      #V_WV <- mu*V_WV + t*grad_WV
      
      # update U, V
      U <- U - V_U
      V <- V - V_V
      
      
      for(qwq in 1){
        grad <- grad_ws_lpmf(df, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv, batch, nrow(data))
        grad_WU <- t(grad$WUt)
        grad_WV <- t(grad$WVt)
        V_WU <- mu*V_WU + t*grad_WU
        V_WV <- mu*V_WV + t*grad_WV
        
        WU <- WU - V_WU
        WV <- WV - V_WV
        #browser()
      }
      #upd_num <- upd_num + 1
      
      
    }# batch
    #browser()
    # record loss/mae/rmse/dlt
    loss <- loss_ws_lpmf(data, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv)
    aux <- pred_ws_lpmf(test, t(FU), t(FV), t(WU), t(WV))
    tr.rmse <- c(tr.rmse, aux$rmse)
    tr.mae <- c(tr.mae, aux$mae)
    last_loss <- tr.loss[length(tr.loss)]
    tr.loss <- c(tr.loss, loss)
    dlt <- abs((loss - last_loss)/last_loss)
    tr.dlt <- c(tr.dlt, dlt)
    
    # record the best mae and corresponding U, V
    if(min.mae > aux$mae){
      min.mae <- aux$mae
      min.rmse <- aux$rmse
      min.U <- U
      min.V <- V
      min.WU <- WU
      min.WV <- WV
    }
    
    
    # print info
    if(verbose && epoch %% 2 == 1){
      print(Sys.time())
      cat(paste("Epoch", epoch, "\t loss:", loss, "\t dlt:", dlt))
      cat(paste("\nmae:", aux$mae, "\trmse:", aux$rmse, "\n"))
      #if(epoch %% 20 == 0){
      #  print(paste("par:", sig2, sig2u, sig2v))
      #}
      #cat(paste("\t#updates:", upd_num, "\n"))
    }
    
    # record U, V every 10 epochs
    #if(epoch %% 100 == 0){
    #  dir <- "~/PMF/lpmf/"
    #  save(U, file = paste0(dir, "U/", dim, "-", epoch, ".Rda"))
    #  save(V, file = paste0(dir, "V/", dim, "-", epoch, ".Rda"))
    #}
    
    # update parameters every 10/100 updates of feature matrix
    #if(epoch %% 20 == 0){
    #  pn <- par_lpmf(data, t(U), t(V))
    #  pn[2] <- pn[2]/(N+2)/dim
    #  pn[3] <- pn[3]/(M+2)/dim
    #if(epoch %% 100 == 0){
    # update all parameters
    #   sig2 <- pn[1]
    #   sig2u <- pn[2]
    #   sig2v <- pn[3]
    #   tr.sig2 <- c(tr.sig2, sig2)
    #   tr.sig2u <- c(tr.sig2u, sig2u)
    #   tr.sig2v <- c(tr.sig2v, sig2v)
    #}else{
    # update all but sig2
    #    sig2u <- pn[2]
    #    sig2v <- pn[3]
    #    tr.sig2u <- c(tr.sig2u, sig2u)
    #    tr.sig2v <- c(tr.sig2v, sig2v)
    # }
    #}# if
    
    # check if convergent
    if(dlt < eps){
      break
    }
    
  }# epoch
  
  result <- list(U = min.U,
                 V = min.V,
                 WU = min.WU,
                 WV = min.WV,
                 parameters = c(sig2, sig2u, sig2v, sig2wu, sig2wv),
                 mae = min.mae,
                 rmse = min.rmse,
                 tr.mae = tr.mae,
                 tr.rmse = tr.rmse,
                 tr.loss = tr.loss,
                 tr.delta = tr.dlt)
  #dir <- "~/nPMF/result/lpmf/"
  #save(result, file = paste0(dir ,"mm-",dim, ".Rda"))
  return(result)
}