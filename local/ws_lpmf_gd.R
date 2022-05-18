ws_lpmf_gd <- function(data,
                    dim,
                    init,
                    U,
                    V,
                    WU,
                    WV,
                    FU,
                    FV,
                    eps = 1e-6,
                    max_iter = 2e4,
                    alpha = 0.4,
                    beta = 0.7,
                    verbose = FALSE,
                    batch = 5e3){
  
  
  # initialize parameters
  sig2 <- init[1]
  sig2u <- init[2]
  sig2v <- init[3]
  sig2wu <- init[4]
  sig2wv <- init[5]
  a <- max(FU[2, ])
  b <- max(FV[2, ])
  
  # define variables to record the process
  aux <- pred_ws_lpmf(test, t(FU), t(FV), t(WU), t(WV))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  min.mae <- aux$mae
  min.rmse <- aux$rmse
  tr.loss <- loss_ws_lpmf(data, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv)
  tr.dlt <- NULL #relative change of the loss
  
  
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
      grad <- grad_ws_lpmf(df, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv, batch, nrow(data))
      grad_U <- t(grad$Ut)
      grad_V <- t(grad$Vt)
      #grad_WU <- t(grad$WUt)
      #grad_WV <- t(grad$WVt)
      
      # calculate step size
      t <- 1
      while(loss_ws_lpmf(data, t(U-t*grad_U), t(V-t*grad_V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv) > tr.loss[length(tr.loss)]  - alpha*t*(sum((grad_U)^2)+sum(grad_V^2))){
        t <- beta*t
        #browser()
      }
      #browser()
      # update U, V
      U <- U - t*grad_U
      V <- V - t*grad_V
      
      grad <- grad_ws_lpmf(df, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv, batch, nrow(data))
      grad_WU <- t(grad$WUt)
      grad_WV <- t(grad$WVt)
      
      t <- 1
      while(loss_ws_lpmf(data, t(U), t(V), t(FU), t(FV), t(WU-t*grad_WU), t(WV-t*grad_WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv) > tr.loss[length(tr.loss)] - alpha*t*(sum(grad_WU^2)+sum(grad_WV^2))){
        t <- beta*t
        #browser()
      }
      
      
      WU <- WU - t*grad_WU
      WV <- WV - t*grad_WV
      
    }# batch
    
    # record loss/mae/rmse/dlt
    loss <- loss_ws_lpmf(data, t(U), t(V), t(FU), t(FV), t(WU), t(WV), sig2/sig2u, sig2/sig2v, sig2/sig2wu, sig2/sig2wv)
    aux <- pred_ws_lpmf(test, t(FU), t(FV), t(WU), t(WV))
    tr.rmse <- c(tr.rmse, aux$rmse)
    tr.mae <- c(tr.mae, aux$mae)
    last_loss <- tr.loss[length(tr.loss)]
    tr.loss <- c(tr.loss, loss)
    dlt <- abs((loss - last_loss)/last_loss)
    tr.dlt <- c(tr.dlt, dlt)
    browser()
    # record the best mae and corresponding U, V
    #  if(min.mae > aux$mae){
    #    min.mae <- aux$mae
    #    min.U <- U
    #    min.V <- V
    #  }
    
    
    # print info
    if(verbose && epoch %% 2 == 1){
      print(Sys.time())
      cat(paste("Epoch", epoch, "\t loss:", loss, "\t dlt:", dlt))
      cat(paste("\nmae:", aux$mae, "\trmse:", aux$rmse, "\n"))
    }
    
    if(abs(loss - last_loss) < eps){
      break
    }
    
  }
  
  
  
  result <- list(U = U,
                 V = V,
                 parameters = c(sig2, sig2u, sig2v),
                 mae = tr.mae[length(tr.mae)],
                 rmse = tr.rmse[length(tr.rmse)],
                 tr.mae = tr.mae,
                 tr.rmse = tr.rmse,
                 tr.loss = tr.loss,
                 tr.delta = tr.dlt)
  #dir <- "~/nPMF/result/lpmf/"
  #save(result, file = paste0(dir ,"gd-",dim, ".Rda"))
  return(result)
}