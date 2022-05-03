cpmf_gd <- function(data,
                    dim,
                    init,
                    Y,
                    V,
                    W,
                    eps = 1e-6,
                    max_iter = 5e1,
                    alpha = 0.4,
                    beta = 0.7,
                    verbose = FALSE,
                    batch = 5e3){
  
  
  # initialize parameters
  sig2 <- init[1]
  sig2u <- init[2]
  sig2v <- init[3]
  sig2w <- init[4]
  

# define variables to record the process
  U <- t(user_cpmf(read, t(Y), t(W)))
  aux <- pred_cpmf(test, t(U), t(V))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  tr.loss <- loss_cpmf(data, t(Y), t(V), t(W), t(U), sig2/sig2u, sig2/sig2v, sig2/sig2w)
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
      U <- t(user_cpmf(read, t(Y), t(W)))
      grad <- grad_cpmf(df, t(Y), t(V), t(W), t(U), read, sig2/sig2u, sig2/sig2v, sig2/sig2w, batch, nrow(data))
      grad_Y <- t(grad$Yt)
      grad_V <- t(grad$Vt)
      grad_W <- t(grad$Wt)
      # calculate step size
      t <- 1
    while(loss_cpmf(data, t(Y-t*grad_Y), t(V - t*grad_V), t(W - t*grad_W), user_cpmf(read, t(Y-t*grad_Y), t(W - t*grad_W)), sig2/sig2u, sig2/sig2v, sig2/sig2w) > tr.loss[length(tr.loss)]  - alpha*t*(sum((grad_Y)^2)+sum(grad_V^2)+sum((grad_W)^2))){
      t <- beta*t
      #browser()
    }
      
      # update U, V
      Y <- Y - t*grad_Y
      V <- V - t*grad_V
      W <- W - t*grad_W
      print(t)
      
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

    #if(dlt < eps){
    #    break
    #  }


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
  dir <- "~/nPMF/result/cpmf/"
  save(result, file = paste0(dir ,"gd-",dim, ".Rda"))
  return(result)
}