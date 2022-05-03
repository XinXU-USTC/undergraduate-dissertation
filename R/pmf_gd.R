pmf_gd <- function(data,
                    dim,
                    init,
                    U,
                    V,
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
  

# define variables to record the process
  aux <- pred_pmf(test, t(U), t(V))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  tr.loss <- loss_pmf(data, t(U), t(V), sig2/sig2u, sig2/sig2v)
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
      grad <- grad_pmf(df, t(U), t(V), sig2/sig2u, sig2/sig2v, batch, nrow(data))
      grad_U <- t(grad$Ut)
      grad_V <- t(grad$Vt)
      
      # calculate step size
      t <- 1
    while(loss_pmf(data, t(U-t*grad_U), t(V-t*grad_V), sig2/sig2u, sig2/sig2v) > tr.loss[length(tr.loss)]  - alpha*t*(sum((grad_U)^2)+sum(grad_V^2))){
      t <- beta*t
      #browser()
    }
      
      # update U, V
      U <- U - t*grad_U
      V <- V - t*grad_V
      
    }# batch
    
    # record loss/mae/rmse/dlt
    loss <- loss_pmf(data, t(U), t(V), sig2/sig2u, sig2/sig2v)
    aux <- pred_pmf(test, t(U), t(V))
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

    if(dlt < eps){
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
  dir <- "~/nPMF/result/pmf/"
  save(result, file = paste0(dir ,"gd-",dim, ".Rda"))
  return(result)
}