pmf_mm <- function(train,
                   test,
                    dim,
                    init,
                    eps = 1e-8,
                    max_iter = 5e3,
                    mu = 0.9,
                    t = 1e-4,
                    batch = 5e4,
                    verbose = FALSE,
                    M = 340556,
                    N = 278854){
  
  # initialize parameters
  sig2 <- init[1]
  sig2u <- init[2]
  sig2v <- init[3]
  
  # initialize U, V
  #U <- matrix(rnorm(N*dim, sd = sqrt(sig2u)), nrow = dim)
  #V <- matrix(rnorm(M*dim, sd = sqrt(sig2v)), nrow = dim)
  U <- matrix(0, nrow = dim, ncol = N)
  V <- matrix(0, nrow = dim, ncol = M)
  
  # define variables to record the process
  aux <- pred_pmf(test, t(U), t(V))
  tr.rmse <- aux$rmse
  tr.mae <- aux$mae
  tr.loss <- loss_pmf(train, t(U), t(V), sig2/sig2u, sig2/sig2v)
  #browser()
  #tr.sig2 <- sig2
  tr.sig2u <- sig2u
  tr.sig2v <- sig2v
  tr.dlt <- NULL #relative change of the loss
  #upd_num <- 0
  
  V_U <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = N)
  V_V <- matrix(rnorm(dim, sd = 0.01), nrow = dim, ncol = M)
  
  for(epoch in 1:max_iter){
    
    # calculate the number of batches
    num_bat <- ceiling(nrow(train)/batch)
    bat_idx <- rep((1:(num_bat-1)), batch)
    red <- nrow(train) %% batch
    bat_idx <- c(bat_idx, rep(num_bat, red))
    bat_idx <- sample(bat_idx)
    
    # for each batch
    for(iter in 1:num_bat){
      idx <- which(bat_idx == iter)
      df <- train[idx, ]
      
      # calculate gradient
      grad <- grad_pmf(df, t(U), t(V), sig2/sig2u, sig2/sig2v, batch, nrow(train))
      grad_U <- t(grad$Ut)
      grad_V <- t(grad$Vt)
      
      # calculate momentum
      V_U <- mu*V_U + t*grad_U
      V_V <- mu*V_V + t*grad_V
      
      # update U, V
      U <- U - V_U
      V <- V - V_V
      #upd_num <- upd_num + 1
      
      
    }# batch
    
    # record loss/mae/rmse/dlt
    loss <- loss_pmf(train, t(U), t(V), sig2/sig2u, sig2/sig2v)
    aux <- pred_pmf(test, t(U), t(V))
    tr.rmse <- c(tr.rmse, aux$rmse)
    tr.mae <- c(tr.mae, aux$mae)
    last_loss <- tr.loss[length(tr.loss)]
    tr.loss <- c(tr.loss, loss)
    dlt <- abs((loss - last_loss)/last_loss)
    tr.dlt <- c(tr.dlt, dlt)
    #browser()
    if(dlt < eps){
      break
    }
    # print info
    if(verbose && epoch %% 10 == 1){
      print(Sys.time())
      cat(paste("Epoch", epoch, "\t loss:", loss, "\t dlt:", dlt))
      cat(paste("\nmae:", aux$mae, "\trmse:", aux$rmse, "\tpar:", sig2, sig2u, sig2v, "\n"))
      #cat(paste("\t#updates:", upd_num, "\n"))
    }
    
    
    
    #browser()
     #check if convergent
  
    
  }# epoch
  
  result <- list(U = U,
                 V = V,
                 parameters = c(sig2, sig2u, sig2v),
                 mae = tr.mae[length(tr.mae)],
                 rmse = tr.rmse[length(tr.rmse)],
                 tr.mae = tr.mae,
                 tr.rmse = tr.rmse,
                 tr.loss = tr.loss,
                 tr.delta = tr.dlt)
  #save(result, file = paste0(dir ,"result/",dim, ".Rda"))
  return(result)
}