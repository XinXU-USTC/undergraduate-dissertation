ndcg <- function(test, result, model, K = 5){
  if(model == "avg"){
    aux <- result
    aux$prediction <- round(aux$prediction*9, 0) + 1
  }
  
  if(model == "pmf"){
    aux <- pred_pmf(test, t(result$U), t(result$V))
  }
  
  if(model == "lpmf"){
    aux <- pred_lpmf(test, t(result$U), t(result$V))
    aux$prediction <- round(aux$prediction*9, 0) + 1
  }
  
  if(model == "ws_lpmf"){
    load("FU.Rda")
    load("FV.Rda")
    aux <- pred_ws_lpmf(test, t(FU), t(FV), t(result$WU), t(result$WV))
    aux$prediction <- round(aux$prediction*9, 0) + 1
  }
  test$pred <- aux$prediction
  by_users <- group_by(test, User.ID) %>% summarise(
    num = length(Book.Rating)
  )
  user_idx <- by_users$User.ID[by_users$num >= K]
  raw_rate <- sapply(user_idx, function(x){
    df <- test[test$User.ID == x, ]
    idx <- order(df$pred, decreasing = TRUE)[1:K]
    df <- df[idx, ]
    return(df$Book.Rating)
  })
  sort_rate <- sapply(user_idx, function(x){
    df <- test[test$User.ID == x, ]
    idx <- order(df$pred, decreasing = TRUE)[1:K]
    df <- df[idx, ]
    return(sort(df$Book.Rating, decreasing = TRUE))
  })
  dscnt <- 1/log2(1+(1:K))
  
  dcg <- apply(raw_rate, 2, function(x){
    sum(dscnt*(2^x - 1))
  })
  dcg0 <- apply(sort_rate, 2, function(x){
    sum(dscnt*(2^x - 1))
  })
  #browser()
  ndcg <- mean(dcg/dcg0)
  return(ndcg)
}