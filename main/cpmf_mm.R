library("Rcpp")
Ratings <- read.csv("~/nPMF/archive/Ratings.csv")
Ratings <- Ratings[Ratings$Book.Rating != 0, ]
N <- max(Ratings$User.ID)
book_idx <- unique(Ratings$ISBN)
M <- length(book_idx)
book_idx <- 1:M
names(book_idx) <- unique(Ratings$ISBN)
train <- read.csv("~/nPMF/archive/train.csv")
train$ISBN <- book_idx[train$ISBN]
test <- read.csv("~/nPMF/archive/test.csv")
test$ISBN <- book_idx[test$ISBN]
train$Book.Rating <- (train$Book.Rating - 1)/9
test$Book.Rating <- (test$Book.Rating - 1)/9
#train$Book.Rating <- train$Book.Rating/10
#test$Book.Rating <- test$Book.Rating/10

library("dplyr")
train.book.avg <- group_by(train, ISBN) %>% summarise(
  avg.rating = mean(Book.Rating)
)
test <- test[which(test$ISBN %in% train.book.avg$ISBN),]
load("~/nPMF/result/cpmf/read.Rda")
sourceCpp("~/nPMF/cpp/user_cpmf.cpp")
sourceCpp("~/nPMF/cpp/grad_cpmf.cpp")
sourceCpp("~/nPMF/cpp/loss_cpmf.cpp")
source("~/nPMF/cpp/loss/loss_cpmf.R")
sourceCpp("~/nPMF/cpp/pred_cpmf.cpp")
source("~/nPMF/R/cpmf_mm.R")

init <- c(1e-5, 1/2, 1, 1/2)
print(init)
dim <- commandArgs(trailingOnly = TRUE)
dim <- as.integer(dim)
print(dim)
set.seed(777)
result <- cpmf_mm(train, dim, init, verbose = TRUE, batch = 1e5, t = 0.3, max_iter = 50)