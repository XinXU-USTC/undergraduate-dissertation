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
#train$Book.Rating <- (2*train$Book.Rating - 1)/20
#test$Book.Rating <- (2*test$Book.Rating - 1)/20
train$Book.Rating <- (train$Book.Rating-1)/9
test$Book.Rating <- (test$Book.Rating-1)/9

library("dplyr")
train.book.avg <- group_by(train, ISBN) %>% summarise(
  avg.rating = mean(Book.Rating)
)
test <- test[which(test$ISBN %in% train.book.avg$ISBN),]

load("~/nPMF/FU.Rda")
load("~/nPMF/FV.Rda")
sourceCpp("~/nPMF/cpp/grad_ws_lpmf.cpp")
sourceCpp("~/nPMF/cpp/loss_ws_lpmf.cpp")
source("~/nPMF/cpp/loss/loss_ws_lpmf.R")
sourceCpp("~/nPMF/cpp/pred_ws_lpmf.cpp")
source("~/nPMF/R/ws_lpmf_mm.R")

init <- c(1e-8, 1, 1, 1e3, 1e3)
print(init)
dim <- commandArgs(trailingOnly = TRUE)
dim <- as.integer(dim)
print(dim)
set.seed(777)
result <- ws_lpmf_mm(train, dim, init, FU, FV, verbose = TRUE, batch = 18e4, t = 0.9, max_iter = 2e2, eps = 1e-10)