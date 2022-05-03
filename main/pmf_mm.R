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

sourceCpp("~/nPMF/cpp/grad_pmf.cpp")
sourceCpp("~/nPMF/cpp/loss_pmf.cpp")
source("~/nPMF/cpp/loss/loss_pmf.R")
sourceCpp("~/nPMF/cpp/par_pmf.cpp")
sourceCpp("~/nPMF/cpp/pred_pmf.cpp")
source("~/nPMF/R/pmf_mm.R")

library("dplyr")
train.book.avg <- group_by(train, ISBN) %>% summarise(
  avg.rating = mean(Book.Rating)
)
test <- test[which(test$ISBN %in% train.book.avg$ISBN),]

init <- c(3e-6, 1, 1)
print(init)
dim <- commandArgs(trailingOnly = TRUE)
dim <- as.integer(dim)
print(dim)
set.seed(777)
result <- pmf_mm(train, dim, init, verbose = TRUE, batch = 3e4, t = 1e-3, max_iter = 50)