Ratings <- read.csv("archive/Ratings.csv")
N <- max(Ratings$User.ID)
book_idx <- unique(Ratings$ISBN)
M <- length(book_idx)
book_idx <- 1:M
names(book_idx) <- unique(Ratings$ISBN)
train <- read.csv("archive/train.csv")
train$ISBN <- book_idx[train$ISBN]
test <- read.csv("archive/test.csv")
test$ISBN <- book_idx[test$ISBN]
train$Book.Rating <- train$Book.Rating/10
test$Book.Rating <- test$Book.Rating/10

sourceCpp("cpp/grad_cpmf.cpp")
sourceCpp("cpp/loss_cpmf.cpp")
sourceCpp("cpp/par_cpmf.cpp")
sourceCpp("cpp/pred_cpmf.cpp")
source("R/cpmf_mm.R")

init <- c(1e-3, 3, 3)
print(init)
dim <- commandArgs(trailingOnly = TRUE)
print(dim)
set.seed(123)
result <- lpmf_mm(train, dim, init, verbose = TRUE, batch = 1e4)