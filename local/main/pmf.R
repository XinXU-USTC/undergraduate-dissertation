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

sourceCpp("cpp/grad_pmf.cpp")
sourceCpp("cpp/loss_pmf.cpp")
sourceCpp("cpp/par_pmf.cpp")
sourceCpp("cpp/pred_pmf.cpp")
source("R/pmf_mm.R")

init <- c(1e-3, 3, 3)
print(init)
dim <- commandArgs(trailingOnly = TRUE)
print(dim)
set.seed(123)
result <- pmf_mm(train, dim, init, verbose = TRUE, batch = 1e4)