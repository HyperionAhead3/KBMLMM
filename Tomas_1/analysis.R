library("pls")

setwd("/home/hannes/Documents/UPC/KBML/Tomas_1/")

# Read data
raw.train.data <- read.table("zip_train.dat")
raw.test.data <- read.table("zip_test.dat")

# Basic info for training sample
N <- dim(raw.train.data)[1]
sample.percentage <- 0.05
sample.size <- ceiling(N*sample.percentage)

# Select a sample of rows to get, the retrieve these 
sample.train.data <- raw.train.data[sample(nrow(raw.train.data), size = sample.size, replace = FALSE),]

# So the data is in a big list, with all the correct numbers in [1]
# and the greyscale values in the other 256
response.matrix <- sample.train.data[1]

#ugly way of turning the response vector into a 10xsample.size matrix
numbers <- 10
mat <- matrix(, nrow = sample.size, ncol = numbers)
row <- 1
for (i in as.matrix(response.matrix)) {
  vector <- rep(0,numbers)
  vector[i+1] <- 1
  mat[row,] <- vector
  row <- row+1
}
predictor.matrix <- sample.train.data[-1]

# Center the predictor matrix, without standardizing
predictor.matrix <- scale(predictor.matrix, scale = FALSE)

# MV regression
mreg <- lm(as.matrix(response.matrix) ~ as.matrix(predictor.matrix)) 
