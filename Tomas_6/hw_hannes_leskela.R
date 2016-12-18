library(pls)

set.seed(1991)

train <- read.csv("data_set_ALL_AML_train.csv", header = TRUE, sep = ";")
indep <- read.csv("data_set_ALL_AML_independent.csv", header = TRUE, sep = ";")

# 1 represents ALL, 0 AML
# 27 is the index of the last ALL in the training data, 20 in test data. Rest is AML
# The order in the response document is mixed, while they are ordered by ALL first, AML later
# in the .csv files

train.response <- append(rep(1,27), rep(0,11) , after =  27)
indep.response <- append(rep(1,20), rep(0,14) , after = 20)

# Form the matrices to train and test on, by transposing the original data
#
X <- t(train)
Xt <- t(indep)

# Remove non-numerical rows and make data into data.frames for easier 
# management and function calling.
remove.from.train <- seq(4,78,2)
X <- X[-remove.from.train,]
X <- X[-c(1,2),]
X <- data.frame(X)

remove.from.indep <- seq(4,70,2)
Xt <- Xt[-remove.from.indep,]
Xt <- Xt[-c(1,2),]
Xt <- data.frame(Xt)

# Convert to numerical
X <- data.frame(sapply(X, function(x) as.numeric(as.character(x))))
Xt <- data.frame(sapply(Xt, function(x) as.numeric(as.character(x))))

# scale X and Xt
X_means = colMeans(X)
X = scale(X, scale = FALSE)
Xt = scale(Xt, center=X_means, scale = FALSE)

X <- data.frame(X)
Xt <- data.frame(Xt)

p1 <- plsr(train.response ~ ., ncomp = 10, data = X[1:38,], validation = "LOO")

attributes(p1)
summary(p1)
