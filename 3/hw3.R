# Classification of breast cancer cases
#
# Dataset: http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)
#
# 2) Diagnosis (M = malignant, B = benign)
# 3-32)
# 
# Ten real-valued features are computed for each cell nucleus:
#   
# a) radius (mean of distances from center to points on the perimeter)
# b) texture (standard deviation of gray-scale values)
# c) perimeter
# d) area
# e) smoothness (local variation in radius lengths)
# f) compactness (perimeter^2 / area - 1.0)
# g) concavity (severity of concave portions of the contour)
# h) concave points (number of concave portions of the contour)
# i) symmetry 
# j) fractal dimension ("coastline approximation" - 1)
# 
# Several of the papers listed above contain detailed descriptions of
# how these features are computed. 
# 
# The mean, standard error, and "worst" or largest (mean of the three
# largest values) of these features were computed for each image,
# resulting in 30 features.  For instance, field 3 is Mean Radius, field
# 13 is Radius SE, field 23 is Worst Radius.
# 
# All feature values are recoded with four significant digits.
# 
# 8. Missing attribute values: none
# 
# 9. Class distribution: 357 benign, 212 malignant
# If you haven't installed kernlab, uncomment and run:
#install.packages('kernlab')

library(kernlab)

set.seed(1991)

# Import the data, split into two matrices
#setwd('/home/hannes/Documents/UPC/KBML/3/') # Change to wd where you keep the data
complete_dataset <- read.csv("wdbc.data",stringsAsFactors=FALSE) #Try the saf=FALSE to swap M/B to 1/0
N <- nrow(complete_dataset)
diagnosis <- complete_dataset[,c(1,2)]
#Stripping data of the id.s and M/B so we can normalize it
normalizable_data <- complete_dataset[,c(-1,-2)]

# Switch B/M to 0/1, then make numerical since they're seen as strings
diagnosis[diagnosis=="B"] <- 0
diagnosis[diagnosis=="M"] <- 1
diagnosis[,2] <- strtoi(diagnosis[,2], base = 10)

# Normalize the data
# Normalizing the data for SVMs is especially important depending on what kind of a distance function
# one uses. If Euclidean distance is used, it is of utmost importance, since if one variable ranges between
# 1-2 and another between 10-20, then the latter variable has a 10 to 20 times larger effect on the outcome
# of the distance calculation compared to the first variable

normalized_data <- scale(normalizable_data, center=FALSE, scale=colSums(normalizable_data))
dataset <- cbind(diagnosis, normalized_data)


## Now we define a utility function for performing k-fold CV
## the learning data is split into k equal sized parts
## every time, one part goes for validation and k-1 go for building the model (training)
## the final error is the mean prediction error in the validation parts
## Note k=N corresponds to LOOCV

## a typical choice is k=10
k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

C <- 1
i=1

for (i in 1:k) 
{  
  train <- dataset[folds!=i,] # for building the model (training)
  valid <- dataset[folds==i,] # for prediction (validation)
  
  
  training.ratio <- 0.8
  ntrain <- round(N*training.ratio)
  training_part <- sample(N, ntrain)
  
  (train <- dataset[training_part,])
  (test <- dataset[-training_part,])
  
  x_train <- train[,c(3:32)]
  t_train <- train[,2]
  x_test <- test[,3:32]
  t_test <- test[,2]
  
  model <- ksvm(as.matrix(x_train), t_train, type="C-svc", C=1, kernel='rbfdot',scaled=c())
  
  model
  alpha(model)
  
  t_pred <- predict(model, x_test)
  
  table(t_test, t_pred)
  
  x_valid <- valid[,3:32]
  pred <- predict(model,x_valid)
  t_true <- valid[,2]
  
  # compute validation error for part 'i'
  valid.error[i] <- sum(pred != t_true)/length(t_true)
}
dim(valid.error)
cat("Average validation error:" , sum(valid.error)/length(valid.error))
