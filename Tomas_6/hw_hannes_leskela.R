library(pls)
library(FactoMineR) # Used for step 4, projection
library(calibrate) # Used for step 5, method circle()

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

# A final conversion to run the pls
X <- data.frame(X)
Xt <- data.frame(Xt)

# Run PLS1 regression on our data, looking at up to 10 components if using ncomp =10. 
# Can be changed to nothing to run as many as possible (36)
p1 <- plsr(train.response ~ ., data = X[1:38,], validation = "LOO")

# Details about our model. Especially interesting is summary(p1) that gives us
# information about the number of components that describe the training data
attributes(p1)
summary(p1)

# Plot the components importance
plot(RMSEP(p1), legendpos = "topright")

R2(p1)
plot(R2(p1), legendpos = "bottomright")

# Selecting 4 components for 98.49 of variance explained, based on summary above
nd <- 4

# Incorrect plots?
# plot(p1, ncomp = 4, asp = 1)
# abline(a =0.5, b = 0, lty = 2)

# Projecting the test data onto selected components. Test data is centered around 
# training mean earlier in the code.


# Point 5, plots. These don't make any sense, and should probably be points instead of 
# all the params...
corXp1 <- cor(X,p1$scores)
coryp1 <- cor(train.response,p1$scores)
rownames(coryp1) = "Octane"
corXyp1 <- rbind(corXp1,coryp1)
plot(corXyp1,ylim=c(-1,1),xlim=c(-1,1),asp=1,type="n",main="Correlations of variables with components")
#text(corXyp1,labels=rownames(corXyp1),col=c(rep(1,p),rep(2,1)),adj=1.1,cex=0.85)
p <- ncol(X)
arrows(rep(0,(p+1)),rep(0,(p+1)),corXyp1[,1],corXyp1[,2],col=c(rep(1,p),rep(2,1)),length=0.07)
abline(h=0,v=0, col="gray")
circle()


# the PLS1 model for step 6, not yet ready.
lmY <- lm(y~p1$scores[,1:nd])
summary(lmY)

# or step 6 using predict()
indep.predictions <- round(predict(p1, ncomp = 10, newdata = Xt))
difference.vector <- indep.response == indep.predictions
length(difference.vector[difference.vector == TRUE])
# 33 correct out of 34! Good predictor using 4 components
# On the other hand, 31 correct using one component, i.e 91% correct, when only 78% of
# training variance is explained. Fishy...
