#Building an OLS function
#building an OLS function to see how that would work

OLS <- function (Y, X) {
  
  X <- cbind(1,X)#adds a term for the intercept
  Xt <- t(X)
  comp1 <- (Xt)%*%X 
  comp2 <- solve(comp1)
  comp3 <- Xt %*% Y
  betas <- comp2 %*% comp3 #gets the betas
  yhat <- X %*% betas #gets the estimated ys
  resid <- Y - yhat #gets the residuals
  
  p <- ncol(X)-1 #number of indepdent variables
  n <- nrow(X) #number of observations
  
  
  sig2 <- (t(resid) %*% resid) / (n-p) #sigma squared, variance for errors
  
  s.errors <- sqrt((diag(comp2) * sig2)) #uses var covariance matrix to get standard errors
  
  newList <- list("betas" = betas, "sig2" = sig2, "s.error" = s.errors)
  
  return(newList)
}

X1 <- rnorm(100, 5, 2)
X2 <- rnorm(100, 2, 1)
X3 <- rnorm(100, .5, .2)

y_true <- 3 + .25 * X1 + 4 * X2 + .5 * X3

y_sample <- rnorm(100, mean(y_true), 2)

data1 <- cbind(X1, X2, X3)

#create the errors
error <- rnorm(nrow(data1), 0, 6)
#create the yis
y_sample <- 3 + .25 * X1 + 4 * X2 + .5 * X3 + error



#test with R canned OLS
reg1 <- lm(formula = y_sample ~ data1)
summary(reg1)

#test with new OLS function
reg2 <- OLS(y_sample, data1)
reg2


#Practicing with bootstraps

set.seed(02138)

#set number of iterations
nIter <- 1000

#create a place to put the betas
bootstrap_sampling <- matrix(ncol=4, nrow=nIter)

for (iter in 1:nIter){
  rows <- nrow(data1)
  #create a random index
  index <- sample(1:rows, rows, TRUE)
  
  #run the OLS with the index to pull random rows and store
  regb <- OLS(yi[index], data1[index,])
  #store the betas from each regression in the bootstrap
  bootstrap_sampling[iter,] <- regb$betas
}

#get the standard error for the bootstrap estimators 
#seems about the same, not much improvement???
boot_error <- apply(bootstrap_sampling, 2, sd)
boot_error

reg2$s.error - boot_error
OLS(yi,data1)
