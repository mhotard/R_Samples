#Some maximum likelihood estimators

#Log normal distribution

log_normal <- function(params, Y3, X3)
    {
    sig2 <- params[1]
    sig2 <- exp(sig2)
    beta <- params[-1]
    n <- length(Y3)
    #return (c(sig2, beta, n))
    return((n/2) * log(1/(2*pi*sig2)) - (1/(2*sig2) * sum((Y3-X3%*%beta)^2)))
}

# Log normal that allows for heteroskedasticity 

log_normal_var <- function(params, Y3, X3, Z3)
{
  phi <- params[1:2]
  sig2 <- exp(Z3 %*% phi)
  beta <- params[-(1:2)]
  n <- length(Y3)
  #return (c(sig2, beta, n))
  return (-1/2 * sum (log(2*pi*sig2) + ((Y3-X3%*%beta)^2)/sig2) )
}

#Log logit function

log.logit <- function(par, outcomes, covariates){
  xb <- covariates%*%par
  return(-1 * sum(log(1 + exp((1 - 2 * outcomes)*(xb)))))
}

# Poisson Function

log_pois <- function(params, outcomes, XP){
  XBetas <- XP %*% params
  return(sum(outcomes * XBetas - exp(XBetas)))
}

# to use any of the above models

opt.out <- optim(par = parnew,
              fn = FUNCTION_NAME,
              covariates = COVS_TO_BE_ESTIMATED,
              outcomes = OUTCOMES,
              control = list(fnscale = -1),
              hessian = T,
              method = "BFGS")

fischer_info2 <- -opt2$hessian
vcov2 <- solve(fischer_info2)
se2 <- sqrt(diag(vcov2))
se2