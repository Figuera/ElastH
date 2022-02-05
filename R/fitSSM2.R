fitSSM2 <- function(ssm, init, ...) {
  if(any(is.na(ssm$a1))) init <- c(rep(c(a1=0), nrow(ssm$a1)), init)
  if(any(is.na(ssm$P1))) {
    init <- c(rep(c(P1=10), ncol(ssm$P1)), init)
  }

  fit <- KFAS::fitSSM(ssm, inits=init, updatefn=update_fit, gr=NULL, ...,  method="L-BFGS-B", upper=32, lower=-17)

  fit$model$Q <- replace(fit$model$Q, log(fit$model$Q) == 17, 0) # If the value is too close to the limit set it to 0
  fit$model$H <- replace(fit$model$H, log(fit$model$H) == 17, 0)
  return(fit)
}
