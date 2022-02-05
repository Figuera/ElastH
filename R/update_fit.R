#' Update SSModel fit
#'
#' Internal function. Used in optimization process, updates models between each evaluation.
#'
#' @param pars Current parameters
#' @param model Model structure
#' @return Next model state
#' @seealso
#' \code{\link{build_ssm}}
#' \code{\link{fitSSM}}
#' @keywords internal
update_fit <- function(pars, model) {
  estH        <- any(is.na(model$H))
  estQ        <- any(is.na(model$Q))
  esta1       <- any(is.na(model$a1))
  estP1       <- any(is.na(model$P1))
  
  # Set State's new variances
  if (estQ) {
    Q           <- as.matrix(model$Q[, , 1])
    naQd        <- which(is.na(diag(Q)))
    Qpars_names <- sub("sea_.*", "seas", row.names(model$R)[naQd])
    Q           <- Q[naQd, naQd, drop=F]

    if (any(!(Qpars_names %in% names(pars)))) stop("Some parameters are missing in the initial vector")
    diag(Q)   <- exp(pars[Qpars_names])
    model$Q[naQd, naQd, 1] <- Q
  }
  
  # Set irregular new variance
  if (estH) {
    if(sum(ncol(model$H)) > 1) stop("I don't know how to deal with Higher Dimensions Models. Modify the update function.")
    model$H[, , 1] <- exp(pars["irregular"])
  }

  # Initial State best guesses
  if(estP1){
    nP1 <- ncol(model$P1)
    diag(model$P1) <- exp(pars[1:nP1])
  } else nP1 <- 0
  if(esta1){
    model$a1[,1] <- pars[nP1 + 1:length(model$a1)]
  }

  return(model)
}
