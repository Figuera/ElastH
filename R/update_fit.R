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
  is_gaussian <- all(model$distribution == "gaussian")
  estH        <- is_gaussian && any(is.na(model$H))
  estQ        <- any(is.na(model$Q))
  hasSeas     <- any(attr(model, "state_types") == "seasonal")
  if (estQ) {
    if(hasSeas) {
      primeiro.seas <- which(model$R[, , 1]["sea_trig1", ] != 0)
      ultimo.seas   <- primeiro.seas + sum(grepl("sea_", row.names(model$R[, , 1]))) - 1
      seas.estatico <- (primeiro.seas + 1):ultimo.seas
      Q <- as.matrix(model$Q[-seas.estatico, -seas.estatico, 1])
    } else {
      Q <- as.matrix(model$Q[, , 1])
    }

    naQd <- which(is.na(diag(Q)))
    Q    <- Q[naQd, naQd, drop=F]
    if(FALSE) { # Linhas a seguir estimariam incluse as covariâncias e não são utilizadas
      naQnd           <- which(upper.tri(Q) & is.na(Q))
      Q[lower.tri(Q)] <- 0
      diag(Q)         <- exp(0.5 * pars[1:length(naQd)])
      Q[naQnd]        <- pars[length(naQd) + 1:length(naQnd)]
      model$Q[naQd, naQd, 1] <- crossprod(Q)
    } else {
      diag(Q) <- exp(pars[1:length(naQd)])
      model$Q[naQd, naQd, 1] <- Q
    }

    if(hasSeas)
      diag(model$Q[seas.estatico, seas.estatico, 1]) <- model$Q[primeiro.seas, primeiro.seas, 1]
  }
  else naQnd <- naQd <- NULL
  if (estH) {
    H    <- as.matrix(model$H[, , 1])
    naHd <- which(is.na(diag(H)))
    if(FALSE) { # Equivalente ao comentário acima
      naHnd <- which(upper.tri(H[naHd, naHd]) & is.na(H[naHd, 
                                                    naHd]))
      H[naHd, naHd][lower.tri(H[naHd, naHd])] <- 0
      diag(H)[naHd] <- exp(0.5 * pars[length(naQd) + 
                                        length(naQnd) + 1:length(naHd)])
      H[naHd, naHd][naHnd] <- pars[length(naQd) + length(naQnd) + 
                                     length(naHd) + 1:length(naHnd)]
      model$H[naHd, naHd, 1] <- crossprod(H[naHd, naHd])
    } else {
      model$H[naHd, naHd, 1] <- exp(pars[length(naQd) + 1:length(naHd)])
    }
  }
  # Caso queira deixar comparável com pacote dlm
  # necessário adicional inits = c(init, 16) na função build.ssm
  if(any(is.na(model$P1)))
    diag(model$P1) <- exp(pars[length(naQd) + length(naHd) + 1])
  model
}
