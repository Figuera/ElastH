#' Build SSModel
#'
#' Wrap Function, design an SSM model from parameters provided and find variances that maximize likelihood
#'
#' @param y Dependent variable
#' @param X Independent variables 
#' @param variances List of variances. NA valued variances will defined automatically. NULL values are ignored.
#'   \itemize{
#'   \item \code{irregular}: Variance of the main residue
#'   \item \code{level}: Variance of the level component
#'   \item \code{slope}: Variance of the slope component
#'   \item \code{seas}: Variance of seasonality component
#'   \item \code{regres}: Variance of the regression components (can be a list)
#' @return SSM Object
#' @seealso
#' \code{\link{decompor}}
#' \code{\link{SSModel}}
#' @keywords internal
build_ssm <- function(y, X=NULL, variances=list(irregular=NA, level = NA, slope = NA, seas = NA, regres = NA), a1 = list(a1 = 0, P1 = 0), ...){
  model <- y ~ 0

  # Adicionando Nível e, se necessário, slope
  n       <-  length(c(variances$level, variances$slope, variances$trend3))
  if (n > 0) {
    model <- update(model, .~ . + SSMtrend(degree = n, Q = list(variances$level, variances$slope, variances$trend3)))
  }

  # Adicionando seasonalityalidade trimestral (se necessário)
  if(!is.null(variances$seas)) {
    model <- update(model, . ~ . + SSMseasonal(period=frequency(y), sea.type='trigonometric', Q= variances$seas))
  }

  # Adicionando variáveis independentes
  if(!is.null(X)) {
    rmodel <- as.formula(paste0("~ -1 + ", paste(colnames(X), collapse=" + ")))
    Qx <- unlist(variances[colnames(X)])
    if(ncol(X) > 1 && length(Qx) == 1) {
      Qx <- diag(rep(Qx, ncol(X)))
      diag(Qx)[grepl("^I\\..*", colnames(X))] <- exp(-8)
    } else if (length(Qx) > 1){
      Qx <- diag(Qx)
    }
    model <- update(model, . ~ . + SSMregression(rmodel, data = X, Q = Qx))
  }

  ssm <- KFAS::SSModel(model, H=variances$irregular)

  # Set initial state configuration
  ssm$a1[,1]   <- a1$a1
  diag(ssm$P1) <- a1$P1
  if(is.na(a1$P1) || a1$P1 != 0) diag(ssm$P1inf) <- 0

  return(ssm)
}

