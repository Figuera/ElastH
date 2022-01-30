#' Build and Fit SSModel
#'
#' Function design an SSM model from parameters provided and find variances that maximize likelihood
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
#' \code{\link{fitSSM}}
#' @keywords internal
build_ssm <- function(init, y, X=NULL, variances=list(irregular=NA, level = NA, slope = NA, seas = NA, regres = NA), freq = 4, ...){
  model <- y ~ 0

  # Adicionando Nível e, se necessário, slope
  n       <-  length(c(variances$level, variances$slope))
  if (n > 0) {
    model <- update(model, .~ . + SSMtrend(degree = n, Q = list(variances$level, variances$slope)))
  }

  # Adicionando seasonalityalidade trimestral (se necessário)
  if(!is.null(variances$seas)) {
    model <- update(model, . ~ . + SSMseasonal(period=freq, sea.type='trigonometric', Q= variances$seas))
  }

  # Adicionando variáveis independentes
  if(!is.null(X)) {
    rmodel <- as.formula(paste0("~ -1 + ", paste(colnames(X), sep="+")))
    model <- update(model, . ~ . + SSMregression(rmodel, data = X, Q = variances$regres))
  }

  ssm <- KFAS::SSModel(model, H=variances$irregular)
  fit <- KFAS::fitSSM(ssm, inits=init, updatefn=update_fit, gr=NULL, ...,  method="L-BFGS-B", upper=32, lower=-17)

  fit$model$Q <- replace(fit$model$Q, log(fit$model$Q)==-17, 0)
  fit$model$H <- replace(fit$model$H, log(fit$model$H)==-17, 0)

  return(fit)
}

