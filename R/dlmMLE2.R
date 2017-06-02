#' Otimizacao especializada
#'
#' Esta função prepara os dados para chamar a função optim
#' 
#' @param y Série temporal a ser decomposta
#' @param ipar Parâmetros iniciais da função de otimização
#' @param fun Função de geração do objeto dlm, geralmente build.dlm
#' @param ... Parâmetros da função de geração do objeto dlm
#' @return Parâmetros ótimos para a função de geração do objeto dlm
#'
#' @importFrom stats optim
#' @keywords internal
dlmMLE2 <-
function(y, ipar, fun, ...) {
  logLik <- function(parm, ...) {
    mod <- fun(parm, ...)
    return(dlm::dlmLL(y=y, mod=mod))
  }

  fit <- optim(par=ipar, fn=logLik, gr=NULL, ...,  method="L-BFGS-B", upper=32, lower=-17)

  fit$par <- replace(fit$par, fit$par==-17, -Inf)
  return(fit)
}
