#' Construir objeto DLM
#'
#' Função que cria a partir dos parâmetros desejados as matrizes no formato dlm.
#' Para mais informações ver pacote dlm: \url{https://cran.r-project.org/package=dlm}.
#'
#' @param y Série de tempo a ser decomposta
#' @param f Lista de fundamentos para a construção do objeto dlm:
#'   \itemize{
#'   \item \code{X}: Variáveis independentes
#'   \item \code{irregular}: Tipo de variância do resíduo principal 
#'   \item \code{nivel}: Tipo de variância do resíduo do nível
#'   \item \code{inclinacao}: Tipo de variância do resíduo do tendência
#'   \item \code{sazon}: Tipo de variância do resíduo da sazonalidade
#'   \item \code{regres}: Tipo de variância do resíduo dos coeficientes 
#'   \item \code{freq}: Frequência da Sazonalidade, em geral a mesma que a da série }
#' @param pre.interv Parametro indica se o modelo será posteriormente utilizado para
#' detecção de intervenções. Padrão \code{FALSE}.
#' @return Objeto dlm
#' @seealso
#' \code{\link{decompor}}
#' \code{\link{dlmMLE2}}
#' @keywords internal
build.dlm <-
function(y=rep(0,5), f=list(X=NULL, irregular="S", nivel = "S", inclinacao = "S", sazon = "S", regres = "S", freq=4), pre.interv=F){
  dlm <- NULL
  zero <- if(pre.interv) exp(-32) else 0
  # Adicionando Nível e, se necessário, Inclinacao
  if(f$nivel != "N") {
    if (f$inclinacao != "N") {
      dlm <- dlm::dlmModPoly(2, dW=c(if(f$nivel == "S") exp(y[2]) else if (f$nivel == "F") zero,
                                     if(f$inclinacao == "S") exp(y[3]) else if (f$inclinacao == "F") zero))
    } else {
      dlm <- dlm::dlmModPoly(1, dW=  if(f$nivel == "S") exp(y[2]) else if (f$nivel == "F") zero)
    }
  }

  # Adicionando Sazonalidade trimestral (se necessário)
  if(f$sazon != "N") {
    dlmsazon <- dlm::dlmModTrig(s=f$freq, dW=if (f$sazon == "S") exp(y[4]) else if (f$sazon == "F") 0)
    if(is.null(dlm)) {
      dlm <- dlmsazon
    } else {
      dlm <- dlm + dlmsazon
    }
  }

  # Adicionando variáveis independentes
  if(!is.null(f$X)) {
    for(i in 1:ncol(f$X)){
      if(colnames(f$X)[i] == "X" || colnames(f$X)[i] == "Choque" ) {
        dlmcoef <- dlm::dlmModReg(f$X[,i], FALSE, dW=if(colnames(f$X)[i] == "X" && f$regres == "S")  exp(y[5 + i]) else 0)

        if(is.null(dlm)) {
          dlm <- dlmcoef
        } else {
          dlm <- dlm + dlmcoef
        }
      } else {
        # Adicionando intervenções nos componentes
        dlmcoef <- dlm::dlm(FF=0,GG=1,W=0,V=0,m0=0,C0=12,X=f$X[,i], JGG=1)

        if(is.null(dlm)) {
          dlm <- dlmcoef
        } else {
          dlm <- dlm + dlmcoef
        }

        posc <- if(colnames(f$X)[i] == "C.Nivel") 1 else if (colnames(f$X)[i] == "C.Inclinacao") 2
        dlm$JGG[posc, ncol(dlm$GG)]           <- i
        dlm$JGG[nrow(dlm$JGG), ncol(dlm$JGG)] <- 0
      }        
    }

  }

  # Definindo variância do resíduo principal
  dlm::V(dlm) <- if(f$irregular=="S") exp(y[1]) else if (f$irregular=="F") 0
  # Definindo variância inicial dos resíduo
  dlm::C0(dlm) <- diag(exp(y[5]), ncol(dlm::C0(dlm)))

  return(dlm)
}
