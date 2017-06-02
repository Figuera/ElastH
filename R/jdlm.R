#' Modelo em função do tempo
#'
#' Função transforma objetos dlm tradicionais em listas com modelos para 
#' cada momento no tempo.
#'
#' @param dlm Objeto 'dlm'. Ver função dlm
#' @param n opcional se dlm$X não for NULL. Número de períodos de série de
#'        tempo principal.
#' @return Lista com componentes do modelo dlm:
#'   \itemize{
#'     \item \code{FF} : Lista com n matrizes de transformação. Se dlm$JFF for NULL
#'       então todas as matrizes da lista são identicas. Se não são incorparados
#'       os valores corretos de dlm$X.
#'     \item \code{GG} : Lista com n matrizes de transição. Se dlm$JGG for NULL
#'       então todas as matrizes da lista são identicas. Se não são incorparados
#'       os valores corretos de dlm$X.
#'     \item \code{V} : Lista com n matrizes de variâncias dos choques na equação
#'       principal. Se dlm$JV for NULL então todas as matrizes da lista são
#'       identicas. Se não são incorparados os valores corretos de dlm$X.
#'     \item \code{W} : Lista com n matrizes de variâncias dos choques na equação
#'       de transição. Se dlm$JW for NULL então todas as matrizes da lista são
#'       identicas. Se não são incorparados os valores corretos de dlm$X.
#'  }
#'
#' @keywords internal
jdlm <- function(dlm, n=NULL){
  if(is.null(dlm$X) && is.null(n)){
    stop("Quando nao existe um componente que varie no tempo (X)
         e necessario informar o tamanho da serie principal (parametro n)")
  } else if (!is.null(dlm$X)){
    n <- nrow(dlm$X)
  }

  FF <- rep(list(dlm$FF), n)
  GG <- rep(list(dlm$GG), n)
  V  <- rep(list(dlm$V), n)
  W  <- rep(list(dlm$W), n)

  if(!is.null(dlm$JFF))
    for(t in 1:n) FF[[t]][dlm$JFF > 0] <- dlm$X[t, dlm$JFF[dlm$JFF > 0]]
  if(!is.null(dlm$JGG))
    for(t in 1:n) GG[[t]][dlm$JGG > 0] <- dlm$X[t, dlm$JGG[dlm$JGG > 0]]
  if(!is.null(dlm$JV))
    for(t in 1:n) V[[t]][dlm$JV > 0]   <- dlm$X[t, dlm$JV[dlm$JV > 0]]
  if(!is.null(dlm$JW))
    for(t in 1:n) W[[t]][dlm$JW > 0]   <- dlm$X[t, dlm$JW[dlm$JW > 0]]

  return(list(FF=FF,GG=GG,V=V,W=W))
}
