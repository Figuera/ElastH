#' Teste H
#' 
#' Função para testar a homocedasticidade dos resíduos, teste H
#'
#' @param u Matriz de resíduos
#' @param d Número de Componentes
#' @return Valor do Test H
#'
#' @importFrom stats qf pf
#' @keywords internal
teste.h <- function(u, d){
  u <- u[!is.na(u)]
  n <- length(u)
  h <- round((n - d)/3)
  if (h < 1) return("Nao existem graus de liberdade o suficientes para o teste h")
  H <- sum(u[(n - h + 1):n]**2)/sum(u[(d+1):(d+h)]**2)
  if (H < 1) H <- 1/H

  data.frame(H.valor=H, H.critico=qf(0.95, h,h), pvalor=1-pf(H, h,h))
}
