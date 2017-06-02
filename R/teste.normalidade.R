#' Teste de normalidade
#' 
#' Função para testar a normalidade dos resíduos
#' 
#' @param u Matriz de resíduos
#' @return Valor do teste de normalidade
#' @importFrom stats pchisq
#' @keywords internal
teste.normalidade <- function(u) {
  u <- u[!is.na(u)]
  n <- length(u)
  assimetria <- (sum((u - mean(u))^3)/n)/(sum((u - mean(u))^2)/n)^(3/2)
  curtose <- n * sum((u - mean(u))^4)/(sum((u - mean(u))^2)^2)
  nt <- n*(assimetria^2/6 + (curtose - 3)^2/24)

  data.frame(Normality.test=nt, Normality.critico=qchisq(0.95,2), pvalor=1-pchisq(nt,2))
}

