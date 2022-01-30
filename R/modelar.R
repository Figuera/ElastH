#' Componentes e Testes
#'
#' Função que calcula a partir do objeto dlm e de algumas variáveis estruturais,
#' o valor dos componentes, reestrutura o resultados da função do pacote dlm para fins
#' de legibilidade, e, por fim, realiza testes diversos nos resíduos e valores obtidos.
#'
#' @param y Série temporal a ser decomposta
#' @param dlm Objeto 'dlm'
#' @param estrutura Lista com variáveis importantes para realização de testes:
#'  \itemize{
#'    \item \code{p}: Número de variâncias estocásticas
#'    \item \code{logLike}: Valor da função de log-verossimilhança }
#' @return Lista com 11 variáveis, entre componentes e testes:
#'   \itemize{
#'     \item \code{y}: Série que foi decomposta
#'     \item \code{dlm}: Estrutura do objeto dlm usado na decomposição
#'     \item \code{f}: Componentes resultantes do filtro
#'     \item \code{theta}: Componentes suavizados
#'     \item \code{c}: Subconjunto de theta, com sazonalidade agregada e nomes nas colunas (Resultado Principal)
#'     \item \code{interv}: tabela listando as intervenções, seu componente, período, valor e teste t.
#'     \item \code{u}: Matriz de resíduos
#'     \item \code{q}: Teste de independência dos resíduos
#'     \item \code{h}: Teste de homocedasticidade dos resíduos
#'     \item \code{nt}: Teste de normalidade dos resíduos
#'     \item \code{aic}: Valor AIC da função de verossimilhança usada para estimar modelo.}
#'
#' @importFrom stats Box.test end frequency lag pt qchisq qf sd start ts window
#' @importFrom utils tail
#' @keywords internal
modelar <-
function(fit) {
  # Calculando o valor dos componentes
  mod    <- fit$model
  modelo <- KFS(mod, smoothing=c("state", "signal", "disturbance"), simplify=F)

  freq <- frequency(mod$y)    # Frequência da série principal

  #Teste de intervenção
  intZ  <- grepl("choque",  colnames(mod$T))
  intTn <- grepl("Cnivel",  colnames(mod$T))
  intTi <- grepl("Cincli",  colnames(mod$T))
  int <- intZ | intTn | intTi

  if(sum(int) > 0){
    # Variância "de fato" dos resíduos das intervenções
    var   <- diag(as.matrix(modelo$V[int, int, attr(mod, "n")]))

    posxZ    <- apply(mod$Z[, intZ, , drop=F], 2, which.max) # Posição de intervenção
    posxTn   <- apply(mod$T["nivel", intTn, , drop=F], 2, which.max) # Posição de intervenção

    if(any(attr(mod, "state_types") == "incli")) {
      posxTi   <- apply(mod$T["incli", intTi, , drop=F], 2, which.max) # Posição de intervenção
    } else {
      posxTi <- NULL
    }

    posx     <- c(posxZ, posxTn, posxTi)
    testet   <- 2*(1 - pt(abs(diag(modelo$alphahat[posx, int, drop=F]))/sqrt(var), attr(mod, "n") - attr(mod, "m"))) # Teste t do valor da intervenção
    periodo  <- start(mod$y)[1] + (posx-2+start(mod$y)[2])/freq

    # Matrix de intervenções
    interv.df <- data.frame(periodo=periodo, valor=diag(modelo$alphahat[posx,int, drop=F]), 
                            desvio=sqrt(var), pvalor=testet,
                            row.names=paste0(colnames(mod$T[, int, 1, drop=F]), 1:sum(int)))

    modelo$interv <- interv.df
  }

  # Calculo dos resíduos
  #eps <- modelo$epshat/sqrt(t(modelo$model$H[,,1] - modelo$V_eps))
  #eta <- modelo$etahat/sqrt(t(apply(rep(modelo$model$Q[,,1],dim(modelo$V_eta)[3]) - modelo$V_eta, 3, diag)))
  #modelo$choques <- cbind(eps, eta)
  #states <- attr(mod, "state_types")[!(attr(mod, "state_types") %in% c("Cnivel", "Cincli", "custom"))]
  #colnames(modelo$choques) <- c("choque", paste0('C', states))

  # Testes diversos
  testes     <- list()
  npar       <- length(fit$optim.out$par)
  lags       <- max(2*freq, floor(sqrt(attr(mod, "n")-attr(mod, "m"))) + npar-1)
  q          <- Box.test(modelo$v,   lags, type="Ljung-Box", npar-1)
  testes$q   <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=lags)
  q          <- Box.test(modelo$v, 2*lags, type="Ljung-Box", npar-1)
  testes$q2  <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=2*lags)
  testes$h   <- teste.h(modelo$v, attr(mod, "m")) 
  testes$nt  <- teste.normalidade(modelo$v)
  testes$aic <- 1/attr(mod,"n")*(2*fit$optim.out$value
                                 + 2*(npar+attr(mod, "m")))
  testes$bic <- 1/attr(mod,"n")*(2*fit$optim.out$value
                                 + log(attr(mod,"n"))*(npar+attr(mod,"m")))
  
  # Calculando testes t para coeficientes
  # Variância "de fato" dos resíduos das intervenções
  if("X" %in% rownames(mod$a1)) {
    testes$tt <- lapply(which(rownames(mod$a1) == "X"), function(posc) {
      var    <- modelo$V[posc, posc, attr(mod, "n")]
      tvalor <- modelo$alphahat[attr(mod, "n"), posc]/sqrt(var)
      pvalor <- 2*(1-pt(abs(tvalor), attr(mod, "n") - attr(mod, "m")))
      
      data.frame(valor=modelo$alphahat[attr(mod, "n"), posc],
                 desvio=sqrt(var), tvalor=tvalor, pvalor=pvalor)
    })
  }

  ret <- list(modelo=modelo, testes=testes)
  class(ret) <- "mee"
  return(ret)
}
