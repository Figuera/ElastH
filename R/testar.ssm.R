testar.ssm <- function(kfs){
  testes     <- list()
  mod        <- kfs$mod
  hyper.par  <- attr(mod, "hm")
  n          <- attr(mod, "n")
  m          <- attr(mod, "m")
  v          <- kfs$v/t(sqrt(kfs$F))
  lags       <- max(2*frequency(mod$y), floor(sqrt(n-m)) + hyper.par-1)
  q          <- Box.test(v,   lags, type="Ljung-Box", hyper.par-1)
  testes$q   <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=lags)
  q          <- Box.test(v, 2*lags, type="Ljung-Box", hyper.par-1)
  testes$q2  <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=2*lags)
  testes$h   <- teste.h(v, m) 
  testes$nt  <- teste.normalidade(v)
  testes$aic <- 1/n*(-2*kfs$logLik + 2*(hyper.par + m))
  testes$bic <- 1/n*(-2*kfs$logLik + log(n) * (hyper.par + m))
  
  # Calculando testes t para coeficientes
  # Variância "de fato" dos resíduos das intervenções
  if("X" %in% rownames(mod$a1)) {
    testes$tt <- lapply(which(grepl("X\\d*", rownames(mod$a1))), function(posc) {
      var    <- kfs$V[posc, posc, n]
      tvalor <- kfs$alphahat[n, posc]/sqrt(var)
      pvalor <- 2*(1-pt(abs(tvalor), n - m))
      
      data.frame(valor=kfs$alphahat[n, posc],
                 desvio=sqrt(var), tvalor=tvalor, pvalor=pvalor)
    })
  }

  return(testes)
}
