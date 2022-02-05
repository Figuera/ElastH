summary_tests <- function(kfs){
  tests      <- list()
  mod        <- kfs$mod
  hyper.par  <- length(kfs$fit$par)
  d          <- ifelse(kfs$d == 1, frequency(y) + 2, kfs$d)
  n          <- attr(mod, "n")
  m          <- attr(mod, "m")
  v          <- rstandard(kfs)[(d+1):n]
  if(n - d < m) {
    warning("Not enough degrees of freedom")
  } else {
    lags       <- max(2*frequency(mod$y), floor(sqrt(n-d-m)) + hyper.par-1)
    q          <- Box.test(v,   lags, type="Ljung-Box", hyper.par-1)
    tests$q    <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=lags)
    q          <- Box.test(v, 2*lags, type="Ljung-Box", hyper.par-1)
    tests$q2   <- data.frame(Q.valor= q$statistic,
                           Q.critico=qchisq(0.95, q$parameter),
                           pvalor=q$p.value, lags=2*lags)
  }
  tests$h   <- h_test(v, m) 
  tests$nt  <- normality_test(v)
  tests$aic <- 1/n*(-2*kfs$logLik + 2*(hyper.par + m))
  tests$bic <- 1/n*(-2*kfs$logLik + log(n) * (hyper.par + m))
  tests$R2  <- R2.KFS(kfs)
  
  # T-test for regressions components
  if(length(mod$X) > 0) {
    tests$tt <- sapply(1:ncol(mod$X), function(posc) {
      var    <- kfs$V[posc, posc, n]
      tvalue <- kfs$alphahat[n, posc]/sqrt(var)
      pvalue <- 2*(1-pt(abs(tvalue), n - m))
      matrix(c(kfs$alphahat[n, posc],sqrt(var), tvalue, pvalue), 4, 1)
    })
    rownames(tests$tt) <- c("value", "sd", "T-test", "Pvalue")
    colnames(tests$tt) <- colnames(mod$X)
  }

  return(tests)
}
