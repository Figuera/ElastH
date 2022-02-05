test_intervs <- function(kfs) {
  mod <- kfs$model
  intZ  <- grepl("I.principal\\d*",  colnames(mod$T))
  intTn <- grepl("I.level\\d*",  colnames(mod$T))
  intTi <- grepl("I.slope\\d*",  colnames(mod$T))
  int <- intZ | intTn | intTi

  if(sum(int) > 0){
    # De facto variance of the residues
    var   <- diag(as.matrix(kfs$V[int, int, attr(mod, "n")]))

    # Posicoes das intervencoes
    posxZ    <- apply(mod$Z[, intZ, , drop=F], 2, which.max) 
    if(any(attr(mod, "state_types") == "level")) {
      posxTn   <- apply(mod$T["level", intTn, , drop=F], 2, which.max)
    } else {
      posxTn <- NULL
    }
    if(any(attr(mod, "state_types") == "slope")) {
      posxTi   <- apply(mod$T["slope", intTi, , drop=F], 2, which.max)
    } else {
      posxTi <- NULL
    }

    posx     <- c(posxZ, posxTn, posxTi)
    periodo  <- start(mod$y)[1] + (posx-2+start(mod$y)[2])/frequency(mod$y)
    # Fix time period for interventions on level and slope
    periodo[-1:-length(posxZ)] <- periodo[-1:-length(posxZ)] + 1/frequency(mod$y)
    # t-value of the intervention
    testet   <- pt(abs(diag(kfs$alphahat[posx, int, drop=F]))/sqrt(var),
                   df=attr(mod, "n") - attr(mod, "m"))
    testet   <- 2*(1 - testet)
    ### GAMBIARRA ALERT
    testet[is.nan(testet)] <- 0

    # Final result
    data.frame(time=periodo, coefs=diag(kfs$alphahat[posx,int, drop=F]), 
               sd=sqrt(var), pvalue=testet,
               row.names=colnames(mod$T[, int, 1, drop=F]))
  }
}
