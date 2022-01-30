testar.intervs <- function(kfs) {
  mod <- kfs$model
  intZ  <- grepl("I.principal\\d*",  colnames(mod$T))
  intTn <- grepl("I.nivel\\d*",  colnames(mod$T))
  intTi <- grepl("I.incli\\d*",  colnames(mod$T))
  int <- intZ | intTn | intTi

  if(sum(int) > 0){
    # Variância "de fato" dos resíduos das intervenções
    var   <- diag(as.matrix(kfs$V[int, int, attr(mod, "n")]))

    # Posicoes das intervencoes
    posxZ    <- apply(mod$Z[, intZ, , drop=F], 2, which.max) 
    if(any(attr(mod, "state_types") == "nivel")) {
      posxTn   <- apply(mod$T["nivel", intTn, , drop=F], 2, which.max)
    } else {
      posxTn <- NULL
    }
    if(any(attr(mod, "state_types") == "incli")) {
      posxTi   <- apply(mod$T["incli", intTi, , drop=F], 2, which.max)
    } else {
      posxTi <- NULL
    }

    posx     <- c(posxZ, posxTn, posxTi)
    periodo  <- start(mod$y)[1] + (posx-2++start(mod$y)[2])/frequency(mod$y)
    # Intervenções na equação de transição estão um período defasadas
    periodo[-1:-length(posxZ)] <- periodo[-1:-length(posxZ)] + 1/frequency(mod$y)
    # Teste t do valor da intervenção
    testet   <- pt(abs(diag(kfs$alphahat[posx, int, drop=F]))/sqrt(var),
                   df=attr(mod, "n") - attr(mod, "m"))
    testet   <- 2*(1 - testet)
    ### GAMBIARRA ALERT
    testet[is.nan(testet)] <- 0

    # Matrix de intervenções
    data.frame(periodo=periodo, valor=diag(kfs$alphahat[posx,int, drop=F]), 
               desvio=sqrt(var), pvalor=testet,
               row.names=colnames(mod$T[, int, 1, drop=F]))
  }
}
