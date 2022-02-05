get_intervention_position <- function(residue, scs, lim) {
  inters <- NULL
  sc     <- scs[, residue]
  sc[is.na(sc)] <- 0

  # The loop will look for errors bigger than the limits.
  # Much of the work is to assure that neighbooring errors are not detected at the same time
  j <- 1
  n <- length(sc)
  while(j <= n) {
    if(abs(sc[j]) > lim[[residue]]) {
      X <- rep(0, n)

      if (residue == "principal") {
        X[j] <- 1
      } else {
        # k é igual ao número de períodos que, a partir do ponto j, os residuos são maiores (ou menores)
        # que o limite e não mudam de sinal.
        k <- (-1)^(sc[j]<0) * sc[j:n] > lim[[residue]]
        k <- if(which.min(k) > 1) {
          which.min(k)-1 
        } else {
          n
        }

        pos    <- j - 1 + which.max(abs(sc[j:(j+k)]))
        X[pos] <- 1
        j      <- j + k
      }
      # Retornaremos esse vetor, que será mais tarde utilizado para o filtro de kalman
      # Os nomes das colunas são importantes para identificação do local da intervenção
      X <- as.matrix(X)
      colnames(X) <- paste0(residue, ".", j)
      inters <- cbind(inters, X)
    }
    j <- j + 1
  }
  return(inters)
}
