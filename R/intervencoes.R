#' Intervencoes
#'
#' Função para detectar automaticamente as intervenções
#'
#' @param res Data frame de choques
#' @param dlm Objeto dlm
#' @return Lista com posição da intervenção
#'
#' @importFrom stats end frequency lag sd start ts window
#' @keywords internal
intervencoes <-
  function(res, dlm) {
    ret <- list()
    # Detectamos os períodos em que os erros absolutos são maiores que os limites estabelecidos
    n   <- nrow(res$ch)
    lim <- list(Choque       = if(dlm$V      > exp(-32)) 2.3 else 2.1,
                C.Nivel      = if(dlm$W[1,1] > exp(-32)) 2.5 else 2.3,
                C.Inclinacao = if(dlm$W[2,2] > exp(-32)) 3   else 2.8)

    ret <- lapply(colnames(res$ch), function(choque) {
      ret   <- NULL
      col.c <- res$ch[,choque]
      sd.c  <- res$sd[,choque]
      sc    <- col.c/sd.c

      # Retira o último erro que não está bem definido
      sc[is.na(sc) | is.nan(sc)] <- 0
      # Iniciaremos agora um loop que checará erro por erro se ele é maior que os limites estabelecidos
      j <- 1

      while(j <= n) {
        if(abs(sc[j]) > lim[[choque]]) {
          X <- rep(0, n)
          # k é igual ao número de períodos que, a partir do ponto j, os choques são maiores (ou menores)
          # que o limite e não mudam de sinal.
          k <- (-1)^(sc[j]<0) * sc[j:n] > lim[[choque]]
          k <- if(which.min(k) > 1) which.min(k)-1 else n
          if (choque=="C.Nivel") {
            pos <- j - 1 + which.max(abs(sc[j:(j+k)]))
            X[pos] <- 1
            j <- j + k
          } else if(choque=="C.Inclinacao") {
            pos <- j - 1 + which.max(abs(sc[j:(j+k)]))
            X[pos] <- 1
            j <- j + k
          } else {
            X[j] <- 1
          }
          # Retornaremos esse vetor, que será mais tarde utilizado para o filtro de kalman
          # Os nomes das colunas são importantes para identificação do local da intervenção
          ret <- cbind(ret, X)
          colnames(ret) <- rep(choque, ncol(ret))
        }
        j <- j + 1
      }
      return(ret)
    })

    ret <- do.call(cbind, ret)

    return(ret)
  }
