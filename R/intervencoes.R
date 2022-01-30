#' Intervencoes
#'
#' Função para detectar automaticamente as intervenções
#'
#' @param u Data frame de choques
#' @param dlm Objeto dlm
#' @return Lista com posição da intervenção
#'
#' @importFrom stats end frequency lag sd start ts window
#' @keywords internal
intervencoes <-
  function(kfs) {
    ret <- list()
    mod <- kfs$model
    posQn <- which(attr(mod, "state_types") == "nivel")
    posQi <- which(attr(mod, "state_types") == "incli")
    # Detectamos os períodos em que os erros absolutos são maiores que os limites estabelecidos
    n   <- attr(mod, "n")
    lim <- list(principal = if(mod$H[,,1] > exp(-32)) 2.3 else 2.1,
                nivel  = if(!(length(posQn) > 0))                 NULL
                         else if(mod$Q[posQn,posQn,1] > exp(-32)) 2.5
                         else                                     2.3,
                
                incli  = if(!(length(posQi) > 0))                 NULL
                         else if(mod$Q[posQi,posQi,1] > exp(-32)) 3
                         else                                     2.8)

    #scs <- filtro$choques[, colnames(filtro$choques) %in% c("choque", "Cnivel", "Cincli")]
    scs  <- data.frame(principal=rstandard(kfs, "pearson"),
                       rstandard(kfs, "state"))
    scs  <- scs[, names(scs) %in% names(lim)]

    ret <- lapply(colnames(scs), function(choque) {
      ret   <- NULL
      sc    <- scs[, choque]

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
          if (choque=="nivel") {
            pos <- j - 1 + which.max(abs(sc[j:(j+k)]))
            X[pos] <- 1
            j <- j + k
          } else if(choque=="incli") {
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
    colnames(ret) <- paste0("I.", colnames(ret))
    colnames(ret) <- numerar.nomes(colnames(ret))

    return(ret)
  }
