#' Exportacao de resultado
#'
#' Função que formata os dados para um padrão de exportação.
#' @param resultado Lista resultante da função \code{\link{calcular.elasticidades}} ou
#' ou da função \code{\link{decompor.todos}} com um conjunto de elasticidades calculadas.
#' @return Data frame no formato correto para exportação para o Excel.
#'
#' @importFrom utils write.table
#' @export
#' @examples
#' \dontrun{data(Exemplo)}
#' \donttest{resultado <- calcular.elasticidades(Exemplo$receitas, Exemplo$Hpib, Exemplo$Hpet, fim=c(2015,4))}
#' 
#' \donttest{resultado.exportacao <- exportar(resultado)}
#' \donttest{write.csv2(resultado.exportacao, "/tmp/dados.csv")} #Escreve resultados em arquivo CSV.
#' @seealso
#' \code{\link{decompor.todos}}
#' \code{\link{calcular.elasticidades}}
#' \code{\link{Exemplo}}
exportar <-
  function(resultado){
    receitas <- NULL
    Coef1 <- NULL
    Coef1u <- NULL
    Coef2 <- NULL
    Coef2u <- NULL
    Q <- NULL
    Q2 <- NULL
    lags <- NULL
    H <- NULL
    N <- NULL
    aic <- NULL
    bic <- NULL
    w1 <- NULL
    w2 <- NULL
    t1 <- NULL
    t2 <- NULL
    z <- NULL
    interv <- NULL
    periodo <- NULL

    if(class(resultado[[1]]) == "mee") { resultado <- list(resultado) } 
    if(!is.null(names(resultado))){
      receita <- names(resultado)
    } else {
      receita <- c("trt", "tfp", "trc", "ti", "tm", "tgc", "roy", "pe", "tran", "icms", "iss")
    }

    possibilidades <- matrix(c("S","S","S",
                               "S","S","F",
                               "S","F","S",
                               "S","F","F",
                               "F","S","S",
                               "F","S","F",
                               "F","F","S",
                               "F","F","F"), 8, 3, byrow=T)

    for(i in 1:length(resultado)){
      grupo <- receita[i]
      nome.mod <- gsub("(\\D)$", "\\1 contra Hiato", grupo)
      nome.mod <- gsub("1$", " contra Hiato com lag", nome.mod)
      nome.mod <- gsub("2$", "\\1 contra Hiato e Hiato com lag", nome.mod)

      if(length(resultado[[grupo]]) == nrow(possibilidades)/2) {
        poss <- possibilidades[seq(1,  nrow(possibilidades),2), ]
        poss[,3] <- "N"
      } else {
        poss <- possibilidades
      }

      for(j in 1:length(resultado[[grupo]])){
        res   <- resultado[[grupo]][[j]]
        if("error" %in% class(res)){
          receitas <- c(receitas, toupper(nome.mod))
          periodo <- c(periodo, NA)
          z   <- rbind(z, poss[j,])
          Coef1 <- c(Coef1, NA)
          Coef1u <- c(Coef1u, NA)
          Coef2 <- c(Coef2, NA)
          Coef2u <- c(Coef2u, NA)
          Q <- c(Q, NA)
          Q2 <- c(Q2, NA)
          lags <- c(lags, NA)
          H <- c(H, NA)
          N <- c(N, NA)
          aic <- c(aic,NA)
          bic <- c(bic,NA)
          w1 <- c(w1,NA)
          w2 <- c(w2,NA)
          t1 <- c(t1,NA)
          t2 <- c(t2,NA)
          interv <- c(interv,NA)
        } else {
          ncoef <- posicoes(res$dlm)$norm["Coef1"]

          periodo <- c(periodo, paste(paste(start(res$y), collapse="."), paste(end(res$y), collapse="."), sep="-"))

          Coef1 <- c(Coef1,   mean(res$comp[,"Coef1"]))
          Coef1u <- c(Coef1u, res$comp[nrow(res$comp), "Coef1"])
          w1    <- c(w1, sqrt(res$dlm$W[ncoef,ncoef]))
          t1    <- c(t1,      res$tt$Coef1$pvalor)

          coef2.b <- any(grepl("Coef2", colnames(res$comp)))
          Coef2  <- c(Coef2 , if(coef2.b)      mean(res$comp[,"Coef2"])  else NA)
          Coef2u <- c(Coef2u, if(coef2.b) res$comp[nrow(res$comp), "Coef2"] else NA)
          w2     <- c(w2,     if(coef2.b) sqrt(res$dlm$W[ncoef+1,ncoef+1]) else NA)
          t2     <- c(t2,     if(coef2.b)      res$tt$Coef2$pvalor         else NA)

          receitas <- c(receitas, toupper(nome.mod))
          Q   <- c(Q,  as.numeric(res$q$pvalor))
          Q2  <- c(Q2, as.numeric(res$q2$pvalor))
          lags<- c(lags, as.numeric(res$q$lags))
          H   <- c(H,  as.numeric(res$h$pvalor))
          N   <- c(N,  as.numeric(res$nt$pvalor))
          aic <- c(aic, res$aic)
          bic <- c(bic, res$bic)
          z   <- rbind(z, poss[j,])
          interv <- c(interv, paste(paste(rownames(res$interv), res$interv$periodo), collapse="; "))
        }
    }
  }

  ret <- data.frame(receitas, periodo, "", z, "", Coef1, Coef2, "", t1, t2, "", 
                    Q, Q2, lags, "", H, N, aic, bic, "", w1, w2, "", interv, Coef1u, Coef2u)
  colnames(ret) <- c("Grupo de Receita", "Periodo", "", "nivel", "inclinacao", "sazonalidade","",
                     "Coeficiente", "Coeficiente 2",
                     "", "pvalor Hiato", "pvalor Hiato com lag", "", 
                     "pvalor teste Q com q lags (Correlacao Serial)",
                     "pvalor teste Q com 2q lags", "q", "", 
                     "pvalor teste H (Heterocedasticidade)", "pvalor teste N (Normalidade)",
                     "Criterio Akaike", "Criterio Bayes", "",
                     "Desvio Padrao do disturbio de Coef",
                     "Desvio Padrao do disturbio de Coef2",
                     "", "Intervencoes", "Coef no ultimo ponto", "Coef2 no ultimo ponto")
  
  return(ret)
}
