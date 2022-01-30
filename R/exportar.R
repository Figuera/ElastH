#' Exportacao de resultado
#'
#' Função que formato ao dados para um padrão de exportação.#'
#' @param resultado Lista resultante da função \code{\link{calcularElasticidades}} com o conjunto completo de elasticidades calculadas
#' @return Data frame no formato correto para exportação para o Excel. Em sistemas Windows o data.frame é copiado também
#' para o clipboard, possibilitando a colagem das informações no Excel com um simples Ctrl-V.
#'
#' @importFrom utils write.table
#' @export
#' @examples
#' \dontrun{data(Exemplo)}
#' \donttest{resultado <- calcularElasticidades(Exemplo$receitas, Exemplo$Hpib, Exemplo$Hpet, fim=c(2015,4))}
#' 
#' \donttest{resultado.exportacao <- exportar(resultado)}
#' \donttest{write.csv2(resultado.exportacao, "/tmp/dados.csv")} #Escreve resultados em arquivo CSV.
#' @seealso
#' \code{\link{todas.dlms}}
#' \code{\link{calcularElasticidades}}
#' \code{\link{Exemplo}}
exportar <-
  function(resultado){
    receitas <- NULL
    Coef1 <- NULL
    Coef2 <- NULL
    Q <- NULL
    Q2 <- NULL
    lags <- NULL
    H <- NULL
    N <- NULL
    aic <- NULL
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
                               "F","F","F"),
                               #"S","S","N",
                               #"S","F","N",
                               #"F","S","N",
                               #"F","F","N",
                               #"S","N","F",
                               #"S","N","S",
                               #"F","N","S",
                               #"F","N","F",
                               #"S","N","N",
                               #"F","N","N"),
                             8, 3, byrow=T)

    for(i in 1:length(resultado)){
      grupo <- receita[i]
      for(j in 1:length(resultado[[grupo]])){
        res   <- resultado[[grupo]][[j]]
        if(any(c("character", "error") %in% class(res))){
          receitas <- c(receitas, toupper(grupo))
          periodo <- c(periodo, NA)
          z   <- rbind(z, possibilidades[j,])
          Coef1 <- c(Coef1, NA)
          Coef2 <- c(Coef2, NA)
          Q <- c(Q, NA)
          Q2 <- c(Q2, NA)
          lags <- c(lags, NA)
          H <- c(H, NA)
          N <- c(N, NA)
          aic <- c(aic,NA)
          w1 <- c(w1,NA)
          w2 <- c(w2,NA)
          t1 <- c(t1,NA)
          t2 <- c(t2,NA)
          interv <- c(interv,NA)
        } else {
          grupo <- receita[i]
          res   <- resultado[[grupo]][[j]]
          testes <- testar.ssm(res)
          intervs <- testar.intervs(res)
          ncoef <- which(rownames(res$model$R) == "X")
          ncoef1 <- which(rownames(res$model$R) == "X1")

          periodo <- c(periodo, paste(paste(start(res$model$y), collapse="."), paste(end(res$model$y), collapse="."), sep="-"))

          Coef1 <- c(Coef1,   mean(res$alphahat[,"X"]))
          w1    <- c(w1, sqrt(res$model$Q[ncoef,ncoef, 1]))
          t1    <- c(t1,      testes$tt[[1]]$pvalor)

          coef2.b <- any(grepl("X1", colnames(res$alphahat)))
          Coef2 <- c(Coef2, if(coef2.b)      mean(res$alphahat[,"X1"])  else NA)
          w2    <- c(w2,    if(coef2.b) sqrt(res$model$Q[ncoef1,ncoef1, 1]) else NA)
          t2    <- c(t2,    if(coef2.b)      testes$tt[[2]]$pvalor         else NA)

          receitas <- c(receitas, toupper(grupo))
          Q   <- c(Q,  as.numeric(testes$q$pvalor))
          Q2  <- c(Q2, as.numeric(testes$q2$pvalor))
          lags<- c(lags, as.numeric(testes$q$lags))
          H   <- c(H,  as.numeric(testes$h$pvalor))
          N   <- c(N,  as.numeric(testes$nt$pvalor))
          aic <- c(aic, testes$aic)
          z   <- rbind(z, possibilidades[j,])
          interv <- c(interv, paste(paste(rownames(intervs), intervs$periodo), collapse="; "))
        }
      }
    }

    ret <- data.frame(receitas,periodo,"",z,"",Coef1,Coef2,"",t1,t2,"",Q,Q2,lags,"",H,N,aic,"",w1,w2,"",interv)
    colnames(ret) <- c("Grupo de Receita", "Periodo", "", "nivel", "inclinacao", "sazonalidade","",
                       "Hiato", "Hiato com lag","", "pvalor Hiato",
                       "pvalor Hiato com lag", "", 
                       "pvalor teste Q com q lags (Correlacao Serial)",
                       "pvalor teste Q com 2q lags lags", "q", "", 
                       "pvalor teste H (Heterocedasticidade)", "pvalor teste N (Normalidade)",
                       "Criterio Akaike", "", "Desvio Padrao disturbio Hiato",
                       "Desvio Padrao disturbio Hiato com lag", "", "Intervencoes")

    if(.Platform$OS.type != "unix"){ 
      write.table(ret, "clipboard", sep="\t", dec=",", row.names=FALSE, col.names=TRUE)
    }

    return(ret)
  }
