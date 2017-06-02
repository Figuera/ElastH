#' Estimação de alternativas hipoteses
#' 
#' Função que calcula 8 formas funcionais para elasticidades para fins de comparação
#'
#' @param y Série de tempo a ser decomposta
#' @param X Série de tempo das variáveis independentes
#' @param comeco Período inicial dos cálculos
#' @param fim Período final dos cálculos
#' @param sazon.b Boolean indicativa se o efeito sazonal será incorporado na forma funcional, Padrão = TRUE
#' @param regres Define que os coeficientes devem ser fixos no tempo ("F") ou estocásticos ("S")
#' @return lista com os 8 modelos estimados
#'
#' @examples
#' seriey <- ts(runif(96), start=1997, end=c(2015,4), frequency=4)
#' # Estimar modelo sem variáveis indepedentes
#' \donttest{ lista.dlm <- decompor.todos(seriey) }
#'
#' seriex <- ts(runif(96), start=1997, end=c(2015,4), frequency=4)
#' # Estimar modelos incluindo variável independente
#' \donttest{ lista.dlm2 <- decompor.todos(y=seriey, X=seriex) }
#' # Estimar modelo, com variavel dependente, mas restringindo o escopo temporal
#' \donttest{ lista.dlm3 <- decompor.todos(y=seriey, X=seriex, comeco=2000, fim=2014) }
#' @export
#' @seealso
#' \code{\link{decompor}}
#' \code{\link{exportar}}
decompor.todos <-
function(y,X=NULL, comeco=NULL, fim=NULL, sazon.b=TRUE, regres="S") {
  possibilidades <- matrix(c("S","S","S",
                             "S","S","F",
                             "S","F","S",
                             "S","F","F",
                             "F","S","S",
                             "F","S","F",
                             "F","F","S",
                             "F","F","F"), 8, 3, byrow=T)

  if(!sazon.b){
    possibilidades <- possibilidades[seq(1, nrow(possibilidades), 2), , drop=F]
    possibilidades[,3] <- "N"
  }

  applyfun <- function(z, y2, X2, comeco2, fim2, sazon.b2=NULL) {
    tryCatch({
      return(decompor(y=y2, X=X2, comeco=comeco2, fim=fim2, regres=regres, nivel=z[1], inclinacao=z[2], sazon=z[3]))
    }, error = function(error) {
      warning(paste("Erro na estimativo do modelo:", paste0(z, collapse=" ") ,
                    "periodo: ", paste0(comeco2, collapse="."),"-",
                    paste0(fim2, collapse="."), "Erro:", error))
      return(error)
    })
  }

  return(apply(possibilidades, 1, applyfun, y2=y, X2=X, comeco2=comeco, fim2=fim, sazon.b2=sazon.b))
}
