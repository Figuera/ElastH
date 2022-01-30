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
#' seriex <- ts(runif(96), start=1997, end=c(2015,4), frequency=4) }
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

  applyfun <- function(nivel, slope, sazon, y, X, comeco, fim, sazon.b=NULL) {
    tryCatch({
      return(decompor(y=y, X=X, comeco=comeco, fim=fim, regres=regres,
                 nivel=nivel, inclinacao=slope,
                 sazon= if(sazon.b) sazon else "N"))
    }, error = function(error) {
      aviso <- paste("Erro na estimativo do modelo:", paste0(c(nivel, slope, sazon), collapse=" ") ,
                    "periodo: ", paste0(comeco, collapse="."), "-",
                    paste0(fim, collapse="."), "Erro:", error)
      warning(aviso)
      return(aviso)
    })
  }

  args <- apply(possibilidades, 1, function(z) list(nivel = z[1], slope = z[2], sazon = z[3],
      y = y, X = X, comeco = comeco, fim = fim, sazon.b = sazon.b))

  lapply(args, function(arg) do.call(applyfun, arg))
}
