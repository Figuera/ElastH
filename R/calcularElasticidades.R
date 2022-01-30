#' Calculo elasticidades de todos os grupos de receitas
#'
#' Função para estimar as elasticidades de todos os grupos de receitas. Exige
#' entrada de dados no formato correto.
#' 
#' @param Receitas Matrix/ts contendo todos os grupos de receita, que devem
#' seguir ordem: TRT, TFP, TRC, TI, TM, TGC, TRAN, ICMS, ISS, ROY e PE (ROY e
#' PE são opcionais)
#' @param Hpib Série de tempo do hiato do PIB
#' @param Hpet Série de tempo do hiato do Petróleo (opcional)
#' @param fim Período final para o cálculo das elasticidades
#' @return Extensa lista com todas as combinações possíveis de nível, inclinacao e
#' sazon, para os 11 grupos de receita utilizados.
#'
#' @importFrom stats end frequency lag start ts window
#' @export
#' @examples
#' \dontrun{data(Exemplo)}
#' \donttest{resultado <- calcularElasticidades(Exemplo$receitas, 
#'                          Exemplo$Hpib, Exemplo$Hpet, c(2015,4))}
#' 
#' receitas <- ts(matrix(runif(836), nrow=76, ncol=11), start=1997,
#'                   end=c(2015,4), frequency=4)
#' Hpib <- ts(runif(76), start=1997, end=c(2015,4), frequency=4)
#' Hpet <- ts(runif(76), start=1997, end=c(2015,4), frequency=4)
#' \donttest{resultado <- calcularElasticidades(receitas, Hpib, Hpet, fim=c(2015,4))} 
#' @seealso
#' \code{\link{decompor.todos}}
#' \code{\link{decompor}}
#' \code{\link{Exemplo}}
#' \code{\link{exportar}}
calcularElasticidades <-
function(Receitas, Hpib, Hpet=NULL, fim) {
  ###Produto
  message("Calculando TRT")
  trt <- decompor.todos(Receitas[,"TRT"], X=Hpib, comeco=2000, fim=fim)
  message("Calculando TFP")
  tfp <- decompor.todos(Receitas[,"TFP"], X=Hpib, comeco=1997, fim=fim)
  message("Calculando TRC")
  trc <- decompor.todos(Receitas[,"TRC"], X=cbind(Hpib, lag(Hpib, -1)), comeco=c(1997,2), fim=fim)
  message("Calculando TI")
  ti <- decompor.todos(Receitas[,"TI"], X=Hpib, comeco=2005, fim=fim)
  message("Calculando TM")
  tm <- decompor.todos(Receitas[,"TM"], X=Hpib, comeco=2005, fim=fim)
  message("Calculando TGC")
  tgc <- decompor.todos(Receitas[,"TGC"], X=cbind(Hpib,lag(Hpib,-1)), comeco=c(2005,2), fim=fim)
  message("Calculando TRAN")
  tran <- decompor.todos(Receitas[,"TRAN"], X=cbind(Hpib, lag(Hpib,-1)), comeco=c(2002,2), fim=fim)
  message("Calculando ICMS")
  icms <- decompor.todos(Receitas[,"ICMS"], X=Hpib, comeco=1997, fim=fim)
  message("Calculando ISS")
  iss <- decompor.todos(Receitas[,"ISS"], X=cbind(Hpib, lag(Hpib,-1)), comeco=c(2004,2), fim=fim)

  ###Petróleo
  if(!is.null(Hpet)){
    message("Calculando ROY")
    roy <- decompor.todos(Receitas[,"ROY"], X=cbind(Hpet, lag(Hpet,-1)), comeco=c(2004,2), fim=fim)
    message("Calculando PE")
    pe <- decompor.todos(Receitas[,"PE"], X=lag(Hpet,-1), comeco=c(2000,2), fim=fim)
  } else {
    roy <- NULL
    pe <- NULL
  }

  return(list(trt=trt,tfp=tfp,trc=trc,ti=ti,tm=tm,tgc=tgc,roy=roy,pe=pe,tran=tran,icms=icms,iss=iss))
}
