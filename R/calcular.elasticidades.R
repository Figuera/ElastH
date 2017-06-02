#' Calculo elasticidades de todos os grupos de receitas
#'
#' Função para estimar as elasticidades de todos os grupos de receitas. Exige
#' entrada de dados no formato correto.
#' 
#' @param Receitas Matrix/ts contendo vários grupos de receitas que devem
#' ser identificadas por um dos seguintes nomes: TRT, TFP, TRC, TI, TM,
#' TGC, TRAN, ICMS, ISS, ROY e PE.
#' @param Hpib Série de tempo do hiato do PIB.
#' @param Hpet Série de tempo do hiato do Petróleo (necessário se ROY e PE
#' estiverem presentes).
#' @param tce Série de tempo de taxa de cambio efetiva, utilizada como
#' variável de controle.
#' @param fim Período final para o cálculo das elasticidades.
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
calcular.elasticidades <-
function(Receitas, Hpib, Hpet=NULL, tce, fim) {
  ###Produto
  message("Calculando TRT")
  trt  <- decompor.todos(Receitas[, "TRT"] , X = Hpib, comeco = 2000, fim = fim)
  trt1 <- decompor.todos(Receitas[, "TRT"] , X = lag(Hpib,-1), comeco = 2000, fim = fim)
  trt2 <- decompor.todos(Receitas[, "TRT"] , X = cbind(Hpib,lag(Hpib,-1)), comeco = 2000, fim = fim)
  
  message("Calculando TFP")
  tfp  <- decompor.todos(Receitas[, "TFP"] , X = Hpib, comeco = 1997, fim = fim)
  tfp1 <- decompor.todos(Receitas[, "TFP"] , X = lag(Hpib,-1), comeco = 1997, fim = fim)
  tfp2 <- decompor.todos(Receitas[, "TFP"] , X = cbind(Hpib,lag(Hpib,-1)), comeco = 1997, fim = fim)

  message("Calculando TRC")
  trc  <- decompor.todos(Receitas[, "TRC"] , X = Hpib, comeco = 1997, fim = fim)
  trc1 <- decompor.todos(Receitas[, "TRC"] , X = lag(Hpib,-1), comeco = 1997, fim = fim)
  trc2 <- decompor.todos(Receitas[, "TRC"] , X = cbind(Hpib, lag(Hpib,-1)), comeco = 1997, fim = fim)

  message("Calculando TI")
  ti   <- decompor.todos(Receitas[, "TI"] , X = Hpib, comeco = 2001, fim = fim)
  ti1  <- decompor.todos(Receitas[, "TI"] , X = lag(Hpib,-1), comeco = 2001, fim = fim)
  ti2  <- decompor.todos(Receitas[, "TI"] , X = cbind(Hpib,lag(Hpib,-1)), comeco = 2001, fim = fim)

  message("Calculando TM")
  tm   <- decompor.todos(Receitas[, "TM"] , X = cbind(Hpib, tce), comeco = 2005, fim = fim)
  tm1  <- decompor.todos(Receitas[, "TM"] , X = cbind(lag(Hpib,-1), tce), comeco = 2005, fim = fim)
  tm2  <- decompor.todos(Receitas[, "TM"] , X = cbind(Hpib, lag(Hpib,-1), tce), comeco = 2005, fim = fim)

  message("Calculando TGC")
  tgc  <- decompor.todos(Receitas[, "TGC"] , X = Hpib, comeco = 2005, fim = fim)
  tgc1 <- decompor.todos(Receitas[, "TGC"] , X = lag(Hpib, -1), comeco = 2005, fim = fim)
  tgc2 <- decompor.todos(Receitas[, "TGC"] , X = cbind(Hpib, lag(Hpib, -1)), comeco = 2005, fim = fim)

  message("Calculando TRAN")
  tran <- decompor.todos(Receitas[, "TRAN"] , X = Hpib, comeco = 2002, fim = fim)
  tran1<- decompor.todos(Receitas[, "TRAN"] , X = lag(Hpib, -1), comeco = 2002, fim = fim)
  tran2<- decompor.todos(Receitas[, "TRAN"] , X = cbind(Hpib, lag(Hpib, -1)), comeco = 2002, fim = fim)

  message("Calculando ICMS")
  icms <- decompor.todos(Receitas[, "ICMS"], X = Hpib, comeco = 1997, fim = fim)
  icms1<- decompor.todos(Receitas[, "ICMS"], X = lag(Hpib,-1), comeco = 1997, fim = fim)
  icms2<- decompor.todos(Receitas[, "ICMS"], X = cbind(Hpib, lag(Hpib,-1)), comeco = 1997, fim = fim)

  message("Calculando ISS")
  iss  <- decompor.todos(Receitas[, "ISS"], X = Hpib, comeco = 2002, fim = fim)
  iss1 <- decompor.todos(Receitas[, "ISS"], X = lag(Hpib, -1), comeco = 2002, fim = fim)
  iss2 <- decompor.todos(Receitas[, "ISS"], X = cbind(Hpib, lag(Hpib, -1)), comeco = 2002, fim = fim)

  ###Petróleo
  if(!is.null(Hpet)){
    message("Calculando ROY")
    roy  <- decompor.todos(Receitas[, "ROY"] , X = Hpet, comeco = 2000, fim = fim, sazon.b=F)
    roy1 <- decompor.todos(Receitas[, "ROY"] , X = lag(Hpet, -1), comeco = 2000, fim = fim, sazon.b=F)
    roy2 <- decompor.todos(Receitas[, "ROY"] , X = cbind(Hpet, lag(Hpet, -1)), comeco = 2000, fim = fim, sazon.b=F)
  
    message("Calculando PE")
    pe   <- decompor.todos(Receitas[, "PE"] , X = Hpet, comeco = 2000, fim = fim, sazon.b=F)
    pe1  <- decompor.todos(Receitas[, "PE"] , X = lag(Hpet, -1), comeco = 2000, fim = fim, sazon.b=F)
    pe2  <- decompor.todos(Receitas[, "PE"] , X = cbind(Hpet, lag(Hpet, -1)), comeco = 2000, fim = fim, sazon.b=F)
  } else {
    roy <- NULL
    pe <- NULL
  }

  return(list(trt=trt  ,trt1=trt1  ,trt2=trt2  ,
              tfp=tfp  ,tfp1=tfp1  ,tfp2=tfp2  ,
              trc=trc  ,trc1=trc1  ,trc2=trc2  ,
              ti=ti    ,ti1=ti1    ,ti2=ti2    ,
              tm=tm    ,tm1=tm1    ,tm2=tm2    ,
              tgc=tgc  ,tgc1=tgc1  ,tgc2=tgc2  , 
              roy=roy  ,roy1=roy1  ,roy2=roy2  ,
              pe=pe    ,pe1=pe1    ,pe2=pe2    ,
              tran=tran,tran1=tran1,tran2=tran2,
              icms=icms,icms1=icms1,icms2=icms2,
              iss=iss  ,iss1=iss1  ,iss2=iss2  ))

}
