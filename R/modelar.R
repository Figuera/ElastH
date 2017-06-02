#' Componentes e Testes
#'
#' Função que calcula a partir do objeto dlm e de algumas variáveis estruturais,
#' o valor dos componentes, reestrutura o resultados da função do pacote dlm para fins
#' de legibilidade, e, por fim, realiza testes diversos nos resíduos e valores obtidos.
#'
#' @param y Série temporal a ser decomposta
#' @param dlm Objeto 'dlm'
#' @param estrutura Lista com variáveis importantes para realização de testes:
#'  \itemize{
#'    \item \code{p}: Número de variâncias estocásticas
#'    \item \code{logLike}: Valor da função de log-verossimilhança }
#' @return Lista com 15 variáveis, entre componentes e testes:
#'   \itemize{
#'     \item \code{y}: Série que foi decomposta.
#'     \item \code{dlm}: Estrutura do objeto dlm usado na decomposição.
#'     \item \code{f}: Lista de resultados do Filtro de Kalman. Ver \code{dlmFilter}.
#'     \item \code{s}: Lista de resultados do processo de suavização. Ver \code{dlmSmooth}.
#'     \item \code{comp}: Tabela com os componentes de interesses suavizados (Resultado Principal).
#'     \item \code{interv}: tabela listando as intervenções, seu componente.
#'     período, valor, desvio padrão do estimador e o pvalor do teste t.
#'     \item \code{choques}: Matriz dos disturbios suavizados. Ver \code{choques}.
#'     \item \code{e}: Série dos erros de projeção um passo a frente.
#'     \item \code{q}: Teste de independência dos erros de projeção.
#'     \item \code{q2}: Teste de independência dos erros de projeção, com o dobro de lags.
#'     \item \code{h}: Teste de homocedasticidade dos erros de projeção.
#'     \item \code{nt}: Teste de normalidade dos erros de projeção.
#'     \item \code{aic}: Critério de Akaike.
#'     \item \code{bic}: Critério de Bayes.
#'     \item \code{tt}: Lista com resultado de testes t para os coeficientes estimados. }
#
#' @importFrom stats Box.test end frequency lag pt qchisq qf sd start ts window residuals
#' @importFrom utils tail
#' @keywords internal
modelar <-
function(y, dlm, estrutura) {
  # Calculando o valor dos componentes
  f <- dlm::dlmFilter(y, dlm)
  s <- dlm::dlmSmooth(f)

  n    <- length(y)      # Tamanho da série
  d    <- length(dlm$m0) # Número de componentes
  freq <- frequency(y)    # Frequência da série principal

  # Reestruturando Tabela de Componentes
  dlm.posc <- posicoes(dlm)
  int.posc <- dlm.posc$int #Intervenções
  norm.posc <- dlm.posc$norm #Não intervenções

  # Truque para que cs se comporte como uma série de tempo E como uma matriz,
  # ainda que tenha apenas uma coluna
  if("mts" %in% class(s$s)){
    cs <- as.matrix(s$s)
  } else {
    cs <- ts(matrix(s$s), start=start(s$s), frequency=freq)
  }

  comp <- data.frame(y,cs[-1, norm.posc])
  colnames(comp) <- c("Valor", names(norm.posc))

  # Componente de Sazonalidade definição diferenciada
  if(any(grepl("Sazon", names(norm.posc)))) {
    comp["Sazon"] <- comp["Sazon"] + cs[-1, norm.posc["Sazon"] + 2]
  }

  #Modelo sem testes e sem intervenções é criado
  modelo <- list(y = y, dlm = dlm, f = f, s = s, comp = comp)

  #Teste de intervenção
  n.int    <- length(int.posc) 
  if(n.int > 0){
    # Variância "de fato" dos resíduos das intervenções
    covar <- dlm::dlmSvd2var(s$U.S, s$D.S)
    var   <- tail(diag(covar[[length(covar)]]), n=n.int)

    inicio   <- ncol(dlm$X)-n.int
    posx     <- apply(dlm$X[,inicio + seq(n.int), drop=F], 2, which.max) # Posição de intervenção
    testet   <- 2*(1 - pt(abs(diag(cs[posx,int.posc, drop=F]))/sqrt(var), n-d)) # Teste t do valor da intervenção
    periodo  <- start(y)[1] + (posx-2+start(y)[2])/freq
    # Nomes usados na matrix de intervenções, apenas informativo
    #colnomes <- paste(names(int.posc),(start(y)[1] + (posx-2+start(y)[2])%/%freq), (posx%%freq+start(y)[2]-1), sep=".")

    # Matrix de intervenções
    interv.df <- data.frame(periodo=periodo, valor=diag(cs[posx,int.posc, drop=F]), 
                            desvio=sqrt(var), pvalor=testet, row.names=names(int.posc))

    modelo$interv <- interv.df
  }

  # Calculo dos resíduos
  modelo$choques <- choques(modelo$f)
  modelo$e <- residuals(modelo$f, sd=F)

  # Testes diversos
  lags <- max(2*freq, floor(sqrt(n-d)) + estrutura$p-1)
  q <- Box.test(modelo$e,   lags, type="Ljung-Box", estrutura$p-1)
  modelo$q   <- data.frame(Q.valor= q$statistic, Q.critico=qchisq(0.95, q$parameter), pvalor=q$p.value, lags=lags)
  q <- Box.test(modelo$e, 2*lags, type="Ljung-Box", estrutura$p-1)
  modelo$q2  <- data.frame(Q.valor= q$statistic, Q.critico=qchisq(0.95, q$parameter), pvalor=q$p.value, lags=2*lags)

  modelo$h   <- teste.h(modelo$e, d) 
  modelo$nt  <- teste.normalidade(modelo$e)
  modelo$aic <- 1/n*(2*estrutura$loglike + 2*(estrutura$p+d))
  modelo$bic <- 1/n*(2*estrutura$loglike + log(n)*(estrutura$p+d))
  
  # Calculando testes t para coeficientes
  # Variância "de fato" dos resíduos das intervenções
  if("Coef1" %in% names(norm.posc)) {
    coefs <- norm.posc[grepl("Coef", names(norm.posc))]
                             
    modelo$tt <- lapply(coefs, function(posc) {
             covar  <- dlm::dlmSvd2var(s$U.S, s$D.S)
             var    <- diag(covar[[length(covar)]])[posc]
             tvalor <- cs[nrow(cs), posc]/sqrt(var)
             pvalor <- 2*(1-pt(abs(tvalor), n-d))

             data.frame(valor=cs[nrow(cs), posc], desvio=sqrt(var), tvalor=tvalor, pvalor=pvalor)
                        })
  }

  class(modelo) <- "mee"
  return(modelo)
}
