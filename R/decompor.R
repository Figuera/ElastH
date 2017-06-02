#' Estimacao de componentes do Filtro de Kalman
#'
#' Função principal, usada para calcular as elasticidades dos grupos de
#' receitas, ou, de modo mais genérico, decompor por meio de um Filtro de
#' Kalman a série de tempo dependente (\code{y})
#' 
#' @param y Série temporal a ser decomposta.
#' @param X Série temporal independente - (Hiatos).
#' @param irregular String definidora do comportamento do disturbio da equação
#' principal (Vide details):
#'        \itemize{
#'          \item \code{"S"} Variância definida automaticamente
#'          \item \code{"F"} Variância fixa zero
#'          \item \code{"N"} Ignorar Componente (não é possível ignorar a
#'          equação principal)}
#' @param nivel String definidora do comportamento do disturbio da equação do nível.
#' Ver irregular para valores possíveis.
#' @param inclinacao String definidora do comportamento do disturbio da equação do inclinacao.
#' Ver irregular para valores possíveis.
#' @param sazon String definidora do comportamento do disturbio da equação da
#' sazonalidade. Ver irregular para valores possíveis.
#' @param regres String definidora do comportamento do disturbio da equação dos
#' coeficientes. Ver irregular para valores possíveis.
#' @param comeco Período inicial de estimação.
#' @param fim Período final de estimação.
#' @param freq Frequência da série de tempo. Informar somente se \code{y} não
#' for uma série de tempo.
#' @param ipar Parâmetros iniciais para o processo de otimização. Não é
#' aconselhável modificar os valores padrões.
#' @param interv.b \code{TRUE} (Padrão) ou \code{FALSE}. Define se as intervenções serão
#' detectadas automaticamente.
#' @return A função retorna uma lista com 15 variáveis, entre componentes e testes:
#'   \itemize{
#'     \item \code{y}: Série que foi decomposta.
#'     \item \code{dlm}: Estrutura do objeto dlm usado na decomposição.
#'     \item \code{f}: Lista de resultados do Filtro de Kalman. Ver \code{dlmFilter}.
#'     \item \code{s}: Lista de resultados do processo de suavização. Ver \code{dlmSmooth}.
#'     \item \code{comp}: Tabela com os componentes de interesses suavizados (Resultado Principal).
#'     \item \code{interv}: tabela listando as intervenções, seu componente.
#'        período, valor, desvio padrão do estimador e o pvalor do teste t.
#'     \item \code{choques}: Matriz dos disturbios suavizados. Ver \code{choques}.
#'     \item \code{e}: Série dos erros de projeção um passo a frente.
#'     \item \code{q}: Teste de independência dos erros de projeção.
#'     \item \code{q2}: Teste de independência dos erros de projeção, com o dobro de lags.
#'     \item \code{h}: Teste de homocedasticidade dos erros de projeção.
#'     \item \code{nt}: Teste de normalidade dos erros de projeção.
#'     \item \code{aic}: Critério de Akaike.
#'     \item \code{bic}: Critério de Bayes.
#'     \item \code{tt}: Lista com resultado de testes t para os coeficientes estimados. 
#'  }
#'
#' @details
#'
#' O modelo linear dinâmico usado neste pacote tem a seguinte estrutura:
#'
#' \deqn{y_t = \mu_t + \beta_t \cdot{X_t} + \gamma_t + \varepsilon_t}
#' \deqn{\mu_t = \mu_{t-1} + \nu_{t-1} + \xi_t}
#' \deqn{\nu_t = \nu_{t-1} + \zeta_t}
#' \deqn{\gamma_t = \gamma_{1,t} + \gamma_{2,t}}
#' \deqn{\gamma_{1,t} = - \gamma_{1,t-2} + \omega_{1,t}}
#' \deqn{\gamma_{2,t} = - \gamma_{2,t-1} + \omega_{2,t}}
#' \deqn{\beta_t = \beta_{t-1} + \eta_t}
#'
#' Onde \eqn{y_t} é o argumento \code{y}, \eqn{X_t} é o argumento \code{X},
#' e \eqn{\mu_t}, \eqn{\nu_t}, \eqn{\gamma_t} e \eqn{\beta_t} são os
#' componentes não observados estimados pelo Filtro de Kalman, respectivamente,
#' nível, inclinação, sazonalidade e coeficiente(s). Por fim os resíduos seguem
#' as seguintes distribuições: 
#'
#' \deqn{\varepsilon_t \sim \mathcal{N}(0, \sigma^2_\varepsilon)}
#' \deqn{\xi_t \sim \mathcal{N}(0, \sigma^2_\xi)}
#' \deqn{\zeta_t \sim \mathcal{N}(0, \sigma^2_\zeta)}
#' \deqn{\omega_{1,t} \sim \mathcal{N}(0, 2\sigma^2_\omega)}
#' \deqn{\omega_{2,t} \sim \mathcal{N}(0, \sigma^2_\omega)}
#' \deqn{\eta_t \sim \mathcal{N}(0, \sigma^2_\eta)}
#'
#' Os argumentos \code{irregular, nivel, inclinacao, sazon e regres} controlam as
#' variâncias dos resíduos. Quando definidos igual a "S" as variância
#' são estimadas por um processo de otimização. Quanto definidos igual a "F",
#' as variâncias são fixas em 0 (e logo o resíduo é 0 em todo \eqn{t}), por fim
#' se forem definidos igual a "N" o componente é ignorado, por exemplo se sazon
#' igual a "N" então \eqn{\gamma_t = 0\ \forall t} e logo não é estimado efeitos
#' de sazonalidade.
#' 
#' Note que as equações de sazonalidade dependem da frequência dos dados. Logo,
#' a forma funcional apresentada acima só funciona para o caso particular
#' em questão, com frequência trimestral.
#'
#' Note que \code{X} pode ser um data.frame, ou seja, um conjunto de variáveis
#' independentes. Com isso, 2 ou mais coeficientes serão estimados, nesse caso
#' \eqn{X}, \eqn{\beta} e \eqn{\eta} devem ser tratados como matrix (e não um
#' vetor).
#'
#' Caso \code{interv.b} seja definido como \code{FALSE}. Então intervenções não
#' serão calculadas automaticamente, caso se deseje implementer intervenções de
#' modo manual, então os vetores de intervenção devem ser colocados na matrix
#' \code{X}.
#'
#' @importFrom stats end start ts window
#' @export
#' @examples
#' seriey <- ts(runif(76), start=1997, end=c(2015,4), frequency=4)
#'
#' \donttest{ decomposicao <- decompor(seriey) } #Decomposição sem variável independente
#'
#' seriex <- ts(runif(76), start=1997, end=c(2015,4), frequency=4)
#' \donttest{ modelo <- decompor(seriey, seriex) } #Decomposição e estimação de coeficente
#' 
#' #Decomposição e estimação com nível e inclinacao fixos e sem sazonalidade
#' \donttest{ modelo2 <- decompor(seriey, seriex, nivel="F", inclinacao="F", sazon="N") }
#' #Decomposição e estimação com coeficente constante
#' \donttest{ modelo3 <- decompor(seriey, seriex, regres="F") }
#' #Decomposição e estimação usando apenas um subconjunto dos dados
#' \donttest{ modelo4 <- decompor(seriey, seriex, comeco=2000, fim=2010) }
#' #Decomposição e estimação sem a detecção de intervenções
#' \donttest{ modelo5 <- decompor(seriey, seriex, interv.b=F) }
decompor <-
  function(
           y,
           X          = NULL,
           irregular  = "S",
           nivel      = "S",
           inclinacao = "S",
           sazon      = "S",
           regres     = "S",
           comeco     = NULL,
           fim        = NULL,
           freq       = NULL,
           ipar       = c(-0.5,-1,-1.5,-2,16),
           interv.b   = TRUE
           )
  {
    # Se valores não estiverem definidos são inferidos da série principal
    # Note que se y não for uma série de tempo, freq será 1, comeco será 1 e fim será o tamanho de y
    if(is.null(freq))   freq   <- frequency(y)
    if(is.null(comeco)) comeco <- start(y)
    if(is.null(fim))    fim    <- end(y)

    fundamentos <- list(irregular=irregular, nivel=nivel, inclinacao=inclinacao,
                        sazon=sazon, regres=regres, freq=freq)
    p <- sum(fundamentos[c("nivel", "inclinacao", "sazon")] != "N")

    y <- window(y, start=comeco, end=fim)

    if(!is.null(X)) {
      fundamentos$X <- as.matrix(unclass(window(X, start=comeco, end=fim)))
      ipar  <- c(ipar, rep(-8, ncol(as.matrix(X))))

      if(is.null(colnames(X))) {
        colnames(fundamentos$X) <- "X"
      } else {
        nomes <- colnames(X)
        nomes[!(nomes %in% c("X", "Choque", "C.Nivel", "C.Inclinacao"))] <- "X"
        colnames(fundamentos$X) <- nomes
      }
    }

    message("Estimando Variancias...")
    t <- Sys.time()
    fit <- dlmMLE2(y, ipar, build.dlm, fundamentos)
    message(paste("Variancias Estimadas em", format(Sys.time() - t, digits=3)))

    modelo <- modelar(y, build.dlm(fit$par, fundamentos, pre.interv=T),
                      estrutura=list(p=p,loglike=fit$value))

    if(interv.b) {
      message("Criando intervencoes...")
      t <- Sys.time()

      intx <- intervencoes(modelo$choques, modelo$dlm)
      v.X <- fundamentos$X
      repeat {
        if(!is.null(intx)){
          if(is.null(v.X)){
            fundamentos$X <- intx
          } else {
            fundamentos$X <- data.frame(v.X, intx, check.names=F)
          }

          fit    <- dlmMLE2(y, fit$par, build.dlm, fundamentos)
          modelo <- modelar(y, build.dlm(fit$par, fundamentos),
                            estrutura=list(p=p,loglike=fit$value))
        } 
        message(paste("Intervencoes Criadas em", format(Sys.time() - t, digits=3)))

        t <- Sys.time()
        message("Avaliando Intervencoes")
        manual.interv <- sum(colnames(v.X) %in% c("Choques", "C.Nivel", "C.Inclinacao"))
        if(manual.interv > 0) {
          auto.interv <- modelo$interv[-1:-manual.interv, ]
        } else {
          auto.interv <- modelo$interv
        }

        if(any(auto.interv$pvalor > 0.05)) {
          rejeit.b  <- auto.interv$pvalor > 0.05
          rejeitado <- auto.interv[rejeit.b, , drop=F]
          validado  <-          intx[, auto.interv$pvalor < 0.05, drop=F]

          de.novo <- length(rejeitado) > 1
          if(de.novo) 
            de.novo <- apply(rejeitado, 1, function(x) {
                                proximidades <- abs(x[1] - rejeitado$periodo) <= 1/freq
                                return(sum(proximidades) > 1 && all(x[4] <= rejeitado[proximidades, "pvalor"]))
                            })

          if(any(de.novo)) {
            intx <- cbind(validado, intx[, rejeit.b][, de.novo])
            # Aqui termina o loop repeat iniciado acima. Porém não é lançado o break
            # de forma que mais uma interação é realizada.
          } else {

            if(ncol(as.matrix(validado)) > 0){
              if(is.null(v.X)){
                fundamentos$X <- validado
              } else {
                fundamentos$X <- data.frame(v.X, validado, check.names=F)
              }
            } else {
              fundamentos$X <- v.X
            }

            fit    <- dlmMLE2(y, fit$par, build.dlm, fundamentos)
            modelo <- modelar(y, build.dlm(fit$par, fundamentos),
                              estrutura=list(p=p,loglike=fit$value))

            message(paste("Modelo Reestimado em", format(Sys.time() - t, digits=3)))
            break
          }
        } else {
          break
        }
      }
    }
    return(modelo)
  }
