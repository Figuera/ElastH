#' Estimacao de componentes do Filtro de Kalman
#'
#' Função principal, usada para calcular as elasticidades dos grupos de
#' receitas, ou, de modo mais genérico, decompor por meio de um Filtro de
#' Kalman a série de tempo dependente (\code{y})
#' 
#' @param y Série temporal a ser decomposta.
#' @param X Série temporal independente - (Hiatos).
#' @param irregular String definidora da variância do resíduo da equação
#' principal (Vide details):
#'        \itemize{
#'          \item \code{"S"} Variância definida automaticamente
#'          \item \code{"F"} Variância fixa zero
#'          \item \code{"N"} Ignorar Componente (não é possível ignorar a
#'          equação principal)}
#' @param nivel String definidora da variância do resíduo da equação do nível.
#' Ver irregular para valores possíveis.
#' @param inclinacao String definidora da variância do resíduo da equação do inclinacao.
#' Ver irregular para valores possíveis.
#' @param sazon String definidora da variância do resíduo da equação da
#' sazonalidade. Ver irregular para valores possíveis.
#' @param regres String definidora da variância do resíduo da equação dos
#' coeficientes. Ver irregular para valores possíveis.
#' @param comeco Período inicial.
#' @param fim Período final.
#' @param freq Frequência da série de tempo. Padrão é a frequência se y. Se y
#' não for uma série de tempo é necessário informar variável manualmente.
#' @param init Parâmetros iniciais para o processo de otimização (Deixar o
#' padrão).
#' @param interv.b TRUE (Padrão) ou F. Define se as intervenções serão
#' detectadas automaticamente.
#' @return A função retorna uma lista com 11 variáveis, entre componentes e
#' testes:
#'   \itemize{
#'     \item \code{y}: Série que foi decomposta
#'     \item \code{dlm}: Estrutura do objeto dlm usado na decomposição
#'     \item \code{f}: Componentes resultantes do filtro
#'     \item \code{theta}: Componentes suavizados
#'     \item \code{c}: Subconjunto de theta, com sazonalidade agregada e nomes
#'     nas colunas (Resultado Principal)
#'     \item \code{interv}: tabela listando as intervenções, seu componente,
#'     período, valor e teste t.
#'     \item \code{u}: Matriz de resíduos
#'     \item \code{q}: Teste de independência dos resíduos
#'     \item \code{h}: Teste de homocedasticidade dos resíduos
#'     \item \code{nt}: Teste de normalidade dos resíduos
#'     \item \code{aic}: Valor AIC da função de verossimilhança usada para
#'     estimar modelo.}
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
#' Caso \code{interv.b} seja definido como \code{F}. Então intervenções não
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
decompose <- function(formula, data, irregular  = NA) {
  # Retrieve dependent variable
  y <- ts(data[, all.vars(formula)[1], drop = T], start=start(data), frequency = frequency(data))

  # Retrieve other terms
  terms <- attr(terms(formula), "term.labels")

  # Fundamentos para criação de modelo espaço-estado
  # Note: If component is missing it will be ignored
  variances <- list(
    irregular = irregular,
    level = get_variance("level", terms),
    slope = get_variance("slope", terms),
    seas  = get_variance("seas",  terms))

  init <- c(level = -1, slope = -1.5, seas = -2, irregular =-0.5)
  init <- init[ is.na( variances[names(init) ]) ] # If Variance of component is not given estimate it

  # Finding regression components
  # Ignoring state components
  terms             <- terms[!grepl("(level.*)|(slope.*)|(seas.*)", terms)]
  regression_terms  <- gsub("\\(.*", "", terms)
  if (length(regression_terms) > 0) {
    # Set X data.frame
    X                 <- as.data.frame(data[, regression_terms, drop = F])

    # Set X Variances
    var_regres        <- lapply(terms, get_variance, terms)
    names(var_regres) <- rep("regres", length(var_regres))
    variances         <- c(variances, var_regres)

    # Set X initial parameters for variance optimization
    xinit <- rep(-8, ncol(X))[is.na(var_regres)]
    names(xinit) <- rep("regres", ncol(X))
    init <- c(init, xinit)
  } else {
    X <- NULL
  }


  message("Estimando Variancias...")
  t <- Sys.time()
  fit <- build_ssm(init, y, X, variances)
  message(paste("Variancias Estimadas em", format(Sys.time() - t, digits=3)))

  modelo <- KFAS::KFS(fit$model, smoothing=c("state", "signal", "disturbance"))

  attr(modelo$mod, "hm") <- length(fit$optim.out$par)
  return(modelo)
  decompor  }
