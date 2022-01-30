#' Exemplos de Inputs e Outputs
#'
#' Lista da dados extensivamente utilizados para exemplos e testes
#'
#' @format Lista com 15 variáveis
#' \describe{
#'   \item{y}{Exemplo de série de tempo, geralmente usada como variável
#'   dependente}
#'   \item{dlm}{Exemplo de objeto dlm}
#'   \item{f}{Exemplo de componentes calculados pelo por modelo dlm}
#'   \item{theta}{Exemplo de componentes calculados e suavizados}
#'   \item{c}{Exemplo de componentes, similar a theta porém em formato legível}
#'   \item{interv}{Exemplo de lista de intervenções}
#'   \item{u}{Exemplo de matriz de resíduos}
#'   \item{q}{Exemplo de teste Q}
#'   \item{h}{Exemplo de teste H}
#'   \item{nt}{Exemplo de teste de normalidade}
#'   \item{aic}{Exemplo de valor AIC}
#'   \item{loglike}{Exemplo de valor de verossimilhança}
#'   \item{u.pre}{Outro exemplo de matrix de resíduos, caso pré intervenção}
#'   \item{dlm.pre}{Outro exemplo de objeto dlm, caso pré intervenção}
#'   \item{X.pre}{Exemplo de série de tempo de variável independente, diferente
#'   de dlm$X por ser pré intervenção}
#'   \item{receitas}{Exemplo de matriz de séries de tempo no formato correto
#'   para utilização na função \code{\link{calcularElasticidades}}}
#'   \item{Hpib}{Exemplo de série de tempo usada como hiato do produto, similar a dlm$X ou X.pre}
#'   \item{Hpet}{Exemplo de série de tempo usada como hiato do petroleo, similar a Hpib}
#'}
#' @docType data
#' @keywords datasets
#' @usage data(Exemplo)
"Exemplo"

##' Series de tempo dos grupos de receita
##'
##' Data frame com as series de tempo dos grupos de receitas usados no calculo do resultado estrutural.
##' Todos os valores estão em termos reais em logaritmo neperiano.
##'
##' @format Data.frame com 13 variáveis
##' \describe{
##'   \item{TRT}{Série de tempo dos tributos sobre renda do trabalho}
##'   \item{TFP}{Série de tempo dos tributos sobre seguridade social}
##'   \item{TRC}{Série de tempo dos tributos sobre renda do capital}
##'   \item{TI}{Série de tempo dos tributos indiretos}
##'   \item{TM}{Série de tempo dos tributos sobre importação}
##'   \item{TGC}{Série de tempo dos tributos sobre ganhos de capital}
##'   \item{ROY}{Série de tempo dos royalties de petróleo}
##'   \item{PE}{Série de tempo das participações especiais (na produção de petróleo)}
##'   \item{TRAN}{Série de tempo das transferências constitucionais da União para os entes federativos}
##'   \item{ICMS}{Série de tempo do Imposto sobre Circulação de Mercadorias e Serviços}
##'   \item{ISS}{Série de tempo do Imposto sobre Serviços}
##'   \item{Hpib}{Série de tempo do Hiato do Produto}
##'   \item{Hpet}{Série de tempo do Hiato do Petróleo}
##'}
#"Receita.Real"
