#' ElastH: Pacote para calcular as elasticidades de grupos de receita contra
#' hiatos do produto
#'
#' O pacote busca simplificar a estimação de elasticidades dos grupos receitas
#' de interesse para o primário estrutural. Para tanto utiliza-se um modelo
#' estrutural de componentes não observados estimado por filtro de kalman,
#' além de prover as ferramentas para que se possa
#' replicar da melhor forma possível a metodologia da SPE/MF para o cálculo do
#' Resultado Estrutural.
#'
#' @section ElastH - Tutorial Rápido:
#'
#' A forma mais simples de se estimar as elasticidades usando o pacote é por meio
#' da função \code{\link{decompor}}. O resultado da função é uma lista com
#' várias informações, sendo que a mais importante é a variável
#' \code{comp}, que detalha os valores dos componentes. Vide seção
#' \code{\link{decompor}} para mais informações sobre as entradas, as
#' saídas e o funcionamento desta função.
#' 
#' A função \code{\link{decompor.todos}} permite estimar com facilidade um conjunto
#' grande de diferentes hipóteses sobre o comportamento dos componentes não
#' observados do modelo de componentes não observados. Para mais informações sobre a
#' função ver seção \code{\link{decompor.todos}}, para mais informações sobre os
#' componentes não observados ver a seção detalhes da função
#' \code{\link{decompor}}.
#'
#' A função \code{\link{calcular.elasticidades}} faz uso de
#' \code{\link{decompor.todos}}, para calcular as elasticidades de todo o conjunto
#' de 11 grupos de receitas usados na metodologia de resultado estrutural da
#' SPE/MF, inclusive já fazendo uso dos mesmos períodos iniciais que os usados
#' pela SPE/MF. Vide exemplo abaixo e seção:
#' \code{\link{calcular.elasticidades}} para mais informações.
#'
#' Por fim, para facilitar a leitura e cópia de informações para o excel, ver 
#' função \code{\link{exportar}}. Utilize-a a partir dos resultados das
#' funções decompor.todos ou calcular.elasticidades. Depois de estimados os modelos
#' com a diferentes hipóteses e exportado para o excel, ainda é necessário
#' escolher com base nos testes disponíveis, vide variáveis \code{q, h} e
#' \code{nt} da lista resultante da função \code{\link{decompor}}. As melhores
#' hipóteses sobre o comportamento dos componentes não observados, e logo a
#' melhor elasticidade.
#'
#' @seealso
#' \code{\link{decompor}}
#' \code{\link{calcular.elasticidades}}
#'
#' @name ElastH-package
#' @docType package
NULL
