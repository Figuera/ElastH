ElastH: Pacote R para calcular as elasticidades de grupos de receita contra
hiatos do produto
==========================================================================


O pacote busca simplificar a estimação de elasticidades dos grupos receitas
de interesse para o primário estrutural. Para tanto utiliza-se um modelo
estrutural de componentes não observados estimado por filtro de kalman,
além de prover as ferramentas para que se possa
replicar da melhor forma possível a metodologia da SPE/MF para o cálculo do
Resultado Estrutural.


ElastH - Instalação
--------------------------------------------------------------------------

O jeito mais fácil e recomendado de instalar o pacote é pelo comando:

```R
install.packages("ElastH")
```

Que instalará a versão disponível no CRAN. Também é possível instalar a versão
disponibilizada no Github, utilizando os seguintes comandos:

```R
install.packages("devtools")
library(devtools)
install_github("Figuera/ElastH")
```

ElastH - Vinheta
--------------------------------------------------------------------------

Caso se queira aprender a utilizar este pacote o meio mais recomendado é
pela a vinheta disponibilida com o pacote, e que pode ser acessada pelo
seguinte comando:

```R
vignette("ElastH")
```

ElastH - Tutorial Rápido
--------------------------------------------------------------------------

A forma mais simples de se estimar as elasticidades usando o pacote é por meio
da função `decompor`. O resultado da função é uma lista com
várias informações, sendo que a mais importante é a variável
`comp`, que detalha os valores dos componentes. Vide
`help(decompor)` para mais informações sobre as entradas, as
saídas e o funcionamento desta função.

A função `decompor.todos` permite estimar com facilidade um conjunto
grande de diferentes hipóteses sobre o comportamento dos componentes não
observados do modelo de componentes não observados. Vide `help(decompor.todos)`

A função `calcular.elasticidades` faz uso de
`decompor.todos`, para calcular as elasticidades de todo o conjunto
de 11 grupos de receitas usados na metodologia de resultado estrutural da
SPE/MF, inclusive já fazendo uso dos mesmos períodos iniciais que os usados
pela SPE/MF. Vide `help(calcular.elasticidades)` para mais informações.

Por fim, para facilitar a leitura e cópia de informações para o excel, ver 
função `exportar`. Utilize-a a partir dos resultados das
funções decompor.todos ou calcular.elasticidades. Depois de estimados os modelos
com a diferentes hipóteses e exportado para o excel, ainda é necessário
escolher com base nos testes disponíveis, vide variáveis `q, h` e
`nt` da lista resultante da função `decompor`. As melhores
hipóteses sobre o comportamento dos componentes não observados, e logo a
melhor elasticidade.
