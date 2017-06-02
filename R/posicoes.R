#' Posicoes dos componentes
#' 
#' Função que detecta a partir de um objeto dlm as posições dos componentes
#' 
#' @param dlm Objeto dlm
#' @return Vetor com as posições dos componentes no objeto dlm. names(ret): nome dos componentes existentes
#' @keywords internal
posicoes <-
  function(dlm) {
    ret <- list()

    # Loop por todos os componentes do objeto dlm
    for(i in 1:length(dlm$m0)){
      # Se FF = 1 então o componente afeta a equação principal
      # Componente pode ser 'Nivel', 'Sazon', 'Coef', ou 'Residuo'
      # 'Resíduo' é uma intervenção na equação principal
      if(dlm$FF[i] == 1){
        # Se JFF != 0 então a variável não é constante
        # Componente então pode ser 'Coef' ou 'Residuo' (intervenção na equação principal)
        if(!is.null(dlm$JFF) && dlm$JFF[i] != 0){
          X <- dlm$X[, dlm$JFF[i]]
          # Se o max e o sum da variável independente é igual a 1, temos uma intervenção
          # Senão, um Coeficiente
          if(max(X) == 1 && sum(X) == 1) {
            ret[[paste0("Choque",dlm$JFF[i])]] <- i
          } else {
            ret[[paste0("Coef",dlm$JFF[i])]] <- i
          }
        # Se JFF = 0, e GG = 1, o componente é fixo e autorregressivo e logo é o 'Nivel'  
        } else if (dlm$GG[i,i] == 1){
          ret$Nivel <- i
        # Se JFF = 0, e GG ~ 0 (mas diferente de 0), o componente é a Sazonalidade  
        } else if (floor(dlm$GG[i,i]*10e16) == 6){
          ret$Sazon <- i
        }
      # Se FF != 0 então o componente não afeta a equação principal
      # Logo, podemos ter um componente de Inclinacao ou de intervenção secundária
      } else {
        # Se JGG !=0 componente não é contaste, logo é uma intervenção secundária
        if(!is.null(dlm$JGG) && sum(dlm$JGG[,i]) != 0){
          posc <- max(dlm$JGG[,i])
          X <- dlm$X[, posc]
          # Checa se de fato é uma intervenção
          if(max(X) == 1 && sum(X) == 1) {
            # Se a intervenção afeta o componente na posição do Nível, intervenção de Nível
            if(which.max(dlm$JGG[,i]) == ret$Nivel) {
              ret[[paste0("C.Nivel", posc)]] <- i
            # Se a intervenção afeta o componente na posição do Inclinacao, intervenção de Inclinacao
            } else if(which.max(dlm$JGG[,i]) == ret$Inclinacao) {
              ret[[paste0("C.Inclinacao", posc)]] <- i
            }
          # Se não for intervenção, algo está errado  
          } else {
            ret$isso.nao.deveria.acontecer <- i
          }
        # Se JGG = 0 e GG = 1, temos uma variável constante e autorregressiva, Inclinacao
        } else if(dlm$GG[i,i] == 1){
          ret$Inclinacao <- i
        }
      }
    }

    int <- unlist(ret[grepl("Choque|C\\.", names(ret))]) #Intervenções
    norm <- unlist(ret[!grepl("Choque|C\\.", names(ret))]) #Não Intervenções

    return(list(int=int, norm=norm))
  }
