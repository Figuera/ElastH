#' Search of Interventions
#'
#' Look for possible intervetions points in KFS object
#'
#' @param KFS object
#' @return Dataframe with intervention object
#'
#' @importFrom KFAS rstandard
#' @keywords internal
search_interventions <- function(kfs) {
    ret <- list()
    mod <- kfs$model
    posQn <- which(attr(mod, "state_types") == "level")
    posQi <- which(attr(mod, "state_types") == "slope")
    # Detectamos os períodos em que os erros absolutos são maiores que os limites estabelecidos
    lim <- list(principal = if(mod$H[,,1] > exp(-32)) 2.3 else 2.1,
                level  = if(!(length(posQn) > 0))                 NULL
                         else if(mod$Q[posQn,posQn,1] > exp(-32)) 2.5
                         else                                     2.3,
                slope  = if(!(length(posQi) > 0))                 NULL
                         else if(mod$Q[posQi,posQi,1] > exp(-32)) 3
                         else                                     2.8)

    scs  <- data.frame(principal=rstandard(kfs, "pearson"),
                       rstandard(kfs, "state"))
    scs  <- scs[, names(scs) %in% names(lim)]

    ret <- lapply(colnames(scs), get_intervention_position, scs, lim)
    ret <- do.call(cbind, ret)
    colnames(ret) <- paste0("I.", colnames(ret))

    return(ret)
  }
