#' H test
#' 
#' Test for homoscedasticity in the residuals
#'
#' @param u residues
#' @param d number of components
#' @return H test
#'
#' @importFrom stats qf 
#' @keywords internal
h_test <- function(u, d){
  u <- u[!is.na(u)]
  n <- length(u)
  h <- round((n - d)/3)
  if (h < 1) {
    warning("Insufficient degrees of freedom to calculate Homoscedasticity test")
    return(NULL)
  }
  H <- sum(u[(n - h + 1):n]^2)/sum(u[(d+1):(d+h)]^2)
  if (H < 1) H <- 1/H

  data.frame(H.valor=H, H.critico=qf(0.95, h,h), pvalor=1-pf(H, h,h))
}
