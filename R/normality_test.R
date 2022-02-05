#' Normality test
#' 
#' Test the normality condition for the residuals
#' 
#' @param u residues matrix
#' @return Normality Test Dataframe
#' @keywords internal
normality_test <- function(u) {
  u <- u[!is.na(u)]
  n <- length(u)
  assimetry <- (sum((u - mean(u))^3)/n)/(sum((u - mean(u))^2)/n)^(3/2)
  kurtosis  <- n * sum((u - mean(u))^4)/(sum((u - mean(u))^2)^2)
  nt <- n*(assimetry^2/6 + (kurtosis - 3)^2/24)

  data.frame(Normality.test=nt, Normality.critico=qchisq(0.95,2), pvalue=1-pchisq(nt,2))
}

