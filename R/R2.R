R2.KFS <- function(kfs) {
  v             <- rstandard(kfs)
  diffuse_phase <- is.na(v)
  diffuse_phase[1] <- TRUE # Ignore first period prediction error even on non diffuse models
  v             <- kfs$v[!diffuse_phase]
  y             <- kfs$mod$y[!diffuse_phase]
  SSR           <- sum((v - mean(v))^2)
  SST           <- sum((y - mean(y))^2)
  return(1 - SSR/SST)
}
