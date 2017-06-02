#' Calculo de Choques
#'
#' Função retorna os choques, ou disturbios, nas equações principais e nas 
#' equações de transição
#'
#' @param kf Objeto do tipo dlmFiltered. Ver \code{dlmFilter}
#'
#' @return lista com os choques e seus desvios padrão
#'   \itemize{
#'     \item \code{ch} : Matrix com choques com a seguinte ordem: Choque Principal,
#'       Choque no Nível, Choque na Inclinação
#'     \item \code{sd} : Matrix com desvios padrão, segue mesma ordem que \code{ch}
#'   }
#'
#' @importFrom stats residuals
#' @keywords internal
choques <- function(kf) {
  y  <- matrix(kf$y)
  n  <- nrow(y)
  p  <- ncol(y)
  m  <- length(kf$mod$m0)

  jdlm <- jdlm(kf$mod, n)
  
  v <- residuals(kf, sd=F, type="raw")
  P <- dlm::dlmSvd2var(kf$U.R,kf$D.R)
  H <- lapply(1:n, function(t) jdlm$FF[[t]]%*%P[[t]]%*%t(jdlm$FF[[t]]) + jdlm$V[[t]])
  K <- lapply(1:n, function(t) jdlm$GG[[t]]%*%P[[t]]%*%t(jdlm$FF[[t]])%*%solve(H[[t]]))
  L <- lapply(1:n, function(t) jdlm$GG[[t]] - K[[t]]%*%jdlm$FF[[t]])

  r <- rep(list(matrix(0,m,p)), n)
  for(i in n:2) r[[i-1]] <- t(jdlm$FF[[i]])%*%solve(H[[i]])%*%v[[i]] + t(L[[i]])%*%r[[i]]

  eta <- lapply(1:n, function(t) jdlm$W[[t]]%*%r[[t]])

  N  <- rep(list(matrix(0,m,m)), n)
  for(i in n:2) N[[i-1]] <- t(jdlm$FF[[i]])%*%solve(H[[i]])%*%jdlm$FF[[i]] + t(L[[i]])%*%N[[i]]%*%L[[i]]

  vareta <- lapply(1:n, function(t) jdlm$W[[t]]%*%N[[t]]%*%jdlm$W[[t]])

  u      <- lapply(1:n, function(t) solve(H[[t]])%*%v[t] - t(K[[t]])%*%r[[t]])
  eps    <- lapply(1:n, function(t) jdlm$V[[t]]%*%u[[t]])

  DD     <- lapply(1:n, function(t) solve(H[[t]]) + t(K[[t]])%*%N[[t]]%*%K[[t]]) 
  vareps <- lapply(1:n, function(t) jdlm$V[[t]]%*%DD[[t]]%*%jdlm$V[[t]])


  posc   <- posicoes(kf$mod)$norm
  posc   <- posc[names(posc) %in% c("Nivel", "Inclinacao")]

  eps     <- do.call(cbind, lapply(1:nrow(eps[[1]]), function(i) sapply(eps, "[", i, 1)))
  colnames(eps) <- "Choque"
  eta     <- do.call(cbind, lapply(1:nrow(eta[[1]]), function(i) sapply(eta, "[", i, 1)))[ , posc, drop=F]
  colnames(eta) <- paste0("C.", names(posc))
  choques <- cbind(eps, rbind(NA, eta[-nrow(eta), , drop=F]))
  choques <- ts(choques, start=start(kf$y), frequency=frequency(kf$y))

  vareps <- do.call(cbind, lapply(1:nrow(vareps[[1]]), function(i) sapply(vareps, "[", i, i)))
  colnames(vareps) <- "Choque"
  vareta <- do.call(cbind, lapply(1:nrow(vareta[[1]]), function(i) sapply(vareta, "[", i, i)))[, posc, drop=F]
  colnames(vareta) <- paste0("C.", names(posc))
  var <- cbind(vareps, rbind(NA, vareta[-nrow(vareta), , drop=F]))
  var <- ts(var, start=start(kf$y), frequency=frequency(kf$y))

  return(list(ch=choques, sd=sqrt(var)))
}
