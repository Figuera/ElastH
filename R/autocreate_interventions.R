autocreate_interventions <- function(model) {
  y     <- model$mod$y
  new_interventions <- search_interventions(model)

  # LOOP REPEAT:
  # Este loop repete o processo de detecção e avaliação de intervenções se
  # houver intervenções concomitantes, ou seja, intervenções que estão
  # distantes entre si em no máximo um período.  Esse processo de repetição
  # visa impedir que intervenções significantes sejam avaliadas como não
  # significantes por estar correlacionada com outra intervenção muito
  # próxima.
  # Antes definir número máximo de interações (para evitar loops infinitos)
  for(i in 1:10) {
    # new_interventions is a dataframe with the intervenctions as regression vectors
    X     <- as.data.frame(cbind(model$mod$X, new_interventions)) # Add interventions to regression dataframe

    message("Criando intervencoes...")
    ssm       <- build_ssm(y, X, variances = model$varpars, a1 = model$a1pars)
    ssm       <- update_fit(model$fit$par,ssm)
    new_model <- KFAS::KFS(ssm, smoothing=c("state", "signal", "disturbance"))

    t <- Sys.time()
    message("Avaliando Intervencoes")
    n_old_intervs <- sum(grepl("(I.principal|I.nivel|I.incli)\\d*", colnames(model$mod$X)))
    tests <- test_intervs(new_model)
    tests <- tests[(n_old_intervs+1):nrow(tests), ]

    if(all(tests$pvalue <= 0.05)) {
      break
    } else {
      rejectionBool <- tests$pvalue > 0.05
      rejected      <- tests[rejectionBool, , drop=F]
      validated     <- new_interventions[, !rejectionBool, drop=F]

      if(nrow(rejected) == 0) {
        break
      } else {
        second_chance <- apply(rejected, 1, check_neighborhood, rejected, y)

        if(sum(second_chance) == 0) {
          X   <- as.data.frame(cbind(model$mod$X, validated))
          ssm <- build_ssm(y, X, variances = model$varpars, a1 = model$a1pars)
          break
        } else {
          new_interventions <- cbind(validated, new_interventions[, rejectionBool, drop=F][, second_chance, drop=F])
          # Aqui termina o loop repeat iniciado acima. Porém não é lançado o break
          # de forma que mais uma interação é realizada.
        }
      }
    }
  }

  fit       <- fitSSM2(ssm, model$fit$par)
  new_model <- KFAS::KFS(fit$model, smoothing=c("state", "signal", "disturbance"))

  return(new_model)
}
