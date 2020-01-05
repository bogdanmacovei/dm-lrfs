#' Optimizarea l1-regularizata Lasso
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul initial de ponderi
#' @param alpha este rata de invatare
#' @param beta1 este rata de descompunere exponentiala pentru primul moment estimat (valoarea recomandata este 0.9)
#' @param beta2 este rata de descompunere exponentiala pentru al doilea moment si este indicat sa fie apropiata de 1.0
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' adam(X, y, rep(0, dim(X)[2]), alpha = 0.01, beta1 = 0.9, beta2 = 0.999, eps = 1e-3, verbose = TRUE)
adam <- function(X, y, w, alpha, beta1, beta2, eps, verbose) {
  if (verbose == TRUE) {
    minimum = c()
  }

  m0 = 0
  v0 = 0
  t = 0

  while (TRUE)
  {
    t <- t + 1
    rand.index <- sample(1:dim(X)[1], 1)
    gt <- -2 * X[rand.index,] * (y[rand.index] - X[rand.index,] * w)
    mt <- beta1 * m0 + (1 - beta1) * gt
    vt <- beta2 * v0 + (1 - beta2) * gt^2
    mth <- mt / (1 - beta1^t)
    vth <- vt / (1 - beta2^t)
    wn <- w - alpha * mth / (sqrt(vth) + eps)

    RSSw <- -1/dim(X)[1] * t(X) %*% (y - X %*% wn)

    if (verbose == TRUE) {
      minimum = c(minimum, RSSw)
      # print(norm(RSSw, "2"))
    }

    if (norm(RSSw, "2") < eps)
    {
      break;
    }

    m0 <- mt
    v0 <- vt
    w <- wn
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "Adam")
  }

  return(w)
}
