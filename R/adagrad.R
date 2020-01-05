#' Optimizarea stochastica AdaGrad
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul de ponderi
#' @param eta este rata de invatare
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' adagrad(X, y, rep(0, dim(X)[2]), 1e-6, 1e-5, verbose = TRUE)
adagrad <- function(X, y, w, eta, eps, verbose) {
  if (verbose == TRUE) {
    minimum <- c()
  }

  g = 0
  while(TRUE)
  {
    index <- sample(1:dim(X)[1], 1)
    gi <- -1/dim(X)[2] * X[index, ] * (y[index] - X[index, ] * w)
    g <- g + gi ^ 2
    wn <- w - eta * gi / (sqrt(g) + eps)

    RSSw <- -1/dim(X)[2] * t(X) %*% (y - X %*% wn)

    if (verbose == TRUE) {
      minimum <- c(minimum, RSSw)
      # print(norm(RSSw, "2"))
    }

    if (norm(RSSw, "2") < eps)
    {
      break;
    }

    w <- wn
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "AdaGrad")
  }

  return(w)
}
