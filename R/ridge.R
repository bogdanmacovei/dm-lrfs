#' Optimizarea l2-regularizata Ridge
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul initial de ponderi
#' @param eta este rata de invatare
#' @param lambda este constanta de regularizare si este strict pozitiva
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' ridge(X, y, rep(0, dim(X)[2]), eta = 1e-6, lambda = 10, eps = 1e-5, verbose = TRUE)
ridge <- function(X, y, w, eta, lambda, eps, verbose) {
  if (verbose == TRUE) {
    minimum <- c()
  }

  while(TRUE)
  {
    RSSw <- -1/dim(X)[2] * t(X) %*% (y - X %*% w) + 1/dim(X)[2] * lambda * w
    wn <- w + 2 * eta * t(X) %*% (y - X %*% w) - 2 * eta * lambda * w

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
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "Ridge")
  }

  return(w)
}
