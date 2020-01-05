#' Optimizarea batch Gradient Descent
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul de ponderi
#' @param eta este rata de invatare
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' gradient_descent(X, y, rep(0, dim(X)[2]), 1e-6, 1e-5, verbose = TRUE)
gradient_descent <- function(X, y, w, eta, eps, verbose) {
  if (verbose == TRUE) {
    minimum = c()
  }

  while (TRUE) {
    RSSw = -1/dim(X)[2] * t(X) %*% (y - X %*% w)
    wn <- w + 2 * eta * t(X) %*% (y - X %*% w)

    if (verbose == TRUE) {
      minimum = c(minimum, RSSw)
      # print(norm(RSSw, "2"))
    }

    if (norm(RSSw, "2") < eps) {
      break;
    }
    w <- wn
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "GD")
  }

  return(w)
}
