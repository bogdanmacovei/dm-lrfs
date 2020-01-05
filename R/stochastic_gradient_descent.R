#' Optimizarea stochastica pentru Gradient Descent
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul initial de ponderi
#' @param eta este rata de invatare
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' stochastic_gradient_descent(X, y, rep(0, dim(X)[2]), eta = 1e-6, eps = 1e-5, verbose = TRUE)
stochastic_gradient_descent <- function(X, y, w, eta, eps, verbose) {
  if (verbose == TRUE) {
    minimum = c()
  }

  while (TRUE) {
    i <- sample(1:dim(X)[1], 1)
    X_local <- X[i, ]
    y_local <- y[i]
    wn <- w + 2/dim(X)[1] * eta * (y_local - X_local * w) * X_local
    w <- wn

    RSSw <- -1/dim(X)[2] * t(X) %*% (y - X %*% w)

    if (verbose == TRUE) {
      minimum = c(minimum, RSSw)
      # print(norm(RSSw, "2"))
    }

    if (abs(norm(RSSw, "2")) < eps)
    {
      break;
    }
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "SGD")
  }

  return(w)
}
