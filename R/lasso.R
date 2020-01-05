#' Optimizarea l1-regularizata Lasso
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul initial de ponderi
#' @param eta este rata de invatare
#' @param lambda este constanta utilizata in algoritm pentru intervalul centrat de relevanta
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' lasso(X, y, rep(0, dim(X)[2]), eta = 1e-6, lambda = 10, eps = 1e-5, verbose = TRUE)
lasso <- function(X, y, w, eta, lambda, eps, verbose) {
  if (verbose == TRUE) {
    minimum <- c()
  }

  rho = rep(0, dim(X)[2])

  while (TRUE)
  {
    rho <- rho + eta * t(X) %*% (y - X %*% w)

    wn <- rep(0, dim(X)[2])
    for (j in 1:dim(X)[2])
    {
      if (rho[j] < -lambda / 2) {
        wn[j] = rho[j] + lambda / 2
      } else if (rho[j] <= lambda / 2) {
        wn[j] = 0
      } else {
        wn[j] = rho[j] - lambda / 2
      }
    }

    gi <- -1/dim(X)[2] * t(X) %*% (y - X %*% w)

    if (verbose == TRUE) {
      minimum <- c(minimum, gi)
      # print(norm(gi, "2"))
    }

    if (norm(gi, "2") < eps)
    {
      break;
    }

    w <- wn
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "Lasso")
  }

  return(w)
}
