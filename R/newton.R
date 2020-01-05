#' Optimizarea Newton cu Hessiana fixata
#'
#' @param X este setul de date
#' @param y este labelul asociat
#' @param w este vectorul de ponderi
#' @param lambda este constanta care asigura inversabilitatea hessianei, este strict pozitiva
#' @param eps este performanta asteptata a antrenarii
#' @param verbose este parametrul care, setat la TRUE, ofera detalii in timp real despre implementare, dar numai pentru vectori din X bidimensionali
#' @example
#' newton(X, y, rep(0, dim(X)[2]), lambda = 50, eps = 1e-6, verbose = TRUE)
newton <- function(X, y, w, lambda, eps, verbose) {
  if (verbose == TRUE) {
    minimum = c()
  }

  H <- -1/4 * t(X) %*% X - lambda * diag(dim(X)[2])
  while(TRUE)
  {
    gi <- -1/dim(X)[2] * t(X) %*% (y - X %*% w)

    wn <- w + solve(H) %*% gi

    if (verbose == TRUE) {
      minimum = c(minimum, gi)
      # print(norm(gi, "2"))
    }

    if (norm(gi, "2") < eps)
    {
      break;
    }

    w <- wn
  }

  if (verbose == TRUE) {
    plot_for_verbose(X[, 2:dim(X)[2]], y, w, minimum, "Newton")
  }

  return(w)
}
