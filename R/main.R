#' Functia de antrenare a unui model liniar
#'
#' @param X este setul de date
#' @param y este labelul
#' @param w_init este vectorul initial de ponderi (valoare default)
#' @param optimizer este algoritmul de optimizare utilizat
#' @param learning_rate este rata de invatare pentru algoritmul de optimizare
#' @param model_train_error este performanta pe care o asteptam de la model
#' @param newton_lambda este constanta care asigura regularizare in metoda Newton
#' @param adam_alpha este parametrul alpha pentru algoritmul ADAM
#' @param adam_beta1 este parametrul beta1 pentru algoritmul ADAM
#' @param adam_beta2 este parametrul beta2 pentru algoritmul ADAM
#' @param verbose este parametrul care, setat la TRUE, afiseaza detalii despre antrenare
#' @examples
#' fit_linear_regression(X, y, optimizer = "adam", verbose = TRUE)
fit_linear_regression <- function(X
                                , y
                                , w_init = rep(0, dim(X)[2])
                                , optimizer = "gd"
                                , learning_rate = 1e-3
                                , model_train_error = 1e-5
                                , newton_lambda = 10
                                , adam_alpha = 0.01
                                , adam_beta1 = 0.9
                                , adam_beta2 = 0.999
                                , verbose = FALSE) {

  switch (optimizer,
    "sgd" = {
      w_trained <- stochastic_gradient_descent(X, y, w_init, learning_rate, model_train_error, verbose)
    },
    "gd" = {
      w_trained <- gradient_descent(X, y, w_init, learning_rate, model_train_error, verbose)
    },
    "adagrad" = {
      w_trained <- adagrad(X, y, w_init, learning_rate, model_train_error, verbose)
    },
    "adam" = {
      w_trained <- adam(X, y, w_init, adam_alpha, adam_beta1, adam_beta2, model_train_error, verbose)
    },
    "newton" = {
      w_trained <- newton(X, y, w_init, newton_lambda, model_train_error, verbose)
    }
  )

  return(w_trained)
}

#' Functia de antrenare a unui model Ridge
#'
#' @param X este setul de date
#' @param y este labelul
#' @param w_init este vectorul initial de ponderi (valoare default)
#' @param learning_rate este rata de invatare pentru algoritmul de optimizare
#' @param model_train_error este performanta pe care o asteptam de la model
#' @param lambda este constanta de regularizare pentru regularizarea l2
#' @param verbose este parametrul care, setat la TRUE, afiseaza detalii despre antrenare
#' @examples
#' fit_ridge_regression(X, y, verbose = TRUE)
fit_ridge_regression <- function(X
                               , y
                               , w = rep(0, dim(X)[2])
                               , learning_rate = 1e-3
                               , lambda = 10
                               , model_train_error = 1e-5
                               , verbose = FALSE) {
  w_trained <- ridge(X, y, w, learning_rate, lambda, model_train_error, verbose)
  return(w_trained)
}

#' Functia de antrenare a unui model Lasso
#'
#' @param X este setul de date
#' @param y este labelul
#' @param w_init este vectorul initial de ponderi (valoare default)
#' @param learning_rate este rata de invatare pentru algoritmul de optimizare
#' @param model_train_error este performanta pe care o asteptam de la model
#' @param lambda este constanta utilizata in algoritm pentru intervalul centrat de relevanta
#' @param verbose este parametrul care, setat la TRUE, afiseaza detalii despre antrenare
#' @examples
#' fit_lasso_regression(X, y, lambda = 1, verbose = TRUE)
fit_lasso_regression <- function(X
                               , y
                               , w = rep(0, dim(X)[2])
                               , learning_rate = 1e-3
                               , lambda = 1e-2
                               , model_train_error = 1e-5
                               , verbose = FALSE) {
  w_trained <- lasso(X, y, w, learning_rate, lambda, model_train_error, verbose)
  return(w_trained)
}

