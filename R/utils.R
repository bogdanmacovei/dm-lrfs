plot_for_verbose <- function(x, y, w, minimum, method) {
  par(mfrow = c(1, 2))

  plot(x, y, col = "blue",
       main = method,
       cex = 0.7,
       xlab = "Input",
       ylab = "Output")
  grid()

  lines(x, w[1] + x * w[2], lwd = 2, col = "red")

  plot(minimum, type = "l", col = "blue",
       main = paste(method, "- convergenta gradientului"),
       xlab = "Iteratia",
       ylab = "Minimul")

  grid()
}
