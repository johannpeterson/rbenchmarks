permutation_result <- function(obs, p, samples = NULL) {
  obj <- list(
    p.value = p,
    observed = obs,
    samples = samples
  )
  class(obj) <- "permutation_result"
  return(obj)
}

permutation.test <- function(x, y, FUN, n = 999, return.samples = TRUE) {
  # https://thomasleeper.com/Rcourse/Tutorials/permutationtests.html
  # https://mac-theobio.github.io/QMEE/lectures/permutation_examples.notes.html
  # This currently only does 1-sided tests with p based on obs > sample
  size = length(y)
  obs <- FUN(x, y)
  samples <- replicate(
    n,
    FUN(x, y[sample(size)])
  )
  return( 
    permutation_result(
      p = mean(obs > samples),
      obs = obs,
      samples = if(return.samples) samples else NULL
    )
  )
}

plot.permutation_result <- function(obj, ...) {
  # https://stackoverflow.com/questions/29289046/r-ecdf-over-histogram
  if(is.null(obj$samples)) stop("No samples to plot in permutation_result object.")
  
  h <- hist(obj$samples, ...)
  abline(v = obj$observed, col = "blue", lwd = 2)
  par(new = T)
  plot(ecdf(obj$samples), 
       axes = F, 
       xlab = NA, 
       ylab = NA, 
       main = NA, 
       col = "red",
       ...)
  axis(4, 
       at=seq(from = 0, to = 1, length.out = 11), 
       tick = TRUE,
       labels=seq(0, 1, 0.1), 
       col = 'red', 
       col.axis = 'red')
  abline(h = obj$p.value, lty = 2, col = "red")
  mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')
  usr <- par("usr")
  text(x = usr[2], 
       y = obj$p.value, 
       sprintf("p = %f", obj$p.value), 
       adj = c(1.1, 1.2), 
       col = "red")
  text(x = obj$observed,
       y = 1,
       sprintf("observed = %f", obj$observed),
       adj = c(0, -0.2),
       col = "blue"
       )
}
