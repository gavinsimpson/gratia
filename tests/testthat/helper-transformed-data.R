transformed_data <- function() {
  withr::with_seed(42, {
    d <- data.frame(x = runif(150, 1, 5), z = runif(150, 1, 3),
      g = factor(rep(letters[1:3], 50)), w = runif(150, 0.5, 2))
    d$y <- sin(log(d$x)) + log(d$z) + rnorm(150, sd = 0.1)
    d
  })
}
