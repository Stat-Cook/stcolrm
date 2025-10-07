{
  set.seed(205)
  n <- 2000
  synth1 <- data.frame(
    Var1 = rnorm(n),
    Var2 = rnorm(n),
    Var3 = rnorm(n)
  )

  beta.vec <- c(0, 0, 1)

  eta <- -0.5 + as.matrix(synth1) %*% beta.vec
  eta.noise <- rnorm(n, eta, 0.1)
  synth1$n <- sample(5:15, 100, T)
  p <- binomial()$linkinv(eta.noise)
  synth1$y <- rbinom(n, synth1$n, p)
}

usethis::use_data(synth1, overwrite = TRUE)
