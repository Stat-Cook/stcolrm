.sd.y <- function(model){
  .n <- model$prior.weights
  .n.total <- sum(.n)

  .y <- model$y
  cases <- sum(.n*.y)
  mu.y <- cases / .n.total
  (cases * (1-mu.y)^2 + (.n.total - cases) * mu.y^2) / (.n.total - 1)
}

.sd.y.hat <- function(model) {
  .n <- model$prior.weights
  .n.total <- sum(.n)

  y.hat <- model$fitted.values
  mu.y.hat <- sum(y.hat*.n) / .n.total
  sum(.n*(y.hat - mu.y.hat)^2) / (.n.total - 1)
}
