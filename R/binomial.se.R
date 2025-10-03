#' Derivation notes
#'
#' For a series of Bernoulli obervations, Y_i,
#' the variance of the observations is given by
#'
#' $$Var(Y) = \frac{\sum(\left( Y_i - \mu_Y \right)^2)}{n-1}$$
#'
#' where $\mu_Y$ is the mean of the observations.
#'
#' However, if the data is structured as Binomial observations, $P_j = \frac{k_j}{n_j}$
#' ($n_j$ trials, $k_j$  successes, and $r_j$ failures) we need to weight the
#' observations by the number of trials, $n_j$ to understand the underlying Bernoulli
#' process.
#'
#' First, to determine the mean of the underlying Bernoulli process:
#'
#' $$\mu_y = \frac{\sum_i Y_i}{n} = \frac{\sum_j k_j}{n}$$
#'
#' i.e. the sum of observed successes divided by the total number of trials.
#'
#' Then, we can sub-divide the sum of squares into the sum of squares
#' of the successes and the sum of squares of the failures:
#'
#' $$Var(Y) = \frac{\sum(\left( Y_i - \mu_Y \right)^2)}{n-1} $$
#' $$ = \frac{\sum_j k_j (1-\mu_Y)^2 + r_j (0-\mu_Y)^2}{n-1} $$
#' $$ = \frac{\sum_j k_j (1-\mu_Y)^2+ r_j \mu_Y^2}{n-1} $$
#'
#' We follow  simialr process to enumerate the variance of the fitted values, $\hat{y}$.
#' The mean of the fitted values is:
#'
#' $$\mu_{\hat{y}} = \frac{\sum_j n_j \hat{y}_j}{n}$$
#'
#' and the variance is:
#'
#' $$Var(\hat{Y}) = \frac{\sum_j n_j (\hat{y}_j - \mu_{\hat{y}})^2}{n-1}$$
#'

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
