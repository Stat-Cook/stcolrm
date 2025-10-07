#' Synthetic Data Set 1
#'
#' A synthetic data set to validate stcolrm.  It has been generated
#' such that `Var1` and `Var2` have no effect on the binomial response `y`,
#' while `Var3` has a positive effect.
#'
#' @format A data frame with 1000 rows and 5 variables:
#' #' \describe{
#' \item{Var1}{A numeric predictor variable}
#' \item{Var2}{A numeric predictor variable}
#' \item{Var3}{A numeric predictor variable}
#' \item{n}{A numeric variable representing sample size for each observation}
#' \item{y}{A binomial response variable (0/1) of `n` trials}
#' }
"synth1"
