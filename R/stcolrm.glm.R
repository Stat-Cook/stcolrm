stcolrm.glm <- function(object,
                        method = c("simple-preSTD", "simple-postEST", "partial", "full"),
                        data = NA,
                        conf_level = 0.95,
                        coef_conversion=exp) {
  #'
  #'
  #' @exportS3Method
  method <- match.arg(method)
  z_value <- qnorm(1 - (1 - conf_level) / 2)

  if (object$family$family != "binomial") {
    warning("Currently, the package only supports logistic regression models (family='binomial').")
    # readline("Press [enter] to continue or [esc] to stop.")
  }

  preproc.list <- preprocess_glm(object)
  class(preproc.list) <- c(gsub("-", "", method), "list")

  standardise.list <- standardise_coefs(preproc.list)

  standardised_coefs <- standardise.list$standardise_coefs
  scfac <- standardise.list$scfac

  result <- list(
    model = object,
    method = method,
    conf_level = conf_level,
    standardised_coefficients = standardised_coefs
  )

  result$odds_ratios_raw <-  odds_ratios.f(preproc.list$RawCoefs,
                                           preproc.list$StdErr,
                                           z_value)

  result$odds_ratios_std <-  odds_ratios.f(standardised_coefs,
                                           scfac * preproc.list$StdErr,
                                           z_value)

  class(result) <- c("stcolrm", "list")

  return(result)
}


