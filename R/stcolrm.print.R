print.stcolrm <- function(object, digits=3L){
  #' @exportS3Method
  .coef <- object$standardised_coefficients
  if (length(.coef)) {
    cat(sprintf(
      "Standardized Coefficients (%s)\n",
      object$method
    ))
    print.default(format(.coef, digits = digits),
                  print.gap = 2, quote = FALSE)
  }
  else cat("No coefficients\n\n")

  invisible(object)
}

summary.stcolrm <- function(object, ci.type=c("std", "raw"), ...){
  #' @exportS3Method

  ci.type <- match.arg(ci.type)
  .coefs <- object$standardised_coefficients

  tibble(
    Variable = names(.coefs),
    StdCoef = .coefs
  )
}
