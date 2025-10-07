print.stcolrm <- function(object, ...) {
  #' @exportS3Method
  #' @importFrom rlang list2

  dots <- list2(...)
  args <- get_all_with_default(dots, digits=3, print.gap=2, quote=FALSE)

  .coef <- object$standardised_coefficients
  if (length(.coef)) {
    cat(sprintf("Standardized Coefficients (%s)\n", object$method))

    do.call(print.default, c(list(.coef), args))
  }
  else
    cat("No coefficients\n\n")

  invisible(object)
}

get_all_with_default <- function(object, ...){
  #' @importFrom purrr imap
  dots <- list2(...)

  .result <- imap(
    dots,
    ~ get_with_default(object, .y, .x)
  )

  extra_args <- object[setdiff(names(object), names(dots))]
  c(.result, extra_args)
}

get_with_default <- function(x, key, default = NULL) {
  if (key %in% names(x)) {
    return(x[[key]])
  } else {
    return(default)
  }
}


summary.stcolrm <- function(object, ci.type = c("std", "raw"), ...) {
  #' @exportS3Method

  ci.type <- match.arg(ci.type)
  .coefs <- object$standardised_coefficients

  tibble(Variable = names(.coefs), StdCoef = .coefs)
}
