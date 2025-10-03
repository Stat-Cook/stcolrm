standardise_coefs <- function(object) {
  #' @aliases standardize_coefs
  UseMethod("standardise_coefs")
}


standardise_coefs.simplepreSTD <- function(object) {
  #' @exportS3Method
  with(object, list(standardise_coefs = RawCoefs / StdErr, scfac = 1))
}

standardise_coefs.simplepostEST <- function(object) {
  #' @exportS3Method
  with(object, {
    .scfac <- StdDev
    list(standardise_coefs = RawCoefs * .scfac, scfac = .scfac)
  })
}

standardise_coefs.partial <- function(object) {
  #' @exportS3Method
  with(object, {
    .scfac <- StdDev / SDYHat
    list(standardise_coefs = RawCoefs * .scfac, scfac = .scfac)
  })
}


standardise_coefs.full <- function(object) {
  #' @exportS3Method
  with(object, {
    if (!YBinary) {
      stop("Full standardisation requires a binary (0/1) response variable.")
    }
    .scfac <- StdDev / SDY

    list(standardise_coefs = RawCoefs * .scfac, scfac = .scfac)
  })
}



