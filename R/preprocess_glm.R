preprocess_glm <- function(object){
  #'
  #' @importFrom stats var
  #'
  # Extract response and fitted values
  formula <- object$formula
  response_name <- all.vars(formula)[1]
  explanatory_set <- all.vars(formula)[-1]

  y_binary <- all(object$y %in% 0:1)
  y <- object$y
  y_hat <- fitted(object)

  .model.matrix <- model.matrix(object)
  .col.bool <- colnames(.model.matrix) != "(Intercept)"
  .model.matrix <- .model.matrix[,.col.bool, drop=F]

  # Calculate standard deviations of coded variables
  sds <- sqrt(diag(var(
    .model.matrix
  )))

  # Extract coefficients
  coefs <- coef(object)
  .summary <- summary(object)
  std.err <- .summary$coefficients[,2]

  vcov_matrix <- vcov(object)
  predictor_names <- colnames(.model.matrix)  # Exclude intercept NB: may not always have an intercept.
  raw_coefs <- coefs[predictor_names]
  std.err <- std.err[predictor_names]

  list(
    RawCoefs = raw_coefs,
    StdErr = std.err,
    StdDev = sds,
    SDY = .sd.y(object),
    SDYHat = .sd.y.hat(object),
    YBinary = y_binary
  )
}
