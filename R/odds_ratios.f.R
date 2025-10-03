odds_ratios.f <- function(coef, stderr, z_value, coef_conversion = exp) {
  lower.boundary <- coef - z_value * stderr
  upper.boundary <- coef + z_value * stderr

  data.frame(
    OR = coef_conversion(coef),
    CI_lower = coef_conversion(lower.boundary),
    CI_upper = coef_conversion(upper.boundary)
  )
}


