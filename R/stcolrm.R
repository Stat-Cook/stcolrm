# NB: suggested name 'beta_weight'
# Standardised coefficients for logistic regression
# The 'Co-Stand': Coefficient Standardizer
#



#' `stcolrm`: Calculates the standardised coefficients after fitting a logistic regression model
#'
#' `stcolrm` is used to calculate the standardised coefficients after fitting a logistic regression model for a wide
#' variety of explanatory variables.
#'
#' @param object an object of class "formula"  (a symbolic description of the model to be fitted) or 'glm' (a pre-trained binomial model)
#' @param method the method ("simple-preSTD", "simple-postEST", "partial", "full") to be used for estimating the standardised coefficients after fitting the model.
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param conf_level a numeric value between 0 and 1 giving the confidence level for the intervals (default is 0.95).
#' @param coef_conversion a function to convert the coefficients (default is exp for odds ratios).
#'
#' @details stcolrm produces standardised coefficients after fitting a logistic regression model for a wide variety of explanatory variables. It also shows the base results for `glm` fit, and also shows the odds ratios of the estimated coefficients. The odds ratios for standardised coefficients are calculated using delta method.
#'
#' @return A numeric vector of standardised coefficients of the fitted logistic regression model.
#'
#' @importFrom stats glm coef fitted binomial vcov model.matrix qnorm sd setNames
#'
#' @export
#'
#' @examples
#' library(kmed)
#' heartdata <- heart
#' heartdata$classN <- ifelse(heart$class == 0, 0, 1)
#' mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg, data=heartdata, "simple-preSTD")
#' mfit
#'
#' mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg, data=heartdata, "simple-postEST")
#' mfit
#'
#' mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg, data=heartdata, "partial")
#' mfit
#'
#' mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg, data=heartdata, "full")
#' mfit
#'
stcolrm <- function(object,
                    method = c("simple-preSTD", "simple-postEST", "partial", "full"),
                    data,
                    conf_level = 0.95,
                    coef_conversion = exp) {
  #' @export
  UseMethod("stcolrm")
}


stcolrm.formula <- function(formula,
                            method = c("simple-preSTD", "simple-postEST", "partial", "full"),
                            data,
                            conf_level = 0.95,
                            coef_conversion=exp) {
  #' @exportS3Method
  # Fit logistic regression model
  model <- glm(formula, data = data, family = binomial())

  stcolrm(model, data, method = method, conf_level = conf_level)
}
