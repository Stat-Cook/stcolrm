stcolrm_plot <- function(object,
                         .type = c("std", "raw"),
                         log_10=TRUE){
  #' Visualisation of standardised or raw odds ratios from a stcolrm object
  #'
  #' @param object A stcolrm object
  #' @param .type Type of odds ratios to plot, either "std" for standardised or "raw" for raw odds ratios. Default is "std".
  #' @param log_10 Logical, if TRUE (default) the x-axis is on a log10 scale.
  #'
  #' @return A ggplot2 object visualising the odds ratios with confidence intervals.
  #'
  #' @importFrom ggplot2 ggplot aes geom_point geom_errorbar scale_x_log10
  #'
  #' @export
  .type <- match.arg(.type)
  type.keys <- c(std = "odds_ratios_std",
                 raw = "odds_ratios_raw")
  .key <- type.keys[.type]

  .data <- object[[.key]]

  .data$Variable <- rownames(.data)

  Variable <- OR <- CI_lower <- CI_upper <- NA

  p <- .data %>%
    ggplot(aes(
      y = Variable, x = OR, xmin=CI_lower, xmax=CI_upper
    )) + geom_point() + geom_errorbar()

  if (log_10){
    p <- p + scale_x_log10()
  }
  p
}


