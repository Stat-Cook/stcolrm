#' `sdfun`: Computes standard deviation (SD) of a vector of either a nominal or an ordinal or a numeric variable
#'
#' `sdfun` computes SD for nominal, ordinal or ratio/interval scaled variables.
#' For any given set of values (dataframe), SD values are returned.
#'
#' @param col a character, factor or numeric vector of values representing data.
#'
#' @details `sdfun` produces standard deviations for the data. If the data are nominal then standard deviation of
#' total variation is returned unlike 'computesd' function. If the data are ordinal then the standard deviation
#' returned is calculated using 'Agresti' method.
#'
#' @return `sdfun` returns standard deviations for the data.
#'
#' @importFrom stats na.omit
#' @keywords internal
#'
#'
#' @examples
#' x1 <- c("b","c","c","a","a","a","b","c","b","a")
#' x2 <- rnorm(10,35,5)
#' x3 <- as.ordered(c(1,4,1,3,4,2,4,3,3,4))
sdfun <- function(col) {
  # sdfun(x1); sdfun(x2); sdfun(x3)
  if (is.numeric(col)) {
    return(sd(col))
  } else if (is.factor(col) || is.logical(col) || is.character(col) & (length(unique(na.omit(col))) == 2)==TRUE) {
    # Nominal scale: SD calculation
    modcol <- factor(col)
    p <- mean(modcol == levels(modcol)[1])
    varNSV <- p*(1-p)
    #varNSV <- 1 - sum(prop.table(table(col))^2)
    return(sqrt(varNSV))
  } else if (is.factor(col) || is.logical(col) || is.character(col) & (length(unique(na.omit(col))) == 2)==FALSE) {
    # Nominal scale: SD calculation
    varNSV <- 1 - sum(prop.table(table(col))^2)
    return(sqrt(varNSV))
  } else if (is.ordered(col)) {
    col <- as.numeric(col)
    prtab <- prop.table(table(col))
    muOSV <- sum(as.numeric(names(prtab)) * prtab)
    varOSV <- sum((as.numeric(names(prtab)) - muOSV)^2 * prtab)
    return(sqrt(varOSV))
  } else {
    return(NA)
  }
}
