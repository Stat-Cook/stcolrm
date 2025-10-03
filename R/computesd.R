#' `computesd`: Computes standard deviation (SD) by variable type for a data frame
#'
#' `computesd` computes standard deviation for nominal, ordinal or ratio/interval scaled variables.
#' For a data frame with a variety of data types in the columns standard deviation values are returned.
#'
#' @param df a data frame of character(s), factor(s) or numeric vector(s) of values.
#'
#' @details `computesd` produces a table of standard deviations for all columns in the dataframe. If a column is a nominal variable with p categories
#' then (p-1) standard deviations are returned. If the column is ordinal then the standard deviation is
#' calculated using 'Agresti' method.
#'
#' @return `computesd` returns a table of standard deviations for all columns in the dataframe.
#' @keywords internal
#'
#' @examples
#' x1 <- c("b","c","c","b","b","a","b","c","b","a")
#' x2 <- rnorm(10,35,5)
#' x3 <- as.ordered(c(1,4,1,3,4,2,4,3,3,4))
#' x4 <- as.factor(c("L","M","L","H","H","L","H","M","L","H"))
#' mydat <- data.frame(x1,x2,x3,x4)
computesd <- function(df) {
  # computesd(mydat)
  # a0. Make a copy of the data
  df_mod <- df
  # a1. Check for any ordered factors
  df_mod[sapply(df_mod, is.ordered)] <- lapply(df_mod[sapply(df_mod, is.ordered)], as.numeric)
  # a2. Now use model.matrix
  dfmm <- model.matrix(~ ., data = df_mod)[1, -1]  # Drop intercept if needed (also all rows but 1st)

  # b0. Separate ordered columns from the rest
  ordered_cols <- df[, sapply(df, is.ordered), drop = FALSE]
  other_cols   <- df[, !sapply(df, is.ordered), drop = FALSE]
  # b1. Compute SDs for ordered factors manually
  sds_ordered <- sapply(ordered_cols, sdfun)
  # b2. Expand other columns using model.matrix
  mm <- model.matrix(~ ., data = other_cols)[, -1, drop = FALSE]
  mmcolnames <- colnames(mm)
  mm <- as.data.frame(mm)
  mmX <- as.data.frame(lapply(mm, function(col) if (all(na.omit(col) %in% c(0,1))) as.factor(col) else col))
  # b3. Compute SDs for expanded columns
  sds_others <- sapply(as.data.frame(mmX), sdfun)
  # b4. Combine both sets of SDs
  sds_all <- c(sds_others, sds_ordered)
  # b5. Create result as one-row data frame
  resultTMP <- as.data.frame(as.list(setNames(sds_all, names(sds_all))))

  # c0. Re-order the columns
  result <- resultTMP[, names(dfmm)]

  return(result)
}
