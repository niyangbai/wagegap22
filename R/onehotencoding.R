#' One hot encoding
#'
#' @param dataset A data set
#' @param column A one hot encoded column
#'
#' @return A data set with one hot encoded variables
#' @export
#'
#' @examples
#' onehotencoding(mtcars, "gear")
onehotencoding <- function(dataset, column) {
  uniq <- sort(unique(dataset[[column]]))
  df <- matrix(NA, nrow = nrow(dataset), ncol = length(uniq), dimnames = list(seq(1, nrow(dataset)), paste0("is_", uniq)))
  for (i in 1:length(uniq)) {
    df[, i] <- dataset[[column]] == uniq[i]
  }
  df <- cbind(dataset, df)
  return(df)
}
