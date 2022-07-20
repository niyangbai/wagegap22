#' Get unemployment data
#'
#' @description Get unemployment data from U.S. Bureau of Labor Statistics website
#'
#' @param urls The list of urls <example.xlsx> where unemployment data are stored.
#'
#' @return The unemployment dataset
#' @export
#'
#' @examples
#' get_unemp_from_bls("https://www.bls.gov/lau/laucnty20.xlsx")
get_unemp_from_bls <- function(urls) {
  df <- data.frame()
  for (i in urls) {
    data <- read.xlsx(i, startRow = 5)
    data <- data[-c(nrow(data):(nrow(data) - 1)),]
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  return(df)
}
