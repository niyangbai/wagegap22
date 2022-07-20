#' Get Bea Data
#'
#' @description Get data from U.S. Bureau of Economic Analysis (BEA) with API
#'
#' @param beaKey API key
#' @param table_name The table name of the data
#' @param line_code The line of the data
#' @param period The time period of the data
#'
#' @return A data set from BEA
#' @export
#'
#' @examples
#' get_bea_data("abcdefghijklmn", "CAGDP1", 1, 2010:2015)
get_bea_data <- function(beaKey, table_name, line_code, period) {
  df <- data.frame()
  for (i in period) {
    gdp <- list(
      "UserID" = beaKey,
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = table_name,
      "GeoFips" = "COUNTY",
      "LineCode" = line_code,
      "Year" = i
    )
    data <- beaGet(gdp, asWide = FALSE)
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  return(df)
}
