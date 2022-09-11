#' Get ACS Data
#'
#' @description Get American Community Survey (ACS) Data from censusapi.
#'
#' @param table_name The ACS table name of the dataset
#' @param period The time period of the dataset
#' @param data_code The ACS Code of the variable
#'
#' @return ACS Data
#' @export
#'
#' @examples
#' get_acs5_data("acs/acs5", 2010:2015, c("B20017A", "B20017B"))
get_acs5_data <- function(table_name, period, data_code) {
  df <- data.frame()
  for (year in period) {
    data <- getCensus(
      name = table_name,
      vintage = year,
      vars = data_code,
      region = "county:*")
    data$year <- year
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  df$fips <- paste0(df$state, df$county)
  return(df)
}

#' Get ACS Distrubution Data
#'
#' @param table_name The ACS table name of the dataset
#' @param period The time period of the dataset
#' @param data_code The ACS Code of the variable
#'
#' @return ACS Data
#' @export
#'
get_acs5_distrubution <- function(table_name, period, data_code) {
  distrubution <- data.frame()
  for (year in period) {
    data <- getCensus(
      name = table_name,
      vintage = year,
      vars = unlist(data_code),
      region = "county:*")
    data$fips <- paste0(data$state, data$county)
    data$year <- year
    data$ID <- paste0(data$year, data$fips)
    df <- data["ID"]
    x <- ncol(df)
    for (i in 1:nrow(data_code)) {
      df[,(i+x)] <- rowSums(data[,data_code[i,]])
    }
    if (nrow(distrubution) == 0) {
      distrubution <- df
    } else {
      distrubution <- rbind(distrubution, df)
    }
  }
  return(distrubution)
}
